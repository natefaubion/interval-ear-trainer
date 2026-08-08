module EarTrainer.Component.Practice
  ( Input
  , Output(..)
  , Query
  , component
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import EarTrainer.Audio as Audio
import EarTrainer.Config
  ( AnswerDisplay(..)
  , ExerciseConfig
  , GhostMode(..)
  , QuizMode(..)
  , QuizProgression(..)
  , quizModeUsesRecognition
  , quizModeUsesSinging
  )
import EarTrainer.Music
  ( Accidental(..)
  , Interval
  , Pitch(..)
  , PitchClass(..)
  , intervalName
  , pitchFromMidiLike
  , playbackModeName
  )
import EarTrainer.Notation as Notation
import EarTrainer.PitchDetection as Detection
import EarTrainer.Quiz as Quiz
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.DOM.Element as Element
import Web.HTML.HTMLElement as HTMLElement

data CaptureStatus
  = ReadyToPlay
  | PlayingAudio PlaybackDestination
  | Listening
  | CaptureFailed String
  | PlaybackFailed String
  | IntervalError Progression
  | ChoosingAnswer (Array Interval)
  | AnswerComplete (Array Interval) Progression

data PlaybackDestination
  = BeginSinging
  | ResumeAnswers (Array Interval)

data Progression
  = AwaitingInput
  | Scheduled

derive instance Eq CaptureStatus
derive instance Eq PlaybackDestination
derive instance Eq Progression

type State =
  { activityRevision :: Int
  , captureStatus :: CaptureStatus
  , choices :: Array Quiz.IntervalChoice
  , config :: ExerciseConfig
  , ghostFiber :: Maybe H.ForkId
  , ghostMidi :: Maybe Int
  , monitor :: Maybe Detection.Monitor
  , playbackFiber :: Maybe H.ForkId
  , prompt :: Quiz.Prompt
  , progressionFiber :: Maybe H.ForkId
  , recognition :: Detection.Recognition
  , sampler :: Audio.Sampler
  }

data Action
  = Initialize
  | Finalize
  | PlayPrompt
  | PlaybackStarted Int
  | AudioFailed Int String
  | StartListening
  | PitchDetected Int Detection.PitchSample
  | ClearGhost
  | FinishSinging
  | MicrophoneFailed Int String
  | ChooseInterval Interval
  | NextPrompt
  | AdvanceAutomatically
  | RetryAutomatically
  | EditSetup

notationRef :: H.RefLabel
notationRef = H.RefLabel "prompt-notation"

practiceContentRef :: H.RefLabel
practiceContentRef = H.RefLabel "practice-content"

choiceNotationRef :: Int -> H.RefLabel
choiceNotationRef index = H.RefLabel ("choice-notation-" <> show index)

type Input =
  { config :: ExerciseConfig
  , sampler :: Audio.Sampler
  , seed :: Int
  }

data Output = BackToSetup

data Query :: Type -> Type
data Query a

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval:
        H.mkEval H.defaultEval
          { handleAction = handleAction
          , initialize = Just Initialize
          , finalize = Just Finalize
          }
    }
  where
  initialState input =
    let
      prompt = Quiz.makePrompt input.seed input.config
    in
      { activityRevision: 0
      , captureStatus: ReadyToPlay
      , choices: Quiz.makeChoices input.seed input.config prompt
      , config: input.config
      , ghostFiber: Nothing
      , ghostMidi: Nothing
      , monitor: Nothing
      , playbackFiber: Nothing
      , prompt: prompt
      , progressionFiber: Nothing
      , recognition: Detection.initialRecognition
      , sampler: input.sampler
      }

  render :: State -> H.ComponentHTML Action () m
  render = renderPractice

  renderPractice state =
    HH.section
      [ HP.class_ (H.ClassName "practice-card") ]
      [ HH.div
          [ HP.class_ (H.ClassName "practice-toolbar") ]
          [ HH.div_
              [ HH.h2_ [ HH.text (quizModeTitle state.config.quizMode) ] ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ H.ClassName "secondary-button", H.ClassName "icon-button" ]
              , HP.attr (H.AttrName "aria-label") "Setup"
              , HP.title "Setup"
              , HE.onClick \_ -> EditSetup
              ]
              [ HH.span
                  [ HP.class_ (H.ClassName "settings-icon")
                  , HP.attr (H.AttrName "aria-hidden") "true"
                  ]
                  []
              ]
          ]
      , HH.div
          [ HP.ref practiceContentRef
          , HP.class_ (H.ClassName "practice-content")
          ]
          [ HH.div
              [ HP.class_ (H.ClassName "notation-panel") ]
              [ HH.div
                  [ HP.ref notationRef
                  , HP.class_ (H.ClassName "notation-canvas")
                  ]
                  []
              , if shouldShowIntervalName state then
                  HH.p
                    [ HP.class_ (H.ClassName "completed-interval-name") ]
                    [ HH.text (promptIntervalLabel state) ]
                else
                  HH.text ""
              , renderPitchMeter state
              , renderIntervalChoices state
              ]
          ]
      , HH.footer
          [ HP.class_ (H.ClassName "practice-actions") ]
          [ HH.p_
              [ HH.text (practiceInstruction state) ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "primary-button play-button")
              , HP.disabled (footerButtonDisabled state)
              , HE.onClick \_ -> footerButtonAction state
              ]
              [ HH.span
                  [ HP.class_ (H.ClassName "play-icon") ]
                  [ HH.text (footerButtonIcon state) ]
              , HH.text (footerButtonLabel state)
              ]
          ]
      ]

  renderPitchMeter state
    | state.config.quizMode == RecognitionOnly = HH.text ""
    | not state.config.showPitchTuner = HH.text ""
    | isIntervalError state.captureStatus = HH.text ""
    | isResumingAnswers state.captureStatus = HH.text ""
    | isChoosingAnswer state.captureStatus || isAnswerComplete state.captureStatus = HH.text ""
    | otherwise =
        HH.div_
          [ HH.div
              [ HP.class_ (H.ClassName "pitch-feedback") ]
              [ HH.span
                  [ HP.class_ (H.ClassName "tuner-readout") ]
                  [ HH.text (feedbackName state.recognition.feedback) ]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "tuner-track") ]
              [ HH.div [ HP.class_ (H.ClassName "tuner-center") ] []
              , case state.recognition.feedback of
                  Nothing -> HH.text ""
                  Just feedback ->
                    HH.div
                      [ HP.classes
                          ( [ H.ClassName "tuner-dot" ]
                              <> if feedbackInRange feedback then [ H.ClassName "in-range" ] else []
                          )
                      , HP.style ("left: " <> show (feedbackPosition feedback.cents) <> "%")
                      ]
                      []
              ]
          ]

  renderIntervalChoices state
    | not (quizModeUsesRecognition state.config.quizMode) = HH.text ""
    | not (isChoosingAnswer state.captureStatus)
        && not (isAnswerComplete state.captureStatus)
        && not (isResumingAnswers state.captureStatus) = HH.text ""
    | otherwise =
        HH.section
          [ HP.class_ (H.ClassName "answer-panel") ]
          [ HH.div
              [ HP.class_ (H.ClassName "answer-grid") ]
              (Array.mapWithIndex (renderIntervalChoice state) state.choices)
          ]

  renderIntervalChoice state index choice =
    let
      revealed = Array.elem choice.interval (revealedChoices state.captureStatus)
      correct = revealed && choice.interval == state.prompt.interval
      incorrect = revealed && not correct
      showNotation = state.config.answerDisplay /= AnswerName
      showName = state.config.answerDisplay /= AnswerNotation || revealed
      resultClasses =
        if correct then [ H.ClassName "answer-correct" ]
        else if incorrect then [ H.ClassName "answer-incorrect" ]
        else []
    in
      HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes
            ( [ H.ClassName "interval-answer" ]
                <>
                  ( if state.config.answerDisplay == AnswerName then [ H.ClassName "name-only" ]
                    else []
                  )
                <>
                  ( if state.config.answerDisplay /= AnswerNotation then [ H.ClassName "names-visible" ]
                    else []
                  )
                <> resultClasses
            )
        , HP.disabled (isAnswerComplete state.captureStatus || isPlaying state.captureStatus)
        , HE.onClick \_ -> ChooseInterval choice.interval
        ]
        [ if revealed then
            HH.span
              [ HP.classes
                  [ H.ClassName "choice-result-icon"
                  , H.ClassName if correct then "result-correct" else "result-incorrect"
                  ]
              ]
              []
          else
            HH.text ""
        , if showNotation then
            HH.div
              [ HP.ref (choiceNotationRef index)
              , HP.class_ (H.ClassName "choice-notation")
              ]
              []
          else
            HH.text ""
        , HH.span
            [ HP.class_ (H.ClassName "choice-label") ]
            [ HH.text if showName then intervalName choice.interval else "Interval hidden" ]
        ]

  footerButtonAction state =
    if isAnswerComplete state.captureStatus then NextPrompt else PlayPrompt

  footerButtonDisabled state =
    progressionScheduled state.captureStatus
      || isPlaying state.captureStatus
      || (state.captureStatus == Listening && state.recognition.phase == Detection.RecognitionComplete)

  footerButtonLabel state
    | isPlaying state.captureStatus = "Playing…"
    | isAnswerComplete state.captureStatus = "Next interval"
    | state.captureStatus == Listening && state.recognition.phase == Detection.RecognitionComplete = "Next interval"
    | state.config.quizMode == Audiation = "Play root"
    | otherwise = "Play interval"

  footerButtonIcon state
    | isPlaying state.captureStatus = ""
    | footerButtonLabel state == "Next interval" = "→"
    | otherwise = "▶"

  practiceInstruction state = case state.captureStatus of
    ReadyToPlay ->
      if state.config.quizMode == Audiation then
        "Listen to the root, then sing the interval."
      else if quizModeUsesSinging state.config.quizMode then
        "Listen to the interval, then sing."
      else
        "Listen to the interval, then choose."
    PlayingAudio _ -> "Listen carefully."
    Listening -> Detection.phaseInstruction state.recognition.phase
    CaptureFailed message -> "Microphone unavailable: " <> message
    PlaybackFailed message -> "Audio playback failed: " <> message
    IntervalError _ -> "Incorrect pitch."
    ChoosingAnswer revealed ->
      if Array.null revealed then
        "Choose the matching interval."
      else
        "Not quite. Try Again."
    AnswerComplete _ _ -> "Correct!"

  isPlaying = case _ of
    PlayingAudio _ -> true
    _ -> false

  isResumingAnswers = case _ of
    PlayingAudio (ResumeAnswers _) -> true
    _ -> false

  isIntervalError = case _ of
    IntervalError _ -> true
    _ -> false

  isChoosingAnswer = case _ of
    ChoosingAnswer _ -> true
    _ -> false

  isAnswerComplete = case _ of
    AnswerComplete _ _ -> true
    _ -> false

  revealedChoices = case _ of
    ChoosingAnswer revealed -> revealed
    AnswerComplete revealed _ -> revealed
    PlayingAudio (ResumeAnswers revealed) -> revealed
    _ -> []

  progressionScheduled = case _ of
    IntervalError Scheduled -> true
    AnswerComplete _ Scheduled -> true
    _ -> false

  feedbackName = case _ of
    Nothing -> "—"
    Just feedback ->
      let
        cents = Int.round feedback.cents
      in
        if cents >= -3 && cents <= 3 then
          "◆ 0¢"
        else if cents > 0 then
          "↓ " <> show cents <> "¢"
        else
          "↑ " <> show (-cents) <> "¢"

  feedbackPosition cents = 50.0 + max (-50.0) (min 50.0 cents)

  quizModeTitle SingingOnly = "Singing"
  quizModeTitle RecognitionOnly = "Recognition"
  quizModeTitle SingingAndRecognition = "Singing & Recognition"
  quizModeTitle Audiation = "Audiation"

  shouldShowIntervalName state =
    state.config.quizMode == Audiation
      || (state.config.quizMode == SingingOnly && isAnswerComplete state.captureStatus)

  promptIntervalLabel state =
    intervalName state.prompt.interval
      <>
        if state.config.quizMode == Audiation || state.config.quizMode == SingingOnly then
          " · " <> playbackModeName state.prompt.mode
        else ""

  feedbackInRange feedback =
    feedback.clarity >= Detection.defaultRecognitionSettings.clarityThreshold
      && feedback.cents >= -Detection.defaultRecognitionSettings.toleranceCents
      && feedback.cents <= Detection.defaultRecognitionSettings.toleranceCents

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      renderPromptNotation state.prompt false
      when (state.config.quizProgression == AutomaticProgression) do
        handleAction PlayPrompt
    Finalize -> do
      state <- H.get
      cancelDelayedActions state
      stopMonitor state.monitor
      H.liftEffect (Audio.stop state.sampler)
    PlayPrompt -> do
      state <- H.get
      cancelDelayedActions state
      stopMonitor state.monitor
      let
        activityRevision = state.activityRevision + 1
        destination = case state.captureStatus of
          ChoosingAnswer revealed -> ResumeAnswers revealed
          _ -> BeginSinging
      H.modify_ _
        { activityRevision = activityRevision
        , captureStatus = PlayingAudio destination
        , ghostFiber = Nothing
        , ghostMidi = Nothing
        , monitor = Nothing
        , playbackFiber = Nothing
        , progressionFiber = Nothing
        , recognition = Detection.initialRecognition
        }
      renderPromptNotation state.prompt
        (isChoosingAnswer state.captureStatus && quizModeUsesSinging state.config.quizMode)
      { emitter, listener } <- H.liftEffect HS.create
      void (H.subscribe emitter)
      if state.config.quizMode == Audiation then
        H.liftEffect $ Audio.playRoot state.sampler state.prompt.root
          (HS.notify listener (PlaybackStarted activityRevision))
          (HS.notify listener <<< AudioFailed activityRevision)
      else
        H.liftEffect $ Audio.playInterval state.sampler state.prompt.mode state.prompt.root state.prompt.target
          (HS.notify listener (PlaybackStarted activityRevision))
          (HS.notify listener <<< AudioFailed activityRevision)
    PlaybackStarted activityRevision -> do
      state <- H.get
      when
        ( isPlaying state.captureStatus
            && state.activityRevision == activityRevision
        )
        do
          cancelFiber state.playbackFiber
          fiber <- H.fork do
            let
              playbackMilliseconds =
                if state.config.quizMode == Audiation then Audio.rootPlaybackDurationMilliseconds
                else Audio.playbackDurationMilliseconds state.prompt.mode
            H.liftAff (delay (Milliseconds (playbackMilliseconds + 350.0)))
            handleAction StartListening
          H.modify_ _ { playbackFiber = Just fiber }
    AudioFailed activityRevision message -> do
      state <- H.get
      when (state.activityRevision == activityRevision) do
        cancelFiber state.playbackFiber
        H.modify_ _ { captureStatus = PlaybackFailed message, playbackFiber = Nothing }
    StartListening -> do
      state <- H.get
      H.modify_ _ { playbackFiber = Nothing }
      case state.captureStatus of
        PlayingAudio (ResumeAnswers revealed) -> do
          H.modify_ _ { captureStatus = ChoosingAnswer revealed }
          renderChoiceNotation state.prompt.root state.choices
        PlayingAudio BeginSinging
          | not (quizModeUsesSinging state.config.quizMode) -> do
              H.modify_ _ { captureStatus = ChoosingAnswer [] }
              renderChoiceNotation state.prompt.root state.choices
        PlayingAudio BeginSinging -> do
          { emitter, listener } <- H.liftEffect HS.create
          void (H.subscribe emitter)
          monitor <- H.liftEffect $ Detection.start
            (HS.notify listener <<< PitchDetected state.activityRevision)
            (HS.notify listener <<< MicrophoneFailed state.activityRevision)
          H.modify_ _ { captureStatus = Listening, monitor = Just monitor }
        _ -> pure unit
    PitchDetected activityRevision sample -> do
      state <- H.get
      when (state.captureStatus == Listening && state.activityRevision == activityRevision) do
        let
          detectedMidi =
            if sample.frequency > 0.0 then Just (Detection.nearestMidi sample.frequency) else Nothing
          next = Detection.stepRecognition
            Detection.defaultRecognitionSettings
            state.config.octavePolicy
            state.prompt.root
            state.prompt.target
            sample
            state.recognition
          detectedGhost =
            if state.config.ghostMode == GhostOff then Nothing
            else map (Detection.relativeMidi state.config.octavePolicy state.prompt.root next) detectedMidi
          nextGhost = case detectedGhost of
            Nothing -> state.ghostMidi
            Just midi -> Just midi
          completed =
            state.recognition.phase /= Detection.RecognitionComplete
              && next.phase == Detection.RecognitionComplete
          incorrect =
            state.recognition.phase /= Detection.RecognitionIncorrect
              && next.phase == Detection.RecognitionIncorrect
          firstAccepted = next.firstMidi /= Nothing
          firstJustAccepted = state.recognition.firstMidi == Nothing && firstAccepted
        when (detectedGhost /= Nothing) do
          cancelFiber state.ghostFiber
        H.modify_ _
          { ghostFiber = if detectedGhost == Nothing then state.ghostFiber else Nothing
          , ghostMidi = nextGhost
          , recognition = next
          }
        when ((detectedGhost /= Nothing && detectedGhost /= state.ghostMidi) || firstJustAccepted) do
          case nextGhost of
            Just midi ->
              let
                spellingReference = case state.prompt.root, state.prompt.target of
                  Pitch (PitchClass _ (Accidental rootAccidental)) _,
                  Pitch (PitchClass _ (Accidental targetAccidental)) _ ->
                    if rootAccidental /= 0 then state.prompt.root
                    else if targetAccidental /= 0 then state.prompt.target
                    else state.prompt.root
              in
                renderGhostNotation state.prompt (pitchFromMidiLike spellingReference midi) firstAccepted
            Nothing -> renderPromptNotation state.prompt firstAccepted
        when
          ( state.config.ghostMode == GhostOn
              && detectedGhost == Nothing
              && state.ghostMidi /= Nothing
          )
          do
            cancelFiber state.ghostFiber
            fiber <- H.fork do
              H.liftAff (delay (Milliseconds 700.0))
              handleAction ClearGhost
            H.modify_ _ { ghostFiber = Just fiber }
        when completed do
          cancelFiber state.ghostFiber
          stopMonitor state.monitor
          H.modify_ _ { ghostFiber = Nothing, monitor = Nothing }
          handleAction FinishSinging
        when incorrect do
          cancelFiber state.ghostFiber
          stopMonitor state.monitor
          H.modify_ _
            { captureStatus = IntervalError AwaitingInput
            , ghostFiber = Nothing
            , monitor = Nothing
            , recognition = next
            }
          case next.feedback of
            Just feedback -> do
              let
                midi = Detection.relativeMidi state.config.octavePolicy state.prompt.root next feedback.midi
                spellingReference = case state.prompt.root, state.prompt.target of
                  Pitch (PitchClass _ (Accidental rootAccidental)) _,
                  Pitch (PitchClass _ (Accidental targetAccidental)) _ ->
                    if rootAccidental /= 0 then state.prompt.root
                    else if targetAccidental /= 0 then state.prompt.target
                    else state.prompt.root
              renderIncorrectNotation
                state.prompt
                (pitchFromMidiLike spellingReference midi)
                firstAccepted
            Nothing -> pure unit
          scheduleAutomaticRetry state
    ClearGhost -> do
      state <- H.get
      H.modify_ _ { ghostFiber = Nothing }
      when
        ( state.captureStatus == Listening
            && state.ghostMidi /= Nothing
        )
        do
          when (state.config.ghostMode == GhostOn) do
            H.modify_ _ { ghostMidi = Nothing }
            renderPromptNotation state.prompt (state.recognition.phase /= Detection.WaitingForFirst)
    FinishSinging -> do
      state <- H.get
      when
        ( state.captureStatus == Listening
            && state.recognition.phase == Detection.RecognitionComplete
        )
        do
          let persistGhost = state.config.ghostMode == GhostPersist
          if not (quizModeUsesRecognition state.config.quizMode) then do
            H.modify_ _ { captureStatus = AnswerComplete [] AwaitingInput, ghostMidi = Nothing }
            renderCompletedNotation state.prompt
            scheduleAutomaticAdvance state
          else do
            H.modify_ _
              { captureStatus = ChoosingAnswer []
              , ghostMidi = if persistGhost then state.ghostMidi else Nothing
              }
            unless persistGhost (renderPromptNotation state.prompt true)
            renderChoiceNotation state.prompt.root state.choices
    MicrophoneFailed activityRevision message -> do
      state <- H.get
      when (state.activityRevision == activityRevision) do
        cancelFiber state.ghostFiber
        H.modify_ _ { captureStatus = CaptureFailed message, ghostFiber = Nothing, monitor = Nothing }
    ChooseInterval interval -> do
      state <- H.get
      case state.captureStatus of
        ChoosingAnswer previous
          | not (Array.elem interval previous) -> do
              case Array.find (\choice -> choice.interval == interval) state.choices of
                Just choice ->
                  H.liftEffect $ Audio.playInterval state.sampler state.prompt.mode state.prompt.root choice.target
                    (pure unit)
                    (\_ -> pure unit)
                Nothing -> pure unit
              let
                correct = interval == state.prompt.interval
                revealed = Array.snoc previous interval
              H.modify_ _
                { captureStatus =
                    if correct then AnswerComplete revealed AwaitingInput
                    else ChoosingAnswer revealed
                }
              when correct do
                renderCompletedNotation state.prompt
                scheduleAutomaticAdvance state
        _ -> pure unit
    NextPrompt -> do
      state <- H.get
      cancelDelayedActions state
      H.liftEffect (Audio.stop state.sampler)
      seed <- H.liftEffect (randomInt 0 2147483647)
      let
        prompt = Quiz.makePrompt seed state.config
        choices = Quiz.makeChoices seed state.config prompt
      H.modify_ _
        { activityRevision = state.activityRevision + 1
        , captureStatus = ReadyToPlay
        , choices = choices
        , ghostFiber = Nothing
        , ghostMidi = Nothing
        , playbackFiber = Nothing
        , prompt = prompt
        , progressionFiber = Nothing
        , recognition = Detection.initialRecognition
        }
      resetPracticeScroll
      renderPromptNotation prompt false
    AdvanceAutomatically -> do
      state <- H.get
      H.modify_ _ { progressionFiber = Nothing }
      when
        ( progressionScheduled state.captureStatus
            && isAnswerComplete state.captureStatus
        )
        do
          handleAction NextPrompt
          handleAction PlayPrompt
    RetryAutomatically -> do
      state <- H.get
      H.modify_ _ { progressionFiber = Nothing }
      when
        (state.captureStatus == IntervalError Scheduled)
        do
          handleAction PlayPrompt
    EditSetup -> do
      state <- H.get
      cancelDelayedActions state
      stopMonitor state.monitor
      H.liftEffect (Audio.stop state.sampler)
      H.raise BackToSetup

  stopMonitor = case _ of
    Nothing -> pure unit
    Just monitor -> H.liftEffect (Detection.stop monitor)

  cancelFiber = case _ of
    Nothing -> pure unit
    Just fiber -> H.kill fiber

  cancelDelayedActions state = do
    cancelFiber state.ghostFiber
    cancelFiber state.playbackFiber
    cancelFiber state.progressionFiber

  resetPracticeScroll = do
    maybeElement <- H.getHTMLElementRef practiceContentRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement -> H.liftEffect (Element.setScrollTop 0.0 (HTMLElement.toElement htmlElement))

  scheduleAutomaticAdvance state =
    when (state.config.quizProgression == AutomaticProgression) do
      currentState <- H.get
      cancelFiber currentState.progressionFiber
      H.modify_ \current -> current
        { captureStatus = case current.captureStatus of
            AnswerComplete revealed _ -> AnswerComplete revealed Scheduled
            status -> status
        }
      fiber <- H.fork do
        let
          waitMilliseconds =
            if not (quizModeUsesRecognition state.config.quizMode) then 1200.0
            else Audio.playbackDurationMilliseconds state.prompt.mode + 500.0
        H.liftAff (delay (Milliseconds waitMilliseconds))
        handleAction AdvanceAutomatically
      H.modify_ _ { progressionFiber = Just fiber }

  scheduleAutomaticRetry state =
    when (state.config.quizProgression == AutomaticProgression) do
      currentState <- H.get
      cancelFiber currentState.progressionFiber
      H.modify_ \current -> current
        { captureStatus = case current.captureStatus of
            IntervalError _ -> IntervalError Scheduled
            status -> status
        }
      fiber <- H.fork do
        H.liftAff (delay (Milliseconds 1200.0))
        handleAction RetryAutomatically
      H.modify_ _ { progressionFiber = Just fiber }

  renderPromptNotation prompt rootAccepted = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect (Notation.renderPrompt (HTMLElement.toElement htmlElement) prompt.root prompt.target rootAccepted)

  renderCompletedNotation prompt = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect (Notation.renderCompleted (HTMLElement.toElement htmlElement) prompt.root prompt.target)

  renderGhostNotation prompt detected rootAccepted = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect (Notation.renderGhost (HTMLElement.toElement htmlElement) prompt.root prompt.target detected rootAccepted)

  renderIncorrectNotation prompt detected rootAccepted = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect
          (Notation.renderIncorrect (HTMLElement.toElement htmlElement) prompt.root prompt.target detected rootAccepted)

  renderChoiceNotation root choices =
    for_ (Array.mapWithIndex (\index choice -> { choice, index }) choices) \item -> do
      maybeElement <- H.getHTMLElementRef (choiceNotationRef item.index)
      case maybeElement of
        Nothing -> pure unit
        Just htmlElement ->
          H.liftEffect (Notation.renderIntervalChoice (HTMLElement.toElement htmlElement) root item.choice.target)
