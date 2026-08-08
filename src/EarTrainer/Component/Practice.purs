module EarTrainer.Component.Practice
  ( Input
  , Output(..)
  , Query
  , component
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import EarTrainer.Audio as Audio
import EarTrainer.Capability.Audio as AudioCapability
import EarTrainer.Capability.PitchInput as PitchInput
import EarTrainer.Component.Notation as NotationComponent
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
import EarTrainer.Quiz as Quiz
import EarTrainer.Recognition as Recognition
import EarTrainer.UI.Button as Button
import Effect.Aff (attempt, delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (message)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Web.DOM.Element as Element
import Web.HTML.HTMLElement as HTMLElement

data CaptureStatus
  = ReadyToPlay
  | PlayingAudio PlaybackDestination
  | StartingCapture
  | Listening
  | CaptureFailed String
  | PlaybackFailed String
  | IntervalError
  | ChoosingAnswer (Array Interval)
  | AnswerComplete (Array Interval)

data PlaybackDestination
  = BeginSinging
  | ResumeAnswers (Array Interval)

derive instance Eq CaptureStatus
derive instance Eq PlaybackDestination

type State =
  { captureFiber :: Maybe H.ForkId
  , captureStatus :: CaptureStatus
  , choices :: Array Quiz.IntervalChoice
  , config :: ExerciseConfig
  , ghostFiber :: Maybe H.ForkId
  , ghostMidi :: Maybe Int
  , monitor :: Maybe PitchInput.Monitor
  , observation :: Recognition.Observation
  , playbackFiber :: Maybe H.ForkId
  , prompt :: Quiz.Prompt
  , prompts :: Quiz.PromptSet
  , progressionFiber :: Maybe H.ForkId
  , previewFiber :: Maybe H.ForkId
  , recognition :: Recognition.Recognition
  , sampler :: AudioCapability.Sampler
  }

data Action
  = Initialize
  | Finalize
  | PlayPrompt
  | StartListening
  | AudioFailed String
  | PitchObserved PitchInput.Sample
  | PitchDetected Recognition.PitchSample
  | MicrophoneStarted PitchInput.Monitor
  | ClearGhost
  | FinishSinging
  | MicrophoneFailed String
  | ChooseInterval Interval
  | NextPrompt
  | AdvanceAutomatically
  | RetryAutomatically
  | EditSetup

practiceContentRef :: H.RefLabel
practiceContentRef = H.RefLabel "practice-content"

data NotationSlot
  = PromptNotation
  | ChoiceNotation Int

derive instance Eq NotationSlot
derive instance Ord NotationSlot

type Slots =
  ( notation :: H.Slot NotationComponent.Query Void NotationSlot
  )

notationSlot :: Proxy "notation"
notationSlot = Proxy

type Input =
  { config :: ExerciseConfig
  , prompts :: Quiz.PromptSet
  , sampler :: AudioCapability.Sampler
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
      prompt = Quiz.makePrompt input.seed input.prompts
    in
      { captureFiber: Nothing
      , captureStatus: ReadyToPlay
      , choices: Quiz.makeChoices input.seed input.config prompt
      , config: input.config
      , ghostFiber: Nothing
      , ghostMidi: Nothing
      , monitor: Nothing
      , observation: Recognition.initialObservation
      , playbackFiber: Nothing
      , prompt: prompt
      , prompts: input.prompts
      , progressionFiber: Nothing
      , previewFiber: Nothing
      , recognition: Recognition.initialRecognition
      , sampler: input.sampler
      }

  render :: State -> H.ComponentHTML Action Slots m
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
                  [ HP.class_ (H.ClassName "notation-canvas") ]
                  [ HH.slot_ notationSlot PromptNotation NotationComponent.component (promptNotation state) ]
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
          , Button.button
              { action: footerButtonAction state
              , classes: [ H.ClassName "play-button" ]
              , disabled: footerButtonDisabled state
              , variant: Button.Primary
              }
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
      Button.button
        { action: ChooseInterval choice.interval
        , classes:
            [ H.ClassName "interval-answer" ]
              <>
                ( if state.config.answerDisplay == AnswerName then [ H.ClassName "name-only" ]
                  else []
                )
              <>
                ( if state.config.answerDisplay /= AnswerNotation then [ H.ClassName "names-visible" ]
                  else []
                )
              <> resultClasses
        , disabled: isAnswerComplete state.captureStatus || isBusy state.captureStatus
        , variant: Button.Unstyled
        }
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
              [ HP.class_ (H.ClassName "choice-notation") ]
              [ HH.slot_ notationSlot (ChoiceNotation index) NotationComponent.component
                  (Notation.intervalChoice state.prompt.root choice.target)
              ]
          else
            HH.text ""
        , HH.span
            [ HP.class_ (H.ClassName "choice-label") ]
            [ HH.text if showName then intervalName choice.interval else "Interval hidden" ]
        ]

  footerButtonAction state =
    if isAnswerComplete state.captureStatus then NextPrompt else PlayPrompt

  footerButtonDisabled state =
    hasFiber state.progressionFiber
      || isBusy state.captureStatus
      || (state.captureStatus == Listening && state.recognition.phase == Recognition.RecognitionComplete)

  footerButtonLabel state
    | isPlaying state.captureStatus = "Playing…"
    | state.captureStatus == StartingCapture = "Requesting…"
    | isAnswerComplete state.captureStatus = "Next interval"
    | state.captureStatus == Listening && state.recognition.phase == Recognition.RecognitionComplete = "Next interval"
    | state.config.quizMode == Audiation = "Play root"
    | otherwise = "Play interval"

  footerButtonIcon state
    | isBusy state.captureStatus = ""
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
    StartingCapture -> "Requesting microphone access."
    Listening -> Recognition.phaseInstruction state.recognition.phase
    CaptureFailed message -> "Microphone unavailable: " <> message
    PlaybackFailed message -> "Audio playback failed: " <> message
    IntervalError -> "Incorrect pitch."
    ChoosingAnswer revealed ->
      if Array.null revealed then
        "Choose the matching interval."
      else
        "Not quite. Try Again."
    AnswerComplete _ -> "Correct!"

  isPlaying = case _ of
    PlayingAudio _ -> true
    _ -> false

  isBusy = case _ of
    PlayingAudio _ -> true
    StartingCapture -> true
    _ -> false

  isResumingAnswers = case _ of
    PlayingAudio (ResumeAnswers _) -> true
    _ -> false

  isIntervalError = case _ of
    IntervalError -> true
    _ -> false

  isChoosingAnswer = case _ of
    ChoosingAnswer _ -> true
    _ -> false

  isAnswerComplete = case _ of
    AnswerComplete _ -> true
    _ -> false

  revealedChoices = case _ of
    ChoosingAnswer revealed -> revealed
    AnswerComplete revealed -> revealed
    PlayingAudio (ResumeAnswers revealed) -> revealed
    _ -> []

  hasFiber = case _ of
    Just _ -> true
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
    feedback.clarity >= Recognition.defaultRecognitionSettings.clarityThreshold
      && feedback.cents >= -Recognition.defaultRecognitionSettings.toleranceCents
      && feedback.cents <= Recognition.defaultRecognitionSettings.toleranceCents

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      when (state.config.quizProgression == AutomaticProgression) do
        handleAction PlayPrompt
    Finalize -> do
      state <- H.get
      cancelTasks state
      stopMonitor state.monitor
      H.liftEffect (AudioCapability.stop state.sampler)
    PlayPrompt -> do
      state <- H.get
      cancelTasks state
      stopMonitor state.monitor
      let
        destination = case state.captureStatus of
          ChoosingAnswer revealed -> ResumeAnswers revealed
          _ -> BeginSinging
      H.modify_ _
        { captureFiber = Nothing
        , captureStatus = PlayingAudio destination
        , ghostFiber = Nothing
        , ghostMidi = Nothing
        , monitor = Nothing
        , observation = Recognition.initialObservation
        , playbackFiber = Nothing
        , progressionFiber = Nothing
        , previewFiber = Nothing
        , recognition = Recognition.initialRecognition
        }
      fiber <- H.fork do
        result <- H.liftAff $ attempt
          $ AudioCapability.play state.sampler
          $
            if state.config.quizMode == Audiation then Audio.rootPlan state.prompt.root
            else Audio.intervalPlan state.prompt.mode state.prompt.root state.prompt.target
        case result of
          Left error -> handleAction (AudioFailed (message error))
          Right _ -> do
            H.liftAff (delay (Milliseconds 350.0))
            handleAction StartListening
      H.modify_ _ { playbackFiber = Just fiber }
    AudioFailed failure -> do
      H.modify_ _ { captureStatus = PlaybackFailed failure, playbackFiber = Nothing }
    StartListening -> do
      state <- H.get
      H.modify_ _ { playbackFiber = Nothing }
      case state.captureStatus of
        PlayingAudio (ResumeAnswers revealed) -> do
          H.modify_ _ { captureStatus = ChoosingAnswer revealed }
        PlayingAudio BeginSinging
          | not (quizModeUsesSinging state.config.quizMode) -> do
              H.modify_ _ { captureStatus = ChoosingAnswer [] }
        PlayingAudio BeginSinging -> do
          { emitter, listener } <- H.liftEffect HS.create
          void (H.subscribe emitter)
          H.modify_ _ { captureStatus = StartingCapture }
          fiber <- H.fork do
            result <- H.liftAff $ attempt $ PitchInput.start (HS.notify listener <<< PitchObserved)
            case result of
              Left error -> handleAction (MicrophoneFailed (message error))
              Right monitor -> handleAction (MicrophoneStarted monitor)
          H.modify_ _ { captureFiber = Just fiber }
        _ -> pure unit
    PitchObserved raw -> do
      state <- H.get
      when (state.captureStatus == Listening) do
        let observed = Recognition.observePitch Recognition.defaultCaptureSettings raw state.observation
        H.modify_ _ { observation = observed.observation }
        for_ observed.sample (handleAction <<< PitchDetected)
    PitchDetected sample -> do
      state <- H.get
      when (state.captureStatus == Listening) do
        let
          detectedMidi =
            if sample.frequency > 0.0 then Just (Recognition.nearestMidi sample.frequency) else Nothing
          next = Recognition.stepRecognition
            Recognition.defaultRecognitionSettings
            state.config.octavePolicy
            state.prompt.root
            state.prompt.target
            sample
            state.recognition
          detectedGhost =
            if state.config.ghostMode == GhostOff then Nothing
            else map (Recognition.relativeMidi state.config.octavePolicy state.prompt.root next) detectedMidi
          nextGhost = case detectedGhost of
            Nothing -> state.ghostMidi
            Just midi -> Just midi
          completed =
            state.recognition.phase /= Recognition.RecognitionComplete
              && next.phase == Recognition.RecognitionComplete
          incorrect =
            state.recognition.phase /= Recognition.RecognitionIncorrect
              && next.phase == Recognition.RecognitionIncorrect
        when (detectedGhost /= Nothing) do
          cancelFiber state.ghostFiber
        H.modify_ _
          { ghostFiber = if detectedGhost == Nothing then state.ghostFiber else Nothing
          , ghostMidi = nextGhost
          , recognition = next
          }
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
          let
            incorrectMidi = map
              (Recognition.relativeMidi state.config.octavePolicy state.prompt.root next <<< _.midi)
              next.feedback
          H.modify_ _
            { captureStatus = IntervalError
            , ghostFiber = Nothing
            , ghostMidi = incorrectMidi
            , monitor = Nothing
            , recognition = next
            }
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
    FinishSinging -> do
      state <- H.get
      when
        ( state.captureStatus == Listening
            && state.recognition.phase == Recognition.RecognitionComplete
        )
        do
          let persistGhost = state.config.ghostMode == GhostPersist
          if not (quizModeUsesRecognition state.config.quizMode) then do
            H.modify_ _ { captureStatus = AnswerComplete [], ghostMidi = Nothing }
            scheduleAutomaticAdvance state
          else do
            H.modify_ _
              { captureStatus = ChoosingAnswer []
              , ghostMidi = if persistGhost then state.ghostMidi else Nothing
              }
    MicrophoneStarted monitor -> do
      state <- H.get
      H.modify_ _ { captureFiber = Nothing }
      if state.captureStatus == StartingCapture then
        H.modify_ _ { captureStatus = Listening, monitor = Just monitor }
      else
        H.liftEffect (PitchInput.stop monitor)
    MicrophoneFailed failure -> do
      state <- H.get
      when (state.captureStatus == StartingCapture || state.captureStatus == Listening) do
        cancelFiber state.ghostFiber
        H.modify_ _
          { captureFiber = Nothing
          , captureStatus = CaptureFailed failure
          , ghostFiber = Nothing
          , monitor = Nothing
          }
    ChooseInterval interval -> do
      state <- H.get
      case state.captureStatus of
        ChoosingAnswer previous
          | not (Array.elem interval previous) -> do
              case Array.find (\choice -> choice.interval == interval) state.choices of
                Just choice -> do
                  cancelFiber state.previewFiber
                  fiber <- H.fork do
                    void $ H.liftAff $ attempt $
                      AudioCapability.play state.sampler (Audio.intervalPlan state.prompt.mode state.prompt.root choice.target)
                  H.modify_ _ { previewFiber = Just fiber }
                Nothing -> pure unit
              let
                correct = interval == state.prompt.interval
                revealed = Array.snoc previous interval
              H.modify_ _
                { captureStatus =
                    if correct then AnswerComplete revealed
                    else ChoosingAnswer revealed
                }
              when correct do
                scheduleAutomaticAdvance state
        _ -> pure unit
    NextPrompt -> do
      state <- H.get
      cancelTasks state
      H.liftEffect (AudioCapability.stop state.sampler)
      seed <- H.liftEffect (randomInt 0 2147483647)
      let
        prompt = Quiz.makePrompt seed state.prompts
        choices = Quiz.makeChoices seed state.config prompt
      H.modify_ _
        { captureFiber = Nothing
        , captureStatus = ReadyToPlay
        , choices = choices
        , ghostFiber = Nothing
        , ghostMidi = Nothing
        , playbackFiber = Nothing
        , prompt = prompt
        , progressionFiber = Nothing
        , previewFiber = Nothing
        , observation = Recognition.initialObservation
        , recognition = Recognition.initialRecognition
        }
      resetPracticeScroll
    AdvanceAutomatically -> do
      state <- H.get
      H.modify_ _ { progressionFiber = Nothing }
      when (isAnswerComplete state.captureStatus) do
        handleAction NextPrompt
        handleAction PlayPrompt
    RetryAutomatically -> do
      state <- H.get
      H.modify_ _ { progressionFiber = Nothing }
      when (state.captureStatus == IntervalError) do
        handleAction PlayPrompt
    EditSetup -> do
      state <- H.get
      cancelTasks state
      stopMonitor state.monitor
      H.liftEffect (AudioCapability.stop state.sampler)
      H.raise BackToSetup

  stopMonitor = case _ of
    Nothing -> pure unit
    Just monitor -> H.liftEffect (PitchInput.stop monitor)

  cancelFiber = case _ of
    Nothing -> pure unit
    Just fiber -> H.kill fiber

  cancelTasks state = do
    cancelFiber state.captureFiber
    cancelFiber state.ghostFiber
    cancelFiber state.playbackFiber
    cancelFiber state.progressionFiber
    cancelFiber state.previewFiber

  resetPracticeScroll = do
    maybeElement <- H.getHTMLElementRef practiceContentRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement -> H.liftEffect (Element.setScrollTop 0.0 (HTMLElement.toElement htmlElement))

  scheduleAutomaticAdvance state =
    when (state.config.quizProgression == AutomaticProgression) do
      currentState <- H.get
      cancelFiber currentState.progressionFiber
      fiber <- H.fork do
        let
          waitMilliseconds =
            if not (quizModeUsesRecognition state.config.quizMode) then 1200.0
            else (Audio.intervalPlan state.prompt.mode state.prompt.root state.prompt.target).durationMilliseconds + 500.0
        H.liftAff (delay (Milliseconds waitMilliseconds))
        handleAction AdvanceAutomatically
      H.modify_ _ { progressionFiber = Just fiber }

  scheduleAutomaticRetry state =
    when (state.config.quizProgression == AutomaticProgression) do
      currentState <- H.get
      cancelFiber currentState.progressionFiber
      fiber <- H.fork do
        H.liftAff (delay (Milliseconds 1200.0))
        handleAction RetryAutomatically
      H.modify_ _ { progressionFiber = Just fiber }

  promptNotation state =
    let
      rootAccepted =
        state.recognition.phase /= Recognition.WaitingForFirst
          || (quizModeUsesSinging state.config.quizMode && isChoosingAnswer state.captureStatus)
          || case state.captureStatus of
            PlayingAudio (ResumeAnswers _) -> quizModeUsesSinging state.config.quizMode
            _ -> false
      detected = map
        (pitchFromMidiLike (spellingReference state.prompt))
        state.ghostMidi
    in
      case state.captureStatus, detected of
        AnswerComplete _, _ -> Notation.completed state.prompt.root state.prompt.target
        IntervalError, Just pitch -> Notation.incorrect state.prompt.root state.prompt.target pitch rootAccepted
        _, Just pitch -> Notation.ghost state.prompt.root state.prompt.target pitch rootAccepted
        _, Nothing -> Notation.prompt state.prompt.root state.prompt.target rootAccepted

  spellingReference prompt = case prompt.root, prompt.target of
    Pitch (PitchClass _ (Accidental rootAccidental)) _,
    Pitch (PitchClass _ (Accidental targetAccidental)) _ ->
      if rootAccidental /= 0 then prompt.root
      else if targetAccidental /= 0 then prompt.target
      else prompt.root
