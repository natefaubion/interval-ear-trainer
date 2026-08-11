module EarTrainer.Component.Practice
  ( Input
  , Output(..)
  , Query
  , component
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import EarTrainer.Audio as Audio
import EarTrainer.Capability.Audio as AudioCapability
import EarTrainer.Capability.AudioSession as AudioSession
import EarTrainer.Capability.PitchInput as PitchInput
import EarTrainer.Component.Notation as NotationComponent
import EarTrainer.Config
  ( AnswerDisplay(..)
  , ExerciseConfig
  , GhostMode(..)
  , QuizMode(..)
  , QuizProgression(..)
  , quizModeUsesPitchInput
  , quizModeUsesRecognition
  )
import EarTrainer.Music
  ( Accidental(..)
  , Interval(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
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
  | StartingCapture H.ForkId
  | Listening PitchInput.Monitor
  | CaptureFailed String
  | PlaybackFailed String
  | IntervalError
  | ChoosingAnswer (Array Interval)
  | AnswerComplete (Array Interval)

data PlaybackDestination
  = BeginImitation
  | ResumeAnswers (Array Interval)

derive instance Eq PlaybackDestination

data PracticeExercise
  = IntervalPractice
      { choices :: Array Quiz.IntervalChoice
      , prompt :: Quiz.Prompt
      , recognition :: Recognition.Recognition
      }
  | MelodyPractice
      { prompt :: Quiz.MelodyPrompt
      , recognition :: Recognition.SequenceRecognition
      }

type State =
  { captureStatus :: CaptureStatus
  , config :: ExerciseConfig
  , exercise :: PracticeExercise
  , ghostFiber :: Maybe H.ForkId
  , ghostMidi :: Maybe Int
  , observation :: Recognition.Observation
  , playbackFiber :: Maybe H.ForkId
  , prompts :: Quiz.PromptSet
  , progressionFiber :: Maybe H.ForkId
  , previewFiber :: Maybe H.ForkId
  , sampler :: AudioCapability.Sampler
  }

data Action
  = Initialize
  | Finalize
  | PlayPrompt
  | StartListening
  | AudioFailed String
  | PitchObserved PitchInput.Sample
  | PitchDetected Recognition.PitchObservation
  | MicrophoneStarted PitchInput.Monitor
  | ClearGhost
  | FinishImitation
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
  initialState input = do
    let
      promptMode = Quiz.makePrompt input.seed input.prompts
    { captureStatus: ReadyToPlay
    , config: input.config
    , exercise: makePracticeExercise input.seed input.config promptMode
    , ghostFiber: Nothing
    , ghostMidi: Nothing
    , observation: Recognition.initialObservation
    , playbackFiber: Nothing
    , prompts: input.prompts
    , progressionFiber: Nothing
    , previewFiber: Nothing
    , sampler: input.sampler
    }

  makePracticeExercise seed config = case _ of
    Quiz.IntervalPrompt prompt ->
      IntervalPractice
        { choices: Quiz.makeChoices seed config prompt
        , prompt
        , recognition: Recognition.initialRecognition
        }
    Quiz.MelodyPromptMode prompt ->
      MelodyPractice
        { prompt
        , recognition: Recognition.initialSequenceRecognition
        }

  resetPracticeExercise = case _ of
    IntervalPractice exercise ->
      IntervalPractice (exercise { recognition = Recognition.initialRecognition })
    MelodyPractice exercise ->
      MelodyPractice (exercise { recognition = Recognition.initialSequenceRecognition })

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
              [ HP.classes
                  ( [ H.ClassName "notation-panel" ]
                      <>
                        if notationLayout state == Notation.Compact then
                          [ H.ClassName "compact" ]
                        else [ H.ClassName "full" ]
                  )
              ]
              [ HH.div
                  [ HP.class_ (H.ClassName "prompt-panel") ]
                  [ HH.div
                      [ HP.class_ (H.ClassName "notation-stage") ]
                      [ HH.div
                          [ HP.class_ (H.ClassName "notation-canvas") ]
                          [ HH.slot_ notationSlot PromptNotation NotationComponent.component (promptNotation state) ]
                      ]
                  , HH.div
                      [ HP.class_ (H.ClassName "prompt-feedback") ]
                      [ if shouldShowIntervalName state then
                          HH.p
                            [ HP.class_ (H.ClassName "completed-interval-name") ]
                            [ HH.text (promptIntervalLabel state) ]
                        else
                          HH.text ""
                      , renderPitchMeter state
                      ]
                  ]
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
        HH.div
          [ HP.class_ (H.ClassName "pitch-meter") ]
          [ HH.div
              [ HP.class_ (H.ClassName "pitch-feedback") ]
              [ HH.span
                  [ HP.class_ (H.ClassName "tuner-readout") ]
                  [ HH.text (pitchFeedbackName state) ]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "tuner-track") ]
              [ HH.div [ HP.class_ (H.ClassName "tuner-center") ] []
              , case currentFeedback state of
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
    | otherwise = case state.exercise of
        IntervalPractice exercise ->
          HH.section
            [ HP.class_ (H.ClassName "answer-panel") ]
            [ HH.div
                [ HP.class_ (H.ClassName "answer-grid") ]
                (Array.mapWithIndex (renderIntervalChoice state) exercise.choices)
            ]
        MelodyPractice _ -> HH.text ""

  renderIntervalChoice state index choice = do
    case intervalPrompt state of
      Nothing -> HH.text ""
      Just prompt -> do
        let
          revealed = Array.elem choice.interval (revealedChoices state.captureStatus)
          correct = revealed && choice.interval == prompt.interval
          incorrect = revealed && not correct
          showNotation = state.config.answerDisplay /= AnswerName
          showName = state.config.answerDisplay /= AnswerNotation || revealed
          resultClasses =
            if correct then [ H.ClassName "answer-correct" ]
            else if incorrect then [ H.ClassName "answer-incorrect" ]
            else []
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
                    (Notation.intervalChoice Notation.Compact prompt.root choice.target)
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
      || (isListening state.captureStatus && imitationComplete state)

  footerButtonLabel state
    | isPlaying state.captureStatus = "Playing…"
    | isStartingCapture state.captureStatus = "Playing…"
    | isAnswerComplete state.captureStatus = if isMelody state then "Next melody" else "Next interval"
    | isListening state.captureStatus && imitationComplete state = if isMelody state then "Next melody" else "Next interval"
    | state.config.quizMode == Audiation = "Play root"
    | isMelody state = "Play melody"
    | otherwise = "Play interval"

  footerButtonIcon state
    | isBusy state.captureStatus = ""
    | isAnswerComplete state.captureStatus = "→"
    | otherwise = "▶"

  practiceInstruction state = case state.captureStatus of
    ReadyToPlay -> ""
    PlayingAudio _ -> ""
    StartingCapture _ -> ""
    Listening _ -> "Listening…"
    CaptureFailed message -> "Microphone unavailable: " <> message
    PlaybackFailed message -> "Audio playback failed: " <> message
    IntervalError -> "Incorrect pitch."
    ChoosingAnswer revealed ->
      if Array.null revealed then
        "Choose the matching interval."
      else
        "Not quite. Try Again."
    AnswerComplete _ -> if isMelody state then "Melody complete." else "Correct!"

  isPlaying = case _ of
    PlayingAudio _ -> true
    _ -> false

  isBusy = case _ of
    PlayingAudio _ -> true
    StartingCapture _ -> true
    _ -> false

  isResumingAnswers = case _ of
    PlayingAudio (ResumeAnswers _) -> true
    _ -> false

  isStartingCapture = case _ of
    StartingCapture _ -> true
    _ -> false

  isListening = case _ of
    Listening _ -> true
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
    Just feedback -> do
      let
        cents = Int.round feedback.cents
      if cents >= -3 && cents <= 3 then
        "◆ 0¢"
      else if cents > 0 then
        "↓ " <> show cents <> "¢"
      else
        "↑ " <> show (-cents) <> "¢"

  pitchFeedbackName state
    | isAwaitingArticulation state = ""
    | otherwise = feedbackName (currentFeedback state)

  feedbackPosition cents = 50.0 + max (-50.0) (min 50.0 cents)

  quizModeTitle = case _ of
    ImitationOnly -> "Imitation"
    RecognitionOnly -> "Recognition"
    ImitationAndRecognition -> "Imitation & Recognition"
    Audiation -> "Audiation"
    MelodyImitation -> "Melody imitation"

  shouldShowIntervalName state =
    state.config.quizMode /= MelodyImitation
      &&
        ( state.config.quizMode == Audiation
            || (state.config.quizMode == ImitationOnly && isAnswerComplete state.captureStatus)
        )

  promptIntervalLabel state =
    case intervalPrompt state of
      Nothing -> ""
      Just prompt ->
        intervalName prompt.interval
          <>
            if state.config.quizMode == Audiation || state.config.quizMode == ImitationOnly then
              playbackModeLabel prompt.interval prompt.mode
            else ""

  playbackModeLabel interval mode = case interval, mode of
    PerfectUnison, MelodicAscending -> ""
    PerfectUnison, MelodicDescending -> ""
    _, _ -> " · " <> playbackModeName mode

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
      H.liftEffect (AudioCapability.stop state.sampler)
    PlayPrompt -> do
      state <- H.get
      cancelTasks state
      let
        destination = case state.captureStatus of
          ChoosingAnswer revealed -> ResumeAnswers revealed
          _ -> BeginImitation
      H.modify_ _
        { captureStatus = PlayingAudio destination
        , ghostFiber = Nothing
        , ghostMidi = Nothing
        , observation = Recognition.initialObservation
        , playbackFiber = Nothing
        , progressionFiber = Nothing
        , previewFiber = Nothing
        , exercise = resetPracticeExercise state.exercise
        }
      fiber <- H.fork do
        result <- H.liftAff $ attempt
          $ AudioCapability.play state.sampler
          $ playbackPlan state
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
        PlayingAudio BeginImitation
          | not (quizModeUsesPitchInput state.config.quizMode) -> do
              H.modify_ _ { captureStatus = ChoosingAnswer [] }
        PlayingAudio BeginImitation -> do
          H.liftEffect (AudioSession.setType AudioSession.PlayAndRecord)
          { emitter, listener } <- H.liftEffect HS.create
          void (H.subscribe emitter)
          fiber <- H.fork do
            result <- H.liftAff $ attempt $ PitchInput.start (HS.notify listener <<< PitchObserved)
            case result of
              Left error -> handleAction (MicrophoneFailed (message error))
              Right monitor -> handleAction (MicrophoneStarted monitor)
          H.modify_ _ { captureStatus = StartingCapture fiber }
        _ -> pure unit
    PitchObserved raw -> do
      state <- H.get
      when (isListening state.captureStatus) do
        let
          observed = Recognition.observePitch
            Recognition.defaultCaptureSettings
            (capturePhase state)
            (pitchExpectation state)
            raw
            state.observation
        H.modify_ _ { observation = observed.observation }
        handleAction (PitchDetected observed.event)
    PitchDetected sample -> do
      state <- H.get
      when (isListening state.captureStatus) do
        case state.exercise of
          IntervalPractice exercise -> handleIntervalPitch state exercise sample
          MelodyPractice exercise -> handleMelodyPitch state exercise sample
    ClearGhost -> do
      state <- H.get
      H.modify_ _ { ghostFiber = Nothing }
      when
        ( isListening state.captureStatus
            && state.ghostMidi /= Nothing
        )
        do
          when (state.config.ghostMode == GhostOn) do
            H.modify_ _ { ghostMidi = Nothing }
    FinishImitation -> do
      state <- H.get
      when
        ( isListening state.captureStatus
            && imitationComplete state
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
      case state.captureStatus of
        StartingCapture _ -> H.modify_ _ { captureStatus = Listening monitor }
        _ -> do
          H.liftEffect (PitchInput.stop monitor)
          H.liftEffect (AudioSession.setType AudioSession.Auto)
    MicrophoneFailed failure -> do
      state <- H.get
      case state.captureStatus of
        StartingCapture _ -> captureFailed state failure
        Listening _ -> captureFailed state failure
        _ -> pure unit
    ChooseInterval interval -> do
      state <- H.get
      case state.captureStatus, state.exercise of
        ChoosingAnswer previous, IntervalPractice exercise
          | not (Array.elem interval previous) -> do
              case Array.find (\choice -> choice.interval == interval) exercise.choices of
                Just choice -> do
                  cancelFiber state.previewFiber
                  fiber <- H.fork do
                    void $ H.liftAff $ attempt $
                      AudioCapability.play state.sampler
                        (Audio.intervalPlan exercise.prompt.mode exercise.prompt.root choice.target)
                  H.modify_ _ { previewFiber = Just fiber }
                Nothing -> pure unit
              let
                correct = interval == exercise.prompt.interval
                revealed = Array.snoc previous interval
              H.modify_ _
                { captureStatus =
                    if correct then AnswerComplete revealed
                    else ChoosingAnswer revealed
                }
              when correct do
                scheduleAutomaticAdvance state
        _, _ -> pure unit
    NextPrompt -> do
      state <- H.get
      cancelTasks state
      H.liftEffect (AudioCapability.stop state.sampler)
      seed <- H.liftEffect (randomInt 0 2147483647)
      let
        prompt = Quiz.makePrompt seed state.prompts
      H.modify_ _
        { captureStatus = ReadyToPlay
        , exercise = makePracticeExercise seed state.config prompt
        , ghostFiber = Nothing
        , ghostMidi = Nothing
        , playbackFiber = Nothing
        , progressionFiber = Nothing
        , previewFiber = Nothing
        , observation = Recognition.initialObservation
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
      when (isIntervalError state.captureStatus) do
        handleAction PlayPrompt
    EditSetup -> do
      state <- H.get
      cancelTasks state
      H.liftEffect (AudioCapability.stop state.sampler)
      H.raise BackToSetup

  handleIntervalPitch state exercise observation = do
    let
      prompt = exercise.prompt
      detectedMidi = case observation of
        Recognition.ObservedPitch sample -> Just (Recognition.nearestMidi sample.frequency)
        _ -> Nothing
      next = Recognition.stepRecognition
        Recognition.defaultRecognitionSettings
        state.config.octavePolicy
        prompt.root
        prompt.target
        observation
        exercise.recognition
      detectedGhost =
        if state.config.ghostMode == GhostOff then Nothing
        else map (Recognition.relativeMidi state.config.octavePolicy prompt.root next) detectedMidi
      completed = Recognition.phase exercise.recognition /= Recognition.RecognitionComplete
        && Recognition.phase next == Recognition.RecognitionComplete
      incorrect = Recognition.phase exercise.recognition /= Recognition.RecognitionIncorrect
        && Recognition.phase next == Recognition.RecognitionIncorrect
    updateGhost state detectedGhost
    H.modify_ \current -> current
      { exercise = IntervalPractice (exercise { recognition = next }) }
    when completed do
      finishCapture state
    when incorrect do
      finishIncorrect state
        (map (Recognition.relativeMidi state.config.octavePolicy prompt.root next <<< _.midi) (Recognition.feedback next))

  handleMelodyPitch state exercise observation = do
    let
      pitches = Quiz.melodyPitches exercise.prompt
      detectedMidi = case observation of
        Recognition.ObservedPitch sample -> Just (Recognition.nearestMidi sample.frequency)
        _ -> Nothing
      next = Recognition.stepSequenceRecognition
        Recognition.defaultRecognitionSettings
        state.config.octavePolicy
        pitches
        observation
        exercise.recognition
      detectedGhost =
        if state.config.ghostMode == GhostOff then Nothing
        else map (Recognition.sequenceRelativeMidi state.config.octavePolicy pitches next) detectedMidi
      completed = Recognition.sequencePhase exercise.recognition /= Recognition.SequenceComplete
        && Recognition.sequencePhase next == Recognition.SequenceComplete
      incorrect = Recognition.sequencePhase exercise.recognition /= Recognition.SequenceIncorrect
        && Recognition.sequencePhase next == Recognition.SequenceIncorrect
    updateGhost state detectedGhost
    H.modify_ \current -> current
      { exercise = MelodyPractice (exercise { recognition = next }) }
    when completed do
      finishCapture state
    when incorrect do
      finishIncorrect state
        ( map
            (Recognition.sequenceRelativeMidi state.config.octavePolicy pitches next <<< _.midi)
            (Recognition.sequenceFeedback next)
        )

  updateGhost state detectedGhost = do
    when (detectedGhost /= Nothing) do
      cancelFiber state.ghostFiber
    H.modify_ _
      { ghostFiber = if detectedGhost == Nothing then state.ghostFiber else Nothing
      , ghostMidi = case detectedGhost of
          Nothing -> state.ghostMidi
          Just midi -> Just midi
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

  finishCapture state = do
    cancelFiber state.ghostFiber
    stopCapture state.captureStatus
    H.modify_ _ { ghostFiber = Nothing }
    handleAction FinishImitation

  finishIncorrect state incorrectMidi = do
    cancelFiber state.ghostFiber
    stopCapture state.captureStatus
    H.modify_ _
      { captureStatus = IntervalError
      , ghostFiber = Nothing
      , ghostMidi = incorrectMidi
      }
    scheduleAutomaticRetry state

  captureFailed state failure = do
    cancelFiber state.ghostFiber
    case state.captureStatus of
      Listening monitor -> stopCapture (Listening monitor)
      _ -> H.liftEffect (AudioSession.setType AudioSession.Auto)
    H.modify_ _
      { captureStatus = CaptureFailed failure
      , ghostFiber = Nothing
      }

  stopCapture captureStatus = do
    case captureStatus of
      StartingCapture fiber -> H.kill fiber
      Listening monitor -> H.liftEffect (PitchInput.stop monitor)
      _ -> pure unit
    H.liftEffect (AudioSession.setType AudioSession.Auto)

  cancelFiber = case _ of
    Nothing -> pure unit
    Just fiber -> H.kill fiber

  cancelTasks state = do
    stopCapture state.captureStatus
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
            else (playbackPlan state).durationMilliseconds + 500.0
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

  intervalPrompt state = case state.exercise of
    IntervalPractice exercise -> Just exercise.prompt
    MelodyPractice _ -> Nothing

  pitchExpectation state = case state.exercise of
    IntervalPractice exercise ->
      Recognition.intervalPitchExpectation
        state.config.octavePolicy
        exercise.prompt.root
        exercise.prompt.target
        exercise.recognition
    MelodyPractice exercise ->
      Recognition.sequencePitchExpectation
        state.config.octavePolicy
        (Quiz.melodyPitches exercise.prompt)
        exercise.recognition

  capturePhase state = case state.exercise of
    IntervalPractice exercise -> case Recognition.phase exercise.recognition of
      Recognition.WaitingForRelease -> Recognition.AwaitingArticulation
      _ -> Recognition.DetectingPitch
    MelodyPractice exercise -> case Recognition.sequencePhase exercise.recognition of
      Recognition.SequenceReleasing -> Recognition.AwaitingArticulation
      _ -> Recognition.DetectingPitch

  isMelody state = case state.exercise of
    IntervalPractice _ -> false
    MelodyPractice _ -> true

  currentFeedback state = case state.exercise of
    IntervalPractice exercise -> Recognition.feedback exercise.recognition
    MelodyPractice exercise -> Recognition.sequenceFeedback exercise.recognition

  isAwaitingArticulation state = case state.exercise of
    IntervalPractice exercise -> Recognition.phase exercise.recognition == Recognition.WaitingForRelease
    MelodyPractice exercise -> Recognition.sequencePhase exercise.recognition == Recognition.SequenceReleasing

  imitationComplete state = case state.exercise of
    IntervalPractice exercise -> Recognition.phase exercise.recognition == Recognition.RecognitionComplete
    MelodyPractice exercise -> Recognition.sequencePhase exercise.recognition == Recognition.SequenceComplete

  playbackPlan state = case state.exercise of
    IntervalPractice exercise -> do
      let prompt = exercise.prompt
      if state.config.quizMode == Audiation then Audio.rootPlan prompt.root
      else Audio.intervalPlan prompt.mode prompt.root prompt.target
    MelodyPractice exercise -> Audio.melodyPlan (Quiz.melodyPitches exercise.prompt)

  promptNotation state = do
    case state.exercise of
      IntervalPractice exercise -> intervalNotation state exercise.prompt exercise.recognition
      MelodyPractice exercise ->
        melodyNotation state (Quiz.melodyPitches exercise.prompt) exercise.recognition

  intervalNotation state prompt recognition = do
    let
      layout = notationLayout state
      rootAccepted =
        Recognition.phase recognition /= Recognition.WaitingForFirst
          || (quizModeUsesPitchInput state.config.quizMode && isChoosingAnswer state.captureStatus)
          || case state.captureStatus of
            PlayingAudio (ResumeAnswers _) -> quizModeUsesPitchInput state.config.quizMode
            _ -> false
      detected = map
        (pitchFromMidiLike (spellingReference prompt))
        state.ghostMidi
    case state.captureStatus, detected of
      AnswerComplete _, _ -> Notation.completed layout prompt.root prompt.target
      IntervalError, Just pitch -> Notation.incorrect layout prompt.root prompt.target pitch rootAccepted
      _, Just pitch -> Notation.ghost layout prompt.root prompt.target pitch rootAccepted
      _, Nothing -> Notation.prompt layout prompt.root prompt.target rootAccepted

  melodyNotation state pitches recognition = do
    let
      acceptedCount = Recognition.sequenceAcceptedCount recognition
      reference = NonEmptyArray.head pitches
      detected = map (pitchFromMidiLike reference) state.ghostMidi
      current = case state.captureStatus, detected of
        IntervalError, Just pitch -> Just { appearance: Notation.Incorrect, pitch }
        _, Just pitch -> Just { appearance: Notation.Dim, pitch }
        _, Nothing -> Nothing
    Notation.sequenceScore (notationLayout state) pitches acceptedCount current true

  notationLayout state
    | isMelody state = Notation.Full
    | not (quizModeUsesRecognition state.config.quizMode) = Notation.Full
    | otherwise = case state.captureStatus of
        ChoosingAnswer _ -> Notation.Compact
        AnswerComplete _ -> Notation.Compact
        PlayingAudio (ResumeAnswers _) -> Notation.Compact
        _ -> Notation.Full

  spellingReference prompt = case prompt.root, prompt.target of
    Pitch (PitchClass _ (Accidental rootAccidental)) _,
    Pitch (PitchClass _ (Accidental targetAccidental)) _ ->
      if rootAccidental /= 0 then prompt.root
      else if targetAccidental /= 0 then prompt.target
      else prompt.root
