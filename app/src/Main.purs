module Main where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import EarTrainer.Audio as Audio
import EarTrainer.Config
  ( AnswerCount(..)
  , AnswerDisplay(..)
  , ExerciseConfig
  , GhostMode(..)
  , defaultConfig
  , isValid
  , toggleInterval
  , togglePlaybackMode
  , toggleRootPitchClass
  )
import EarTrainer.Music
  ( Accidental(..)
  , Interval
  , OctavePolicy(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode
  , VocalRangePreset
  , allIntervals
  , allPlaybackModes
  , allRootPitchClasses
  , allVocalRangePresets
  , intervalName
  , pitchClassName
  , pitchFromMidiLike
  , pitchName
  , playbackModeName
  , presetName
  , presetRange
  )
import EarTrainer.Notation as Notation
import EarTrainer.PitchDetection as Detection
import EarTrainer.Quiz as Quiz
import EarTrainer.Settings as Settings
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement as HTMLElement

data Screen = Setup | Practice

derive instance Eq Screen

data CaptureStatus
  = ReadyToPlay
  | PlayingAudio
  | Listening
  | CaptureFailed String
  | PlaybackFailed String
  | ChoosingAnswer
  | AnswerComplete

derive instance Eq CaptureStatus

type State =
  { answerCorrect :: Boolean
  , captureStatus :: CaptureStatus
  , choices :: Array Quiz.IntervalChoice
  , config :: ExerciseConfig
  , ghostMidi :: Maybe Int
  , ghostRevision :: Int
  , monitor :: Maybe Detection.Monitor
  , prompt :: Quiz.Prompt
  , recognition :: Detection.Recognition
  , revealedChoices :: Array Interval
  , sampler :: Maybe Audio.Sampler
  , screen :: Screen
  }

data Action
  = Initialize
  | ToggleInterval Interval
  | TogglePlaybackMode PlaybackMode
  | ToggleRoot PitchClass
  | SelectRange VocalRangePreset
  | SelectOctavePolicy OctavePolicy
  | SelectGhostMode GhostMode
  | SelectAnswerCount AnswerCount
  | SelectAnswerDisplay AnswerDisplay
  | BeginPractice
  | PlayPrompt
  | PlaybackStarted
  | AudioFailed String
  | StartListening
  | PitchDetected Detection.PitchSample
  | ClearGhost Int
  | FinishSinging Int
  | MicrophoneFailed String
  | ChooseInterval Interval
  | NextPrompt
  | EditSetup

notationRef :: H.RefLabel
notationRef = H.RefLabel "prompt-notation"

choiceNotationRef :: Int -> H.RefLabel
choiceNotationRef index = H.RefLabel ("choice-notation-" <> show index)

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  initialState =
    { answerCorrect: false
    , captureStatus: ReadyToPlay
    , choices: Quiz.makeChoices 0 defaultConfig (Quiz.makePrompt 0 defaultConfig)
    , config: defaultConfig
    , ghostMidi: Nothing
    , ghostRevision: 0
    , monitor: Nothing
    , prompt: Quiz.makePrompt 0 defaultConfig
    , recognition: Detection.initialRecognition
    , revealedChoices: []
    , sampler: Nothing
    , screen: Setup
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.main
      [ HP.class_ (H.ClassName "app-shell") ]
      [ if state.screen == Setup then renderSetup state else renderPractice state ]

  renderSetup state =
    HH.section
      [ HP.class_ (H.ClassName "setup-card") ]
      [ HH.div
          [ HP.class_ (H.ClassName "setup-heading") ]
          [ HH.h2_ [ HH.text "Exercise setup" ] ]
      , HH.div
          [ HP.class_ (H.ClassName "setup-content") ]
          [ settingGroup
              "Playback"
              "How the interval is played before the microphone begins listening."
              (map (modeButton state.config) allPlaybackModes)
              (if Array.null state.config.playbackModes then Just "Select at least one playback mode." else Nothing)
          , settingGroup
              "Intervals"
              "Choose the intervals that may appear in an exercise."
              (map (intervalButton state.config) allIntervals)
              (if Array.null state.config.intervals then Just "Select at least one interval." else Nothing)
          , settingGroup
              "Singing range"
              "Choose the written and playback register."
              (map (rangeButton state.config) allVocalRangePresets)
              Nothing
          , settingGroup
              "Root notes"
              "Choose the individual pitch classes that may begin an exercise."
              (map (rootButton state.config) allRootPitchClasses)
              (if Array.null state.config.rootPitchClasses then Just "Select at least one root note." else Nothing)
          , settingGroup
              "Octave matching"
              "Choose whether the detected pitch must be in the written octave."
              [ choiceButton
                  (state.config.octavePolicy == AnyOctave)
                  (SelectOctavePolicy AnyOctave)
                  "Any comfortable octave"
              , choiceButton
                  (state.config.octavePolicy == WrittenOctave)
                  (SelectOctavePolicy WrittenOctave)
                  "Written octave only"
              ]
              Nothing
          , settingGroup
              "Ghost note"
              "The ghost note shows the pitch you are currently singing on the staff."
              [ choiceButton (state.config.ghostMode == GhostOff) (SelectGhostMode GhostOff) "Hidden"
              , choiceButton (state.config.ghostMode == GhostOn) (SelectGhostMode GhostOn) "Shown briefly"
              , choiceButton (state.config.ghostMode == GhostPersist) (SelectGhostMode GhostPersist) "Kept visible"
              ]
              Nothing
          , settingGroup
              "Available answers"
              "Choose how many interval choices are shown for each question."
              [ choiceButton (state.config.answerCount == AFew) (SelectAnswerCount AFew) "A few"
              , choiceButton
                  (state.config.answerCount == AllSelected)
                  (SelectAnswerCount AllSelected)
                  "All selected choices"
              ]
              Nothing
          , settingGroup
              "Answer display"
              "Choose how interval answers are presented."
              [ choiceButton
                  (state.config.answerDisplay == AnswerNotation)
                  (SelectAnswerDisplay AnswerNotation)
                  "Notation"
              , choiceButton
                  (state.config.answerDisplay == AnswerName)
                  (SelectAnswerDisplay AnswerName)
                  "Interval name"
              , choiceButton
                  (state.config.answerDisplay == AnswerBoth)
                  (SelectAnswerDisplay AnswerBoth)
                  "Both"
              ]
              Nothing
          ]
      , HH.footer
          [ HP.class_ (H.ClassName "setup-footer") ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "primary-button")
              , HP.disabled (not (isValid state.config))
              , HE.onClick \_ -> BeginPractice
              ]
              [ HH.text "Begin practice" ]
          ]
      ]

  renderPractice state =
    HH.section
      [ HP.class_ (H.ClassName "practice-card") ]
      [ HH.div
          [ HP.class_ (H.ClassName "practice-toolbar") ]
          [ HH.div_
              [ HH.h2_ [ HH.text (playbackModeName state.prompt.mode) ] ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "text-button")
              , HE.onClick \_ -> EditSetup
              ]
              [ HH.text "Edit setup" ]
          ]
      , HH.div
          [ HP.class_ (H.ClassName "practice-content") ]
          [ HH.div
              [ HP.class_ (H.ClassName "notation-panel") ]
              [ HH.div
                  [ HP.ref notationRef
                  , HP.class_ (H.ClassName "notation-canvas")
                  ]
                  []
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
    | state.captureStatus == ChoosingAnswer || state.captureStatus == AnswerComplete = HH.text ""
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
    | state.captureStatus /= ChoosingAnswer && state.captureStatus /= AnswerComplete = HH.text ""
    | otherwise =
        HH.section
          [ HP.class_ (H.ClassName "answer-panel") ]
          [ HH.div
              [ HP.class_ (H.ClassName "answer-grid") ]
              (Array.mapWithIndex (renderIntervalChoice state) state.choices)
          ]

  renderIntervalChoice state index choice =
    let
      revealed = Array.elem choice.interval state.revealedChoices
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
        , HP.disabled state.answerCorrect
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
    if state.captureStatus == AnswerComplete then NextPrompt else PlayPrompt

  footerButtonDisabled state =
    state.captureStatus == PlayingAudio
      || state.captureStatus == ChoosingAnswer
      || (state.captureStatus == Listening && state.recognition.phase == Detection.RecognitionComplete)

  footerButtonLabel state
    | state.captureStatus == PlayingAudio = "Playing…"
    | state.captureStatus == ChoosingAnswer = "Next interval"
    | state.captureStatus == AnswerComplete = "Next interval"
    | state.captureStatus == Listening && state.recognition.phase == Detection.RecognitionComplete = "Next interval"
    | otherwise = "Play interval"

  footerButtonIcon state
    | state.captureStatus == PlayingAudio = ""
    | footerButtonLabel state == "Next interval" = "→"
    | otherwise = "▶"

  settingGroup title description controls validation =
    HH.fieldset
      [ HP.class_ (H.ClassName "setting-group") ]
      [ HH.legend_ [ HH.text title ]
      , HH.p
          [ HP.class_ (H.ClassName "setting-description") ]
          [ HH.text description ]
      , HH.div
          [ HP.class_ (H.ClassName "choice-grid") ]
          controls
      , case validation of
          Nothing -> HH.text ""
          Just message ->
            HH.p
              [ HP.class_ (H.ClassName "setting-error") ]
              [ HH.text message ]
      ]

  choiceButton selected action label =
    HH.button
      [ HP.type_ HP.ButtonButton
      , HP.classes
          if selected then
            [ H.ClassName "choice-chip", H.ClassName "selected" ]
          else
            [ H.ClassName "choice-chip" ]
      , HE.onClick \_ -> action
      ]
      [ HH.text label ]

  modeButton config mode =
    choiceButton
      (Array.elem mode config.playbackModes)
      (TogglePlaybackMode mode)
      (playbackModeName mode)

  intervalButton config interval =
    choiceButton
      (Array.elem interval config.intervals)
      (ToggleInterval interval)
      (intervalName interval)

  rootButton config root =
    choiceButton
      (Array.elem root config.rootPitchClasses)
      (ToggleRoot root)
      (pitchClassName root)

  rangeButton config preset =
    let
      range = presetRange preset
      label = presetName preset <> " · " <> pitchName range.low <> "–" <> pitchName range.high
    in
      choiceButton
        (config.vocalRange == preset)
        (SelectRange preset)
        label

  practiceInstruction state = case state.captureStatus of
    ReadyToPlay ->
      "Listen to the interval, then sing."
    PlayingAudio -> "Listen carefully."
    Listening -> Detection.phaseInstruction state.recognition.phase
    CaptureFailed message -> "Microphone unavailable: " <> message
    PlaybackFailed message -> "Audio playback failed: " <> message
    ChoosingAnswer ->
      if Array.null state.revealedChoices then
        "Choose the matching interval."
      else
        "Not quite. Compare the choices and try again."
    AnswerComplete -> "Correct!"

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

  feedbackInRange feedback =
    feedback.clarity >= Detection.defaultRecognitionSettings.clarityThreshold
      && feedback.cents >= -Detection.defaultRecognitionSettings.toleranceCents
      && feedback.cents <= Detection.defaultRecognitionSettings.toleranceCents

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      config <- H.liftEffect Settings.load
      H.modify_ _ { config = config }
    ToggleInterval interval ->
      updateConfig (toggleInterval interval)
    TogglePlaybackMode mode ->
      updateConfig (togglePlaybackMode mode)
    ToggleRoot root ->
      updateConfig (toggleRootPitchClass root)
    SelectRange preset ->
      updateConfig (_ { vocalRange = preset })
    SelectOctavePolicy policy ->
      updateConfig (_ { octavePolicy = policy })
    SelectGhostMode mode ->
      updateConfig (_ { ghostMode = mode })
    SelectAnswerCount count ->
      updateConfig (_ { answerCount = count })
    SelectAnswerDisplay display ->
      updateConfig (_ { answerDisplay = display })
    BeginPractice -> do
      state <- H.get
      seed <- H.liftEffect (randomInt 0 2147483647)
      sampler <- case state.sampler of
        Just existing -> pure existing
        Nothing -> H.liftEffect Audio.createSampler
      let
        prompt = Quiz.makePrompt seed state.config
        choices = Quiz.makeChoices seed state.config prompt
      H.modify_ _
        { answerCorrect = false
        , captureStatus = ReadyToPlay
        , choices = choices
        , ghostMidi = Nothing
        , ghostRevision = state.ghostRevision + 1
        , prompt = prompt
        , recognition = Detection.initialRecognition
        , revealedChoices = []
        , sampler = Just sampler
        , screen = Practice
        }
      renderPromptNotation prompt false
    PlayPrompt -> do
      state <- H.get
      stopMonitor state.monitor
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> do
          H.modify_ _
            { captureStatus = PlayingAudio
            , ghostMidi = Nothing
            , ghostRevision = state.ghostRevision + 1
            , monitor = Nothing
            , recognition = Detection.initialRecognition
            }
          renderPromptNotation state.prompt false
          { emitter, listener } <- H.liftEffect HS.create
          void (H.subscribe emitter)
          H.liftEffect $ Audio.playInterval sampler state.prompt.mode state.prompt.root state.prompt.target
            (HS.notify listener PlaybackStarted)
            (HS.notify listener <<< AudioFailed)
    PlaybackStarted -> do
      state <- H.get
      when (state.screen == Practice && state.captureStatus == PlayingAudio) do
        void $ H.fork do
          H.liftAff (delay (Milliseconds (Audio.playbackDurationMilliseconds state.prompt.mode + 350.0)))
          handleAction StartListening
    AudioFailed message ->
      H.modify_ _ { captureStatus = PlaybackFailed message }
    StartListening -> do
      state <- H.get
      when (state.screen == Practice && state.captureStatus == PlayingAudio) do
        { emitter, listener } <- H.liftEffect HS.create
        void (H.subscribe emitter)
        monitor <- H.liftEffect $ Detection.start
          (HS.notify listener <<< PitchDetected)
          (HS.notify listener <<< MicrophoneFailed)
        H.modify_ _ { captureStatus = Listening, monitor = Just monitor }
    PitchDetected sample -> do
      state <- H.get
      when (state.captureStatus == Listening) do
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
          revision = if detectedGhost == Nothing then state.ghostRevision else state.ghostRevision + 1
          completed =
            state.recognition.phase /= Detection.RecognitionComplete
              && next.phase == Detection.RecognitionComplete
          firstAccepted = next.phase /= Detection.WaitingForFirst
          firstJustAccepted = state.recognition.phase == Detection.WaitingForFirst && firstAccepted
        H.modify_ _ { ghostMidi = nextGhost, ghostRevision = revision, recognition = next }
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
            void $ H.fork do
              H.liftAff (delay (Milliseconds 700.0))
              handleAction (ClearGhost revision)
        when completed do
          stopMonitor state.monitor
          H.modify_ _ { monitor = Nothing }
          if state.config.ghostMode == GhostOn then
            void $ H.fork do
              H.liftAff (delay (Milliseconds 700.0))
              handleAction (FinishSinging revision)
          else
            handleAction (FinishSinging revision)
    ClearGhost revision -> do
      state <- H.get
      when
        ( state.captureStatus == Listening
            && state.ghostRevision == revision
            && state.ghostMidi /= Nothing
        )
        do
          when (state.config.ghostMode == GhostOn) do
            H.modify_ _ { ghostMidi = Nothing }
            renderPromptNotation state.prompt (state.recognition.phase /= Detection.WaitingForFirst)
    FinishSinging revision -> do
      state <- H.get
      when
        ( state.captureStatus == Listening
            && state.recognition.phase == Detection.RecognitionComplete
            && state.ghostRevision == revision
        )
        do
          let persistGhost = state.config.ghostMode == GhostPersist
          H.modify_ _
            { captureStatus = ChoosingAnswer
            , ghostMidi = if persistGhost then state.ghostMidi else Nothing
            }
          unless persistGhost (renderPromptNotation state.prompt true)
          renderChoiceNotation state.prompt.root state.choices
    MicrophoneFailed message ->
      H.modify_ _ { captureStatus = CaptureFailed message, monitor = Nothing }
    ChooseInterval interval -> do
      state <- H.get
      when (state.captureStatus == ChoosingAnswer && not (Array.elem interval state.revealedChoices)) do
        case state.sampler, Array.find (\choice -> choice.interval == interval) state.choices of
          Just sampler, Just choice ->
            H.liftEffect $ Audio.playInterval sampler state.prompt.mode state.prompt.root choice.target
              (pure unit)
              (\_ -> pure unit)
          _, _ -> pure unit
        let
          correct = interval == state.prompt.interval
          revealed = Array.snoc state.revealedChoices interval
        H.modify_ _
          { answerCorrect = correct
          , captureStatus = if correct then AnswerComplete else ChoosingAnswer
          , revealedChoices = revealed
          }
    NextPrompt -> do
      state <- H.get
      seed <- H.liftEffect (randomInt 0 2147483647)
      let
        prompt = Quiz.makePrompt seed state.config
        choices = Quiz.makeChoices seed state.config prompt
      H.modify_ _
        { answerCorrect = false
        , captureStatus = ReadyToPlay
        , choices = choices
        , ghostMidi = Nothing
        , ghostRevision = state.ghostRevision + 1
        , prompt = prompt
        , recognition = Detection.initialRecognition
        , revealedChoices = []
        }
      renderPromptNotation prompt false
    EditSetup -> do
      state <- H.get
      stopMonitor state.monitor
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> H.liftEffect (Audio.stop sampler)
      H.modify_ _
        { captureStatus = ReadyToPlay
        , ghostMidi = Nothing
        , ghostRevision = state.ghostRevision + 1
        , monitor = Nothing
        , screen = Setup
        }

  stopMonitor = case _ of
    Nothing -> pure unit
    Just monitor -> H.liftEffect (Detection.stop monitor)

  updateConfig update = do
    H.modify_ \state -> state { config = update state.config }
    state <- H.get
    H.liftEffect (Settings.save state.config)

  renderPromptNotation prompt rootAccepted = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect (Notation.renderPrompt (HTMLElement.toElement htmlElement) prompt.root prompt.target rootAccepted)

  renderGhostNotation prompt detected rootAccepted = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect (Notation.renderGhost (HTMLElement.toElement htmlElement) prompt.root prompt.target detected rootAccepted)

  renderChoiceNotation root choices =
    for_ (Array.mapWithIndex (\index choice -> { choice, index }) choices) \item -> do
      maybeElement <- H.getHTMLElementRef (choiceNotationRef item.index)
      case maybeElement of
        Nothing -> pure unit
        Just htmlElement ->
          H.liftEffect (Notation.renderIntervalChoice (HTMLElement.toElement htmlElement) root item.choice.target)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
