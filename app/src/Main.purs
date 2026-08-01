module Main where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import EarTrainer.Audio as Audio
import EarTrainer.Config (ExerciseConfig, defaultConfig, isValid, toggleInterval, togglePlaybackMode, toggleRootPitchClass)
import EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , Letter(..)
  , OctavePolicy(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , VocalRangePreset
  , allIntervals
  , allPlaybackModes
  , allRootPitchClasses
  , allVocalRangePresets
  , intervalName
  , midiNumber
  , pitchClassName
  , pitchName
  , playbackModeName
  , presetName
  , presetRange
  , transpose
  )
import EarTrainer.Notation as Notation
import EarTrainer.PitchDetection as Detection
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
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

derive instance Eq CaptureStatus

type Prompt =
  { interval :: Interval
  , mode :: PlaybackMode
  , root :: Pitch
  , target :: Pitch
  }

type State =
  { captureStatus :: CaptureStatus
  , config :: ExerciseConfig
  , monitor :: Maybe Detection.Monitor
  , prompt :: Prompt
  , recognition :: Detection.Recognition
  , sampler :: Maybe Audio.Sampler
  , screen :: Screen
  }

data Action
  = ToggleInterval Interval
  | TogglePlaybackMode PlaybackMode
  | ToggleRoot PitchClass
  | SelectRange VocalRangePreset
  | SelectOctavePolicy OctavePolicy
  | BeginPractice
  | PlayPrompt
  | PlaybackStarted
  | AudioFailed String
  | StartListening
  | PitchDetected Detection.PitchSample
  | MicrophoneFailed String
  | EditSetup

notationRef :: H.RefLabel
notationRef = H.RefLabel "prompt-notation"

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState =
    { captureStatus: ReadyToPlay
    , config: defaultConfig
    , monitor: Nothing
    , prompt: makePrompt defaultConfig
    , recognition: Detection.initialRecognition
    , sampler: Nothing
    , screen: Setup
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.main
      [ HP.class_ (H.ClassName "app-shell") ]
      [ HH.header
          [ HP.class_ (H.ClassName "hero") ]
          [ HH.p
              [ HP.class_ (H.ClassName "eyebrow") ]
              [ HH.text "Interval practice" ]
          , HH.h1_ [ HH.text "Train the space between notes." ]
          , HH.p
              [ HP.class_ (H.ClassName "lede") ]
              [ HH.text "Hear an interval, sing both notes, then connect what you sang to notation." ]
          ]
      , if state.screen == Setup then renderSetup state else renderPractice state
      ]

  renderSetup state =
    HH.section
      [ HP.class_ (H.ClassName "setup-card") ]
      [ HH.div
          [ HP.class_ (H.ClassName "setup-heading") ]
          [ HH.div_
              [ HH.p
                  [ HP.class_ (H.ClassName "step-label") ]
                  [ HH.text "Exercise setup" ]
              , HH.h2_ [ HH.text "Choose what to practice" ]
              ]
          , HH.p_
              [ HH.text "Select at least one option in each group. These settings stay editable between questions." ]
          ]
      , settingGroup
          "Playback"
          "How the interval is played before the microphone begins listening."
          (map (modeButton state.config) allPlaybackModes)
      , settingGroup
          "Intervals"
          "The interval name is shown here only during setup. Quiz choices use notation."
          (map (intervalButton state.config) allIntervals)
      , settingGroup
          "Singing range"
          "A preset controls the written and playback register; octave-equivalent singing is accepted by default."
          (map (rangeButton state.config) allVocalRangePresets)
      , settingGroup
          "Root notes"
          "Choose the individual pitch classes that may begin an exercise."
          (map (rootButton state.config) allRootPitchClasses)
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
      , HH.footer
          [ HP.class_ (H.ClassName "setup-footer") ]
          [ HH.p
              [ HP.class_ (H.ClassName "setup-message") ]
              [ HH.text
                  if isValid state.config then
                    "Ready to begin."
                  else
                    "Select at least one playback mode, interval, and root note."
              ]
          , HH.button
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
              [ HH.p
                  [ HP.class_ (H.ClassName "step-label") ]
                  [ HH.text "Listen, then sing" ]
              , HH.h2_ [ HH.text (playbackModeName state.prompt.mode) ]
              ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "text-button")
              , HE.onClick \_ -> EditSetup
              ]
              [ HH.text "Edit setup" ]
          ]
      , HH.div
          [ HP.class_ (H.ClassName "notation-panel") ]
          [ HH.div
              [ HP.ref notationRef
              , HP.class_ (H.ClassName "notation-canvas")
              ]
              []
          , HH.p_
              [ HH.text (practiceInstruction state) ]
          , HH.div
              [ HP.class_ (H.ClassName "pitch-feedback") ]
              [ HH.span
                  [ HP.class_ (H.ClassName "feedback-status") ]
                  [ HH.text (captureStatusName state.captureStatus) ]
              , HH.span_
                  [ HH.text (feedbackName state.recognition.feedback) ]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "tuner-track") ]
              [ HH.div [ HP.class_ (H.ClassName "tuner-center") ] []
              , case state.recognition.feedback of
                  Nothing -> HH.text ""
                  Just feedback ->
                    HH.div
                      [ HP.class_ (H.ClassName "tuner-dot")
                      , HP.style ("left: " <> show (feedbackPosition feedback.cents) <> "%")
                      ]
                      []
              ]
          ]
      , HH.footer
          [ HP.class_ (H.ClassName "practice-actions") ]
          [ HH.p_
              [ HH.text "Playback stops before microphone input begins." ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "primary-button play-button")
              , HP.disabled (state.captureStatus == PlayingAudio)
              , HE.onClick \_ -> PlayPrompt
              ]
              [ HH.span
                  [ HP.class_ (H.ClassName "play-icon") ]
                  [ HH.text "▶" ]
              , HH.text if state.captureStatus == PlayingAudio then "Playing…" else "Play interval"
              ]
          ]
      ]

  settingGroup title description controls =
    HH.fieldset
      [ HP.class_ (H.ClassName "setting-group") ]
      [ HH.legend_ [ HH.text title ]
      , HH.p
          [ HP.class_ (H.ClassName "setting-description") ]
          [ HH.text description ]
      , HH.div
          [ HP.class_ (H.ClassName "choice-grid") ]
          controls
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
      "Play the interval, then sing " <> pitchName state.prompt.root <> " followed by the second note."
    PlayingAudio -> "Listen carefully. Microphone capture remains off during playback."
    Listening -> Detection.phaseInstruction state.recognition.phase
    CaptureFailed message -> "Microphone unavailable: " <> message
    PlaybackFailed message -> "Audio playback failed: " <> message

  captureStatusName = case _ of
    ReadyToPlay -> "Ready"
    PlayingAudio -> "Playing"
    Listening -> "Listening"
    CaptureFailed _ -> "Microphone unavailable"
    PlaybackFailed _ -> "Playback unavailable"

  feedbackName = case _ of
    Nothing -> "No stable pitch yet"
    Just feedback ->
      let
        cents = Int.round feedback.cents
      in
        if cents >= -3 && cents <= 3 then
          "In tune"
        else if cents > 0 then
          show cents <> " cents sharp — sing lower"
        else
          show (-cents) <> " cents flat — sing higher"

  feedbackPosition cents = 50.0 + max (-50.0) (min 50.0 cents)

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    ToggleInterval interval ->
      H.modify_ \state -> state { config = toggleInterval interval state.config }
    TogglePlaybackMode mode ->
      H.modify_ \state -> state { config = togglePlaybackMode mode state.config }
    ToggleRoot root ->
      H.modify_ \state -> state { config = toggleRootPitchClass root state.config }
    SelectRange preset ->
      H.modify_ \state -> state { config = state.config { vocalRange = preset } }
    SelectOctavePolicy policy ->
      H.modify_ \state -> state { config = state.config { octavePolicy = policy } }
    BeginPractice -> do
      state <- H.get
      sampler <- case state.sampler of
        Just existing -> pure existing
        Nothing -> H.liftEffect Audio.createSampler
      let prompt = makePrompt state.config
      H.modify_ _
        { captureStatus = ReadyToPlay
        , prompt = prompt
        , recognition = Detection.initialRecognition
        , sampler = Just sampler
        , screen = Practice
        }
      renderNotation [ prompt.root ]
    PlayPrompt -> do
      state <- H.get
      stopMonitor state.monitor
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> do
          H.modify_ _
            { captureStatus = PlayingAudio
            , monitor = Nothing
            , recognition = Detection.initialRecognition
            }
          renderNotation [ state.prompt.root ]
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
          next = Detection.stepRecognition
            Detection.defaultRecognitionSettings
            state.config.octavePolicy
            state.prompt.root
            state.prompt.target
            sample
            state.recognition
          completed =
            state.recognition.phase /= Detection.RecognitionComplete
              && next.phase == Detection.RecognitionComplete
        H.modify_ _ { recognition = next }
        when completed do
          stopMonitor state.monitor
          H.modify_ _ { monitor = Nothing }
          renderNotation [ state.prompt.root, state.prompt.target ]
    MicrophoneFailed message ->
      H.modify_ _ { captureStatus = CaptureFailed message, monitor = Nothing }
    EditSetup -> do
      state <- H.get
      stopMonitor state.monitor
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> H.liftEffect (Audio.stop sampler)
      H.modify_ _ { captureStatus = ReadyToPlay, monitor = Nothing, screen = Setup }

  stopMonitor = case _ of
    Nothing -> pure unit
    Just monitor -> H.liftEffect (Detection.stop monitor)

  renderNotation notes = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement -> H.liftEffect (Notation.renderNotes (HTMLElement.toElement htmlElement) notes)

makePrompt :: ExerciseConfig -> Prompt
makePrompt config =
  let
    interval = fromMaybe MinorThird (Array.head config.intervals)
    mode = fromMaybe MelodicAscending (Array.head config.playbackModes)
    selectedRoot = fromMaybe (PitchClass C (Accidental 0)) (Array.head config.rootPitchClasses)
    range = presetRange config.vocalRange
    Pitch _ lowOctave = range.low
    lowCandidate = Pitch selectedRoot lowOctave
    root =
      if midiNumber lowCandidate < midiNumber range.low then
        Pitch selectedRoot (lowOctave + 1)
      else
        lowCandidate
    direction = case mode of
      MelodicDescending -> Descending
      _ -> Ascending
  in
    { interval
    , mode
    , root
    , target: transpose direction interval root
    }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
