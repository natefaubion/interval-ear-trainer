module EarTrainer.Component.App (component) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import EarTrainer.Capability.Audio as Audio
import EarTrainer.Component.Practice as Practice
import EarTrainer.Component.Setup as Setup
import EarTrainer.Config (ExerciseConfig, defaultConfig)
import EarTrainer.Quiz as Quiz
import EarTrainer.Settings as Settings
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type State =
  { activePresetId :: Maybe Settings.PresetId
  , config :: ExerciseConfig
  , phase :: Phase
  , presets :: Array Settings.Preset
  , storageError :: Maybe String
  }

data Phase
  = Loading
  | Setup Audio.Sampler
  | Practice Practice.Input

data Action
  = Initialize
  | PracticeOutput Practice.Output
  | SetupOutput Setup.Output

type Slots =
  ( practice :: H.Slot Practice.Query Practice.Output Unit
  , setup :: H.Slot Setup.Query Setup.Output Unit
  )

practiceSlot :: Proxy "practice"
practiceSlot = Proxy

setupSlot :: Proxy "setup"
setupSlot = Proxy

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const
        { activePresetId: Nothing
        , config: defaultConfig
        , phase: Loading
        , presets: []
        , storageError: Nothing
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.main
      [ HP.class_ (H.ClassName "app-shell") ]
      [ case state.phase of
          Loading -> renderSetup false
          Setup _ -> renderSetup true
          Practice input -> HH.slot practiceSlot unit Practice.component input PracticeOutput
      ]
    where
    renderSetup samplerReady =
      HH.slot setupSlot unit Setup.component
        { activePresetId: state.activePresetId
        , config: state.config
        , presets: state.presets
        , samplerReady
        , storageError: state.storageError
        }
        SetupOutput

  handleAction = case _ of
    Initialize -> do
      loaded <- H.liftAff (attempt Settings.load)
      let
        appData = case loaded of
          Left _ -> { activePresetId: Nothing, config: defaultConfig, presets: [] }
          Right (Left _) -> { activePresetId: Nothing, config: defaultConfig, presets: [] }
          Right (Right value) -> value
        storageError = case loaded of
          Left _ -> Just "Saved settings could not be loaded on this device."
          Right (Left _) -> Just "Saved settings use an unsupported or malformed format."
          Right (Right _) -> Nothing
      sampler <- H.liftEffect Audio.createSampler
      H.modify_ _
        { activePresetId = appData.activePresetId
        , config = appData.config
        , phase = Setup sampler
        , presets = appData.presets
        , storageError = storageError
        }
    PracticeOutput Practice.BackToSetup -> do
      state <- H.get
      case state.phase of
        Practice input -> H.modify_ _ { phase = Setup input.sampler }
        _ -> pure unit
    SetupOutput (Setup.DataChanged appData) -> do
      H.modify_ _
        { activePresetId = appData.activePresetId
        , config = appData.config
        , presets = appData.presets
        }
      persist
    SetupOutput Setup.BeginRequested -> do
      state <- H.get
      case state.phase of
        Setup sampler -> case Quiz.promptSet state.config of
          Nothing -> pure unit
          Just prompts -> do
            void $ H.liftAff $ attempt Audio.start
            seed <- H.liftEffect (randomInt 0 2147483647)
            H.modify_ _ { phase = Practice { config: state.config, prompts, sampler, seed } }
        _ -> pure unit
    SetupOutput Setup.PersistenceRequested ->
      void $ H.fork do
        void $ H.liftAff Settings.requestPersistence

  persist = do
    state <- H.get
    result <- H.liftAff $ attempt $ Settings.save
      { activePresetId: state.activePresetId
      , config: state.config
      , presets: state.presets
      }
    H.modify_ _
      { storageError = case result of
          Left _ -> Just "Changes could not be saved on this device."
          Right _ -> Nothing
      }
