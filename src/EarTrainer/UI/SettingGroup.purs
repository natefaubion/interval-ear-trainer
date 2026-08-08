module EarTrainer.UI.SettingGroup
  ( SettingGroupProps
  , settingGroup
  ) where

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type SettingGroupProps =
  { description :: String
  , title :: String
  , validation :: Maybe String
  }

settingGroup
  :: forall slots action
   . SettingGroupProps
  -> Array (HH.HTML slots action)
  -> HH.HTML slots action
settingGroup props children =
  HH.fieldset
    [ HP.class_ (H.ClassName "setting-group") ]
    [ HH.legend_ [ HH.text props.title ]
    , HH.p [ HP.class_ (H.ClassName "setting-description") ] [ HH.text props.description ]
    , HH.div [ HP.class_ (H.ClassName "choice-grid") ] children
    , case props.validation of
        Nothing -> HH.text ""
        Just message -> HH.p [ HP.class_ (H.ClassName "setting-error") ] [ HH.text message ]
    ]
