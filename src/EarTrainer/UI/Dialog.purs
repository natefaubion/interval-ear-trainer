module EarTrainer.UI.Dialog
  ( DialogProps
  , dialog
  ) where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude ((<>))

type DialogProps =
  { classes :: Array H.ClassName
  , ref :: H.RefLabel
  , title :: String
  }

dialog
  :: forall slots action
   . DialogProps
  -> Array (HH.HTML slots action)
  -> HH.HTML slots action
dialog props children =
  HH.dialog
    [ HP.ref props.ref
    , HP.classes props.classes
    , HP.attr (H.AttrName "aria-label") props.title
    ]
    ([ HH.h3_ [ HH.text props.title ] ] <> children)
