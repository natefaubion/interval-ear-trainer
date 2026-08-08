module EarTrainer.UI.Dialog
  ( DialogProps
  , dialog
  ) where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type DialogProps =
  { classes :: Array H.ClassName
  , labelledBy :: String
  , ref :: H.RefLabel
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
    , HP.attr (H.AttrName "aria-labelledby") props.labelledBy
    ]
    children
