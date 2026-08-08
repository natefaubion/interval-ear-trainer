module EarTrainer.UI.Button
  ( ButtonProps
  , Variant(..)
  , button
  ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Variant
  = Primary
  | Secondary
  | Choice Boolean
  | SmallText
  | Unstyled

type ButtonProps action =
  { action :: action
  , classes :: Array H.ClassName
  , disabled :: Boolean
  , variant :: Variant
  }

button
  :: forall slots action
   . ButtonProps action
  -> Array (HH.HTML slots action)
  -> HH.HTML slots action
button props children =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HP.classes (variantClasses props.variant <> props.classes)
    , HP.disabled props.disabled
    , HE.onClick \_ -> props.action
    ]
    children

variantClasses :: Variant -> Array H.ClassName
variantClasses = case _ of
  Primary -> [ H.ClassName "primary-button" ]
  Secondary -> [ H.ClassName "secondary-button" ]
  Choice selected ->
    [ H.ClassName "choice-chip" ]
      <> if selected then [ H.ClassName "selected" ] else []
  SmallText -> [ H.ClassName "small-text-button" ]
  Unstyled -> []
