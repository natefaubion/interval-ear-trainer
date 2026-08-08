module EarTrainer.UI.Button
  ( Variant(..)
  , button
  , text
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

type ButtonConfig slots action =
  { action :: action
  , classes :: Array H.ClassName
  , content :: Array (HH.HTML slots action)
  , disabled :: Boolean
  , variant :: Variant
  }

button :: forall slots action. ButtonConfig slots action -> HH.HTML slots action
button config =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HP.classes (variantClasses config.variant <> config.classes)
    , HP.disabled config.disabled
    , HE.onClick \_ -> config.action
    ]
    config.content

text
  :: forall slots action
   . Variant
  -> Array H.ClassName
  -> Boolean
  -> action
  -> String
  -> HH.HTML slots action
text variant classes disabled action label =
  button
    { action
    , classes
    , content: [ HH.text label ]
    , disabled
    , variant
    }

variantClasses :: Variant -> Array H.ClassName
variantClasses = case _ of
  Primary -> [ H.ClassName "primary-button" ]
  Secondary -> [ H.ClassName "secondary-button" ]
  Choice selected ->
    [ H.ClassName "choice-chip" ]
      <> if selected then [ H.ClassName "selected" ] else []
  SmallText -> [ H.ClassName "small-text-button" ]
  Unstyled -> []
