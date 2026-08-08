module EarTrainer.Component.Notation
  ( Query
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import EarTrainer.Capability.Notation as NotationCapability
import EarTrainer.Notation (Score)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement as HTMLElement

data Action
  = Initialize
  | Receive Score

data Query :: Type -> Type
data Query a

notationRef :: H.RefLabel
notationRef = H.RefLabel "notation"

component :: forall output m. MonadEffect m => H.Component Query Score output m
component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval:
        H.mkEval H.defaultEval
          { handleAction = handleAction
          , initialize = Just Initialize
          , receive = Just <<< Receive
          }
    }
  where
  render =
    HH.div
      [ HP.ref notationRef ]
      []

  handleAction = case _ of
    Initialize -> draw
    Receive score -> do
      previous <- H.get
      when (score /= previous) do
        H.put score
        draw

  draw = do
    score <- H.get
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect (NotationCapability.render (HTMLElement.toElement htmlElement) score)
