module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State =
  { configured :: Boolean
  }

data Action = StartSetup

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState: const { configured: false }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
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
      , HH.section
          [ HP.class_ (H.ClassName "practice-card") ]
          [ HH.div
              [ HP.class_ (H.ClassName "staff-placeholder") ]
              [ HH.span_ [ HH.text "Notation workspace" ] ]
          , HH.div
              [ HP.class_ (H.ClassName "status-row") ]
              [ HH.span_ [ HH.text if state.configured then "Setup started" else "Choose your exercise to begin" ]
              , HH.button
                  [ HP.type_ HP.ButtonButton
                  , HP.class_ (H.ClassName "primary-button")
                  , HE.onClick \_ -> StartSetup
                  ]
                  [ HH.text if state.configured then "Continue setup" else "Set up practice" ]
              ]
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction StartSetup = H.modify_ _ { configured = true }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
