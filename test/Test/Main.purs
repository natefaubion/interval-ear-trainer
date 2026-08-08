module Test.Main where

import Prelude

import Effect (Effect)
import Test.Media as Media
import Test.Music as Music
import Test.Quiz as Quiz
import Test.Recognition as Recognition
import Test.Settings as Settings

main :: Effect Unit
main = do
  Music.run
  Quiz.run
  Recognition.run
  Settings.run
  Media.run
