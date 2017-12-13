{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
    ( exec
    ) where


import Text.Read (readMaybe)
import Miso
import Miso.String (toMisoString)

type Model = Maybe Integer

data Action = Update (Maybe Int)
  | NoOp

exec :: IO ()
exec = startApp App {..}
  where
    initialAction = NoOp
    model  = 0
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

updateModel :: Action -> Model -> Effect Action Model
updateModel (Update n) _
  | n < Just 0 = noEff Nothing
  | otherwise = noEff $ fmap (fibs !!) n
updateModel NoOp m = noEff m

viewModel :: Model -> View Action
viewModel x = div_ []
    [
      input_ [ onIput (Update . readMaybe . filter (/= '"') . show) ] []
    , text (toMisoString (show x))
    ]
