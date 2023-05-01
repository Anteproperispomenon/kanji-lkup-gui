{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Monomer
import TextShow

import Kanji.Widgets.WrapGrid
import Kanji.Widgets.Radicals

import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppNull
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack 
      [ label "Hello world"
      , spacer
      , hstack 
        [ label $ "Click count: " <> showt (model ^. clickCount)
        , spacer
        , button "Increase count" AppIncrease
        ]
      -- , box $ vwgrid_ [maxMainCells 10] (map basicButton [1..40])
      , (radicalGrid (\_ -> AppNull) 20) -- `styleBasic` [textFont "SC"]
    ] `styleBasic` [padding 10]
  basicButton n = button (T.pack $ show n) AppNull


handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]
  AppNull -> []

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Hello world",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Japanese" "./assets/fonts/NotoSansJP-Regular.ttf",
      appFontDef "SC" "./assets/fonts/NotoSansSC-Regular.otf",
      appInitEvent AppInit
      ]
    model = AppModel 0
