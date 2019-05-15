{-# LANGUAGE OverloadedStrings #-}

module MicroSmos
  ( microSmos
  ) where

import Data.Maybe

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.Exit

import Cursor.Text
import Cursor.Tree

import Data.Tree

import Brick as Brick
import Brick.Main as Brick
import Brick.Widgets.Border as Brick
import Brick.Widgets.Center as Brick
import Brick.Widgets.Core as Brick

import Graphics.Vty.Attributes as Vty
import Graphics.Vty.Input.Events as Vty

microSmos :: IO ()
microSmos = do
  let tc = singletonTreeCursor emptyTextCursor
  _ <- Brick.defaultMain microSmosApp tc
  pure ()
  -- TODO persist the tree

type State = TreeCursor TextCursor Text

microSmosApp :: App State e Text
microSmosApp =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap Vty.defAttr []
    }

draw :: State -> [Widget Text]
draw tc = [centerLayer $ border $ padAll 1 $ emptyWidget]

handleEvent :: State -> BrickEvent Text e -> EventM Text (Next State)
handleEvent tc e =
  case e of
    VtyEvent ve ->
      case ve of
        EvKey key mods ->
          let mDo func = continue . fromMaybe tc $ func tc
           in case key of
                _ -> continue tc
        _ -> continue tc
    _ -> continue tc
