{-# LANGUAGE OverloadedStrings #-}

module MicroSmos
  ( microSmos
  ) where

import Data.Maybe

import qualified Data.ByteString as SB
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.Exit

import Cursor.Text
import Cursor.Tree

import Data.Tree
import Data.Yaml as Yaml

import Path.IO

import Brick as Brick
import Brick.Main as Brick
import Brick.Widgets.Border as Brick
import Brick.Widgets.Center as Brick
import Brick.Widgets.Core as Brick

import Graphics.Vty.Attributes as Vty
import Graphics.Vty.Input.Events as Vty

microSmos :: IO ()
microSmos = do
  (file:_) <- getArgs
  errOrContents <- forgivingAbsence $ SB.readFile file
  tc <-
    case errOrContents of
      Nothing -> pure $ singletonTreeCursor emptyTextCursor
      Just contents ->
        case Yaml.decodeEither' contents of
          Left err -> die $ "Failed to read tree file: " <> prettyPrintParseException err
          Right tree -> pure $ makeTreeCursor toTextCursor (cTree True (tree :: Tree Text))
  tc' <- Brick.defaultMain microSmosApp tc
  SB.writeFile file $ Yaml.encode $ rebuildCTree $ rebuildTreeCursor toText tc'

toTextCursor :: Text -> TextCursor
toTextCursor = fromMaybe (error "Wasn't a single line") . makeTextCursor

toText :: TextCursor -> Text
toText = rebuildTextCursor

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
                KEsc -> halt tc
                _ -> continue tc
        _ -> continue tc
    _ -> continue tc
