{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module MicroSmos
  ( microSmos
  ) where

import Data.Maybe

import qualified Data.ByteString as SB
import qualified Data.List.NonEmpty as NE
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

microSmosApp :: App State e ResourceName
microSmosApp =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap Vty.defAttr []
    }

draw :: State -> [Widget ResourceName]
draw tc = [centerLayer $ border $ padAll 1 $ drawTreeCursor wrap cur tc]
  where
    drawTreeCursor ::
         forall a b n.
         ([CTree b] -> b -> [CTree b] -> Widget n -> Widget n)
      -> (a -> CForest b -> Widget n)
      -> TreeCursor a b
      -> Widget n
    drawTreeCursor wrapAboveFunc currentFunc TreeCursor {..} =
      wrapAbove treeAbove $ currentFunc treeCurrent treeBelow
      where
        wrapAbove :: Maybe (TreeAbove b) -> Widget n -> Widget n
        wrapAbove Nothing = id
        wrapAbove (Just ta) = goAbove ta
        goAbove :: TreeAbove b -> Widget n -> Widget n
        goAbove TreeAbove {..} =
          wrapAbove treeAboveAbove .
          wrapAboveFunc (reverse treeAboveLefts) treeAboveNode treeAboveRights
    cur :: TextCursor -> CForest Text -> Widget ResourceName
    cur ec cf =
      case cf of
        EmptyCForest -> drawTextCursor ec
        ClosedForest _ -> drawTextCursor ec
        OpenForest ts ->
          let ecw = drawTextCursor ec
              etws = map drawTextCTree $ NE.toList ts
           in ecw <=> padLeft defaultPadding (vBox etws)
    wrap :: [CTree Text] -> Text -> [CTree Text] -> Widget n -> Widget n
    wrap tsl e tsr w =
      let befores = map drawTextCTree tsl
          ew = txt e
          afters = map drawTextCTree tsr
       in ew <=> padLeft defaultPadding (vBox $ concat [befores, [w], afters])

drawTextCTree :: CTree Text -> Widget n
drawTextCTree (CNode t cf) =
  case cf of
    EmptyCForest -> txt t
    ClosedForest _ -> txt t
    OpenForest ts ->
      let ew = txt t
          etws = map drawTextCTree $ NE.toList ts
       in ew <=> padLeft defaultPadding (vBox etws)

drawTextCursor :: TextCursor -> Widget ResourceName
drawTextCursor tc =
  visible . showCursor TextResource (Brick.Location (textCursorIndex tc, 0)) $
  txt $ rebuildTextCursor tc

defaultPadding :: Padding
defaultPadding = Pad 4

data ResourceName =
  TextResource
  deriving (Show, Eq, Ord)

handleEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleEvent tc e =
  case e of
    VtyEvent ve ->
      case ve of
        EvKey key mods ->
          let textDo :: (TextCursor -> Maybe TextCursor) -> (EventM n (Next State))
              textDo func = continue . fromMaybe tc $ (treeCursorCurrentL func) tc
              mDo :: (State -> Maybe State) -> (EventM n (Next State))
              mDo func = continue . fromMaybe tc $ func tc
           in case key of
                KChar c -> textDo $ textCursorInsert c
                KBS -> textDo textCursorRemove
                KDel -> textDo textCursorDelete
                KLeft -> textDo textCursorSelectPrev
                KRight -> textDo textCursorSelectNext
                KEsc -> halt tc
                _ -> continue tc
        _ -> continue tc
    _ -> continue tc
