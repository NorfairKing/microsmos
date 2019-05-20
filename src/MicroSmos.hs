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
import Cursor.Types

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
  let s = State {stateCursor = tc, stateMode = EditTree}
  s' <- Brick.defaultMain microSmosApp s
  SB.writeFile file $ Yaml.encode $ rebuildCTree $ rebuildTreeCursor toText $ stateCursor s'

toTextCursor :: Text -> TextCursor
toTextCursor = fromMaybe (error "Wasn't a single line") . makeTextCursor

toText :: TextCursor -> Text
toText = rebuildTextCursor

data State =
  State
    { stateCursor :: TreeCursor TextCursor Text
    , stateMode :: Mode
    }
  deriving (Show, Eq)

data Mode
  = EditTree
  | EditText
  deriving (Show, Eq)

microSmosApp :: App State e ResourceName
microSmosApp =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap =
        const $
        attrMap
          Vty.defAttr
          [ (nodeAttr, fg yellow)
          , (selectedAttr, fg brightWhite)
          , (modeAttr, fg green)
          , (infoAttr, fg blue)
          , (warningAttr, fg red)
          ]
    }

draw :: State -> [Widget ResourceName]
draw s =
  [ padAll 1 $
    vBox
      [ withAttr nodeAttr $ padBottom Max $ drawTreeCursor wrap cur $ stateCursor s
      , hBorder
      , withAttr modeAttr drawMode
      , hBorder
      , withAttr infoAttr drawInfo
      , hBorder
      , hCenterLayer $
        withAttr warningAttr $
        strWrap
          "Note that this is a tree editor, not a forest editor, so there is no way to add a sibling to the root node."
      ]
  ]
  where
    drawMode :: Widget n
    drawMode =
      str $
      case stateMode s of
        EditText -> "Editing selected node's text"
        EditTree -> "Editing tree"
    drawInfo :: Widget n
    drawInfo =
      let explanations =
            map (\(k, v) -> (str k, str v)) $
            case stateMode s of
              EditText ->
                [ ("<char>", "insert character")
                , ("Backspace", "remove character")
                , ("Delete", "delete character")
                , ("Left", "move left within node")
                , ("Right", "move right within node")
                , ("Enter, Esc", "Switch mode")
                ]
              EditTree ->
                [ ("i, I, a, A", "switch mode")
                , ("j", "Add a sibling node")
                , ("J", "Add a child node")
                , ("d", "Delete selected node")
                , ("D", "Delete selected subtree")
                , ("Up", "move to previous node")
                , ("Down", "move to next node")
                , ("Esc", "Exit")
                ]
       in (\(ks, vs) -> hBox $ [vBox ks, padLeft (Pad 2) $ vBox vs]) $ unzip explanations
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
    cur tc cf =
      let ecw = withAttr selectedAttr $ (str "> " <+>) $ drawTextCursor tc
          rest = padLeft defaultPadding $ drawCForest cf
       in vBox [ecw, rest]
    wrap :: [CTree Text] -> Text -> [CTree Text] -> Widget n -> Widget n
    wrap tsl e tsr w =
      let befores = map drawTextCTree tsl
          ew = txt e
          afters = map drawTextCTree tsr
       in (str "- " <+> ew) <=> padLeft defaultPadding (vBox $ concat [befores, [w], afters])
    drawTextCursor :: TextCursor -> Widget ResourceName
    drawTextCursor tc =
      visible .
      (case stateMode s of
         EditText -> showCursor TextResource (Brick.Location (textCursorIndex tc, 0))
         EditTree -> id) $
      txt $
      case rebuildTextCursor tc of
        "" -> " "
        t -> t

drawCForest :: CForest Text -> Widget n
drawCForest cf =
  case cf of
    EmptyCForest -> emptyWidget
    ClosedForest _ -> emptyWidget
    OpenForest ts ->
      let etws = map drawTextCTree $ NE.toList ts
       in vBox etws

drawTextCTree :: CTree Text -> Widget n
drawTextCTree (CNode t cf) = vBox [hBox [str "- ", txt t], padLeft defaultPadding (drawCForest cf)]

nodeAttr :: AttrName
nodeAttr = "node"

selectedAttr :: AttrName
selectedAttr = "selected"

modeAttr :: AttrName
modeAttr = "mode"

infoAttr :: AttrName
infoAttr = "info"

warningAttr :: AttrName
warningAttr = "warning"

defaultPadding :: Padding
defaultPadding = Pad 4

data ResourceName =
  TextResource
  deriving (Show, Eq, Ord)

handleEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleEvent s e =
  case e of
    VtyEvent ve ->
      case ve of
        EvKey key mods ->
          let textDo :: (TextCursor -> Maybe TextCursor) -> (EventM n (Next State))
              textDo func = mDo $ treeCursorCurrentL func
              mDo ::
                   (TreeCursor TextCursor Text -> Maybe (TreeCursor TextCursor Text))
                -> (EventM n (Next State))
              mDo func =
                let tc = stateCursor s
                    tc' = fromMaybe tc $ func tc
                 in continue $ s {stateCursor = tc'}
           in case stateMode s of
                EditText ->
                  let switchMode = continue $ s {stateMode = EditTree}
                   in case key of
                        KChar c -> textDo $ textCursorInsert c
                        KBS -> textDo textCursorRemove
                        KDel -> textDo textCursorDelete
                        KLeft -> textDo textCursorSelectPrev
                        KRight -> textDo textCursorSelectNext
                        KEnter -> switchMode
                        KEsc -> switchMode
                        _ -> continue s
                EditTree ->
                  let switchMode = continue $ s {stateMode = EditText}
                   in case key of
                        KChar 'i' -> switchMode
                        KChar 'I' -> switchMode
                        KChar 'a' -> switchMode
                        KChar 'A' -> switchMode
                        KChar 'j' -> mDo $ treeCursorAppend (Node "" [])
                        KChar 'J' -> mDo $ Just . treeCursorAddChildAtStart (Node "" [])
                        KChar 'd' ->
                          mDo $ \tc ->
                            case treeCursorRemoveElem toTextCursor tc of
                              Deleted -> Nothing
                              Updated tc' -> Just tc'
                        KChar 'D' ->
                          mDo $ \tc ->
                            case treeCursorRemoveSubTree toTextCursor tc of
                              Deleted -> Nothing
                              Updated tc' -> Just tc'
                        KUp -> mDo $ treeCursorSelectPrev toText toTextCursor
                        KDown -> mDo $ treeCursorSelectNext toText toTextCursor
                        KEsc -> halt s
                        _ -> continue s
        _ -> continue s
    _ -> continue s
