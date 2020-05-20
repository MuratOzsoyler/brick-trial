{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Brick
import           Brick.Focus
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import           Data.Text.Zipper
import           Data.Char                      ( isSpace )
import           Graphics.Vty
import           Debug.Trace

data Name = EditLeft | EditRight
        deriving (Eq, Ord, Show)
data St n a = St
        { fring :: FocusRing n
        , lEditor :: Editor a n
        , rEditor :: Editor a n
        , copyBuffer :: a
        }

main :: IO ()
main = do
    defaultMain app initialState
    return ()
  where
    initialState = St { .. }
      where
        fring      = focusRing [EditLeft, EditRight]
        lEditor    = editor EditLeft (Just 1) "Sol deneme"
        rEditor    = editor EditRight (Just 1) "Sağ deneme"
        copyBuffer = ""
    app = App { .. }
    appDraw St {..} =
        [ joinBorders $ withBorderStyle unicode $ borderWithLabel
              (str "Hello!")
              ((   center (str "Left" <=> renderEdit lEditor)
               <+> vBorder
               <+> center (str "Right" <=> renderEdit rEditor)
               )
            --   <=> str (unlines eventLog)
              )
        ]
        where renderEdit = withFocusRing fring $ renderEditor (str . unlines)
    appChooseCursor = {- const $ const (Nothing :: Maybe (CursorLocation Name)) -}focusRingCursor fring
    appHandleEvent st@St {..} = \case
        VtyEvent evt                    -> keyEvent evt
            -- fmap (\s -> s { eventLog = show evt : eventLog }) <$> keyEvent evt
        AppEvent evt                    -> undefined
        MouseDown name btn modifier loc -> undefined
        MouseUp name mbBtn loc          -> undefined
      where
        keyEvent evt = case evt of
            EvKey key modifier -> case (key, modifier) of
                (KBackTab  , []     ) -> continue st { fring = focusPrev fring }
                (KChar '\t', []     ) -> continue st { fring = focusNext fring }
                -- (KChar 'a' , [MCtrl]) -> undefined
                (KChar 'c' , [MCtrl]) -> continue st
                    { copyBuffer = unlines $ getEditContents $ case fcurr of
                                       Just EditLeft  -> lEditor
                                       Just EditRight -> rEditor
                                       _              -> undefined
                    }
                (KChar 'v', [MCtrl]) -> continue $ case fcurr of
                    Just EditLeft ->
                        applyAnyEdit updLEditor (paste copyBuffer) lEditor
                    Just EditRight ->
                        applyAnyEdit updREditor (paste copyBuffer) rEditor
                    _ -> undefined
                (KEnd, []) -> case fcurr of
                    Just EditLeft ->
                        continue $ updLEditor $ applyEdit gotoEOL lEditor
                    Just EditRight ->
                        continue $ updREditor $ applyEdit gotoEOL rEditor
                    _ -> undefined
                (KEnd, [MCtrl]) -> case fcurr of
                    Just EditLeft ->
                        continue $ applyAnyEdit updLEditor gotoEOT lEditor
                    Just EditRight ->
                        continue $ applyAnyEdit updREditor gotoEOT rEditor
                    _ -> undefined
                -- (KEnd  , [MShift]       ) -> undefined
                -- (KEnd  , [MCtrl, MShift]) -> undefined
                -- (KEnter, []             ) -> undefined
                (KEsc , []) -> halt st
                (KHome, []) -> case fcurr of
                    Just EditLeft ->
                        continue $ updLEditor $ applyEdit gotoBOL lEditor
                    Just EditRight ->
                        continue $ updREditor $ applyEdit gotoBOL rEditor
                    _ -> undefined
                (KHome, [MCtrl]) -> case fcurr of
                    Just EditLeft ->
                        continue $ applyAnyEdit updLEditor gotoBOT lEditor
                    Just EditRight ->
                        continue $ applyAnyEdit updREditor gotoBOT rEditor
                    _ -> undefined
                -- (KHome , [MShift]       ) -> undefined
                -- (KHome , [MCtrl, MShift]) -> undefined
                -- (KLeft , []             ) -> undefined
                (KLeft, [MCtrl]) -> case fcurr of
                    Just EditLeft -> continue $ applyAnyEdit updLEditor
                                                             gotoPW -- (gotoWord previousChar moveLeft)
                                                             lEditor
                    Just EditRight -> continue $ applyAnyEdit updREditor
                                                              gotoPW -- (gotoWord previousChar moveLeft)
                                                              rEditor
                    _ -> undefined
                -- (KLeft , [MShift]       ) -> undefined
                -- (KRight, []             ) -> undefined
                (KRight, [MCtrl]) -> case fcurr of
                    Just EditLeft -> continue $ applyAnyEdit updLEditor
                                                             gotoNW -- (gotoWord nextChar moveRight)
                                                             lEditor
                    Just EditRight -> continue $ applyAnyEdit updREditor
                                                              gotoNW -- (gotoWord nextChar moveRight)
                                                              rEditor
                    _ -> undefined
                -- (KRight, [MShift]       ) -> undefined
                _ -> case fcurr of
                    Just EditLeft ->
                        continue =<< handleAnyEditEvt updLEditor lEditor
                    Just EditRight ->
                        continue =<< handleAnyEditEvt updREditor rEditor
                    _ -> undefined
              where
                fcurr = focusGetCurrent fring
                updLEditor e = st { lEditor = e }
                updREditor e = st { rEditor = e }
                handleAnyEditEvt updSt e = updSt <$> handleEditorEvent evt e
                applyAnyEdit updSt updZpr = updSt . applyEdit updZpr
                gotoBOT = moveCursor (0, 0)
                gotoEOT zpr =
                    let lls    = lineLengths zpr
                        lastll = lastMaybe lls
                    in  case lastll of
                            Just ll -> moveCursor (length lls - 1, ll) zpr
                            Nothing -> zpr
                gotoPW = go True
                  where
                    go isFst zpr =
                        let
                            cur  = currentChar zpr
                            succ = previousChar zpr
                        in
                            case (cur, succ) of
                                (Nothing, Nothing) -> zpr
                                (Nothing, Just _ ) -> go False $ moveLeft zpr
                                (Just _ , Nothing) -> moveLeft zpr
                                (Just c, Just s) ->
                                    let isCurSpc  = isSpace c
                                        isSuccSpc = isSpace s
                                    in  if isFst && isCurSpc /= isSuccSpc
                                            then go False $ moveLeft zpr
                                            else
                                                let
                                                    ctrlFunc =
                                                        (if isCurSpc
                                                                then id
                                                                else not
                                                            )
                                                            . isSpace
                                                in  if ctrlFunc c && ctrlFunc s
                                                        then go False
                                                            $ moveLeft zpr
                                                        else zpr
                gotoNW zpr =
                    let cur  = currentChar zpr
                        succ = nextChar zpr
                    in  case (cur, succ) of
                            (Nothing, Nothing) -> zpr
                            (Nothing, Just _ ) -> undefined -- trace "N, J" $ go $ moveFunc zpr
                            (Just _ , Nothing) -> moveRight zpr
                            (Just c, Just s) ->
                                let
                                    ctrlFunc =
                                        (if isSpace c then id else not)
                                            . isSpace
                                in  if ctrlFunc c && ctrlFunc s
                                        then gotoNW $ moveRight zpr
                                        else moveRight zpr
                paste = insertMany
    appStartEvent = return
    appAttrMap _ = attrMap
        defAttr
        [(editAttr, white `on` blue), (editFocusedAttr, black `on` yellow)]

lastMaybe :: [a] -> Maybe a
lastMaybe = \case
    [] -> Nothing
    xs -> Just $ last xs
