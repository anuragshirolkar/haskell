module Events (
    Movement (..), Action (..), fromEvent
) where

import Graphics.Gloss.Interface.IO.Interact
import Point3

data Movement = 
    MoveForward
    | MoveBackward
    | MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown
    | PanRight
    | PanLeft
    | PanUp
    | PanDown
    | TiltRight
    | TiltLeft
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Action = Add Movement | Remove Movement | Reset | NoAction

fromEvent :: Event -> Action
fromEvent (EventKey (SpecialKey KeyUp) Up _ _) = Add MoveForward
fromEvent (EventKey (SpecialKey KeyUp) Down _ _) = Remove MoveForward
fromEvent (EventKey (SpecialKey KeyDown) Up _ _) = Add MoveBackward
fromEvent (EventKey (SpecialKey KeyDown) Down _ _) = Remove MoveBackward
fromEvent (EventKey (SpecialKey KeyLeft) Up _ _) = Add MoveLeft
fromEvent (EventKey (SpecialKey KeyLeft) Down _ _) = Remove MoveLeft
fromEvent (EventKey (SpecialKey KeyRight) Up _ _) = Add MoveRight
fromEvent (EventKey (SpecialKey KeyRight) Down _ _) = Remove MoveRight
fromEvent (EventKey (Char 'j') Down _ _) = Add MoveDown
fromEvent (EventKey (Char 'j') Up _ _) = Remove MoveDown
fromEvent (EventKey (Char 'k') Down _ _) = Add MoveUp
fromEvent (EventKey (Char 'k') Up _ _) = Remove MoveUp
fromEvent (EventKey (Char 'r') Down _ _) = Reset
fromEvent _ = NoAction