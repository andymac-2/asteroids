module Event
    ( Keys (..)
    , KeyStatus (..)
    , keysFromEvents
    , noKeys
    ) where

import SDL
import Data.Foldable (foldl')

data KeyStatus = Down | Held | Up
    deriving (Eq)

data Keys = Keys
    { thrust :: KeyStatus
    , left :: KeyStatus
    , right :: KeyStatus
    , fire :: KeyStatus
    , pause :: KeyStatus
    , quit :: KeyStatus
    }

noKeys :: Keys
noKeys = Keys Up Up Up Up Up Up

keysFromEvents :: Keys -> [Event] -> Keys
-- Reset fire to false so it deosn't repeat every frame
keysFromEvents (Keys t l r f p q) = foldl' keyFromEvent keys' where
    go Down = Held
    go Held = Held
    go Up = Up

    keys' = (Keys (go t) (go l) (go r) (go f) (go p) (go q))

keyFromEvent :: Keys -> Event -> Keys
keyFromEvent a@(Keys t l r f p q) 
    (Event _ (KeyboardEvent 
        (KeyboardEventData _ motion False 
            (Keysym _ key _)))) = let 
        
    action = case motion of
        Pressed -> Down
        Released -> Up
        
    in case key of 
        KeycodeUp -> Keys action l r f p q
        KeycodeLeft -> Keys t action r f p q
        KeycodeRight -> Keys t l action f p q
        KeycodeSpace -> Keys t l r action p q
        KeycodeP -> Keys t l r f action q
        KeycodeQ -> Keys t l r f p action
        _ -> a

keyFromEvent a _ = a
