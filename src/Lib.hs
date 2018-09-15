{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( main
    ) where

import qualified SDL as SDL
import SDL (($=))
import SDL.Vect (V4(V4), V2(V2))

import qualified Event as E
import GameState 
    ( newGame
    , updateGame
    , Game
    )
import qualified GameState as G
import Resources (Resources, loadResources)

import Colors (Color, black)
import Foreign.C.Types (CInt)

import qualified System.Random as RNG

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "My SDL Application" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    resources <- loadResources renderer
    rng <- RNG.newStdGen

    let state = newGame rng
    appLoop 0 E.noKeys renderer resources state

appLoop :: CInt -> E.Keys -> SDL.Renderer -> Resources -> Game -> IO ()
appLoop time' keys' renderer resources state = do
    keys <- E.keysFromEvents keys' <$> SDL.pollEvents
    time <- fromIntegral <$> SDL.ticks

    let dt = time - time'
        state' = updateGame dt keys state

    SDL.rendererDrawColor renderer $= black
    SDL.clear renderer

    G.draw renderer resources state'

    SDL.present renderer
    SDL.delay (10)
    if (E.quit keys == E.Held)
        then SDL.quit
        else appLoop time keys renderer resources state'
