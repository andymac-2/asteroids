{-# LANGUAGE TemplateHaskell #-}
module Resources
    ( Resources (..)
    , loadResources
    ) where

import qualified SDL as SDL
import Paths_asteroids (getDataFileName)
import qualified SDL.Font as Font
import SDL.Image (decodeTexture)

import Data.FileEmbed (embedFile)


data Resources = Resources
    { shipSprite :: SDL.Texture
    , shipSpriteFiring :: SDL.Texture

    , asteroidSprites :: [SDL.Texture]

    , regularFont :: Font.Font
    , bigFont :: Font.Font
    }

maxAsteroidSpriteIndex :: Int
maxAsteroidSpriteIndex = 3


loadResources :: SDL.Renderer -> IO Resources
loadResources r = Resources 
    <$> decodeTexture r $(embedFile "assets/ShipNoFire.bmp")    -- shipSprite
    <*> decodeTexture r $(embedFile "assets/ShipFiring.bmp")    -- shipSpriteFiring
    <*> traverse (decodeTexture r)                     -- asteroidSprites
        [ $(embedFile  "assets/Asteroid1.bmp")
        , $(embedFile  "assets/Asteroid2.bmp")
        , $(embedFile  "assets/Asteroid3.bmp")
        , $(embedFile  "assets/Asteroid4.bmp")
        ]
    <*> Font.decode $(embedFile "assets/PressStart2P.ttf") 17       -- regularFont
    <*> Font.decode $(embedFile "assets/PressStart2P.ttf") 40    -- bigFont


