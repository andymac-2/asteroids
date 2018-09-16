module Resources
    ( Resources (..)
    , textureFromBmp
    , loadResources
    ) where

import qualified SDL as SDL
import Paths_asteroids (getDataFileName)
import qualified SDL.Font as Font


data Resources = Resources
    { shipSprite :: SDL.Texture
    , shipSpriteFiring :: SDL.Texture

    , asteroidSprites :: [SDL.Texture]

    , regularFont :: Font.Font
    , bigFont :: Font.Font
    }

maxAsteroidSpriteIndex :: Int
maxAsteroidSpriteIndex = 3

textureFromBmp :: SDL.Renderer -> FilePath -> IO SDL.Texture
textureFromBmp renderer path = do
    fileName <- getDataFileName path
    surface <- SDL.loadBMP fileName
    SDL.createTextureFromSurface renderer surface

loadFont :: FilePath -> Font.PointSize -> IO Font.Font
loadFont path size = do
    fileName <- getDataFileName path
    Font.load fileName size

loadResources :: SDL.Renderer -> IO Resources
loadResources r = Resources 
    <$> textureFromBmp r "assets/ShipNoFire.bmp"    -- shipSprite
    <*> textureFromBmp r "assets/ShipFiring.bmp"    -- shipSpriteFiring
    <*> traverse (textureFromBmp r)                     -- asteroidSprites
        [ "assets/Asteroid1.bmp"
        , "assets/Asteroid2.bmp"
        , "assets/Asteroid3.bmp"
        , "assets/Asteroid4.bmp"
        ]
    <*> loadFont "assets/PressStart2P.ttf" 17           -- regularFont
    <*> loadFont "assets/PressStart2P.ttf" 40           -- bigFont


