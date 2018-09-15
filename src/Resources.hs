module Resources
    ( Resources (..)
    , textureFromBmp
    , loadResources
    ) where

import qualified SDL as SDL
import Paths_asteroids (getDataFileName)


data Resources = Resources
    { shipSprite :: SDL.Texture
    , shipSpriteFiring :: SDL.Texture

    , asteroidSprites :: [SDL.Texture]

    , pressSpaceText :: SDL.Texture
    , gameOverText :: SDL.Texture
    , pausedText :: SDL.Texture
    }

maxAsteroidSpriteIndex :: Int
maxAsteroidSpriteIndex = 3

textureFromBmp :: SDL.Renderer -> FilePath -> IO SDL.Texture
textureFromBmp renderer path = do
    fileName <- getDataFileName path
    surface <- SDL.loadBMP fileName
    SDL.createTextureFromSurface renderer surface

loadResources :: SDL.Renderer -> IO Resources
loadResources r = Resources 
    <$> textureFromBmp r "src/assets/ShipNoFire.bmp"    -- shipSprite
    <*> textureFromBmp r "src/assets/ShipFiring.bmp"    -- shipSpriteFiring
    <*> traverse (textureFromBmp r)                     -- asteroidSprites
        [ "src/assets/Asteroid1.bmp"
        , "src/assets/Asteroid2.bmp"
        , "src/assets/Asteroid3.bmp"
        , "src/assets/Asteroid4.bmp"
        ]
    <*> textureFromBmp r "src/assets/PressSpaceToBegin.bmp" -- pressSpaceText
    <*> textureFromBmp r "src/assets/GameOver.bmp"          -- gameOverText
    <*> textureFromBmp r "src/assets/Paused.bmp"            -- pausedText
