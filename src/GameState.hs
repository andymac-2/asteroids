module GameState
    ( draw
    , Game
    , updateGame
    , newGame
    ) where

import qualified SDL as SDL
import SDL (V2(V2), ($=))
import Colors (Color, white)
import Foreign.C.Types (CInt, CDouble)
import Data.Foldable (traverse_, foldl')
import Data.Fixed (mod')
import qualified Control.Monad.State as St
import qualified System.Random as RNG

import qualified Event as E
import qualified Geometry as G
import Globals (subpixels, screenWidth, screenHeight, subvelocity)
import qualified Resources as Res

type IntVec = V2 CInt


class Drawable g where
    draw :: 
        SDL.Renderer    -- ^ The renderer
        -> Res.Resources -- ^ an object rpresenting currently loaded resources
        -> g        -- ^ the GameObject itself
        -> IO ()    -- ^ the resultant action
-------------------------------------------------------------------------------
-- Utility functions

-- | Tests if a V2 is out of bounds
inBounds :: V2 CInt -> Bool
inBounds (V2 x y)
    | x >= 0 && x < screenWidth && y >=0 && y < screenHeight = True
    | otherwise = False

-- | draw a tecture to the centre of the screen
copyCentre :: SDL.Renderer -> SDL.Texture -> IO ()
copyCentre r tex = do
        (SDL.TextureInfo _ _ w h) <- SDL.queryTexture tex
        let rect = SDL.Rectangle 
                (SDL.P (V2 
                    ((G.fromSubpixels screenWidth - w) `quot` 2) 
                    ((G.fromSubpixels screenHeight - h) `quot` 2))) 
                (V2 w h)
        SDL.copy r tex Nothing (Just rect)


-------------------------------------------------------------------------------
-- | A single asteroid
data Asteroid = Asteroid
    CInt            -- ^ The radius of the asteroid in subpixels
    Int             -- ^ The sprite index number
    IntVec          -- ^ The velocity of the asteroid in subpixels per ms
    IntVec          -- ^ The position of the asteroid in subpixels
    RNG.StdGen      -- ^ The RNG for random initial velocity

-- | The initial asteroid radius in subpixels
asteroidRadius :: CInt
asteroidRadius = 32 * subpixels

asteroidMinRadius :: CInt
asteroidMinRadius = 9 * subpixels

-- | The maximum velocity of the asteroid along one axis in subvelocity units.
-- this is the maximum velocity for an asteroid of radius 1. Larger radius will
-- have correspondingly smaller maximum velocities.
asteroidMaxVel :: CInt
asteroidMaxVel = 400000

newBigAsteroid :: RNG.StdGen -> (Asteroid, RNG.StdGen)
newBigAsteroid rng = (newAsteroid rng2, rng1) where
    (rng1, rng2) = RNG.split rng

newAsteroid :: RNG.StdGen -> Asteroid
newAsteroid rng = St.evalState rngPolls rng where
    rngPolls :: St.State RNG.StdGen Asteroid
    rngPolls = go
        <$> St.state (RNG.randomR (0, velocity))
        <*> St.state (RNG.randomR (-velocity, velocity))
        <*> St.state (RNG.randomR (0, 3))
        <*> St.state (RNG.randomR (0, 3))
        <*> St.state (RNG.randomR (0, screenWidth))
        <*> St.state (RNG.randomR (0, screenHeight))
        <*> St.get
    velocity = asteroidMaxVel `quot` asteroidRadius
    -- Cases for each edge of the screen 0: top, 1: right, 2: bottom, 3: left 
    go :: CInt -> CInt -> CInt -> Int -> CInt -> CInt -> RNG.StdGen -> Asteroid
    go vx vy border sprite px py rng = case border of
        0 -> Asteroid 
            asteroidRadius sprite (V2 vy vx) (V2 px 0) rng
        1 -> Asteroid 
            asteroidRadius sprite (V2 (0 - vx) vy) (V2 screenWidth py) rng
        2 -> Asteroid 
            asteroidRadius sprite (V2 vy (0 - vx)) (V2 px screenHeight) rng
        3 -> Asteroid 
            asteroidRadius sprite (V2 vx vy) (V2 0 py) rng

-- | The graphical bounding rectangle of the asteroid
asteroidRect :: Asteroid -> SDL.Rectangle CInt
asteroidRect (Asteroid r _ _ (V2 x y) _) = let
    width = (2 * r) `quot` subpixels
    height = width
    toplx = (x - r) `quot` subpixels
    toply = (y - r) `quot` subpixels
    in SDL.Rectangle (SDL.P (V2 toplx toply)) (V2 width height)

updateAsteroid :: CInt -> Asteroid -> Asteroid
updateAsteroid dt (Asteroid r s v p rng) = 
    Asteroid r s v (G.stepPositionBounded dt v p) rng

destroyAsteroid :: Asteroid -> Asteroids
destroyAsteroid (Asteroid r _ v p rng)
    | r <= asteroidMinRadius = []
    | otherwise = 
        [ Asteroid r' sprite1 (v1 + v) p rng1' 
        , Asteroid r' sprite2 (v2 + v) p rng2'
        ]
    where
        (rng1, rng2) = RNG.split rng
        velocity = asteroidMaxVel `quot` r
        r' = r `quot` 2

        action = go
            <$> St.state (RNG.randomR (-velocity, velocity))
            <*> St.state (RNG.randomR (-velocity, velocity))
            <*> St.state (RNG.randomR (0, 3))
        
        go x y sprite = (V2 x y, sprite)
        
        ((v1, sprite1), rng1') = St.runState action rng1
        ((v2, sprite2), rng2') = St.runState action rng2

instance Drawable Asteroid where
    draw r res a@(Asteroid _ sprite _ _ _) = let
        tex = (Res.asteroidSprites $ res) !! sprite
        in SDL.copy r tex Nothing (Just . asteroidRect $ a)


-------------------------------------------------------------------------------
-- | A group of asteroids
type Asteroids = [Asteroid]

newAsteroids :: RNG.StdGen -> Int -> (Asteroids, RNG.StdGen)
newAsteroids rng num = St.runState action rng where
    action = traverse St.state . replicate num $ newBigAsteroid

updateAsteroids :: CInt -> Asteroids -> Asteroids
updateAsteroids dt = fmap (updateAsteroid dt)

instance (Drawable g) => Drawable [g] where
    draw r res gs = traverse_ (draw r res) gs 

-------------------------------------------------------------------------------
-- | The spaceship
data Ship = Ship
    IntVec      -- ^ The velocity of the spaceship in subpixels per ms
    IntVec      -- ^ The position of the spaceship in subpixels
    CDouble     -- ^ The rotation of the spaceship in radians
    Bool        -- ^ Whether or not the engines are firing

-- | Create a new ship
newShip :: Ship
newShip = Ship (V2 0 0) (V2 x y) rot False where
    x = screenWidth `quot` 2
    y = screenHeight `quot` 2
    rot = -pi / 2

-- | The ship's rotational speed when holding a direction in radians per ms
shipRotSpeed :: CDouble
shipRotSpeed = 0.004

-- | The radius of the ship's collision
shipRadius :: CInt
shipRadius = 16 * subpixels        

-- | the diamater of the ship in pixels
shipDiameter :: CInt
shipDiameter = G.fromSubpixels (shipRadius * 2)

-- | The Ship's acceleration when under power in subpixels per ms per ms
shipAccel :: CInt
shipAccel = 512

updateShip :: CInt -> E.Keys -> Ship -> Ship
updateShip dt (E.Keys t l r _ _ _) (Ship v@(V2 vx vy) p arg _) = let
    floatDt = fromIntegral dt

    thrust = case t of
        E.Up -> False
        _ -> True
    
    arg' = case l of
        E.Up -> arg
        _ -> arg - (shipRotSpeed * floatDt) `mod'` 360
    
    arg'' = case r of
        E.Up -> arg'
        _ -> arg + (shipRotSpeed * floatDt) `mod'` 360

    v' = case thrust of
        False -> v
        True -> G.stepPosition dt (G.fromPolar shipAccel arg'') v

    p' = G.stepPositionBounded dt v' p

    in Ship v' p' arg'' thrust

noFlip :: V2 Bool
noFlip = V2 False False

instance Drawable Ship where 
    draw r res (Ship _ p rot engine) = let
        rotation = G.fromRadians rot + 90
        topleft = fmap (\x -> G.fromSubpixels x - 16) p
        rect = SDL.Rectangle (SDL.P topleft) (V2 shipDiameter shipDiameter)
        tex = case engine of
            False -> Res.shipSprite res
            True -> Res.shipSpriteFiring res
        in SDL.copyEx r tex Nothing (Just rect) rotation Nothing noFlip

-------------------------------------------------------------------------------
-- | A bullet
data Bullet = Bullet
    IntVec      -- ^ The velocity of the bullet in subpixels per ms
    IntVec      -- ^ The position of the bullet in subpixels

bulletInitSpeed :: CInt
bulletInitSpeed = 4000

bulletSize :: IntVec
bulletSize = V2 3 3

bulletTopLeft :: IntVec
bulletTopLeft = V2 1 1

bulletInBounds :: Bullet -> Bool
bulletInBounds (Bullet _ p) = inBounds p

updateBullet :: CInt -> Bullet -> Bullet
updateBullet time (Bullet v p) = Bullet v (G.stepPosition time v p)

instance Drawable Bullet where
    draw r res (Bullet _ p) = do
        SDL.rendererDrawColor r $= white
        SDL.drawRect r $ Just $ SDL.Rectangle (SDL.P (p' - bulletTopLeft)) bulletSize
        where
            p' = fmap G.fromSubpixels p

-------------------------------------------------------------------------------
-- | A group of bullets
type Bullets = [Bullet]

updateBullets :: CInt -> E.Keys -> Ship -> Bullets -> Bullets
updateBullets dt ks (Ship v p r _) bullets = let
    initVel = v + G.fromPolar bulletInitSpeed r

    bullets' = case E.fire ks of
        E.Down -> Bullet initVel p : bullets
        otherwise -> bullets

    in updateBulletsNoShip dt bullets'

updateBulletsNoShip :: CInt -> Bullets -> Bullets
updateBulletsNoShip dt bs =
    filter bulletInBounds . fmap (updateBullet dt) $ bs

-- drawable insatnce already supplied by asteroids

-------------------------------------------------------------------------------
-- | The current game score.
type Score = CInt

updateScore :: CInt -> CInt -> CInt
updateScore = (+)

-------------------------------------------------------------------------------
-- | The current game level.
newtype Level = Level Int

incLevel :: Level -> Level
incLevel (Level x) = Level (x + 1)

-------------------------------------------------------------------------------
-- | The current game level.
newtype Lives = Lives Int

decLives :: Lives -> Lives
decLives (Lives x) = Lives (x - 1)

-------------------------------------------------------------------------------
-- | A currently active game
data ActiveGame 
    = ActiveGame Ship Asteroids Bullets Score RNG.StdGen Lives Level
    | FinishedLevel CInt Ship Bullets Score RNG.StdGen Lives Level
    | DeadInGame CInt Asteroids Bullets Score RNG.StdGen Lives Level

numberAsteroids :: Level -> Int
numberAsteroids (Level x) = x + 3

-- | Create a new active game
newActiveGame :: RNG.StdGen -> ActiveGame
newActiveGame rng = ActiveGame newShip as [] 0 rng' (Lives 3) (Level 1) where
    (as, rng') = newAsteroids rng (numberAsteroids (Level 1))

isGameOver :: ActiveGame -> Bool
isGameOver (DeadInGame time _ _ _ _ (Lives liv) _)
    | time <= 0 && liv <= 0 = True
    | otherwise = False
isGameOver _ = False

scoreIncrement :: CInt
scoreIncrement = 10

levelWait :: CInt
levelWait = 3000

getAgRNG :: ActiveGame -> RNG.StdGen
getAgRNG (ActiveGame _ _ _ _ rng _ _) = rng
getAgRNG (FinishedLevel _ _ _ _ rng _ _) = rng
getAgRNG (DeadInGame _ _ _ _ rng _ _) = rng


shipAsteroidsCollision :: Ship -> Asteroids -> Bool
shipAsteroidsCollision s as = any (shipAsteroidCollision s) as

shipAsteroidCollision :: Ship -> Asteroid -> Bool
shipAsteroidCollision (Ship _ sp _ _) (Asteroid r _ _ ap _) =
    G.pointCircleCollision sp (r + shipRadius) ap

bulletAsteroidCollision :: Bullet -> Asteroid -> Bool
bulletAsteroidCollision (Bullet _ pb) (Asteroid r _ _ pa _) = 
    G.pointCircleCollision pb r pa

bulletAsteroidsCollision :: Bullet -> Asteroids -> (Asteroids, Bool)
bulletAsteroidsCollision b [] = ([], False)
bulletAsteroidsCollision b (a: as) = case bulletAsteroidCollision b a of
    True -> let
        newAsteroids = destroyAsteroid a
        in (newAsteroids ++ as, True)
    False -> let
        (list, collision) = bulletAsteroidsCollision b as
        in (a: list, collision)

bulletAsteroidCollisions :: Bullets -> Asteroids -> (Bullets, Asteroids, CInt)
bulletAsteroidCollisions bs [] = (bs, [], 0)
bulletAsteroidCollisions bs as = foldl' go ([], as, 0) bs where
    go :: (Bullets, Asteroids, CInt) -> Bullet -> (Bullets, Asteroids, CInt)
    go (bs, as, score) b = let
        (as', collision) = bulletAsteroidsCollision b as
        (bs', score') = case collision of 
            True -> (bs, score + scoreIncrement)
            False -> (b: bs, score)
        in (bs', as', score')

updateActiveGame :: CInt -> E.Keys -> ActiveGame -> ActiveGame
updateActiveGame dt ks (ActiveGame sh as bs sc rng liv lev) = let
    as' = updateAsteroids dt as
    bs' = updateBullets dt ks sh bs

    (bullets, asteroids, sInc) = bulletAsteroidCollisions bs' as'

    ship = updateShip dt ks sh
    score = updateScore sInc sc

    in case (asteroids, shipAsteroidsCollision sh asteroids) of
        ([], _) -> FinishedLevel levelWait ship bullets score rng liv lev
        (_, True) -> DeadInGame levelWait asteroids bullets score rng liv lev
        (_, False) ->  ActiveGame ship asteroids bullets score rng liv lev
updateActiveGame dt ks (FinishedLevel time sh bs sc rng liv lev)
    | time < 0 = let
        lev' = incLevel lev
        (as, rng') = newAsteroids rng (numberAsteroids lev')
        in ActiveGame sh as bs sc rng' liv lev'
    | otherwise = let
        bs' = updateBullets dt ks sh bs
        sh' = updateShip dt ks sh
        in (FinishedLevel (time - dt) sh' bs' sc rng liv lev)
updateActiveGame dt ks (DeadInGame timeLeft as bs sc rng l@(Lives liv) lev)
    | timeLeft < 0 && liv > 0 && not (shipAsteroidsCollision newShip as) = 
        ActiveGame newShip as bs sc rng (decLives l) lev
    | otherwise = let
        as' = updateAsteroids dt as
        bs' = updateBulletsNoShip dt bs

        score = updateScore sInc sc
        (bullets, asteroids, sInc) = bulletAsteroidCollisions bs' as'

        in DeadInGame (timeLeft - dt) asteroids bullets score rng l lev

instance Drawable ActiveGame where
    draw r res (ActiveGame ship asteroids bullets score _ _ _) =
        draw r res asteroids
        <* draw r res bullets
        <* draw r res ship
    draw r res (FinishedLevel _ sh bs _ _ _ _) =
        draw r res bs
        <* draw r res sh
    draw r res (DeadInGame _ as bs _ _ _ _) =
        draw r res as
        *> draw r res bs


-------------------------------------------------------------------------------
-- | A currently active game
data Game 
    = BeforeBegin RNG.StdGen Asteroids
    | Active ActiveGame
    | Paused ActiveGame
    | GameOver RNG.StdGen CInt

-- | the wait between levels in ms
betweenGameWait :: CInt
betweenGameWait = 8000

newGame :: RNG.StdGen -> Game
newGame rng = BeforeBegin rng' as where
    (as, rng') = newAsteroids rng 7

updateGame :: CInt -> E.Keys -> Game -> Game
updateGame dt keys (BeforeBegin rng as) = let
    as' = updateAsteroids dt as
    in case E.fire keys of
        E.Up -> BeforeBegin rng as'
        _ -> Active $ newActiveGame rng
updateGame dt keys (Active activeGame) = let
    activeGame' = updateActiveGame dt keys activeGame
    
    isFinished = case isGameOver activeGame' of
        True -> GameOver (getAgRNG activeGame') betweenGameWait
        False -> Active activeGame'
    
    -- TODO: when ship breaks, lose a life, respawn at beiginning of level
    in case E.pause keys of
        E.Down -> Paused activeGame'
        _ -> isFinished
updateGame _ keys (Paused activeGame) = case E.pause keys of
    E.Down -> Active activeGame
    _ -> Paused activeGame
updateGame dt _ (GameOver rng timeLeft)
    | timeLeft < 0 = newGame rng
    | otherwise = (GameOver rng (timeLeft - dt))
    

instance Drawable Game where
    draw r res (BeforeBegin rng asteroids) = do
        draw r res asteroids
        copyCentre r (Res.pressSpaceText res)
    draw r res (Active activeGame) = draw r res activeGame
    draw r res (Paused activeGame) = 
        draw r res activeGame
        *> copyCentre r (Res.pausedText res)
    draw r res (GameOver _ _) = copyCentre r (Res.gameOverText res)
     
