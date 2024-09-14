module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (mapMaybe)

data Direction = LeftDirection | RightDirection deriving Eq

data GameState = GameState
    { mario :: (Float, Float)
    , characterState :: CharacterState
    , backgroundImage :: Picture
    , movingUp :: Bool
    , movingDown :: Bool
    , movingLeft :: Bool
    , movingRight :: Bool
    , isJumping :: Bool
    , jumping :: Maybe Float
    , barrels :: [Barrel]
    , barrelImage :: Picture
    , lastBarrelTime :: Float
    }

data CharacterState = CharacterState
    { characterDirection :: Direction
    , characterImageLeft :: Picture
    , characterImageRight :: Picture
    }

data Barrel = Barrel
    { pos :: (Float, Float)
    , image :: Picture
    }

initialState :: IO GameState
initialState = do
    charLeft <- loadBMP "images/mario1.bmp"
    charRight <- loadBMP "images/mario2.bmp"
    barril <- loadBMP "images/barril.bmp"
    bgBMP <- loadBMP "images/donkeykong.bmp"
    return GameState
        { mario = (-159, -197)
        , characterState = CharacterState
            { characterDirection = RightDirection
            , characterImageLeft = charLeft
            , characterImageRight = charRight
            }
        , backgroundImage = bgBMP
        , movingUp = False
        , movingDown = False
        , movingLeft = False
        , movingRight = False
        , isJumping = False
        , jumping = Nothing
        , barrels = []
        , barrelImage = barril
        , lastBarrelTime = 0
        }

windowDisplay :: Display
windowDisplay = InWindow "Donkey Kong" (637, 637) (100, 100)

main :: IO ()
main = do
    gameState <- initialState
    play windowDisplay white 60 gameState renderGame handleEvent updateGame

renderGame :: GameState -> Picture
renderGame gameState =
    pictures
        [ background
        , translate x y charImage
        , pictures barrelImages
        ]
    where
        background = backgroundImage gameState
        (x, y) = mario gameState
        charState = characterState gameState
        charImage = if characterDirection charState == RightDirection
                        then characterImageRight charState
                    else characterImageLeft charState
        barrelImages = map renderBarrel (barrels gameState)

-- renderiza um unico barril
renderBarrel :: Barrel -> Picture
renderBarrel barrel = translate x y (image barrel)
    where
        (x, y) = pos barrel

-- Criar um novo barril com base na imagem carregada
createBarrel :: (Float, Float) -> Picture -> Barrel
createBarrel position img = Barrel { pos = position, image = img }

addNewBarrel :: [Barrel] -> Picture -> [Barrel]
addNewBarrel bs img = newBarrel : bs
  where
    newBarrel = createBarrel (-150, 170) img

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) gameState =
    gameState { movingLeft = True, characterState = (characterState gameState) { characterDirection = LeftDirection } }
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) gameState =
    gameState { movingLeft = False }

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) gameState =
    gameState { movingRight = True, characterState = (characterState gameState) { characterDirection = RightDirection } }
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) gameState =
    gameState { movingRight = False }

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) gameState =
    gameState { movingUp = True }
handleEvent (EventKey (SpecialKey KeyUp) Up _ _) gameState =
    gameState { movingUp = False }
    
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) gameState =
    gameState { movingDown = True }
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) gameState =
    gameState { movingDown = False }

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gameState =
    gameState { isJumping = True }

handleEvent _ gameState = gameState

-- Atualiza posição de cada barril
updateBarrelPosition :: Barrel -> Maybe Barrel
updateBarrelPosition barrel =
    if y' <= -300
    then Nothing  -- Retorna Nothing para indicar que o barril deve ser removido
    else Just (barrel { pos = (x', y') }) -- Retorna Just com a posição atualizada
    where
        (x, y) = pos barrel
        (x', y')
            | y >= 164 =
                if ( x==130 || x==70 || x==10 ) then (x+5,y-2)
                else if (x==170) then (x,y-5)
                else (x+5,y)
            | y >= 100 =
                (x,y-5)

            | y >= 84 =
                if (   x==130 || x==70 || x==10 || x==(-50) || x == (-115)
                    || x == (-160) || x == (-215) ) then (x-5,y-2)
                else if (x==(-250)) then (x,y-5)
                else (x-5,y)
            | y >= 32 = 
                (x,y-5)

            | y >= (-20) =
                if (   x==135 || x==100 || x==70 || x==10 || x==(-50) || x == (-115)
                    || x == 0 || x == (-160) || x == (-215) ) then (x+5,y-2)
                else if (x==190) then (x,y-5)
                else (x+5,y)
            | y >= (-46) =
                (x,y-5)

            | y >= (-70) =
                if (   x==135 || x==100 || x==70 || x==10 || x==(-50) || x == (-115)
                    || x == 0 || x == (-160) || x == (-215) ) then (x-5,y-2)
                else if (x==(-235)) then (x,y-5)
                else (x-5,y)
            -- mudar a partir daqui
            | y >= (-120) =
                (x,y-5)

            | y >= (-180) =
                if (   x==135 || x==100 || x==70 || x==10 || x==(-50) || x == (-115)
                    || x == 0 || x == (-160) || x == (-215) ) then (x+5,y-2)
                else if (x==270) then (x,y-5)
                else (x+5,y)
            | y >= (-195) =
                (x,y-5)

            | y >= (-250) =
                if (   x == 5 || x == 70 || x == 135 || x == 195
                    || x == 250 ) then (x-5,y-2)
                else if (x==(-290)) then (x,y-5)
                else (x-5,y)

            | otherwise =
                (x,y-5)

updateGame :: Float -> GameState -> GameState
updateGame deltaTime gameState =
    gameState { mario = newPosition, isJumping = newIsJump, jumping = newJumping
                , barrels = updatedBarrels, lastBarrelTime = newBarrelTime
                }
    where

        currentTime = lastBarrelTime gameState + deltaTime
        newBarrelTime = if currentTime >= 2 then currentTime - 2 else currentTime

        updatedBarrels' = if currentTime >= 2
                         then addNewBarrel (barrels gameState) (barrelImage gameState)
                         else barrels gameState

        updatedBarrels = mapMaybe updateBarrelPosition (updatedBarrels')

        (x, yInit) = mario gameState
        alturaPulo = 3
        -- isJumping :: Bool, jumping :: Maybe Float
        newJumping = case jumping gameState of
            Nothing -> if isJumping gameState
                           then Just 1
                           else Nothing
            Just v -> case v of
                20 -> Nothing
                _ -> Just (v+1) 

        newIsJump = False  -- Resetando o valor de jump

        acumulado
            | Just jumpY <- jumping gameState = if jumpY <= 10
                                           then jumpY*alturaPulo
                                         else if jumpY < 20
                                            then (-(20-jumpY)*alturaPulo)
                                         else (-alturaPulo)
            | otherwise = 0

        y
            | acumulado == 0 = yInit
            | acumulado > 0  = yInit - acumulado
            | otherwise      = yInit + acumulado 
        
        (x', y')
            | ( y == (-197) || y <= (-187) ) =
                --degraus
                 if (x == 6 || x == 72 || x == 135 || x == 195 || x == 252 ) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , stepNewCoord y (movingLeft gameState) (movingRight gameState) )
                --escada
                 else if (x >= 200 && x <= 215 && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (-127) (-189) (movingDown gameState) (movingUp gameState) )
                 -- limites
                 else if ( x <= (-260) ) then
                    ( calculateNewCoord x (False) (movingRight gameState)
                      , y )
                 else if ( x >= 300 ) then
                    ( calculateNewCoord x (movingLeft gameState) (False)
                      , y )

                 else
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , y )
            
            --escadas
            | y < (-127) && (x >= 200 && x <= 215 && acumulado == 0) =
                ( x
                , ladderNewCoord y (-127) (-189) (movingDown gameState) (movingUp gameState) )

            | y <= (-107) =
                --degraus
                 if (   x == 195 || x == 135 || x == 72 || x == 12 || x == (-51)
                     || x == (-114) || x == (-171) || x == (-213) ) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , stepNewCoord' y (movingLeft gameState) (movingRight gameState) )
                --escadas
                 else if (x >= 201 && x <= 215 && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (-127) (-189) (movingDown gameState) (movingUp gameState) )
                 else if (x >= (-20) && x <= (-5) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (-45) (-117) (movingDown gameState) (movingUp gameState) )
                 else if (x >= (-180) && x <= (-160) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (-51) (-112) (movingDown gameState) (movingUp gameState) )
                 else if (x>=252) then
                    ( calculateNewCoord x (movingLeft gameState) (False)
                      , y )
                -- limites
                 else if ( x <= (-245) ) then
                    ( calculateNewCoord x (False) (movingRight gameState)
                      , y )
                 else if ( x >= 245 && y <= (-117) && acumulado > 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , y )
                 else if ( x >= 245 ) then
                    ( calculateNewCoord x (movingLeft gameState) (False)
                      , y )

                 else
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , y )

            | y < (-45) && (x >= (-20) && x <= (-5)) && acumulado == 0 =
                ( x
                , ladderNewCoord y (-45) (-117) (movingDown gameState) (movingUp gameState) )
            
            | y < (-51) && (x >= (-180) && x <= (-160)) && acumulado == 0 =
                ( x
                , ladderNewCoord y (-51) (-112) (movingDown gameState) (movingUp gameState) )
            
            | y == (-45) && (x >= (-20) && x <= (-5)) && acumulado == 0 =
                ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                , ladderNewCoord y (-45) (-117) (movingDown gameState) (movingUp gameState) )

            | y <= (-38) =
                -- degraus
                if (  x == 135 || x == 72 || x == 12 || x == (-51)
                     || x == (-114) || x == (-159) || x == (-213) ) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , stepNewCoord y (movingLeft gameState) (movingRight gameState) )
                -- escadas
                else if (x >= (-180) && x <= (-160) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (-51) (-112) (movingDown gameState) (movingUp gameState) )
                else if (x >= (20) && x <= (35) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (30) (-43) (movingDown gameState) (movingUp gameState) )
                -- limites
                else if ( x <= (-210) ) then
                    ( calculateNewCoord x (False) (movingRight gameState)
                      , y )
                else if ( x >= 205 ) then
                    ( calculateNewCoord x (movingLeft gameState) (False)
                      , y )

                else
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , y )

            | y < (30) && (x >= (20) && x <= (35) && acumulado == 0) =
                ( x
                , ladderNewCoord y (30) (-43) (movingDown gameState) (movingUp gameState) )

            | y <= (40) =
                -- degraus
                if (  x == 135 || x == 72 || x == 12 || x == (-51)
                     || x == (-114) || x == (-159) || x == (-213) ) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , stepNewCoord' y (movingLeft gameState) (movingRight gameState) )
                -- escadas
                else if (x >= (20) && x <= (35) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (30) (-43) (movingDown gameState) (movingUp gameState) )
                else if (x >= (-75) && x <= (-65) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (102) (34) (movingDown gameState) (movingUp gameState) )
                else if (x >= (-195) && x <= (-180) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (98) (38) (movingDown gameState) (movingUp gameState) )
                -- limites
                 else if ( x <= (-245) ) then
                    ( calculateNewCoord x (False) (movingRight gameState)
                      , y )
                 else if ( x >= 175 ) then
                    ( calculateNewCoord x (movingLeft gameState) (False)
                      , y )

                else
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , y )

            | y < (102) && (x >= (-75) && x <= (-65)) && acumulado == 0 =
                ( x
                , ladderNewCoord y (102) (34) (movingDown gameState) (movingUp gameState) )
            | y < (98) && (x >= (-195) && x <= (-180)) && acumulado == 0 =
                ( x
                , ladderNewCoord y (98) (38) (movingDown gameState) (movingUp gameState) )

            | y <= 110 =
                -- degraus
                if (  x == 129 || x == 72 || x == 12 || x == (-51)
                     || x == (-114) || x == (-159) || x == (-213) ) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , stepNewCoord y (movingLeft gameState) (movingRight gameState) )
                -- escadas
                else if (x >= (-75) && x <= (-65) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (102) (34) (movingDown gameState) (movingUp gameState) )
                else if (x >= (-195) && x <= (-180) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (98) (38) (movingDown gameState) (movingUp gameState) )
                else if (x >= (120) && x <= (135) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (174) (110) (movingDown gameState) (movingUp gameState) )
                -- limites
                else if ( x <= (-230) ) then
                    ( calculateNewCoord x (False) (movingRight gameState)
                      , y )
                else if ( x >= 200 ) then
                    ( calculateNewCoord x (movingLeft gameState) (False)
                      , y )
                else
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , y )

            | y < (174) && (x >= (120) && x <= (135)) && acumulado == 0 =
                ( x
                , ladderNewCoord y (174) (110) (movingDown gameState) (movingUp gameState) )

            | y <= 184 =
                -- degraus
                if (  x == 129 || x == 72 || x == 12  ) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , stepNewCoord' y (movingLeft gameState) (movingRight gameState) )
                -- escadas
                else if (x >= (120) && x <= (135) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (174) (110) (movingDown gameState) (movingUp gameState) )
                else if (x >= (40) && x <= (55) && acumulado == 0) then
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , ladderNewCoord y (242) (176) (movingDown gameState) (movingUp gameState) )
                -- limites
                 else if ( x <= (-200) ) then
                    ( calculateNewCoord x (False) (movingRight gameState)
                      , y )
                 else if ( x >= 160 ) then
                    ( calculateNewCoord x (movingLeft gameState) (False)
                      , y )
                 else
                    ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                      , y )
            | y < (242) && (x >= (40) && x <= (55) && acumulado == 0) =
                ( x
                , ladderNewCoord y (242) (176) (movingDown gameState) (movingUp gameState) )

            | y <= 242 =
                -- limites
                if ( x <= (-25) ) then
                    ( calculateNewCoord x (False) (movingRight gameState)
                      , y )
                else if ( x >= 55 ) then
                    ( calculateNewCoord x (movingLeft gameState) (False)
                      , y )
                else
                ( calculateNewCoord x (movingLeft gameState) (movingRight gameState)
                , y )

            | otherwise =  ( x
                            , y )

        newPosition
            | acumulado == 0 = (x',y')
            | acumulado > 0 = (x',y'+acumulado+alturaPulo)
            | otherwise = (x',y'-acumulado-alturaPulo)

calculateNewCoord :: Float -> Bool -> Bool -> Float
calculateNewCoord currentCoord moveNegative movePositive
    | moveNegative = currentCoord - 3
    | movePositive = currentCoord + 3
    | otherwise = currentCoord

-- sobe e desce degrau
stepNewCoord :: Float -> Bool -> Bool -> Float
stepNewCoord currentCoord moveNegative movePositive
    | moveNegative = currentCoord - 2
    | movePositive = currentCoord + 2
    | otherwise = currentCoord

stepNewCoord' :: Float -> Bool -> Bool -> Float
stepNewCoord' currentCoord moveNegative movePositive
    | moveNegative = currentCoord + 2
    | movePositive = currentCoord - 2
    | otherwise = currentCoord

-- sobe e desce escada
ladderNewCoord :: Float -> Float -> Float -> Bool -> Bool -> Float
ladderNewCoord currentCoord maximo minimo moveNegative movePositive
    | moveNegative = max minimo (currentCoord - 3)
    | movePositive = min maximo (currentCoord + 3)
    | otherwise = currentCoord