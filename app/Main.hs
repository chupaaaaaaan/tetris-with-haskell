{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Data.Array.IO
import Graphics.Gloss.Interface.IO.Game
import System.Random.MWC

main :: IO ()
main = do
  world <- generateNewWorld
  playIO window white 3 world drawWorld eventHandler stepWorld
  return ()

-- window parameter
wWidth, wHeight :: Num a => a
wWidth = 300
wHeight = 600

window :: Display
window = InWindow "tetris" (wWidth,wHeight) (100,100)

-- game parameter
cSize, cWidth, cHeight :: Num a => a
cSize = 20
cWidth = fromIntegral $ wWidth `div` cSize
cHeight = fromIntegral $ wHeight `div` cSize



data Tetrimino = TO { _r :: Int, _x :: Int, _y :: Int }
               | TI { _r :: Int, _x :: Int, _y :: Int }
               | TS { _r :: Int, _x :: Int, _y :: Int }
               | TZ { _r :: Int, _x :: Int, _y :: Int }
               | TJ { _r :: Int, _x :: Int, _y :: Int }
               | TL { _r :: Int, _x :: Int, _y :: Int }
               | TT { _r :: Int, _x :: Int, _y :: Int }

data TetriminoAction = TALeft
                     | TARight
                     | TADown
                     | TARotR
                     | TARotL deriving Eq
                     
moveTetrimino :: TetriminoAction -> Tetrimino -> Tetrimino
moveTetrimino TALeft  t = t { _x = _x t - 1 }
moveTetrimino TARight t = t { _x = _x t + 1 }
moveTetrimino TADown  t = t { _y = _y t - 1 }
moveTetrimino TARotR t = t { _r = (_r t-1)`mod`rotN t }
moveTetrimino TARotL t = t { _r = (_r t+1)`mod`rotN t }

rotN :: Tetrimino -> Int
rotN TO{} = 1
rotN TI{} = 2
rotN TS{} = 2
rotN TZ{} = 2
rotN TJ{} = 4
rotN TL{} = 4
rotN TT{} = 4


data GameState = InGame | GameOver

type Field = IOArray (Int,Int) (Bool, Color)

data World = World
  { _state :: GameState
  , _field :: Field
  , _tetrimino :: Tetrimino
  }

convertTetriminoToPoints :: Tetrimino -> [(Int,Int)]
convertTetriminoToPoints TO{..} = [(_x,_y),(_x-1,_y),(_x,_y-1),(_x-1,_y-1)]
convertTetriminoToPoints TI{..}
  | _r == 0 = [(_x,_y),(_x-1,_y),(_x-2,_y),(_x+1,_y)]
  | _r == 1 = [(_x,_y),(_x,_y-1),(_x,_y-2),(_x,_y+1)]
convertTetriminoToPoints TS{..}
  | _r == 0 = [(_x,_y),(_x+1,_y),(_x,_y-1),(_x-1,_y-1)]
  | _r == 1 = [(_x,_y),(_x,_y-1),(_x-1,_y),(_x-1,_y+1)]
convertTetriminoToPoints TZ{..}
  | _r == 0 = [(_x,_y),(_x-1,_y),(_x,_y-1),(_x+1,_y-1)]
  | _r == 1 = [(_x,_y),(_x,_y+1),(_x-1,_y),(_x-1,_y-1)]
convertTetriminoToPoints TJ{..}
  | _r == 0 = [(_x,_y),(_x-1,_y),(_x+1,_y),(_x+1,_y-1)]
  | _r == 1 = [(_x,_y),(_x,_y-1),(_x,_y+1),(_x+1,_y+1)]
  | _r == 2 = [(_x,_y),(_x-1,_y),(_x+1,_y),(_x-1,_y+1)]
  | _r == 3 = [(_x,_y),(_x,_y-1),(_x,_y+1),(_x-1,_y-1)]
convertTetriminoToPoints TL{..}
  | _r == 0 = [(_x,_y),(_x-1,_y),(_x+1,_y),(_x-1,_y-1)]
  | _r == 1 = [(_x,_y),(_x,_y-1),(_x,_y+1),(_x+1,_y-1)]
  | _r == 2 = [(_x,_y),(_x-1,_y),(_x+1,_y),(_x+1,_y+1)]
  | _r == 3 = [(_x,_y),(_x,_y-1),(_x,_y+1),(_x-1,_y+1)]
convertTetriminoToPoints TT{..}
  | _r == 0 = [(_x,_y),(_x-1,_y),(_x+1,_y),(_x,_y-1)]
  | _r == 1 = [(_x,_y),(_x,_y-1),(_x,_y+1),(_x+1,_y)]
  | _r == 2 = [(_x,_y),(_x-1,_y),(_x+1,_y),(_x,_y+1)]
  | _r == 3 = [(_x,_y),(_x,_y-1),(_x,_y+1),(_x-1,_y)]

randomInitTetrimino :: GenIO -> IO Tetrimino
randomInitTetrimino gen = convertIntToInitTetrimino <$> uniformR (0,6) gen <*> uniformR (0,1) gen <*> uniformR (0,3) gen
  where convertIntToInitTetrimino :: Int -> Int -> Int -> Tetrimino
        convertIntToInitTetrimino n x r
          | n == 0 = TO { _r = 0,         _x = cWidth `div` 2 - x, _y = cHeight + 1 }
          | n == 1 = TI { _r = r `mod` 2, _x = cWidth `div` 2 - x, _y = cHeight + 1 }
          | n == 2 = TS { _r = r `mod` 2, _x = cWidth `div` 2 - x, _y = cHeight + 1 }
          | n == 3 = TZ { _r = r `mod` 2, _x = cWidth `div` 2 - x, _y = cHeight + 1 }
          | n == 4 = TJ { _r = r `mod` 4, _x = cWidth `div` 2 - x, _y = cHeight + 1 }
          | n == 5 = TL { _r = r `mod` 4, _x = cWidth `div` 2 - x, _y = cHeight + 1 }
          | n == 6 = TT { _r = r `mod` 4, _x = cWidth `div` 2 - x, _y = cHeight + 1 }
          | otherwise = convertIntToInitTetrimino (n`mod`7) x r

generateNewWorld :: IO World
generateNewWorld = do
  field <- newArray ((0,0), (cWidth-1,cHeight+2)) (False, white)
  tetrimino <- withSystemRandom . asGenIO $ \gen -> randomInitTetrimino gen
  return $ World { _state = InGame
                 , _field = field
                 , _tetrimino = tetrimino
                 }

checkCollided :: Field -> Tetrimino -> IO Bool
checkCollided f t = or <$> forM (convertTetriminoToPoints t) (fmap fst . readArray f)

checkOutOfField :: Tetrimino -> IO Bool
checkOutOfField t = or <$> forM (convertTetriminoToPoints t) (\(x,y) -> pure $ x < 0 || x >= cWidth || y < 0)

eventHandler :: Event -> World -> IO World
eventHandler e w@World{..} = case _state of
  InGame -> case e of
    EventKey (SpecialKey KeyDown) Down _ _  -> nextWorld $ moveTetrimino TADown _tetrimino
    EventKey (SpecialKey KeyRight) Down _ _ -> nextWorld $ moveTetrimino TARight _tetrimino
    EventKey (SpecialKey KeyLeft) Down _ _  -> nextWorld $ moveTetrimino TALeft _tetrimino
    EventKey (Char 'z') Down _ _            -> nextWorld $ moveTetrimino TARotL _tetrimino
    EventKey (Char 'x') Down _ _            -> nextWorld $ moveTetrimino TARotR _tetrimino
    _                                       -> pure w
  GameOver -> case e of
    EventKey (SpecialKey KeyEnter) Down _ _ -> generateNewWorld
    _                                       -> pure w
  where nextWorld nt = do
          wall <- checkOutOfField nt
          if wall then pure w else do
            coll <- checkCollided _field nt
            pure $ if coll then w else w { _tetrimino = nt }

eraseRows :: Field -> IO Field
eraseRows f = go 0 0
  where go y h
          | y + h >= cHeight = do
              uBoundY <- snd . snd <$> getBounds f
              forM_ [y..uBoundY] $ \y' -> forM_ [0..cWidth-1] $ \x -> writeArray f (x,y') (False, greyN 0.5)
              pure f
          | otherwise = do
              forM_ [0..cWidth-1] $ \x -> readArray f (x,y+h) >>= writeArray f (x,y)
              mino <- and <$> forM [0..cWidth-1] (\x -> fst <$> readArray f (x,y))
              if mino then go y (h+1) else go (y+1) h

stepWorld :: Float -> World -> IO World
stepWorld _ w@World{..} = case _state of
  InGame -> do
    let nextTetrimino = moveTetrimino TADown _tetrimino
    wall <- checkOutOfField nextTetrimino
    if wall then nextWorldWithNewField
      else do coll <- checkCollided _field nextTetrimino
              if coll then nextWorldWithNewField
                else pure w { _tetrimino = nextTetrimino }
  GameOver -> pure w

  where nextWorldWithNewField = do
          forM_ (convertTetriminoToPoints _tetrimino) $ \p -> writeArray _field p (True, greyN 0.5)
          newField <- eraseRows _field
          r1 <- fst <$> readArray newField (cWidth `div` 2 - 1, cHeight - 1)
          r2 <- fst <$> readArray newField (cWidth `div` 2    , cHeight - 1)
          if r1 || r2
            then pure w { _field = newField, _state = GameOver }
            else do newTetrimino <- withSystemRandom . asGenIO $ randomInitTetrimino
                    pure w { _field = newField, _tetrimino = newTetrimino }

cell :: Picture
cell = polygon [(0,0),(0,cSize),(cSize,cSize),(cSize,0)]

drawCell :: Color -> (Int,Int) -> Picture
drawCell c (x, y) = translate (fromIntegral x * cSize - wWidth / 2) (fromIntegral y * cSize - wHeight / 2) $ color c cell

drawTetrimino :: Color -> Tetrimino -> Picture
drawTetrimino c t = pictures $ map (drawCell c) (convertTetriminoToPoints t)

drawWorld :: World -> IO Picture
drawWorld World{..} = do
  blocks <- filter (\(_,(b,_)) -> b) <$> getAssocs _field
  case _state of
    InGame -> pure $ pictures
        [ pictures $ map (\(xy,(_,c)) -> drawCell c xy) blocks
        , drawTetrimino blue _tetrimino
        ]
    GameOver -> pure $ pictures
        [ pictures $ map (\(xy,(_,c)) -> drawCell c xy) blocks
        , drawTetrimino red _tetrimino
        , translate (-wWidth / 2 + cSize) (-wHeight / 2 + cSize) . scale 0.2 0.2 $ text "Game Over!"
        ]
