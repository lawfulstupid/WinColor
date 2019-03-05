{-# LANGUAGE LambdaCase #-}

module WinColor.Main where

import AbLib.Control.Parser
import AbLib.Control.Alias
import Data.List (foldl')
import System.Environment (getArgs)

data Color = Black | Red | Green | Yellow | Blue | Purple | Magenta | Cyan | White
    deriving (Enum)
    
type ColorProfile = Maybe (Color, Bool) -- Bool represents bold
    
data TextEffect = FX
    { underline :: Bool
    , inverse   :: Bool
    , fgcolor   :: Maybe (Color, Bool)
    , bgcolor   :: Maybe (Color, Bool) }

fx :: TextEffect
fx = FX False False Nothing Nothing
    
main :: IO ()
main = do
    args <- getArgs
    print $ show $ apply parserFX $ unwords args

parserFX :: Parser TextEffect
parserFX = pure fx

instance Show TextEffect where
    show (FX ul iv fg bg) = handleBG ++ handleFG fg ++ handleIV ++ handleUL
        $> foldl' (\ x y -> x ++ ";" ++ show y) ""
        $> tail
        $> ("\ESC[" ++)
        $> (++ "m")
        where
        
        handleBG :: [Int]
        handleBG = case bg of
            Nothing    -> []
            Just (c,b) -> handleFG bg ++ [7]    -- assign to FG then swap in
        
        handleFG :: ColorProfile -> [Int]
        handleFG = \case
            Nothing -> []
            Just (c,b) -> [fromEnum b, fromEnum c]
        
        handleIV :: [Int]
        handleIV = if iv then [7] else []
        
        handleUL :: [Int]
        handleUL = if ul then [4] else []