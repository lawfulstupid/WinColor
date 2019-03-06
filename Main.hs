{-# LANGUAGE LambdaCase #-}

module WinColor.Main where

import AbLib.Control.Parser
import AbLib.Control.Alias
import Data.List (foldl')
import System.Environment (getArgs)
import Data.Char (toUpper)

data Color = Black | Red | Green | Yellow | Blue | Purple | Magenta | Cyan | White
    deriving (Enum)
    
type ColorProfile = Maybe (Color, Bool) -- Bool represents bold
    
data TextEffect = FX
   { underline :: Bool
   , inverse   :: Bool
   , fgcolor   :: ColorProfile
   , bgcolor   :: ColorProfile }

-- The base 'do nothing' effect
fx :: TextEffect
fx = FX False False Nothing Nothing

main :: IO ()
main = do
   args <- getArgs
   if null args || elem "/h" args || elem "/H" args
   then mapM_ putStrLn help
   else print $ show $ apply parserFX $ unwords args

help :: [String]
help =
   [ "WINCOLOR [bg][fg] [/U] [/I]"
   , " "
   , "\tbg,fg\tColor codes for background and foreground color. Works the same as COLOR."
   , "\t/I\tSwap background and foreground colors."
   , "\t/U\tTurn on underlining."
   , "\t/H\tShow this help screen."
   ]

parserFX :: Parser TextEffect
parserFX = do
   let f = fmap (toUpper . head) $ matchOne "0123456789ABCDEFabcdef" 
   colors <- mfilter ((<= 2) . length) $ many f
   flags  <- many (whitespace >> matchOne ["/U", "/I", "/u", "/i"])
   let flag_ul = elem "/U" flags || elem "/u" flags
   let flag_iv = elem "/I" flags || elem "/i" flags
   let fx' = fx { underline = flag_ul, inverse = flag_iv }
   return $ case map oldToNew colors of
      []      -> fx'
      [fg]    -> fx' { fgcolor = fg }
      [fg,bg] -> fx' { fgcolor = fg, bgcolor = bg }

oldToNew :: Char -> ColorProfile
oldToNew = \case
   '0' -> Just (Black, False)
   '8' -> Just (Black, True)
   '1' -> Just (Blue, False)
   '9' -> Just (Blue, True)
   '2' -> Just (Green, False)
   'A' -> Just (Green, True)
   '3' -> Just (Cyan, False)
   'B' -> Just (Cyan, True)
   '4' -> Just (Red, False)
   'C' -> Just (Red, True)
   '5' -> Just (Purple, False)
   'D' -> Just (Purple, True)
   '6' -> Just (Yellow, False)
   'E' -> Just (Yellow, True)
   '7' -> Just (White, False)
   'F' -> Just (White, True)
   _   -> Nothing
   

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
         Nothing    -> []
         Just (c,b) -> [fromEnum b, fromEnum c]
        
      handleIV :: [Int]
      handleIV = if iv then [7] else []
        
      handleUL :: [Int]
      handleUL = if ul then [4] else []
