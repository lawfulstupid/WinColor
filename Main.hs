{-# LANGUAGE LambdaCase #-}

module WinColor.Main where

import AbLib.Control.Parser
import AbLib.Control.Alias
import Data.List (foldl')
import System.Environment (getArgs)
import Data.Char (toUpper)

data Color = Black | Red | Green | Yellow | Blue | Purple | Cyan | White
    deriving (Enum)
    
type ColorProfile = Maybe (Color, Bool) -- Bool represents bold
    
-- data TextEffect = FX
   -- { underline :: Bool
   -- , inverse   :: Bool
   -- , fgcolor   :: ColorProfile
   -- , bgcolor   :: ColorProfile }

data Bold = Dim | Bright | Dark | Light | Weak | Bold
   deriving (Eq)

instance Enum Bold where
   fromEnum b = if elem b [Dim,Dark,Weak] then 0 else 1
   toEnum 0 = Dim
   toEnum 1 = Bright

data TextEffect = Reset
   | Invert
   | Underline
   | FG Bold Color
   | BG Bold Color

-- main :: IO ()
-- main = do
   -- args <- getArgs
   -- if null args || elem "/h" args || elem "/H" args
   -- then mapM_ putStrLn help
   -- else print $ show $ apply parserFX $ unwords args

help :: [String]
help =
   [ "WINCOLOR [bg][fg] [/U] [/I]"
   , " "
   , "\tbg,fg\tColor codes for background and foreground color. Works the same as COLOR."
   , "\t/I\tSwap background and foreground colors."
   , "\t/U\tTurn on underlining."
   , "\t/H\tShow this help screen."
   ]

-- parserFX :: Parser TextEffect
-- parserFX = do
   -- let f = fmap (toUpper . head) $ matchOne "0123456789ABCDEFabcdef" 
   -- colors <- mfilter ((<= 2) . length) $ many f
   -- flags  <- many (whitespace >> matchOne ["/U", "/I", "/u", "/i"])
   -- let flag_ul = elem "/U" flags || elem "/u" flags
   -- let flag_iv = elem "/I" flags || elem "/i" flags
   -- let fx' = fx { underline = flag_ul, inverse = flag_iv }
   -- return $ case map oldToNew colors of
      -- []      -> fx'
      -- [fg]    -> fx' { fgcolor = fg }
      -- [fg,bg] -> fx' { fgcolor = fg, bgcolor = bg }
   

instance Show TextEffect where
   show effect = "\ESC[" ++ handle effect ++ "m"
      where
      handle = \case
         Reset -> ""
         Invert -> "7"
         Underline -> "4"
         (BG b c) -> shows (fromEnum b) ";" ++ shows (fromEnum c + 30) ";7"
         (FG b c) -> shows (fromEnum b) ";" ++ show (fromEnum c + 30)
