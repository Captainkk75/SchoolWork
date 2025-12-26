module LPBFParser  where

import Data.Text as T
import Data.List as L


-- Change SomeFunction to a composition of many functions that will act on the input file and then write to the output file
someFunction = id

-- 1. Reading and writing process
interactWith function inputFile outputFile = do
	input <- readFile inputFile
    -- readFile gives a String so input will be a string
	writeFile outputFile (function input)
    -- writeFile writes a String

-- 2. Split the read lines
splitLines :: String -> [String]
splitLines [] = []	
splitLines cs = 
	let (pre,suf) = L.break isLineTerminator cs
	in pre : case suf of
		('\r':'\n':rest) -> splitLines rest
		('\r':rest) -> splitLines rest
		('\n':rest) -> splitLines rest
		_ -> []
	
isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'

-- 3. Unsplit (i.e. join) the list of lines for writing
-- Opposite of splitLines for .cli files
unSplitLinesCli :: [String] -> String
unSplitLinesCli [] = []
unSplitLinesCli (x:xs) = x ++ "\r\n" ++ unSplitLines xs

-- Opposite of splitLines for .gcode files
unSplitLinesG :: [String] -> String
unSplitLinesG [] = []
unSplitLinesG (x:xs) = x ++ "\n" ++ unSplitLines xs

fixLines :: String -> String
fixLines input = L.unlines (splitLines input)


{-
0. This information is based on the file: 2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650.cli
The information could be incomplete or not completely general, update if necessary later

1. .cli files have the following structure:

$$HEADERSTART
... some header code
$$HHEADEREND
$$GEOMETRYSTART
... some geometry code
$$GEOMETRYEND

1. -- Let's first not edit any lines from the header for now
EXCEPT for maybe the DIMENSION and LAYERS lines
$$DIMENSION/-37.196,-39.703,0,39.361,49.703,14.16
$$LAYERS/236

2. The geometry lines is one of the following

-}
data GCodeCli = GCodeCli {gcode :: String, x :: Float, y :: Float, z :: Float, f :: Float}

data GCodeCliLine = GCodeCliLine {

