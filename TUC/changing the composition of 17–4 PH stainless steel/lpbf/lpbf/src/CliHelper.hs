module CliHelper where

import Data.Semigroup (Max(Max))
import Data.List as L
import Control.Monad (when)
import Data.Char as C
-- import System.Environment (readFile)

myCliFilePath :: String
myCliFilePath = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/Thesis/try/2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650.cli"

mySLMFilePath :: String
mySLMFilePath = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/Thesis/try/20240208-17-4PH_100W.slm"

myGcodeFilePath :: String
myGcodeFilePath = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/gcode files/Impact Test Samples 4 03-12-2024 rate.gcode"

{-
myRead :: String -> IO [String]
myRead path = do
    contents <- readFile path
    let myLines = words contents
    return myLines

myReadContents = myRead myCliFilePath

printFirstNLines :: Int -> [String] -> IO ()
printFirstNLines n listOfStrings = mapM_ (mapM_ print) (take n myReadContents)
-}

-- Int describes the length of the string that you want to print

myRawReader :: Int-> Int -> String -> IO ()
myRawReader myPrintLength mySkipLength myFilePath = do
    filecontent <- readFile myFilePath
    print (take myPrintLength (drop mySkipLength filecontent))

{-
the .cli file has \r\n as line terminators between the different lines of the file when the file content is taken as a string

Examples: 

>>> myRawReader 100 1000 myCliFilePath 
"\252rfel_10x10_sup_4_Part_Skin Filling\r\n$$LABEL/20,4_W\252rfel_10x10_sup_4_1_Support_Core Filling\r\n$$LABEL"

>>> myRawReader 100 10000 myCliFilePath 
"POLYLINE/10,2,2,-26.405,-0.265,-25.991,-0.678\r\n$$POLYLINE/10,2,2,-23.294,0.867,-22.88,0.453\r\n$$POLYL"


BUT !!!
the .gcode file only has \n as line terminators between the different lines of the file when the file content is taken as a string

Examples:

>>> myRawReader 100 100 myGcodeFilePath  
"extrusion width = 0.11mm\n; perimeters extrusion width = 0.11mm\n; infill extrusion width = 0.11mm\n; s"

>>> myRawReader 100 10000 myGcodeFilePath  
"07 E.31664\nG1 X73.713 Y28.509 E.31671\nG1 X71.287 Y28.509 E.31846\nG1 X71.287 Y28.611 E.31853\nG1 X73.7"
-}

myReader :: String -> IO [String]
myReader myFilePath = do
    filecontent <- readFile myFilePath
    let myLines = lines filecontent
    return myLines

myReaderTempPrint :: String -> Int -> Int -> IO ()
myReaderTempPrint myFilePath nSkipLines nPrintLines = do
    filecontent <- readFile myFilePath
    let myLines = lines filecontent
    print (take nPrintLines (drop nSkipLines myLines))

myReaderTempPrint2 :: String -> Int -> Int -> IO ()
myReaderTempPrint2 myFilePath nSkipLines nPrintLines = do
    myLines <- myReader myFilePath
    print (take nPrintLines (drop nSkipLines myLines))

myPowerChangePrinter :: String -> Int -> IO ()
myPowerChangePrinter myFilePath maxLines = do
    myLines <- myReader myFilePath
    let myPowerChangeLines = take maxLines (filter (L.isInfixOf "POWER") myLines)
    mapM_ print myPowerChangeLines



-------------------------------
myReaderTemp2 :: String -> Int -> Int -> IO [String]
myReaderTemp2 myFilePath nSkipLines nPrintLines = do
    myLines <- myReader myFilePath
    return (take nPrintLines (drop nSkipLines myLines))

{-
Example: 
>>> trimCli "$$POLYLINE/22,2,5,-4.561,20.203,-4.561,10.483,5.159,10.483,5.159,20.203,-4.561,20.203\r"
"POLYLINE/22,2,5,-4.561,20.203,-4.561,10.483,5.159,10.483,5.159,20.203,-4.561,20.203"

Be careful: This function does not detect $$ signs at the beginning of the line or \r at the end of the line
It just drops the first 2 characters in the line which is assumed to always be $$
and the last character in the line which is always assumed to be \r (\n is already taken care of during reading of the file,
using the lines function somewhere)
-}
trimCli :: String -> String
trimCli myLine = drop 2 (init myLine)


-- Q. I would like to get a list of all the keywords/constructors in a list of lines gotten from a .cli file 

getCliLineType :: String -> String
-- The input is a string of the format "POLYLINE/3,2,5,-27.396,34.033,-37.196,34.033,-37.196,24.233,-27.396,24.233,-27.396,34.033"
-- The keyword or data constructor in the beginning seems to be upper case, so let's use that to get the Constructor in a line
getCliLineType [] = []
getCliLineType myLine = fst (span C.isUpper myLine)


-- Example: >>> span isUpper "POLYLINE/2,55"
-- ("POLYLINE","/2,55")

getCliLineTypes :: [String] -> [String]
-- takes in a list of lines without $$ at the beginning and \r at the end and 
-- returns the keywords used in the lines: []
getCliLineTypes [] = []
getCliLineTypes (firstLine:otherLines) = uniqueConstructors where
    allConstructors = (getCliLineType firstLine):(getCliLineTypes otherLines)
    uniqueConstructors = L.nub allConstructors

checkConstructors :: String -> IO ()
checkConstructors filePath = do
    cliLines <- myReader filePath
    let (_,geometryLines,_) = splitGeometryLines cliLines
    let cliTrimmedLines = map trimCli geometryLines
    let lineConstructors = getCliLineTypes cliTrimmedLines
    print lineConstructors

-- getCliLineTypes (map trimCli (myReader myCliFilePath)) -- This has an error, need to extract value outside the IO monad

-- Q. But first I need to drop the header section 
splitGeometryLines :: [String] -> ([String],[String],[String])
splitGeometryLines [] = ([],[],[])
splitGeometryLines allCliLines = (headerLines, geometryLines, footerLines) where
    headerRestTuple = break (L.isPrefixOf "$$GEOMETRYSTART") allCliLines
    headerLines = (fst headerRestTuple) ++ [(head (snd headerRestTuple))]

    geometryFooterTuple = break (L.isPrefixOf "$$GEOMETRYEND") (tail (snd headerRestTuple))
    geometryLines = fst geometryFooterTuple
    footerLines = snd geometryFooterTuple