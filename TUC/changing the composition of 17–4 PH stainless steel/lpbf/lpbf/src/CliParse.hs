module CliParse where
--module Main where

import Data.Text as T
import Text.Printf as TP
import Data.List as L
import Data.Char as C
import Control.Monad (forM_)
import Data.Time (getCurrentTime, diffUTCTime)
import Numeric (showFFloat, readFloat)
import Control.Monad.State
import Control.Exception (try, IOException)

-- Helper: Check if file exists (without System.Directory dependency)
fileExists :: FilePath -> IO Bool
fileExists path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> return False
        Right _ -> return True

-- Helper: serialize LaserState to string
serializeLaserState :: LaserState -> String
serializeLaserState (LaserState mp ms mf) =
    let powerStr = case mp of 
            Just (Power p) -> show p
            Nothing -> "Nothing"
        speedStr = case ms of
            Just (Speed s) -> show s  
            Nothing -> "Nothing"
        focusStr = case mf of
            Just (Focus f) -> show f
            Nothing -> "Nothing"
    in powerStr ++ "," ++ speedStr ++ "," ++ focusStr

-- Helper: deserialize LaserState from string  
deserializeLaserState :: String -> LaserState
deserializeLaserState s =
    let parts = splitOnCommaChar s
        mp = if L.length parts > 0 && parts !! 0 /= "Nothing" 
             then Power <$> readMaybe (parts !! 0)
             else Nothing
        ms = if L.length parts > 1 && parts !! 1 /= "Nothing"
             then Speed <$> readMaybe (parts !! 1) 
             else Nothing
        mf = if L.length parts > 2 && parts !! 2 /= "Nothing"
             then Focus <$> readMaybe (parts !! 2)
             else Nothing
    in LaserState mp ms mf

splitOnCommaChar :: String -> [String]
splitOnCommaChar = splitOnChar ','

splitOnChar :: Eq a => a -> [a] -> [[a]]
splitOnChar _ [] = [[]]
splitOnChar delim (x:xs)
    | x == delim = [] : rest
    | otherwise = (x : L.head rest) : L.tail rest
  where rest = splitOnChar delim xs

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    [(x, _)] -> Just x
    _        -> Nothing
--import System.IO.Strict as Strict

--import System.IO.Error (catchIOError, ioError)

--import System.IO (withFile, IOMode(ReadMode), IOMode(WriteMode) , hGetContents, hPutStrLn, stderr)
--import Control.Exception (IOException, catch, try, SomeException, evaluate)

--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as BS8
--import Control.Monad.IO.Class (MonadIO, liftIO)
--import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
--import Data.Text.Encoding.Error (lenientDecode)
-- import Data.List.Safe as S

--import CliHelper as Helper

{-
main :: IO ()
main = interactWith myCopyFunction myCliFilePath myCliFileCopyPath
-}

{-
main :: IO ()
main = do
    content <- readFileUtf8 myCliFilePath
    let copiedContent = myCopyFunction (T.unpack content)
    writeFile myCliFileCopyPath copiedContent 
-}


-- from 
{-
readFileBS :: MonadIO m => FilePath -> m BS.ByteString
readFileBS path = liftIO $ withFile path ReadMode $ \handle -> do
    contents <- BS.hGetContents handle
    BS.length contents `seq` return contents

readFileUtf8 :: MonadIO m => FilePath -> m T.Text
readFileUtf8 = fmap (decodeUtf8With lenientDecode) . readFileBS


writeFileBS :: MonadIO m => FilePath -> BS.ByteString -> m ()
writeFileBS path content = liftIO $ withFile path WriteMode $ \handle -> do
    BS.hPut handle content

writeFileUtf8 :: MonadIO m => FilePath -> T.Text -> m ()
writeFileUtf8 fp = writeFileBS fp . encodeUtf8
-}

myCliFilePath :: FilePath
myCliFilePath = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650.cli"

myCliFileCopyPath :: FilePath
myCliFileCopyPath = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650_copy.cli"

myCliFileDuplicatePath :: FilePath
myCliFileDuplicatePath = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/all layers duplicated.cli"

myCliFileDupEven :: FilePath
myCliFileDupEven = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/even layers duplicated.cli"


myCliFileDupFst2OfEvery5 :: FilePath
myCliFileDupFst2OfEvery5 = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/first 2 of every 5 duplicated.cli"

myCliFileDup0And2OfEvery5 :: FilePath
myCliFileDup0And2OfEvery5 = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/0 and 2 of every 5 duplicated.cli"

myCliFileDupSerially0And2OfEvery5 :: FilePath
myCliFileDupSerially0And2OfEvery5 = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/serially 0 and 2 of every 5 duplicated.cli"

myCliFileDuplicateSeriallyPath :: FilePath
myCliFileDuplicateSeriallyPath = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/all layers duplicated serially.cli"

miniFile :: FilePath
miniFile = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/mini.cli"

miniDupSerially :: FilePath
miniDupSerially = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/mini duplicated serially.cli"

mini1and2OfEvery4 :: FilePath
mini1and2OfEvery4 = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/mini 1 and 2 of every 4.cli"

miniEvenLinewise :: FilePath
miniEvenLinewise = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/mini even linewise.cli"

-- Change SomeFunction to a composition of many functions that will act on the input file and then write to the output file
someFunction = id

-- Function to read a file and handle errors
--safeReadFile :: FilePath -> IO String
--safeReadFile path = catchIOError (readFile' path) handleReadError
--  where
--    handleReadError e = ioError e

-- Step 1. Reading and writing process
--interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()

--interactWith taking the id function works successfully

interactWith function inputFile outputFile = do 
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    --inputData <- (readFileUtf8 inputFile)
    inputData <- (readFile inputFile)
    --putStrLn "Reading the file was successful"
    -- does not work
    
    -- readFile gives a String so input will be a string
    --let output = function (T.unpack inputData)
    let output = function inputData
    writeFile outputFile output
    --writeFile outputFile (function (T.unpack inputData))
    -- writeFile takes and writes a String

    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

-- Step 2. Splitting the string into lines when reading, and respectively unsplitting the lines to string for writing
-- Step 2.1. Split the read lines
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

-- Step 2.2. Unsplit (i.e. join) the list of lines for writing
-- Opposite of splitLines for .cli files
unSplitLinesCli :: [String] -> String
unSplitLinesCli [] = []
unSplitLinesCli (x:xs) = x ++ "\r\n" ++ unSplitLinesCli xs


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

-- Step 3 Getting the geometry lines when reading and respectively, putting the geometry lines back together with the header and the footer lines when writing
-- Step 3.1 Get the geometry lines only
splitGeometryLines :: [String] -> ([String],[String],[String])
splitGeometryLines [] = ([],[],[])
splitGeometryLines allCliLines = (headerLines, geometryLines, footerLines) where
    headerRestTuple = L.break (L.isPrefixOf "$$GEOMETRYSTART") allCliLines
    headerLines = case snd headerRestTuple of
        [] -> fst headerRestTuple
        xs -> (fst headerRestTuple) ++ [(L.head (snd headerRestTuple))]

    geometryFooterTuple = case snd headerRestTuple of 
        [] -> ([],[])
        xs -> L.break (L.isPrefixOf "$$GEOMETRYEND") (L.tail (snd headerRestTuple))
    geometryLines = fst geometryFooterTuple
    footerLines = snd geometryFooterTuple

-- Step 3.2. Put the geometry lines back together with the header and the footer lines
unSplitGeometryLines :: ([String],[String],[String]) -> [String]
unSplitGeometryLines (headerLines, geometryLines, footerLines) = headerLines ++ geometryLines ++ footerLines


-- Step 4 Trimming and getting the geometry lines ready for parsing, and respectively, putting the trims back together when writing
-- Step 4.1 Trim the geometry lines
expectedKeywords :: [String]
expectedKeywords = ["LAYER","POWER","SPEED","FOCUS","POLYLINE","HATCHES"]
-- gotten from the file: 2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650.cli
-- could import checkConstructors function from CliHelper.hs and check for other files.


-- This function takes a string of a geometry line and returns a tuple of the string of keyword/constructor and
-- and the rest of the line which is to be understood as the parameters corresponding to the keyword/constructor
trimGeoLine :: String -> (String, String)
trimGeoLine geoLine = (constructor,params) where
    --trimmedLine = L.drop 2 (L.init geoLine)
    -- CORRECTED
    trimmedLine = L.drop 2 geoLine

    -- "$$POLYLINE/42,2,175,34.164,-30.885,34.16,-30.892,34.042,-31.129,34.038,-31.136,33.904,-31.366,33.9\r" ->
    --    "POLYLINE/42,2,175,34.164,-30.885,34.16,-30.892,34.042,-31.129,34.038,-31.136,33.904,-31.366,33.9" 
    constructor = fst (L.span C.isAlpha trimmedLine)
    params = L.tail (snd (L.span C.isAlpha trimmedLine))
    -- taking the tail to remove the "/"

unTrimGeoLine :: (String, String) -> String
--unTrimGeoLine (constructor, params) = "$$" ++ constructor ++ "/" ++ params ++ "\r"
-- \r removed for now. Will need to check if it still works in windows.
-- unTrimGeoLine (constructor, params) = "$$" ++ constructor ++ "/" ++ params
-- That was not the source of extra spaces. it turns out.
unTrimGeoLine (constructor, params) = "$$" ++ constructor ++ "/" ++ params ++ "\r"
-- Turns out this function is not used at all.

-- Some information based on reference: https://www.hmilch.net/downloads/cli_format.html#:~:text=The%20Common%20Layer%20Interface%20(CLI,layer%20manufacturing%20technologies%20(LMT).
-- Step 5. Parsing the constructor and parameters obtained from the geometry lines into data types
-- Step 5.1. Define the data types
{-
data Power = Power {power :: Int} deriving (Show, Read) -- Build from and into ("POWER","320")
-- Power used as an Integer for now. Can be changed to Double later if needed.
data Speed = Speed {speed :: Int} deriving (Show, Read) -- Build from and into ("SPEED","1550")
-- Speed used as an Integer for now. Can be changed to Double later if needed.
-}
data Power = Power {power :: Double} deriving (Show, Read) -- Build from and into ("POWER","320")
-- changed to Double because the files for experiment 4 had a double value
data Speed = Speed {speed :: Double} deriving (Show, Read) -- Build from and into ("SPEED","1550")
-- changed to Double because the files for experiment 4 had a double value

data Focus = Focus {focus :: Double} deriving (Show, Read) -- Build from and into ("FOCUS","3")
data LayerChange = LayerChange {layerChange :: Double} deriving (Show, Read) -- Build from and into ("LAYER","0.06") -- !!! the parameter is the cumulative build height and NOT the layer thickness
data Polyline = Polyline {idPL :: Int, dirPL :: Int, numPtsPL :: Int, pointsPL :: [(Double,Double)] } deriving (Show, Read)
data Hatches = Hatches {idHat :: Int, numHat :: Int, pointsHat :: [(Double,Double)] } deriving (Show, Read)
-- numHat * 4 = number of x-coordinates + number of y-coordiantes. So, numHat = 2 * number of points
-- But the webiste on cli format says that numHat (n in the format) is the number of Hatches.
-- I don't know what that means.

-- Explicit use of Prelude.read has only occurred in get functions.
-- Step 5.2 Parse the constructor and parameters into the data types
getPower :: String -> Power
-- First use of Prelude.read
--getPower powerValue = Power {power = (read powerValue) :: Int} -- Power in Watts
-- Power used as an Integer for now. Can be changed to Double later if needed.
getPower powerValue = Power {power = (read powerValue) :: Double} -- Power in Watts
-- Changed to Double because the files for experiment 4 had a double value

-- The set functions combine 5.2 and 4.2 and directly give a string that can be written to file.
setPower :: Power -> String
-- setPower (Power powerValue) = "$$POWER/" ++ show powerValue ++ "\r"
-- NEED TO CHECK IF the \r is needed for windows. In my computer it creates extras spaces.
setPower (Power powerValue) = "$$POWER/" ++ show powerValue


getSpeed :: String -> Speed
-- Second use of Prelude.read
--getSpeed speedValue = Speed {speed = (read speedValue) :: Int} -- Speed in mm/s
-- Speed used as an Integer for now. Can be changed to Double later if needed.
getSpeed speedValue = Speed {speed = (read speedValue) :: Double} -- Speed in mm/s
-- Changed to Double because the files for experiment 4 had a double value

setSpeed :: Speed -> String
-- setSpeed (Speed speedValue) = "$$SPEED/" ++ show speedValue ++ "\r"
-- NEED TO CHECK IF the \r is needed for windows
setSpeed (Speed speedValue) = "$$SPEED/" ++ show speedValue

getFocus :: String -> Focus
-- Third use of Prelude.read
getFocus focusValue = Focus {focus = (read focusValue) :: Double}
-- Focus value of 0.06 was also observed in one of the .cli files, so I am using a Double here.

setFocus :: Focus -> String
-- setFocus (Focus focusValue) = "$$FOCUS/" ++ show focusValue ++ "\r"
-- NEED TO CHECK IF the \r is needed for windows
setFocus (Focus focusValue) = "$$FOCUS/" ++ (removeDecimals . show $ focusValue) where
-- focusValue' is already a string because of having shown in the code below
    -- ("5",".0") so decimal pt is also included in the second part

    
removeDecimals :: String -> String
-- String is assumed here as the input
removeDecimals aReal = if (L.all (=='0') (L.tail decimals)) then aInt else aReal where
    (aInt, decimals) = L.break (== '.') $ aReal
    

getLayerChange :: String -> LayerChange
-- Fourth use of Prelude.read
getLayerChange layerChangeValue = LayerChange {layerChange = (read layerChangeValue) :: Double}

setLayerChange :: LayerChange -> String
-- setLayerChange (LayerChange layerChangeValue) = "$$LAYER/" ++ show layerChangeValue ++ "\r"
-- NEED TO CHECK IF the \r is needed for windows
setLayerChange (LayerChange layerChangeValue) = "$$LAYER/" ++ removeDecimals (decimalFloatShow layerChangeValue)
-- decimalFloatShow function defined below is used here instead of just show to 
-- change the scientific notation to decimal notation
-- Eg: 6.0e-2 to 0.06

{-
Citation for decimalFloatShow function:
https://discourse.haskell.org/t/ghci-has-unusual-non-configurable-numeric-representations/1595/2
-}

decimalFloatShow :: Show a => a -> String
decimalFloatShow = go . show where
  go "" = ""
  go s | ((x,xs):_) <- readFloat s = showFFloat Nothing x (go xs)
  go (x:xs) = x : go xs

getPolyline :: String -> Polyline
-- Example: 45,2,2,29.62,-30.558,29.62,-31.253 is given after splitting from "$$POLYPOLYLINE/... \r"
-- needs to be parsed into Polyline {id = 45, dir = 2, numPts = 2, points = [(29.62,-30.558),(29.62,-31.253)]}
-- Fifth, Sixth, Seventh, Eighth use of Prelude.read
getPolyline params = Polyline {idPL = read (L.head stringParams), dirPL = read (stringParams !! 1), numPtsPL = read (stringParams !! 2), pointsPL = pairPoints onlyPoints} where 
    onlyPoints = (L.map read (L.drop 3 stringParams)) :: [Double]
    stringParams = wordsAt params ','
    pairPoints :: [Double] -> [(Double,Double)]
    pairPoints [] = []
    pairPoints (x:y:xs) = (x,y) : pairPoints xs

setPolyline :: Polyline -> String
-- setPolyline (Polyline idPL dirPL numPtsPL pointsPL) = "$$POLYLINE/" ++ show idPL ++ "," ++ show dirPL ++ "," ++ show numPtsPL ++ "," ++ unPairPoints pointsPL ++ "\r" where
-- NEED TO CHECK IF the \r is needed for windows
setPolyline (Polyline idPL dirPL numPtsPL pointsPL) = "$$POLYLINE/" ++ show idPL ++ "," ++ show dirPL ++ "," ++ show numPtsPL ++ "," ++ (L.init . unPairPoints $ pointsPL) where
    -- L.init to remove the comma at the end
    unPairPoints :: [(Double,Double)] -> String
    unPairPoints [] = []
    unPairPoints ((x,y):xs) = (removeDecimals (decimalFloatShow x)) ++ "," ++ (removeDecimals (decimalFloatShow y)) ++ "," ++ unPairPoints xs
-- NOTE: decimalFloatShow used here as well. 

{-
myUnPairPoints :: [(Double,Double)] -> String
myUnPairPoints [] = []
myUnPairPoints ((x,y):xs) = L.init (show x ++ "," ++ show y ++ "," ++ myUnPairPoints xs)
-}

getHatches :: String -> Hatches
-- Ninth, Tenth, Eleventh use of Prelude.read
getHatches params = Hatches {idHat = read (L.head stringParams), numHat = read (stringParams !! 1), pointsHat = pairPoints onlyPoints} where 
    onlyPoints = (L.map read (L.drop 2 stringParams)) :: [Double]
    stringParams = wordsAt params ','
    pairPoints :: [Double] -> [(Double,Double)]
    pairPoints [] = []
    pairPoints (x:y:xs) = (x,y) : pairPoints xs

setHatches :: Hatches -> String
-- setHatches (Hatches idHat numHat pointsHat) = "$$HATCHES/" ++ show idHat ++ "," ++ show numHat ++ "," ++ unPairPoints pointsHat ++ "\r" where
-- NEED TO CHECK IF the \r is needed for windows
setHatches (Hatches idHat numHat pointsHat) = "$$HATCHES/" ++ show idHat ++ "," ++ show numHat ++ "," ++ (L.init . unPairPoints $ pointsHat) where
    -- L.init to remove the comma at the end
    unPairPoints :: [(Double,Double)] -> String
    unPairPoints [] = []
    unPairPoints ((x,y):xs) = (removeDecimals . decimalFloatShow $ x) ++ "," ++ (removeDecimals . decimalFloatShow $ y) ++ "," ++ unPairPoints xs
-- NOTE: decimalFloatShow used here as well. 

myHatFst :: String
-- line 118857
myHatFst = "$$HATCHES/4,72,-27.436,28.336,-30.028,27.341,-30.015,27.421,-27.436,28.411,-27.436,28.486,-30.003,27.501,-29.99,27.581,-27.436,28.561,-27.436,28.636,-29.978,27.661,-29.965,27.74,-27.436,28.711,-27.436,28.786,-29.952,27.82,-29.94,27.9,-27.436,28.861,-27.436,28.936,-29.927,27.98,-29.914,28.06,-27.436,29.011,-27.436,29.086,-29.902,28.14,-29.889,28.219,-27.436,29.161,-27.436,29.236,-29.876,28.299,-29.864,28.379,-27.436,29.311,-27.436,29.386,-29.851,28.459,-29.838,28.539,-27.436,29.461,-27.436,29.536,-29.826,28.619,-29.813,28.698,-27.436,29.611,-27.436,29.686,-29.801,28.778,-29.788,28.858,-27.436,29.761,-27.436,29.836,-29.775,28.938,-29.763,29.018,-27.436,29.911,-27.436,29.986,-29.75,29.098,-29.737,29.177,-27.436,30.061,-27.436,30.136,-29.725,29.257,-29.712,29.337,-27.436,30.211,-27.436,30.286,-29.699,29.417,-29.687,29.497,-27.436,30.361,-27.436,30.436,-29.674,29.577,-29.661,29.656,-27.436,30.511,-27.436,30.586,-29.649,29.736,-29.636,29.816,-27.436,30.661,-27.436,30.736,-29.624,29.896,-29.611,29.976,-27.436,30.811,-27.436,30.886,-29.598,30.056,-29.586,30.135,-27.436,30.961,-27.436,31.036,-29.573,30.215,-29.56,30.295,-27.436,31.111,-27.436,31.186,-29.548,30.375,-29.535,30.455,-27.436,31.261,-27.436,31.336,-29.522,30.535,-29.51,30.614,-27.436,31.411,-27.436,31.486,-29.497,30.694,-29.484,30.774,-27.436,31.561,-27.436,31.636,-29.472,30.854,-29.459,30.934,-27.436,31.71,-27.436,31.785,-29.446,31.014,-29.434,31.093,-27.436,31.86,-27.436,31.935,-29.421,31.173,-29.409,31.253,-27.436,32.01,-27.436,32.085,-29.396,31.333,-29.383,31.413,-27.436,32.16,-27.436,32.235,-29.371,31.493,-29.358,31.572,-27.436,32.31,-27.436,32.385,-29.345,31.652,-29.333,31.732,-27.436,32.46,-27.436,32.535,-29.32,31.812,-29.307,31.892,-27.436,32.61,-27.436,32.685,-29.295,31.972,-29.282,32.051,-27.436,32.76,-27.436,32.835,-29.269,32.131,-29.257,32.211,-27.436,32.91,-27.436,32.985,-29.244,32.291,-29.232,32.371,-27.436,33.06,-27.436,33.135,-29.219,32.451,-29.206,32.53,-27.436,33.21,-27.436,33.285,-29.194,32.61,-29.181,32.69,-27.436,33.36,-27.436,33.435,-29.168,32.77,-29.156,32.85,-27.436,33.51,-27.436,33.585,-29.143,32.93,-29.13,33.009,-27.436,33.66"

myHatFstCp :: String
-- line 118857 in copy file
myHatFstCp = "$$HATCHES/4,72,-27.436,28.336,-30.028,27.341,-30.015,27.421,-27.436,28.411,-27.436,28.486,-30.003,27.501,-29.99,27.581,-27.436,28.561,-27.436,28.636,-29.978,27.661,-29.965,27.74,-27.436,28.711,-27.436,28.786,-29.952,27.82,-29.94,27.9,-27.436,28.861,-27.436,28.936,-29.927,27.98,-29.914,28.06,-27.436,29.011,-27.436,29.086,-29.902,28.14,-29.889,28.219,-27.436,29.161,-27.436,29.236,-29.876,28.299,-29.864,28.379,-27.436,29.311,-27.436,29.386,-29.851,28.459,-29.838,28.539,-27.436,29.461,-27.436,29.536,-29.826,28.619,-29.813,28.698,-27.436,29.611,-27.436,29.686,-29.801,28.778,-29.788,28.858,-27.436,29.761,-27.436,29.836,-29.775,28.938,-29.763,29.018,-27.436,29.911,-27.436,29.986,-29.75,29.098,-29.737,29.177,-27.436,30.061,-27.436,30.136,-29.725,29.257,-29.712,29.337,-27.436,30.211,-27.436,30.286,-29.699,29.417,-29.687,29.497,-27.436,30.361,-27.436,30.436,-29.674,29.577,-29.661,29.656,-27.436,30.511,-27.436,30.586,-29.649,29.736,-29.636,29.816,-27.436,30.661,-27.436,30.736,-29.624,29.896,-29.611,29.976,-27.436,30.811,-27.436,30.886,-29.598,30.056,-29.586,30.135,-27.436,30.961,-27.436,31.036,-29.573,30.215,-29.56,30.295,-27.436,31.111,-27.436,31.186,-29.548,30.375,-29.535,30.455,-27.436,31.261,-27.436,31.336,-29.522,30.535,-29.51,30.614,-27.436,31.411,-27.436,31.486,-29.497,30.694,-29.484,30.774,-27.436,31.561,-27.436,31.636,-29.472,30.854,-29.459,30.934,-27.436,31.71,-27.436,31.785,-29.446,31.014,-29.434,31.093,-27.436,31.86,-27.436,31.935,-29.421,31.173,-29.409,31.253,-27.436,32.01,-27.436,32.085,-29.396,31.333,-29.383,31.413,-27.436,32.16,-27.436,32.235,-29.371,31.493,-29.358,31.572,-27.436,32.31,-27.436,32.385,-29.345,31.652,-29.333,31.732,-27.436,32.46,-27.436,32.535,-29.32,31.812,-29.307,31.892,-27.436,32.61,-27.436,32.685,-29.295,31.972,-29.282,32.051,-27.436,32.76,-27.436,32.835,-29.269,32.131,-29.257,32.211,-27.436,32.91,-27.436,32.985,-29.244,32.291,-29.232,32.371,-27.436,33.06,-27.436,33.135,-29.219,32.451,-29.206,32.53,-27.436,33.21,-27.436,33.285,-29.194,32.61,-29.181,32.69,-27.436,33.36,-27.436,33.435,-29.168,32.77,-29.156,32.85,-27.436,33.51,-27.436,33.585,-29.143,32.93,-29.13,33.009,-27.436,33.66"

myHatLast :: String
-- line 140566
myHatLast = "$$HATCHES/41,5,36.731,-32.031,36.685,-32.23,36.787,-32.325,36.858,-32.017,36.975,-32.04,36.888,-32.419,36.989,-32.514,37.082,-32.112,37.174,-32.249,37.091,-32.608"

myHatLastCp :: String
-- line 140566 in copy file
myHatLastCp = "$$HATCHES/41,5,36.731,-32.031,36.685,-32.23,36.787,-32.325,36.858,-32.017,36.975,-32.04,36.888,-32.419,36.989,-32.514,37.082,-32.112,37.174,-32.249,37.091,-32.608"

-- The first and the last hatches are in fact identical.


{-
myHat = pointsHat . getHatches $ "4,72,-27.436,28.336,-30.028,27.341,-30.015,27.421,-27.436,28.411,-27.436,28.486,-30.003,27.501,-29.99,27.581,-27.436,28.561,-27.436,28.636,-29.978,27.661,-29.965,27.74,-27.436,28.711,-27.436,28.786,-29.952,27.82,-29.94,27.9,-27.436,28.861,-27.436,28.936,-29.927,27.98,-29.914,28.06,-27.436,29.011,-27.436,29.086,-29.902,28.14,-29.889,28.219,-27.436,29.161,-27.436,29.236,-29.876,28.299,-29.864,28.379,-27.436,29.311,-27.436,29.386,-29.851,28.459,-29.838,28.539,-27.436,29.461,-27.436,29.536,-29.826,28.619,-29.813,28.698,-27.436,29.611,-27.436,29.686,-29.801,28.778,-29.788,28.858,-27.436,29.761,-27.436,29.836,-29.775,28.938,-29.763,29.018,-27.436,29.911,-27.436,29.986,-29.75,29.098,-29.737,29.177,-27.436,30.061,-27.436,30.136,-29.725,29.257,-29.712,29.337,-27.436,30.211,-27.436,30.286,-29.699,29.417,-29.687,29.497,-27.436,30.361,-27.436,30.436,-29.674,29.577,-29.661,29.656,-27.436,30.511,-27.436,30.586,-29.649,29.736,-29.636,29.816,-27.436,30.661,-27.436,30.736,-29.624,29.896,-29.611,29.976,-27.436,30.811,-27.436,30.886,-29.598,30.056,-29.586,30.135,-27.436,30.961,-27.436,31.036,-29.573,30.215,-29.56,30.295,-27.436,31.111,-27.436,31.186,-29.548,30.375,-29.535,30.455,-27.436,31.261,-27.436,31.336,-29.522,30.535,-29.51,30.614,-27.436,31.411,-27.436,31.486,-29.497,30.694,-29.484,30.774,-27.436,31.561,-27.436,31.636,-29.472,30.854,-29.459,30.934,-27.436,31.71,-27.436,31.785,-29.446,31.014,-29.434,31.093,-27.436,31.86,-27.436,31.935,-29.421,31.173,-29.409,31.253,-27.436,32.01,-27.436,32.085,-29.396,31.333,-29.383,31.413,-27.436,32.16,-27.436,32.235,-29.371,31.493,-29.358,31.572,-27.436,32.31,-27.436,32.385,-29.345,31.652,-29.333,31.732,-27.436,32.46,-27.436,32.535,-29.32,31.812,-29.307,31.892,-27.436,32.61,-27.436,32.685,-29.295,31.972,-29.282,32.051,-27.436,32.76,-27.436,32.835,-29.269,32.131,-29.257,32.211,-27.436,32.91,-27.436,32.985,-29.244,32.291,-29.232,32.371,-27.436,33.06,-27.436,33.135,-29.219,32.451,-29.206,32.53,-27.436,33.21,-27.436,33.285,-29.194,32.61,-29.181,32.69,-27.436,33.36,-27.436,33.435,-29.168,32.77,-29.156,32.85,-27.436,33.51,-27.436,33.585,-29.143,32.93,-29.13,33.009,-27.436,33.66"
ghci> 
ghci> L.length myHat
144
ghci> L.tail myHat
[(-30.028,27.341),(-30.015,27.421),(-27.436,28.411),(-27.436,28.486),(-30.003,27.501),(-29.99,27.581),(-27.436,28.561),(-27.436,28.636),(-29.978,27.661),(-29.965,27.74),(-27.436,28.711),(-27.436,28.786),(-29.952,27.82),(-29.94,27.9),(-27.436,28.861),(-27.436,28.936),(-29.927,27.98),(-29.914,28.06),(-27.436,29.011),(-27.436,29.086),(-29.902,28.14),(-29.889,28.219),(-27.436,29.161),(-27.436,29.236),(-29.876,28.299),(-29.864,28.379),(-27.436,29.311),(-27.436,29.386),(-29.851,28.459),(-29.838,28.539),(-27.436,29.461),(-27.436,29.536),(-29.826,28.619),(-29.813,28.698),(-27.436,29.611),(-27.436,29.686),(-29.801,28.778),(-29.788,28.858),(-27.436,29.761),(-27.436,29.836),(-29.775,28.938),(-29.763,29.018),(-27.436,29.911),(-27.436,29.986),(-29.75,29.098),(-29.737,29.177),(-27.436,30.061),(-27.436,30.136),(-29.725,29.257),(-29.712,29.337),(-27.436,30.211),(-27.436,30.286),(-29.699,29.417),(-29.687,29.497),(-27.436,30.361),(-27.436,30.436),(-29.674,29.577),(-29.661,29.656),(-27.436,30.511),(-27.436,30.586),(-29.649,29.736),(-29.636,29.816),(-27.436,30.661),(-27.436,30.736),(-29.624,29.896),(-29.611,29.976),(-27.436,30.811),(-27.436,30.886),(-29.598,30.056),(-29.586,30.135),(-27.436,30.961),(-27.436,31.036),(-29.573,30.215),(-29.56,30.295),(-27.436,31.111),(-27.436,31.186),(-29.548,30.375),(-29.535,30.455),(-27.436,31.261),(-27.436,31.336),(-29.522,30.535),(-29.51,30.614),(-27.436,31.411),(-27.436,31.486),(-29.497,30.694),(-29.484,30.774),(-27.436,31.561),(-27.436,31.636),(-29.472,30.854),(-29.459,30.934),(-27.436,31.71),(-27.436,31.785),(-29.446,31.014),(-29.434,31.093),(-27.436,31.86),(-27.436,31.935),(-29.421,31.173),(-29.409,31.253),(-27.436,32.01),(-27.436,32.085),(-29.396,31.333),(-29.383,31.413),(-27.436,32.16),(-27.436,32.235),(-29.371,31.493),(-29.358,31.572),(-27.436,32.31),(-27.436,32.385),(-29.345,31.652),(-29.333,31.732),(-27.436,32.46),(-27.436,32.535),(-29.32,31.812),(-29.307,31.892),(-27.436,32.61),(-27.436,32.685),(-29.295,31.972),(-29.282,32.051),(-27.436,32.76),(-27.436,32.835),(-29.269,32.131),(-29.257,32.211),(-27.436,32.91),(-27.436,32.985),(-29.244,32.291),(-29.232,32.371),(-27.436,33.06),(-27.436,33.135),(-29.219,32.451),(-29.206,32.53),(-27.436,33.21),(-27.436,33.285),(-29.194,32.61),(-29.181,32.69),(-27.436,33.36),(-27.436,33.435),(-29.168,32.77),(-29.156,32.85),(-27.436,33.51),(-27.436,33.585),(-29.143,32.93),(-29.13,33.009),(-27.436,33.66)]
ghci> myHat
[(-27.436,28.336),(-30.028,27.341),(-30.015,27.421),(-27.436,28.411),(-27.436,28.486),(-30.003,27.501),(-29.99,27.581),(-27.436,28.561),(-27.436,28.636),(-29.978,27.661),(-29.965,27.74),(-27.436,28.711),(-27.436,28.786),(-29.952,27.82),(-29.94,27.9),(-27.436,28.861),(-27.436,28.936),(-29.927,27.98),(-29.914,28.06),(-27.436,29.011),(-27.436,29.086),(-29.902,28.14),(-29.889,28.219),(-27.436,29.161),(-27.436,29.236),(-29.876,28.299),(-29.864,28.379),(-27.436,29.311),(-27.436,29.386),(-29.851,28.459),(-29.838,28.539),(-27.436,29.461),(-27.436,29.536),(-29.826,28.619),(-29.813,28.698),(-27.436,29.611),(-27.436,29.686),(-29.801,28.778),(-29.788,28.858),(-27.436,29.761),(-27.436,29.836),(-29.775,28.938),(-29.763,29.018),(-27.436,29.911),(-27.436,29.986),(-29.75,29.098),(-29.737,29.177),(-27.436,30.061),(-27.436,30.136),(-29.725,29.257),(-29.712,29.337),(-27.436,30.211),(-27.436,30.286),(-29.699,29.417),(-29.687,29.497),(-27.436,30.361),(-27.436,30.436),(-29.674,29.577),(-29.661,29.656),(-27.436,30.511),(-27.436,30.586),(-29.649,29.736),(-29.636,29.816),(-27.436,30.661),(-27.436,30.736),(-29.624,29.896),(-29.611,29.976),(-27.436,30.811),(-27.436,30.886),(-29.598,30.056),(-29.586,30.135),(-27.436,30.961),(-27.436,31.036),(-29.573,30.215),(-29.56,30.295),(-27.436,31.111),(-27.436,31.186),(-29.548,30.375),(-29.535,30.455),(-27.436,31.261),(-27.436,31.336),(-29.522,30.535),(-29.51,30.614),(-27.436,31.411),(-27.436,31.486),(-29.497,30.694),(-29.484,30.774),(-27.436,31.561),(-27.436,31.636),(-29.472,30.854),(-29.459,30.934),(-27.436,31.71),(-27.436,31.785),(-29.446,31.014),(-29.434,31.093),(-27.436,31.86),(-27.436,31.935),(-29.421,31.173),(-29.409,31.253),(-27.436,32.01),(-27.436,32.085),(-29.396,31.333),(-29.383,31.413),(-27.436,32.16),(-27.436,32.235),(-29.371,31.493),(-29.358,31.572),(-27.436,32.31),(-27.436,32.385),(-29.345,31.652),(-29.333,31.732),(-27.436,32.46),(-27.436,32.535),(-29.32,31.812),(-29.307,31.892),(-27.436,32.61),(-27.436,32.685),(-29.295,31.972),(-29.282,32.051),(-27.436,32.76),(-27.436,32.835),(-29.269,32.131),(-29.257,32.211),(-27.436,32.91),(-27.436,32.985),(-29.244,32.291),(-29.232,32.371),(-27.436,33.06),(-27.436,33.135),(-29.219,32.451),(-29.206,32.53),(-27.436,33.21),(-27.436,33.285),(-29.194,32.61),(-29.181,32.69),(-27.436,33.36),(-27.436,33.435),(-29.168,32.77),(-29.156,32.85),(-27.436,33.51),(-27.436,33.585),(-29.143,32.93),(-29.13,33.009),(-27.436,33.66)]
ghci> L.last myHat
(-27.436,33.66)

Reading of the hatches is working well. The writing might be causing problems.

Somehow it is only partially written. 
4,72,-27.436,28.336,-30.028,27.341,-30.015,27.421,-27.436,28.411,-27.436,28.486,-30.003,27.501,-29.99,27.581,-27.436,28.561,-27.436,28.636,-29.978,27.661,-29.965,27.74,-27.436,28.711,-27.436,28.786,-29.952,27.82,-29.94,27.9,-27.436,28.861,-27.436,28.936,-29.927,27.98,-29.914,28.06,-27.436,29.011,-27.436,29.086,-29.902,28.14,-29.889,28.219,-27.436,29.161,-27.436,29.236,-29.876,28.299,-29.864,28.379,-27.436,29.311,-27.436,29.386,-29.851,28.459,-29.838,28.539,-27.436,29.461,-27.436,29.536,-29.826,28.619,-29.813,28.698,-27.436,29.611,-27.436,29.686,-29.801,28.778,-29.788,28.858,-27.436,29.761,-27.436,29.836,-29.775,28.938,-29.763,29.018,-27.436,29.911,-27.436,29.986,-29.75,29.098,-29.737,29.177,-27.436,30.061,-27.436,30.136,-29.725,29.257,-29.712,29.337,-27.436,30.211,-27.436,30.286,-29.699,29.417,-29.687,29.497,-27.436,30.361,-27.436,30.436,-29.674,29.577,-29.661,29.656,-27.436,30.511,-27.436,30.586,-29.649,29.736,-29.636,29.816,-27.436,30.661,-27.436,30.736,-29.624,29.896,-29.611,29.976,-27.436,30.811,-27.436,30.886,-29.598,30.056,-29.586,30.135,-27.436,30.961,-27.436,31.036,-29.573,30.215,-29.56,30.295,-27.436,31.111,-27.436,31.186,-29.548,30.375,-29.535,30.455,-27.436,31.261,-27.436,31.336,-29.522,30.535,-29.51,30.614,-27.436,31.411,-27.436,31.486,-29.497,30.694,-29.484,30.774,-27.436,31.561,-27.436,31.636,-29.472,30.854,-29.459,30.934,-27.436,31.71,-27.436,31.785,-29.446,31.014,-29.434,31.093,-27.436,31.86,-27.436,31.935,-29.421,31.173,-29.409,31.253,-27.436,32.01,-27.436,32.085,-29.396,31.333,-29.383,31.413,-27.436,32.16,-27.436,32.235,-29.371,31.493,-29.358,31.572,-27.436,32.31,-27.436,32.385,-29.345,31.652,-29.333,31.732,-27.436,32.46,-27.436,32.535,-29.32,31.812,-29.307,31.892,-27.436,32.61,-27.436,32.685,-29.295,31.972,-29.282,32.051,-27.436,32.76,-27.436,32.835,-29.269,32.131,-29.257,32.211,-27.436,32.91,-27.436,32.985,-29.244,32.291,-29.232,32.371,-27.436,33.06,-27.436,33.135,-29.219,32.451,-29.206,32.53,-27.436,33.21,-27.436,33.285,-29.194,32.61,-29.181,32.69,-27.436,33.36,-27.436,33.435,-29.168,32.77,-29.156,32.85,-27.436,33.51,-27.436,33.585,-29.143,32.93,-29.13,33.009,-27.436,33.66

becomes

4,72,-27.436,28.336,-30.028,27.341,-30.015,27.421,-27.436,28.411,-27.436,28.486,-30.003,27.501,-29.99,27.581,-27.436,28.561,-27.436,28.636,-29.978,27.661,-29.965,27.74,-27.436,28.711,-27.436,28.786,-29.952,27.82,-29.94,27.9,-27.436,28.861,-27.436,28.936,-29.927,27.98,-29.914,28.06,-27.436,29.011,-27.436,29.086,-29.902,28.14,-29.889,28.219,-27.436,29.161,-27.436,29.236,-29.876,28.299,-29.864,28.379,-27.436,29.311,-27.436,29.386,-29.851,28.459,-29.838,28.539,-27.436,29.461,-27.436,29.536,-29.826,28.619,-29.813,28.698,-27.436,29.611,-27.436,29.686,-29.801,28.778,-29.788,28.858,-27.436,29.761,-27.436,29.836,-29.775,28.938,-29.763,29.018,-27.436,29.911,-27.436,29.986,-29.75,29.098,-29.737,29.177,-27.436,30.061,-27.436,30.136,-29.725,29.257,-29.712,29.337,-27.436,30.211,-27.436,30.286,-29.699,29.417,-29.687,29.497,-27.436,30.361,-27.436,30.436,-29.674,29.577,-29.661,29.656,-27.436,30.511,-27.436,30.586,-29.649,29.736,-29.636,29.816,-27.436,30.661,-27.436,30.736,-29.624,29.896,-29.611,29.976,-27.436,30.811,-27.436,30.886,-29.598,30.056,-29.586,30.135,-27.436,30.961,-27.436,31.036,-29.573,30.215,-29.56,30.295,-27.436,31.111,-27.436,31.186,-29.548,30.375,-29.535,30.455,-27.436,31.261,-27.436,31.336,-29.522,30.535,-29.51,30.614,-27.436,31.411,-27.436,31.486,-29.497,30.694,-29.484,30.774,-27.436,31.561,-27.436,31.636,-29.472,30.854,-29.459,30.934,-27.436,31.71,-27.436,31.785,-29.446,31.014,-29.434,31.093,-27.436,31.86,-27.436,31.935,-29.421,31.173,-29.409,31.253,-27.436,32.01,-27.436,32.085,-29.396,31.333,-29.383,31.413,-27.436,32.16,-27.436,32.235,-29.371,31.493,-29.358,31.572,-27.436,32.31,-27.436,32.385,-29.345,31.652,-29.333,31.732,-27.436,32.46,-27.436,32.535,-29.32,31.812,-29.307,31.892,-27.436,32.61,-27.436,32.685,-29.295,31.972,-29.282,32.051,-27.436,32.76,-27.436,32.835,-29.269,32.131,-29.257,32.211,-27.436,32.91,-27.436,32.985,-29.244,32.291,-29.232,32.371,-27.436,33.06,-27.436,33.135,-29.219,32.451,-29.206,32.53,-27.436,33.21,-27.436,33.285,-29.194,32.6


-}


-- A function like words but should break the list at the separator instead of spaces. The separator is not included in the result.
wordsAt :: Eq a => [a] -> a -> [[a]]
wordsAt [] _ = [[]]
wordsAt [x] sep = 
    case x == sep of
        True -> [[]]
        False -> [[x]]
wordsAt xs sep = case zs of 
    [] -> [ys]
    (_:rest) -> ys : wordsAt rest sep
    where (ys,zs) = L.break (==sep) xs

{-
Seems to work:
wordsAt "1,2,3,4" ','
["1","2","3","4"]
-}


-- Step 5.3 Putting together the different types of geometry lines into a single geometry line data type
data GeometryLine
    = GLPower Power
    | GLSpeed Speed
    | GLFocus Focus
    | GLLayerChange LayerChange
    | GLPolyline Polyline
    | GLHatches Hatches

-- write the string if needed to print the geometry line
instance Show GeometryLine where
    show (GLPower power) = setPower power
    show (GLSpeed speed) = setSpeed speed
    show (GLFocus focus) = setFocus focus
    show (GLLayerChange layerChange) = setLayerChange layerChange
    show (GLPolyline polyline) = setPolyline polyline
    show (GLHatches hatches) = setHatches hatches

readGeometryLine :: String -> GeometryLine
readGeometryLine geoLine = case constructor of
    "POWER" -> GLPower (getPower params)
    "SPEED" -> GLSpeed (getSpeed params)
    "FOCUS" -> GLFocus (getFocus params)
    "LAYER" -> GLLayerChange (getLayerChange params)
    "POLYLINE" -> GLPolyline (getPolyline params)
    "HATCHES" -> GLHatches (getHatches params)
    otherwise -> error "Unknown constructor"
    where
        (constructor,params) = trimGeoLine geoLine

writeGeometryLine :: GeometryLine -> String
writeGeometryLine geoLine = show geoLine

-- Older thought, seems like not very good. The above seems better
{-

-- Step 6. Putting the parsed data types back together with the header and the footer lines when writing
-- Step 6.1. Define the data type for the geometry lines
class GeometryLine a where
    readGeometryLine :: String -> a
    writeGeometryLine :: a -> String

instance GeometryLine Power where
    readGeometryLine = getPower . snd . trimGeoLine
    writeGeometryLine = setPower

instance GeometryLine Speed where
    readGeometryLine = getSpeed . snd . trimGeoLine
    writeGeometryLine = setSpeed

instance GeometryLine Focus where
    readGeometryLine = getFocus . snd . trimGeoLine
    writeGeometryLine = setFocus

instance GeometryLine LayerChange where
    readGeometryLine = getLayerChange . snd . trimGeoLine
    writeGeometryLine = setLayerChange

instance GeometryLine Polyline where
    readGeometryLine = getPolyline . snd . trimGeoLine
    writeGeometryLine = setPolyline

instance GeometryLine Hatches where
    readGeometryLine = getHatches . snd . trimGeoLine
    writeGeometryLine = setHatches

-}


-- Step 6: A layer is a list of lines
data Layer = Layer {layerNumber :: Int,
    layerLines :: [GeometryLine]}

instance Show Layer where
    show (Layer layerNumber []) = "Layer Number: " ++ show layerNumber ++ "\n" ++ "This layer has no lines \n"
    show (Layer layerNumber layerLines) = "Layer Number: " ++ show layerNumber ++ "\n" ++ "This layer has: " ++ show (L.length (layerLines)) ++ " lines \n" ++ "First line: " ++ show (L.head layerLines) ++ "\n" ++ "...\n" ++ "Last line: " ++ show (L.last layerLines) ++ "\n"
-- All the geometry data is a list of layers
-- L.head and L.tail may faily on empty lists.

writeLayer :: Layer -> [String]
writeLayer (Layer layerNumber layerLines) = L.map writeGeometryLine layerLines

head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

data GeometryData = GeometryData {layers :: [Layer]}

instance Show GeometryData where
    show (GeometryData layers) = (show . length' $ layers) ++ " layers \n" ++  "First layer: \n" ++ show (L.head layers) ++ "...\n" ++ "Last layer: \n" ++ show (L.last layers)

writeGeometryData :: GeometryData -> [String]
writeGeometryData (GeometryData layers) = L.concatMap writeLayer layers
-- layers is a list of layers. Each layer has a list of geometry lines.

-- Example for L.concatMap
mylister :: [Int] -> [String]
mylister xs = L.map show xs

mylister2 :: [[Int]] -> [String]
mylister2 xss = L.concatMap mylister xss
--

length' :: [a] -> Int
length' [] = 0
length' xs = L.length xs


-- Example 1.
-- Read a file, copy its contents to another file
-- interactWith function inputFile outputFile
-- function for this use
myCopyFunction :: String -> String 
myCopyFunction wholeString = unSplitLinesCli . unSplitGeometryLines $ (headerLines, copiedGeometryLines, footerLines) where 
    (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ wholeString
    newGeometryLines = L.map readGeometryLine geometryLines
    copiedGeometryLines = L.map writeGeometryLine newGeometryLines 


myCopyChecker :: String -> String -> [(String,String)]
myCopyChecker original copied = pairsUnequalLines origLines cpLines where
    origLines = splitLines original
    cpLines = splitLines copied
    pairsUnequalLines :: [String] -> [String] -> [(String,String)]
    pairsUnequalLines [] [] = []
    pairsUnequalLines [] (x:xs) = ("",x) : pairsUnequalLines [] xs
    pairsUnequalLines (x:xs) [] = (x,"") : pairsUnequalLines xs []
    pairsUnequalLines (xOrig:xsOrig) (xCopy:xsCopy) = if xOrig == xCopy
        then pairsUnequalLines xsOrig xsCopy
        else (xOrig,xCopy) : pairsUnequalLines xsOrig xsCopy

myCheckSplit :: FilePath -> IO ()
myCheckSplit inputFile = do
    inputString <- readFile inputFile
    let (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString in
        dropTakePrint 0 10 headerLines
-- The reading and splitting part seem to be working well.

lastN' :: Int -> [a] -> [a]
lastN' n xs = L.foldl' (const . L.drop 1) xs (L.drop n xs)

printlastN' :: Show a => Int -> [a] -> IO ()
printlastN' n xs = forM_ (lastN' n xs) print
-- By dropping n elements from the front, you allow n elements n elements to remain in the list
-- Brilliant use of L.foldl'
-- Step function = just drop the first element of the accumulator, ignore the second argument
-- Zero value = Whole list
-- accmulate until you consume the list with the first n elements dropped.

myCheckSplit2 :: FilePath -> IO ()
myCheckSplit2 inputFile = do
    inputString <- readFile inputFile
    let (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString in
        printlastN' 10 geometryLines
-- The reading and splitting part seem to be working well.

-- Function to drop the first n1 elements, take the next n2 elements, and print them each to a new line
dropTakePrint :: Show a => Int -> Int -> [a] -> IO ()
dropTakePrint nDropElems nTakeElems xs = forM_ (L.take nTakeElems (L.drop nDropElems xs)) print


-- Now let's check the readGeometryLines bit
myCheckReadG :: FilePath -> IO ()
myCheckReadG inputFile = do
    inputString <- readFile inputFile
    let (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString in
        dropTakePrint 0 10 (L.map readGeometryLine geometryLines)

myCheckfstLLayer :: FilePath -> IO ()
myCheckfstLLayer inputFile = do
    inputString <- readFile inputFile
    let (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString in
        print . trimGeoLine $ (L.head geometryLines)


myStringChecker :: FilePath -> Int -> Int -> IO ()
myStringChecker inputFile nDropChar nTakeChars = do
    inputString <- readFile inputFile
    let toPrint = L.take nTakeChars (L.drop nDropChar inputString) in
        print toPrint


myCopyCheckFunction :: FilePath -> FilePath -> IO ()
myCopyCheckFunction origFile copyFile = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    origString <- readFile origFile
    copyString <- readFile copyFile
    let pairs = myCopyChecker origString copyString 
    if L.length pairs > 0
        then putStrLn (show (L.length pairs) ++ " unequal lines found")
        else putStrLn "No unequal lines found"
    --putStrLn (show (L.length pairs) ++ " unequal lines found")
    forM_ (L.take 10 pairs) print

    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration
    

{-
Differences between original and copied files:
1. $$LAYER/0.06 -> $$LAYER/6.0e-2  shown with exponential notation
2. $$FOCUS/3 -> $$FOCUS/3.0  shown with decimal point FOCUS since it is double to accomodate 
 focus values of possibly 0.06
 3. Hatches are not read completely. Solve this issue.

 4. See, if I can use some pretty printing with significant figures for the floating point numbers,
    for LAYER and FOCUS values.

-}


{-
readGeometryLineSafe :: String -> Either String GeometryLine
readGeometryLineSafe geoLine = catch (Right $ readGeometryLine geoLine) handleReadError
    where
        handleReadError :: IOException -> Either String GeometryLine
        handleReadError err = Left $ "Error reading geometry line:" ++ show err

{-
readGeometryLineSafe2 :: String -> Either String GeometryLine
readGeometryLineSafe2 geoLine = case val of
    (Left err) -> Left $ "Error reading geometry line:" ++ show (err :: SomeException)
    (Right geoLine) -> Right val
    where val = try (evaluate (readGeometryLine geoLine))

myCopyFunctionErr :: String -> String
myCopyFunctionErr wholeString =
    let (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ wholeString
        newGeometryLines = L.map readGeometryLineSafe2 geometryLines
        (errors, validGeometryLines) = partitionEithers newGeometryLines
    in if L.null errors
        then unSplitLinesCli . unSplitGeometryLines $ (headerLines, L.map writeGeometryLine validGeometryLines, footerLines)
        else L.unlines $ "Errors in geometry lines:" : errors
-}
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = L.foldr (either left right) ([], [])
  where
    left  a (l, r) = (a:l, r)
    right b (l, r) = (l, b:r)
-}

{-
safeInteractWith :: (String -> String) -> FilePath -> FilePath -> IO ()
safeInteractWith function inputFile outputFile = do 
    result :: (Either SomeException T.Text) <- (try (readFileUtf8 inputFile))
    case result of 
        Left err -> print $ "Error reading file:" ++ show err
        (Right content) -> writeFile outputFile (function (T.unpack content))
    
-}

-- TODO
-- 1. Write a function to print the headerLines, geometryLines, and footerLines to see if the split and unsplit functions work correctly
-- print some say 10 lines after skipping some lines and see. 
-- Attach print functions of IO () type after the read functions to see what was read.

--  read the file -> run the read/parse functions -> print and check
-- especially the splitting and putting back together of header, geometry and footer lines


{- IMPORTANT:
removeDecimals is fine but
decimalShowFloat takes the time to copy from 33 seconds to 44 seconds.
It does add a lot of time.
-}




{-
Now add functionality with the layers.
-}

-- Step 7 Parse a layer

{-
accumulateGLine :: GeometryData -> GeometryLine -> GeometryData
-- GeometryData has a field called layers which is a list of layers
accumulateGline accumulatedLayersGeo [] = accumulatedLayersGeo
accumulateGLine accumulatedLayersGeo aGLine = 
    let
        accumulatedLayers = layers accumulatedLayersGeo 
        recentLayerLines = layerLines . L.last $ accumulatedLayers
        recentLayerNumber = layerNumber . L.last $ accumulatedLayers
        --updatedLayerLines = consAtEnd recentLayerLines aGLine 
        updatedLayerLines = recentLayerLines ++ [aGLine]
        updatedLayer = Layer recentLayerNumber updatedLayerLines
        --accumulatedLayers2 = consAtEnd (L.init accumulatedLayers) updatedLayer in
        accumulatedLayers2 = (L.init accumulatedLayers) ++ [updatedLayer] in
    case aGLine of
        --GLLayerChange layerChange -> accumulatedLayers' = consAtEnd accumulatedLayers2 (Layer ((recentLayerNumber+1) []) 
        --GLLayerChange layerChange -> consAtEnd accumulatedLayers2 (Layer (recentLayerNumber+1) [] )
        GLLayerChange layerChange -> GeometryData (accumulatedLayers2 ++ [(Layer (recentLayerNumber+1) [] )])
        --otherwise -> accumulatedLayers' = accumulatedLayers2where
        otherwise -> GeometryData accumulatedLayers2
        -- start with a currentLNum of 1, i.e. the current layer number is 1
-}

accumulateGLine :: GeometryData -> GeometryLine -> GeometryData
-- GeometryData has a field called layers which is a list of layers
accumulateGline accumulatedLayersGeo [] = accumulatedLayersGeo
accumulateGLine accumulatedLayersGeo aGLine = 
    let
        accumulatedLayers = layers accumulatedLayersGeo 
        recentLayerLines = layerLines . L.last $ accumulatedLayers
        recentLayerNumber = layerNumber . L.last $ accumulatedLayers
        in
        --updatedLayerLines = consAtEnd recentLayerLines aGLine 

    case aGLine of
        GLLayerChange layerChange -> GeometryData (accumulatedLayers ++ [(Layer (recentLayerNumber+1) [aGLine] )])
        otherwise -> GeometryData (L.init accumulatedLayers ++ [(Layer recentLayerNumber (recentLayerLines ++ [aGLine]) )])

        

consAtEnd :: [a] -> a -> [a]
consAtEnd [] a = [a]
consAtEnd (x:xs) a = x : consAtEnd xs a

accumulateGLines :: [GeometryLine] -> GeometryData
-- accumulateGlines [] = 
-- I think the base case is handled in the accumulateGLine function
-- Use L.foldl' accumulateGLine [] geometryLines
accumulateGLines geometryLines =  GeometryData (L.tail . layers $ accuWithZeroLayer) where
    accuWithZeroLayer = L.foldl' accumulateGLine (GeometryData [Layer 0 []]) geometryLines
    -- This will contain a layer zero with an empty list of lines
    -- This layer will need to be removed
    -- This method is done to use the L.foldl' function
-- L.foldl' (flip accumulateGLine) [Layer 1 []] geometryLines    

myViewGLines :: FilePath -> IO ()
myViewGLines inputFile = do
    inputString <- readFile inputFile
    let (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
    let gLines = L.map readGeometryLine geometryLines
    putStrLn $ show . L.head $ gLines
    putStrLn $ show . L.last $ gLines

{-
The geometryLines start with a layer change but not end with a layer change.

This is the structure of the geometryLines:
{   
    { -- Layer 1
        $$LAYER/0.06
        ... other lines
        }

    { -- Layer 2
        $$LAYER/0.06
        ... other lines
        }

    ...

    { -- last Layer
        $$LAYER/0.06
        ... other lines
        }
    
    }

myViewGLines myCliFilePath
$$LAYER/0.06
$$POLYLINE/43,2,93,34.009,-26.801,33.804,-26.72,33.588,-26.672,33.36,-26.656,33.09,-26.676,32.838,-26.735,32.605,-26.834,32.388,-26.972,32.188,-27.151,32.003,-27.372,31.869,-27.575,31.759,-27.786,31.674,-28.007,31.613,-28.238,31.576,-28.477,31.564,-28.726,31.576,-28.979,31.613,-29.214,31.673,-29.431,31.757,-29.63,31.865,-29.81,31.996,-29.973,32.144,-30.115,32.3,-30.229,32.464,-30.317,32.637,-30.38,32.821,-30.418,33.016,-30.43,33.197,-30.418,33.387,-30.382,33.586,-30.322,33.794,-30.238,34.012,-30.128,34.299,-29.959,34.406,-30.041,34.319,-30.362,34.317,-30.368,34.232,-30.618,34.229,-30.625,34.127,-30.868,34.125,-30.874,34.007,-31.11,34.004,-31.116,33.87,-31.345,33.867,-31.351,33.718,-31.572,33.715,-31.576,33.56,-31.783,33.556,-31.788,33.398,-31.973,33.394,-31.979,33.233,-32.142,33.228,-32.148,33.065,-32.29,33.059,-32.295,32.895,-32.415,32.887,-32.421,32.723,-32.519,32.714,-32.524,32.544,-32.604,32.537,-32.607,32.363,-32.673,32.355,-32.675,32.177,-32.726,32.169,-32.728,32.007,-32.761,32.111,-32.756,32.375,-32.718,32.631,-32.654,32.88,-32.565,33.122,-32.451,33.359,-32.311,33.637,-32.116,33.894,-31.905,34.13,-31.677,34.345,-31.433,34.538,-31.172,34.709,-30.894,34.857,-30.607,34.977,-30.313,35.072,-30.012,35.139,-29.703,35.18,-29.387,35.193,-29.063,35.176,-28.709,35.124,-28.375,35.036,-28.063,34.914,-27.77,34.755,-27.496,34.56,-27.24,34.387,-27.06,34.203,-26.914,34.009,-26.801
-}


myCheckLayers :: FilePath -> IO ()
myCheckLayers inputFile = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    inputString <- readFile inputFile
    let 
        (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
        gLines = L.map readGeometryLine geometryLines
        geoData = accumulateGLines gLines
    putStrLn $ show $ geoData
    
    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

countNumLayers :: FilePath -> IO ()
countNumLayers inputFile = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    inputString <- readFile inputFile
    let 
        (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
        gLines = L.map readGeometryLine geometryLines
        
        countedLayers = L.foldl' countLayerChanges 0 gLines
        countLayerChanges :: Int -> GeometryLine -> Int
        countLayerChanges acc (GLLayerChange _) = acc + 1
        countLayerChanges acc _ = acc

    putStrLn $ ((show countedLayers) ++ " layer changes counted \n")
    
    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

showSomeLayers :: FilePath -> IO ()
showSomeLayers inputFile = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    inputString <- readFile inputFile
    let 
        (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
        gLines = L.map readGeometryLine geometryLines
        
        geoData = accumulateGLines gLines
        geoLayers = layers geoData
    forM_ (L.take 5 geoLayers ++ lastN' 5 geoLayers) print
    
    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

duplicateLayer :: Layer -> Layer
{- This will take a layer and duplicate the 
-- GeomtetryLine s that are GLPolyline and GLHatches,
-- each line is duplicated and put right after the original line

$$LAYER/0.06
$$POWER/180
$$SPEED/1350
$$FOCUS/3
$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675
$$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733

will become

$$LAYER/0.06
$$POWER/180
$$SPEED/1350
$$FOCUS/3
$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675
$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675
$$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733
$$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733

-}

duplicateLayer (Layer layerNumber layerLines) = Layer layerNumber (L.foldl' duplicateLine [] layerLines) where
    duplicateLine :: [GeometryLine] -> GeometryLine -> [GeometryLine]
    duplicateLine acc (GLPolyline polyline) = acc ++ [GLPolyline polyline, GLPolyline polyline]
    duplicateLine acc (GLHatches hatches) = acc ++ [GLHatches hatches, GLHatches hatches]
    duplicateLine acc aLine = acc ++ [aLine]

duplicateLayers :: GeometryData -> [Int] -> GeometryData
duplicateLayers geoData layerNums = GeometryData (L.foldl' duplicateLayerAtEnd [] geoLayers) where
    geoLayers = layers geoData
    duplicateLayerAtEnd :: [Layer] -> Layer -> [Layer]
    duplicateLayerAtEnd acc layer = if L.elem (layerNumber layer) layerNums
        then acc ++ [duplicateLayer layer]
        else acc ++ [layer]

duplicateLayersSerially :: GeometryData -> [Int] -> GeometryData
duplicateLayersSerially geoData layerNums = GeometryData dupGeoLayers where
    geoLayers = layers geoData
    dupGeoLayers = dupLayers geoLayers layerNums

    dupLayers :: [Layer] -> [Int] -> [Layer]
    dupLayers [] _ = []
    dupLayers (x:xs) layerNums2 = if L.elem (layerNumber x) layerNums2
        then (duplicateLayerSerially x) : (dupLayers xs layerNums2)
        else x : (dupLayers xs layerNums2)

    
    
-- All geometry lines in a layer, except for the first layer change line should be duplicated
duplicateLayerSerially :: Layer -> Layer
duplicateLayerSerially (Layer layerNumber layerLines) = Layer layerNumber dupAddedLayerLines where
    dupAddedLayerLines = layerLines ++ dropInitialLayerChange layerLines

    -- The layer change at the beginning of the layer should be removed
    dropInitialLayerChange :: [GeometryLine] -> [GeometryLine]
    dropInitialLayerChange [] = []
    dropInitialLayerChange (x:xs) = case x of
        GLLayerChange _ -> dropInitialLayerChange xs
        {-
        GLPower _ -> dropInitialLines xs
        GLSpeed _ -> dropInitialLines xs
        GLFocus _ -> dropInitialLines xs
        -- No. The Power, Speed and Focus at the end of the layer could be different 
        so will need to be set back to the values they were at the beginning of the layer
        -}
        otherwise -> x:xs

myStr = "$$LAYER/0.06 $$POWER/180 $$SPEED/1350 $$FOCUS/3 $$POLYLINE/5,2,2,-36,33,-36,33 $$POLYLINE/5,2,2,-32,27,-32,26 $$LAYER/0.06 $$POWER/180 $$SPEED/1350 $$FOCUS/3 $$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675 $$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733"
{-
ghci> L.map (L.tail . L.tail) (L.words myStr)
["LAYER/0.06","POWER/180","SPEED/1350","FOCUS/3","POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675","POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733"]
-}
--myLines = ["LAYER/0.06","POWER/180","SPEED/1350","FOCUS/3","POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675","POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733"]

myGeoData4 = L.map readGeometryLine (L.words myStr)
myGeoData5 = duplicateLayersSerially (accumulateGLines myGeoData4) [1]
myGeoData6 = duplicateLayersSerially (accumulateGLines myGeoData4) [1,2]

{-
Looks like duplicateLayerSerially and duplicateLayersSerially are working fine.


ghci> L.concatMap layerLines (layers myGeoData6)
[$$LAYER/0.06,$$POWER/180,$$SPEED/1350,$$FOCUS/3,$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675,$$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733,$$POWER/180,$$SPEED/1350,$$FOCUS/3,$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675,$$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733,$$LAYER/0.06,$$POWER/180,$$SPEED/1350,$$FOCUS/3,$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675,$$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733,$$POWER/180,$$SPEED/1350,$$FOCUS/3,$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675,$$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733]

ghci> forM_ (L.concatMap layerLines (layers myGeoData5)) print
$$LAYER/0.06
$$POWER/180
$$SPEED/1350
$$FOCUS/3
$$POLYLINE/5,2,2,-36,33,-36,33
$$POLYLINE/5,2,2,-32,27,-32,26
$$POWER/180
$$SPEED/1350
$$FOCUS/3
$$POLYLINE/5,2,2,-36,33,-36,33
$$POLYLINE/5,2,2,-32,27,-32,26
$$LAYER/0.06
$$POWER/180
$$SPEED/1350
$$FOCUS/3
$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675
$$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733


ghci> forM_ (L.concatMap layerLines (layers myGeoData6)) print
$$LAYER/0.06
$$POWER/180
$$SPEED/1350
$$FOCUS/3
$$POLYLINE/5,2,2,-36,33,-36,33
$$POLYLINE/5,2,2,-32,27,-32,26
$$POWER/180
$$SPEED/1350
$$FOCUS/3
$$POLYLINE/5,2,2,-36,33,-36,33
$$POLYLINE/5,2,2,-32,27,-32,26
$$LAYER/0.06
$$POWER/180
$$SPEED/1350
$$FOCUS/3
$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675
$$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733
$$POWER/180
$$SPEED/1350
$$FOCUS/3
$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675
$$POLYLINE/5,2,2,-32.16,27.147,-32.574,26.733
-}

myDuplicateLayers :: FilePath -> FilePath -> [Int] -> IO ()
myDuplicateLayers inputFile outputFile layersToDup = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    inputString <- readFile inputFile
    let 
        (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
        gLines = L.map readGeometryLine geometryLines
        
        geoData = accumulateGLines gLines
        
        allDupGeoData = duplicateLayers geoData layersToDup

        newGeometryLines = writeGeometryData allDupGeoData

        newString = unSplitLinesCli . unSplitGeometryLines $ (headerLines, newGeometryLines, footerLines)
    
    writeFile outputFile newString

    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

myDuplicateAllLayers :: FilePath -> FilePath -> IO ()
myDuplicateAllLayers inputFile outputFile = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    inputString <- readFile inputFile
    let 
        (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
        gLines = L.map readGeometryLine geometryLines
        
        geoData = accumulateGLines gLines

        -- Check this line. 
        layersToDup = L.map layerNumber . layers $ geoData
        -- get all the layer numbers

        -- Maybe L.map is not working as expected
        
        allDupGeoData = duplicateLayers geoData layersToDup

        newGeometryLines = writeGeometryData allDupGeoData

        newString = unSplitLinesCli . unSplitGeometryLines $ (headerLines, newGeometryLines, footerLines)
    
    writeFile outputFile newString

    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

{-
ghci> myDuplicateAllLayers myCliFilePath myCliFileDuplicatePath
Start time: 2025-01-21 11:06:00.532901324 UTC
End time: 2025-01-21 11:07:31.599428688 UTC
Duration: 91.066527364s
-}

myGeoData :: GeometryData
myGeoData = GeometryData [Layer 1 [GLLayerChange (LayerChange 0.06)], Layer 2 [GLLayerChange (LayerChange 0.06)], Layer 3 [GLLayerChange (LayerChange 0.06)]]

--myDupData = duplicateLayers myGeoData [1,3]
--forM_ (layerLines . layers $ myDupData) print
-- L.concatMap layerLines (layers myDupData)
{-
L.concatMap layerLines (layers myDupData)
[$$LAYER/0.06,$$LAYER/0.06,$$LAYER/0.06]

It looks like duplicateLayers is not working.
-}

myGeoData2 :: GeometryData
myGeoData2 = GeometryData [Layer 1 [GLLayerChange (LayerChange 0.06)]]

myGeoData2Dup = duplicateLayers myGeoData2 [1]

myLayer1 = Layer 1 [GLPolyline (Polyline 5 2 2 [(-36.666,33.248),(-36.239,33.675)])]
myLayer2 = Layer 2 [GLPolyline (Polyline 5 2 2 [(-36,33),(-36,33)])]
myLayer3 = Layer 3 [GLPolyline (Polyline 5 2 2 [(-3,3),(-3,3)])]


myGeoData3 = GeometryData [myLayer1, myLayer2, myLayer3]
{-
ghci> forM_ (layerLines . duplicateLayer $ myLayer) print
$$LAYER/0.06

duplicateLayer does not seem to work.

ghci> forM_ (layerLines . duplicateLayer $ myLayer) print
$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675
$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675

Sorry, it works. It only copies the POLYLINE and HATCHES lines.


ghci> L.concatMap layerLines (layers (duplicateLayers myGeoData3 [1,3]))
[$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675,$$POLYLINE/5,2,2,-36.666,33.248,-36.239,33.675,$$POLYLINE/5,2,2,-36,33,-36,33,$$POLYLINE/5,2,2,-3,3,-3,3,$$POLYLINE/5,2,2,-3,3,-3,3

duplicateLayers also seems to work.
-}



myDuplicateFstMOfEveryN :: FilePath -> FilePath -> Int -> Int -> IO ()
myDuplicateFstMOfEveryN inputFile outputFile fstM everyN = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    inputString <- readFile inputFile
    let 
        (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
        gLines = L.map readGeometryLine geometryLines
        
        geoData = accumulateGLines gLines

        allLayerNums = L.map layerNumber . layers $ geoData
        -- get all the layer numbers

        layersToDup = myTakeFstMOfEveryN allLayerNums fstM everyN
        
    putStrLn $ "Layers to duplicate: " ++ show layersToDup ++ "\n"
    -- works until here.

    let allDupGeoData = duplicateLayers geoData layersToDup

        newGeometryLines = writeGeometryData allDupGeoData

        newString = unSplitLinesCli . unSplitGeometryLines $ (headerLines, newGeometryLines, footerLines)
    
    writeFile outputFile newString

    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

-- It is working well.


myTakeFstMOfEveryN :: [a] -> Int -> Int -> [a]
myTakeFstMOfEveryN xs fstM everyN = L.concatMap (L.take fstM) (myTakeEvery everyN xs)
{-
ghci> myTakeFstMOfEveryN [1,2,3,4,5,6,7,8,9,10] 2 3
[1,2,4,5,7,8,10]
-}

{-
ghci> myTakeEvery 3 [1,2,3,4,5,6,7,8,9,10]
[[1,2,3],[4,5,6],[7,8,9],[10]]

make the list into sublists of every 3 elements
-}
myTakeEvery :: Int -> [a] -> [[a]]
myTakeEvery n xs = case L.splitAt n xs of
    (ys, []) -> [ys]
    (ys, zs) -> ys : myTakeEvery n zs

{-
ghci> myDuplicateFstMOfEveryN myCliFilePath myCliFileDupEven 1 2
Start time: 2025-01-21 11:33:44.736486928 UTC
End time: 2025-01-21 11:34:58.383996371 UTC
Duration: 73.647509443s

Start time: 2025-01-21 11:36:05.076715221 UTC
End time: 2025-01-21 11:37:16.613925482 UTC
Duration: 71.537210261s

-- All layers seem to have been duplicated

-}

{-
myfun :: Int -> IO ()
myfun x = do
    let y = x + 1
    putStrLn $ "The value of y is: " ++ show y
    let z = x + 2
    putStrLn $ "The value of z is: " ++ show z
-}

myTakePatternOfEveryN :: [a] -> Int -> [Int] -> [a]
-- The pattern is assumed to be in increasing order.
-- CAREFUL: The pattern is assumed to start at 0 possibly. i.e. 0 to period - 1
-- pattern of [1,3] means the 2nd and the 4th elements of every period
-- Patternperiod is the period of the pattern
-- Example [1,3] 6 is supposed to take the 1st and the 3rd elements of every 6 elements
myTakePatternOfEveryN [] _ _ = []
myTakePatternOfEveryN xs patternPeriod [] = []

myTakePatternOfEveryN xs patternPeriod pattern
    | patternPeriod < (L.last pattern) = error "Pattern period is less than the last element of the pattern, which is assumed to be the greatest element of the pattern"
    | otherwise = L.concatMap (\chunk -> elemsInIndices chunk pattern) (myTakeEvery patternPeriod xs)

{-
elemsInIndices :: [a] -> [Int] -> [a]
elemsInIndices xs [] = []
elemsInIndices [] indices = []
elemsInIndices xs indices = L.map (xs !!) indices
-}

elemsInIndices :: [a] -> [Int] -> [a]
elemsInIndices xs [] = []
elemsInIndices [] indices = []
elemsInIndices xs (index1:indexRest) = safeIndex xs index1 ++ elemsInIndices xs indexRest where
    safeIndex :: [a] -> Int -> [a]
    safeIndex xs index = case L.length xs > index of
        True -> [xs !! index]
        False -> []

{-
ghci> elemsInIndices [1,2,3,4,5,6,7,8,9] [0,4,13]
[1,5]
ghci> myTakePatternOfEveryN [1,2,3,4,5,6,7,8,9,10,11,12] 5 [1,3]
[2,4,7,9,12]
-}


myDuplicatePatternOfEveryN :: FilePath -> FilePath -> [Int] -> Int -> IO ()
myDuplicatePatternOfEveryN inputFile outputFile pattern patternPeriod = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    inputString <- readFile inputFile
    let 
        (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
        gLines = L.map readGeometryLine geometryLines
        
        geoData = accumulateGLines gLines

        allLayerNums = L.map layerNumber . layers $ geoData
        -- get all the layer numbers

        layersToDup = myTakePatternOfEveryN allLayerNums patternPeriod pattern
        
    putStrLn $ "Layers to duplicate: " ++ show layersToDup ++ "\n"
    -- works until here.

    let allDupGeoData = duplicateLayers geoData layersToDup

        newGeometryLines = writeGeometryData allDupGeoData

        newString = unSplitLinesCli . unSplitGeometryLines $ (headerLines, newGeometryLines, footerLines)
    
    writeFile outputFile newString

    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration



myDuplicateSeriallyPatternOfEveryN :: FilePath -> FilePath -> [Int] -> Int -> IO ()
-- CAREFUL: The pattern is assumed to start at 0 i.e. 0 to period - 1
myDuplicateSeriallyPatternOfEveryN inputFile outputFile pattern patternPeriod = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    inputString <- readFile inputFile
    let 
        (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
        gLines = L.map readGeometryLine geometryLines
        
        geoData = accumulateGLines gLines

        allLayerNums = L.map layerNumber . layers $ geoData
        -- get all the layer numbers

        layersToDup = myTakePatternOfEveryN allLayerNums patternPeriod pattern
        
    putStrLn $ "Layers to duplicate: " ++ show layersToDup ++ "\n"
    -- works until here.

    let allDupGeoData = duplicateLayersSerially geoData layersToDup

        newGeometryLines = writeGeometryData allDupGeoData

        newString = unSplitLinesCli . unSplitGeometryLines $ (headerLines, newGeometryLines, footerLines)
    
    writeFile outputFile newString

    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

{-
For mini:
ghci> myDuplicateSeriallyPatternOfEveryN miniFile miniDupSerially [0] 1
Start time: 2025-01-28 17:02:44.83818381 UTC
Layers to duplicate: [1,2,3,4,5]

End time: 2025-01-28 17:02:44.887474513 UTC
Duration: 0.049290703s
ghci> myDuplicateSeriallyPatternOfEveryN miniFile mini1and2OfEvery4 [1,2] 4
Start time: 2025-01-28 17:03:44.326617089 UTC
Layers to duplicate: [2,3]

End time: 2025-01-28 17:03:44.359216099 UTC
Duration: 0.03259901s
ghci> myDuplicatePatternOfEveryN miniFile miniEvenLinewise [1] 2
Start time: 2025-01-28 17:04:57.493758007 UTC
Layers to duplicate: [2,4]

End time: 2025-01-28 17:04:57.533012443 UTC
Duration: 0.039254436s

-}

myCombineLayersFrom2FilesAndExecute :: FilePath -> FilePath -> Int -> FilePath -> IO ()
myCombineLayersFrom2FilesAndExecute inputFile1 inputFile2 partStartLayer outputFile = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime

    inputString1 <- readFile inputFile1
    inputString2 <- readFile inputFile2
    let 
        (headerLines1, geometryLines1, footerLines1) = splitGeometryLines . splitLines $ inputString1
        (headerLines2, geometryLines2, footerLines2) = splitGeometryLines . splitLines $ inputString2

        gLines1 = L.map readGeometryLine geometryLines1
        gLines2 = L.map readGeometryLine geometryLines2

        geoData1 = accumulateGLines gLines1
        geoData2 = accumulateGLines gLines2

        combinedGeoData = combineLayersFrom2Files geoData1 geoData2 partStartLayer

        newGeometryLines = writeGeometryData combinedGeoData

        newHeaderLines = combineHeaderLinesFrom2Files headerLines1 headerLines2
        newFooterLines = combineFooterLinesFrom2Files footerLines1 footerLines2

        newString = unSplitLinesCli . unSplitGeometryLines $ (newHeaderLines, newGeometryLines, newFooterLines)

    writeFile outputFile newString

    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime

    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

-- Add this more memory-efficient version
myCombineLayersFrom2FilesAndExecuteEfficient :: FilePath -> FilePath -> Int -> FilePath -> IO ()
myCombineLayersFrom2FilesAndExecuteEfficient inputFile1 inputFile2 partStartLayer outputFile = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime
    
    -- Use lazy I/O for large files
    inputString1 <- readFile inputFile1  -- This is already lazy
    inputString2 <- readFile inputFile2
    
    let (headerLines1, geometryLines1, footerLines1) = splitGeometryLines . splitLines $ inputString1
    let (headerLines2, geometryLines2, footerLines2) = splitGeometryLines . splitLines $ inputString2
    
    -- Process geometry lines lazily
    let gLines1 = L.map readGeometryLine geometryLines1
    let gLines2 = L.map readGeometryLine geometryLines2
    
    -- Force evaluation of data length for debugging
    putStrLn $ "File 1 geometry lines: " ++ show (L.length geometryLines1)
    putStrLn $ "File 2 geometry lines: " ++ show (L.length geometryLines2)

    -- Process data
    let geoData1 = accumulateGLines gLines1
    let geoData2 = accumulateGLines gLines2

    putStrLn $ "File 1 layers: " ++ show (L.length . layers $ geoData1)
    putStrLn $ "File 2 layers: " ++ show (L.length . layers $ geoData2)

    let combinedGeoData = combineLayersFrom2Files geoData1 geoData2 partStartLayer
    let newGeometryLines = writeGeometryData combinedGeoData
    
    let newHeaderLines = combineHeaderLinesFrom2Files headerLines1 headerLines2
    let newFooterLines = combineFooterLinesFrom2Files footerLines1 footerLines2
    
    let newString = unSplitLinesCli . unSplitGeometryLines $ (newHeaderLines, newGeometryLines, newFooterLines)
    
    -- Use lazy writing
    writeFile outputFile newString
    
    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

combineLayersFrom2Files :: GeometryData -> GeometryData -> Int -> GeometryData
combineLayersFrom2Files geoData1 geoData2 partStartLayer = 
    let
        allLayers1 = layers geoData1
        allLayers2 = layers geoData2

        -- Keep layers before partStartLayer from layers1
        initialLayers1 = L.take (partStartLayer - 1) allLayers1
        
        -- Drop layers before partStartLayer from both files for combination
        layers1ToCombine = L.drop (partStartLayer - 1) allLayers1
        layers2ToCombine = L.drop (partStartLayer - 1) allLayers2

        -- Start at the partStartLayer in both files,
        -- for each layer, append the lines from the second file after the last line of the same layer in the first file
        -- the new layer starts only after all the lines of the layer from the second file have been added to the lines of the layer of the first file

        -- Helper function to combine a single layer from layers1 with corresponding layer from layers2
        combineLayerWithNext :: ([Layer], [Layer]) -> Layer -> ([Layer], [Layer])
        combineLayerWithNext (accLayers, remainingLayers2) layer1 = 
            case remainingLayers2 of
                [] -> error $ "Unequal number of layers: layers2 has fewer layers than layers1 from partStartLayer onwards. Layer " ++ show (layerNumber layer1) ++ " from layers1 has no corresponding layer in layers2."
                (layer2:restLayers2) -> 
                    let 
                        -- Skip the layer change line from layer2 (first line is typically $$LAYER/...)
                        layer2LinesWithoutLayerChange = dropInitialLayerChange (layerLines layer2)
                        
                        -- Combine layer1 lines with layer2 lines (without the layer change)
                        combinedLayer = Layer (layerNumber layer1) (layerLines layer1 ++ layer2LinesWithoutLayerChange)
                    in (accLayers ++ [combinedLayer], restLayers2)

        -- Helper function to drop initial layer change lines from a list of geometry lines
        dropInitialLayerChange :: [GeometryLine] -> [GeometryLine]
        dropInitialLayerChange [] = []
        dropInitialLayerChange (x:xs) = case x of
            GLLayerChange _ -> dropInitialLayerChange xs
            otherwise -> x:xs

        -- Use foldl' to process each layer from layers1ToCombine and consume layers2ToCombine
        (combinedLayers, _) = L.foldl' combineLayerWithNext ([], layers2ToCombine) layers1ToCombine
        
        -- Combine initial layers with combined layers
        finalLayers = initialLayers1 ++ combinedLayers
        
    in GeometryData finalLayers  

-- Crashes in vs code but runs in terminal in 47 seconds
combineLayersFrom2Files' :: GeometryData -> GeometryData -> Int -> [GeometryLine]-> GeometryData
combineLayersFrom2Files' geoData1 geoData2 partStartLayer additionalLines = 
    let
        allLayers1 = layers geoData1
        allLayers2 = layers geoData2

        -- Keep layers before partStartLayer from layers1
        initialLayers1 = L.take (partStartLayer - 1) allLayers1
        
        -- Drop layers before partStartLayer from both files for combination
        layers1ToCombine = L.drop (partStartLayer - 1) allLayers1
        layers2ToCombine = L.drop (partStartLayer - 1) allLayers2

        -- Combine the layers as before, but now also add additional lines at the end of each combined layer
        combineLayerWithNext :: ([Layer], [Layer]) -> Layer -> ([Layer], [Layer])
        combineLayerWithNext (accLayers, remainingLayers2) layer1 = 
            case remainingLayers2 of
                [] -> error $ "Unequal number of layers: layers2 has fewer layers than layers1 from partStartLayer onwards. Layer " ++ show (layerNumber layer1) ++ " from layers1 has no corresponding layer in layers2."
                (layer2:restLayers2) -> 
                    let 
                        layer2LinesWithoutLayerChange = dropInitialLayerChange (layerLines layer2)
                        combinedLayer = Layer (layerNumber layer1) (layerLines layer1  ++ additionalLines ++ layer2LinesWithoutLayerChange)
                    in (accLayers ++ [combinedLayer], restLayers2)

        dropInitialLayerChange :: [GeometryLine] -> [GeometryLine]
        dropInitialLayerChange [] = []
        dropInitialLayerChange (x:xs) = case x of
            GLLayerChange _ -> dropInitialLayerChange xs
            otherwise -> x:xs

        (combinedLayers, _) = L.foldl' combineLayerWithNext ([], layers2ToCombine) layers1ToCombine
        
        finalLayers = initialLayers1 ++ combinedLayers
        
    in GeometryData finalLayers

-- Takes much longer than combineLayersFrom2Files' but does not crash in vs code
--- Does not seem to exit, something is not terminating
combineLayersFrom2Files'2 :: GeometryData -> GeometryData -> Int -> [GeometryLine]-> GeometryData
combineLayersFrom2Files'2 geoData1 geoData2 partStartLayer additionalLines = 
    let
        allLayers1 = layers geoData1
        allLayers2 = layers geoData2

        -- Keep layers before partStartLayer from layers1
        initialLayers1 = L.take (partStartLayer - 1) allLayers1
        
        -- Drop layers before partStartLayer from both files for combination
        layers1ToCombine = L.drop (partStartLayer - 1) allLayers1
        layers2ToCombine = L.drop (partStartLayer - 1) allLayers2

        -- Combine the layers as before, but now also add additional lines at the end of each combined layer
        combineLayerWithNext :: ([Layer], [Layer]) -> Layer -> ([Layer], [Layer])
        combineLayerWithNext (accLayers, remainingLayers2) layer1 = 
            case remainingLayers2 of
                [] -> error $ "Unequal number of layers: layers2 has fewer layers than layers1 from partStartLayer onwards. Layer " ++ show (layerNumber layer1) ++ " from layers1 has no corresponding layer in layers2."
                (layer2:restLayers2) -> 
                    let 
                        layer2LinesWithoutLayerChange = dropInitialLayerChange (layerLines layer2)
                        combinedLayer = Layer (layerNumber layer1) (layerLines layer1 ++ layer2LinesWithoutLayerChange ++ additionalLines)
                    in (combinedLayer:accLayers, restLayers2)

        dropInitialLayerChange :: [GeometryLine] -> [GeometryLine]
        dropInitialLayerChange [] = []
        dropInitialLayerChange (x:xs) = case x of
            GLLayerChange _ -> dropInitialLayerChange xs
            otherwise -> x:xs

        (reversedCombinedLayers, _) = L.foldl' combineLayerWithNext ([], layers2ToCombine) layers1ToCombine
        -- Reverse the combined layers to maintain the original order
        combinedLayers = L.reverse reversedCombinedLayers

        finalLayers = initialLayers1 ++ combinedLayers

    in GeometryData finalLayers

myCombineLayersFrom2FilesStreamlined':: FilePath -> FilePath -> Int -> FilePath -> IO ()
myCombineLayersFrom2FilesStreamlined' inputFile1 inputFile2 partStartLayer outputFile = do
    startTime <- getCurrentTime
    putStrLn "Reading file 1..."
    inputString1 <- readFile inputFile1
    
    putStrLn "Processing file 1..."
    let (headerLines1, geomLines1, footerLines1) = splitGeometryLines . splitLines $ inputString1
    
    -- Remove any empty lines at the beginning of geometryLines1 and geometryLines2
    let geometryLines1 = L.dropWhile (\line -> L.null (L.dropWhile C.isSpace line)) geomLines1
    -- Remove any empty lines at the end of geometryLines1 and geometryLines2
    --let geometryLines1 = L.dropWhile (\line -> L.null (L.dropWhile C.isSpace line)) $
    --                     L.reverse $ L.dropWhile (\line -> L.null (L.dropWhile C.isSpace line)) (L.reverse geomLines1)

    let gLines1 = L.map readGeometryLine geometryLines1
    let geoData1 = accumulateGLines gLines1
    
    -- Force evaluation and allow garbage collection
    putStrLn $ "File 1 layers: " ++ show (L.length . layers $ geoData1)
    
    putStrLn "Reading file 2..."
    inputString2 <- readFile inputFile2
    
    putStrLn "Processing file 2..."
    let (headerLines2, geomLines2, footerLines2) = splitGeometryLines . splitLines $ inputString2
    
    let geometryLines2 = L.dropWhile (\line -> L.null (L.dropWhile C.isSpace line)) geomLines2
    
    let gLines2 = L.map readGeometryLine geometryLines2
    let geoData2 = accumulateGLines gLines2
    
    putStrLn $ "File 2 layers: " ++ show (L.length . layers $ geoData2)
    
    -- Now combine
    putStrLn "Combining layers..."
    let combinedGeoData = combineLayersFrom2Files'2 geoData1 geoData2 partStartLayer sampleWaitingLines1

    let newGeometryLines = writeGeometryData combinedGeoData
    
    let newHeaderLines = combineHeaderLinesFrom2Files headerLines1 headerLines2
    let newFooterLines = combineFooterLinesFrom2Files footerLines1 footerLines2
    
    let newString = unSplitLinesCli . unSplitGeometryLines $ (newHeaderLines, newGeometryLines, newFooterLines)
    
    -- Use lazy writing
    writeFile outputFile newString
    
    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

sampleWaitingLines1 :: [GeometryLine]
sampleWaitingLines1 = [GLPower (Power 0), GLSpeed (Speed 5), 
    GLPolyline (Polyline 5 2 2 [(-25.000, -8.000),(25.000,-8.000)])]

-- Just take the header lines from the first file for now
combineHeaderLinesFrom2Files :: [String] -> [String] -> [String]
combineHeaderLinesFrom2Files headerLines1 headerLines2 = headerLines1

-- Just take the footer lines from the first file for now
combineFooterLinesFrom2Files :: [String] -> [String] -> [String]
combineFooterLinesFrom2Files footerLines1 footerLines2 = footerLines1

myInputFile1Mini :: FilePath
myInputFile1Mini = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/mini.cli"

myInputFile2Mini :: FilePath
myInputFile2Mini = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/mini.cli"

myOutputFileMini :: FilePath
myOutputFileMini = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/miniCombined.cli"

-- myCombineLayersFrom2FilesAndExecute myInputFile1Mini myInputFile2Mini 2 myOutputFileMini

myInputFile1Big :: FilePath
myInputFile1Big = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650.cli"

myInputFile2Big :: FilePath
myInputFile2Big = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/Ti file copied with change.cli"

myOutputFileBig :: FilePath
myOutputFileBig = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650_combined.cli"

experiment4CarbonAngle0File :: FilePath
experiment4CarbonAngle0File = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/2025-08-04_17-4PH_Carbon_Cubes_81W-225W.cli"

experiment4CarbonAngle90File :: FilePath
experiment4CarbonAngle90File = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/2025-08-04_17-4PH_Carbon_Cubes_90deg_RM_99W-275W.cli"

experiment4CarbonRemeltingCombinedFile :: FilePath
experiment4CarbonRemeltingCombinedFile = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/2025-08-04_17-4PH_Carbon_Cubes_Combined.cli"
-- myCombineLayersFrom2FilesAndExecute myInputFile1Big myInputFile2Big 12 myOutputFileBig

experiment4CarbonRemeltingCombinedFileWithoutWaitingLines :: FilePath
experiment4CarbonRemeltingCombinedFileWithoutWaitingLines = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/2025-08-04_17-4PH_Carbon_Cubes_Combined_without_waiting_lines.cli"

ex4TensileFirstMeltingFile :: FilePath
ex4TensileFirstMeltingFile = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/tensile test specimen/2025-08-14_Zugproben_17-4PH+C_Doublemelt_testseries_1st.cli"

ex4TensileFirstMeltingFile2 :: FilePath
ex4TensileFirstMeltingFile2 = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/tensile test specimen/2025-08-14_Zugproben_17-4PH+C_Doublemelt_testseries_1st_170W.cli"

ex4TensileSecondMeltingFile :: FilePath
ex4TensileSecondMeltingFile = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/tensile test specimen/2025-08-14_Zugproben_17-4PH+C_Doublemelt_testseries_2nd.cli"

ex4TensileCombinedFile :: FilePath
ex4TensileCombinedFile = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/tensile test specimen/2025-08-14_Zugproben_17-4PH+C_Doublemelt_testseries_combined.cli" 

ex4TensileCombinedFile2 :: FilePath
ex4TensileCombinedFile2 = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/tensile test specimen/2025-08-14_Zugproben_17-4PH+C_Doublemelt_testseries_combined_170W.cli" 

ex4TensileFirstMeltingFileWithContour100W :: FilePath
ex4TensileFirstMeltingFileWithContour100W = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/tensile test specimen/100 W contour power/2025-08-25_Zugproben_V2_K100W_1st.cli"

ex4TensileSecondMeltingFileWithContour100W :: FilePath
ex4TensileSecondMeltingFileWithContour100W = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/tensile test specimen/100 W contour power/2025-08-25_Zugproben_V2_K100W_2st.cli"

ex4TensileCombinedFileWithContour100W :: FilePath
ex4TensileCombinedFileWithContour100W = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/tensile test specimen/100 W contour power/2025-08-25_Zugproben_V2_K100W_combined.cli"


ex4cubes2FirstMeltingFile :: FilePath
ex4cubes2FirstMeltingFile = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/2 cubes new/2025-08-15_Wuerfel_2x_Remelting_1st.cli"

ex4cubes2SecondMeltingFile :: FilePath
ex4cubes2SecondMeltingFile = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/2 cubes new/2025-08-15_Wuerfel_2x_Remelting_2st.cli" 

ex4cubes2CombinedFile :: FilePath
ex4cubes2CombinedFile = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/0-01 C + 17-4 PH experiment 4/2 cubes new/2025-08-15_Wuerfel_2x_Remelting_Combined.cli"


-- Need to check the files if the combining worked, then later think about the buffer lines in between the layers from the two 




-- Handling state of power, speed, focus, etc. in the combined file

-- State to track the latest power, speed, and focus values
data LaserState = LaserState 
    { statePower :: Maybe Power
    , stateSpeed :: Maybe Speed  
    , stateFocus :: Maybe Focus
    }

instance Show LaserState where
    show (LaserState mp ms mf) = 
        case (mp, ms, mf) of
            (Nothing, Nothing, Nothing) -> "LaserState { power: None, speed: None, focus: None }"
            (Just p, Nothing, Nothing) -> "LaserState { power: " ++ show p ++ ", speed: None, focus: None }"
            (Nothing, Just s, Nothing) -> "LaserState { power: None, speed: " ++ show s ++ ", focus: None }"
            (Nothing, Nothing, Just f) -> "LaserState { power: None, speed: None, focus: " ++ show f ++ " }"
            (Just p, Just s, Nothing) -> "LaserState { power: " ++ show p ++ ", speed: " ++ show s ++ ", focus: None }"
            (Just p, Nothing, Just f) -> "LaserState { power: " ++ show p ++ ", speed: None, focus: " ++ show f ++ " }"
            (Nothing, Just s, Just f) -> "LaserState { power: None, speed: " ++ show s ++ ", focus: " ++ show f ++ " }"
            (Just p, Just s, Just f) -> "LaserState { power: " ++ show p ++ ", speed: " ++ show s ++ ", focus: " ++ show f ++ " }"

initialLaserState :: LaserState
initialLaserState = LaserState Nothing Nothing Nothing

-- Update state with geometry line if it contains power/speed/focus
updateLaserState :: GeometryLine -> State LaserState ()
updateLaserState (GLPower power) = modify $ \s -> s { statePower = Just power }
updateLaserState (GLSpeed speed) = modify $ \s -> s { stateSpeed = Just speed }
updateLaserState (GLFocus focus) = modify $ \s -> s { stateFocus = Just focus }
updateLaserState _ = return ()

-- Extract current state values as geometry lines
getCurrentStateLines :: State LaserState [GeometryLine]
getCurrentStateLines = do
    LaserState maybePower maybeSpeed maybeFocus <- get
    let powerLine = maybe [] (\p -> [GLPower p]) maybePower
        speedLine = maybe [] (\s -> [GLSpeed s]) maybeSpeed  
        focusLine = maybe [] (\f -> [GLFocus f]) maybeFocus
    return $ powerLine ++ speedLine ++ focusLine


-- Process layer2 lines to extract final state and get lines without layer changes
processLayer2Lines :: [GeometryLine] -> (LaserState, [GeometryLine])
processLayer2Lines layer2Lines = 
    let layer2LinesWithoutLayerChange = dropInitialLayerChange layer2Lines
        finalState = execState (mapM_ updateLaserState layer2LinesWithoutLayerChange) initialLaserState
    in (finalState, layer2LinesWithoutLayerChange)


-- Helper function to drop initial layer change lines
dropInitialLayerChange :: [GeometryLine] -> [GeometryLine]
dropInitialLayerChange [] = []
dropInitialLayerChange (x:xs) = case x of
    GLLayerChange _ -> dropInitialLayerChange xs
    otherwise -> x:xs

{- Corrected next, revert to previous if it is problematic
combineLayersFrom2FilesWithState :: GeometryData -> GeometryData -> Int -> [GeometryLine] -> GeometryData
combineLayersFrom2FilesWithState geoData1 geoData2 partStartLayer additionalLines = 
    let
        allLayers1 = layers geoData1
        allLayers2 = layers geoData2

        -- Keep layers before partStartLayer from layers1
        initialLayers1 = L.take (partStartLayer - 1) allLayers1
        
        -- Drop layers before partStartLayer from both files for combination
        layers1ToCombine = L.drop (partStartLayer - 1) allLayers1
        layers2ToCombine = L.drop (partStartLayer - 1) allLayers2

        -- Combine layers with state tracking
        combineLayerWithState :: ([Layer], [Layer]) -> Layer -> ([Layer], [Layer])
        combineLayerWithState (accLayers, remainingLayers2) layer1 = 
            case remainingLayers2 of
                [] -> error $ "Unequal number of layers: layers2 has fewer layers than layers1 from partStartLayer onwards. Layer " ++ show (layerNumber layer1) ++ " from layers1 has no corresponding layer in layers2."
                (layer2:restLayers2) -> 
                    let 
                        -- Process layer2 to get final state and clean lines
                        (finalState, layer2LinesClean) = processLayer2Lines (layerLines layer2)
                        
                        -- Get state restoration lines
                        stateRestorationLines = evalState getCurrentStateLines finalState
                        
                        -- Combine: layer1 + additionalLines + stateRestoration + layer2Clean
                        combinedLayerLines = layerLines layer1 ++ additionalLines ++ stateRestorationLines ++ layer2LinesClean
                        combinedLayer = Layer (layerNumber layer1) combinedLayerLines
                    in (accLayers ++ [combinedLayer], restLayers2)

        -- Use foldl' to process each layer
        (combinedLayers, _) = L.foldl' combineLayerWithState ([], layers2ToCombine) layers1ToCombine
        
        -- Combine initial layers with combined layers
        finalLayers = initialLayers1 ++ combinedLayers
        
    in GeometryData finalLayers
    -}

{-
-- Fixed version that ensures state restoration lines are always added
combineLayersFrom2FilesWithState :: GeometryData -> GeometryData -> Int -> [GeometryLine] -> GeometryData
combineLayersFrom2FilesWithState geoData1 geoData2 partStartLayer additionalLines = 
    let
        allLayers1 = layers geoData1
        allLayers2 = layers geoData2

        -- Keep layers before partStartLayer from layers1
        initialLayers1 = L.take (partStartLayer - 1) allLayers1
        
        -- Drop layers before partStartLayer from both files for combination
        layers1ToCombine = L.drop (partStartLayer - 1) allLayers1
        layers2ToCombine = L.drop (partStartLayer - 1) allLayers2

        -- Combine layers with state tracking
        combineLayerWithState :: ([Layer], [Layer]) -> Layer -> ([Layer], [Layer])
        combineLayerWithState (accLayers, remainingLayers2) layer1 = 
            case remainingLayers2 of
                [] -> error $ "Unequal number of layers: layers2 has fewer layers than layers1 from partStartLayer onwards. Layer " ++ show (layerNumber layer1) ++ " from layers1 has no corresponding layer in layers2."
                (layer2:restLayers2) -> 
                    let 
                        -- Process layer2 to get final state and clean lines
                        (finalState, layer2LinesClean) = processLayer2Lines (layerLines layer2)
                        
                        -- Get state restoration lines - ensure we always get valid restoration lines
                        stateRestorationLines = getStateRestorationLinesWithDefaults finalState
                        
                        -- Debug: print what state we extracted
                        -- putStrLn $ "Layer " ++ show (layerNumber layer1) ++ " final state: " ++ show finalState
                        
                        -- Combine: layer1 + additionalLines + stateRestoration + layer2Clean
                        combinedLayerLines = layerLines layer1 ++ additionalLines ++ stateRestorationLines ++ layer2LinesClean
                        combinedLayer = Layer (layerNumber layer1) combinedLayerLines
                    in (accLayers ++ [combinedLayer], restLayers2)

        -- Use foldl' to process each layer
        (combinedLayers, _) = L.foldl' combineLayerWithState ([], layers2ToCombine) layers1ToCombine
        
        -- Combine initial layers with combined layers
        finalLayers = initialLayers1 ++ combinedLayers
        
    in GeometryData finalLayers
-}
{-
-- New function to get state restoration lines with better handling
getStateRestorationLines :: LaserState -> [GeometryLine]
getStateRestorationLines (LaserState maybePower maybeSpeed maybeFocus) = 
    let powerLine = case maybePower of
            Just power -> [GLPower power]
            Nothing -> []  -- Could also provide a default: [GLPower (Power 220)]
        speedLine = case maybeSpeed of  
            Just speed -> [GLSpeed speed]
            Nothing -> []  -- Could also provide a default: [GLSpeed (Speed 1550)]
        focusLine = case maybeFocus of
            Just focus -> [GLFocus focus] 
            Nothing -> []  -- Could also provide a default: [GLFocus (Focus 3)]
    in powerLine ++ speedLine ++ focusLine
    -}


-- Alternative version that provides default values when state is Nothing
-- Default values are 0 for power, 10 for speed, and 3 for focus
getStateRestorationLinesWithDefaults :: LaserState -> [GeometryLine]
getStateRestorationLinesWithDefaults (LaserState maybePower maybeSpeed maybeFocus) = 
    let powerLine = case maybePower of
            Just power -> [GLPower power]
            Nothing -> [GLPower (Power 0)]  -- Default power
        speedLine = case maybeSpeed of  
            Just speed -> [GLSpeed speed]
            Nothing -> [GLSpeed (Speed 10)]  -- Default speed
        focusLine = case maybeFocus of
            Just focus -> [GLFocus focus] 
            Nothing -> [GLFocus (Focus 3)]    -- Default focus
    in powerLine ++ speedLine ++ focusLine

-- Sample GeometryData 1 (3 layers)
testGeoData1 :: GeometryData
testGeoData1 = GeometryData 
    [ Layer 1 
        [ GLLayerChange (LayerChange 0.06)
        , GLPower (Power 180)
        , GLSpeed (Speed 1350)
        , GLFocus (Focus 3)
        , GLPolyline (Polyline 1 2 2 [(10.0, 10.0), (20.0, 10.0)])
        ]
    , Layer 2 
        [ GLLayerChange (LayerChange 0.12)
        , GLPower (Power 200)
        , GLSpeed (Speed 1400)
        , GLPolyline (Polyline 2 2 2 [(15.0, 15.0), (25.0, 15.0)])
        ]
    , Layer 3 
        [ GLLayerChange (LayerChange 0.18)
        , GLPower (Power 220)
        , GLSpeed (Speed 1500)
        , GLFocus (Focus 4)
        , GLPolyline (Polyline 3 2 2 [(20.0, 20.0), (30.0, 20.0)])
        ]
    ]

-- Sample GeometryData 2 (3 layers) 
testGeoData2 :: GeometryData
testGeoData2 = GeometryData
    [ Layer 1
        [ GLLayerChange (LayerChange 0.06)
        , GLPower (Power 160)
        , GLSpeed (Speed 1200)
        , GLFocus (Focus 2)
        , GLPolyline (Polyline 10 2 2 [(5.0, 5.0), (15.0, 5.0)])
        ]
    , Layer 2
        [ GLLayerChange (LayerChange 0.12)
        , GLPower (Power 170)
        , GLSpeed (Speed 1250)
        , GLPolyline (Polyline 11 2 2 [(8.0, 8.0), (18.0, 8.0)])
        , GLFocus (Focus 2.5)  -- Focus comes after polyline
        ]
    , Layer 3
        [ GLLayerChange (LayerChange 0.18)
        , GLPower (Power 190)
        , GLPolyline (Polyline 12 2 2 [(12.0, 12.0), (22.0, 12.0)])
        , GLSpeed (Speed 1300)  -- Speed comes after polyline
        , GLFocus (Focus 3.5)
        ]
    ]

-- Sample waiting lines
testWaitingLines :: [GeometryLine]
testWaitingLines = 
    [ GLPower (Power 0)
    , GLSpeed (Speed 10)
    , GLPolyline (Polyline 99 2 2 [(0.0, 0.0), (1.0, 0.0)])
    ]

-- Test function to display results nicely
testCombineWithState :: IO ()
testCombineWithState = do
    putStrLn "=== Testing combineLayersFrom2FilesWithState ==="
    putStrLn "\n--- Original GeoData1 layers from partStartLayer=2 onwards ---"
    let layers1ToCombine = L.drop 1 (layers testGeoData1)  -- Layers 2,3
    mapM_ (putStrLn . show) layers1ToCombine
    
    putStrLn "\n--- Original GeoData2 layers from partStartLayer=2 onwards ---"
    let layers2ToCombine = L.drop 1 (layers testGeoData2)  -- Layers 2,3
    mapM_ (putStrLn . show) layers2ToCombine
    
    putStrLn "\n--- Waiting lines ---"
    mapM_ (putStrLn . show) testWaitingLines
    
    putStrLn "\n--- Combined result (partStartLayer=2) ---"
    let (result, _) = combineLayersFrom2FilesWithState testGeoData1 testGeoData2 2 testWaitingLines initialLaserState
    mapM_ (putStrLn . show) (layers result)
    
    putStrLn "\n--- Layer 2 detailed lines ---"
    let layer2 = (layers result) !! 1  -- Second layer (index 1)
    putStrLn $ "Layer " ++ show (layerNumber layer2) ++ " lines:"
    mapM_ (putStrLn . ("  " ++) . show) (layerLines layer2)

-- Test function to check state extraction from layer2
testStateExtraction :: IO ()
testStateExtraction = do
    putStrLn "\n=== Testing state extraction from layer2 ==="
    let layer2_data2 = (layers testGeoData2) !! 1  -- Layer 2 from geoData2
    putStrLn "Layer 2 from geoData2:"
    mapM_ (putStrLn . ("  " ++) . show) (layerLines layer2_data2)
    
    let (finalState, cleanLines) = processLayer2Lines (layerLines layer2_data2)
    putStrLn "\nExtracted state:"
    putStrLn $ "Power: " ++ show (statePower finalState)
    putStrLn $ "Speed: " ++ show (stateSpeed finalState) 
    putStrLn $ "Focus: " ++ show (stateFocus finalState)

    putStrLn "\nClean lines (without layer change):"
    mapM_ (putStrLn . ("  " ++) . show) cleanLines
    
    putStrLn "\nState restoration lines:"
    let restorationLines = evalState getCurrentStateLines finalState
    mapM_ (putStrLn . ("  " ++) . show) restorationLines

-- Quick test function
quickTest :: IO ()
quickTest = do
    let (result, _) = combineLayersFrom2FilesWithState testGeoData1 testGeoData2 2 testWaitingLines initialLaserState
    putStrLn $ "Number of layers in result: " ++ show (L.length (layers result))
    putStrLn $ "Layer 1 (unchanged): " ++ show (L.length (layerLines ((layers result) !! 0))) ++ " lines"
    putStrLn $ "Layer 2 (combined): " ++ show (L.length (layerLines ((layers result) !! 1))) ++ " lines"
    putStrLn $ "Layer 3 (combined): " ++ show (L.length (layerLines ((layers result) !! 2))) ++ " lines"

-- Updated main function using the new combiner
-- Runs in 53.5 seconds in terminal
-- Killed when using the new combineLayersFrom2FilesWithState
myCombineLayersFrom2FilesWithStateAndExecute :: FilePath -> FilePath -> Int -> FilePath -> IO ()
myCombineLayersFrom2FilesWithStateAndExecute inputFile1 inputFile2 partStartLayer outputFile = do
    startTime <- getCurrentTime
    putStrLn "Reading file 1..."
    inputString1 <- readFile inputFile1
    
    putStrLn "Processing file 1..."
    let (headerLines1, geometryLines1, footerLines1) = splitGeometryLines . splitLines $ inputString1
    let gLines1 = L.map readGeometryLine geometryLines1
    let geoData1 = accumulateGLines gLines1
    
    putStrLn $ "File 1 layers: " ++ show (L.length . layers $ geoData1)
    
    putStrLn "Reading file 2..."
    inputString2 <- readFile inputFile2
    
    putStrLn "Processing file 2..."
    let (headerLines2, geometryLines2, footerLines2) = splitGeometryLines . splitLines $ inputString2
    let gLines2 = L.map readGeometryLine geometryLines2
    let geoData2 = accumulateGLines gLines2
    
    putStrLn $ "File 2 layers: " ++ show (L.length . layers $ geoData2)
    
    putStrLn "Combining layers with state tracking..."
    let (combinedGeoData, _) = combineLayersFrom2FilesWithState geoData1 geoData2 partStartLayer sampleWaitingLines1 initialLaserState

    let newGeometryLines = writeGeometryData combinedGeoData
    
    let newHeaderLines = combineHeaderLinesFrom2Files headerLines1 headerLines2
    let newFooterLines = combineFooterLinesFrom2Files footerLines1 footerLines2
    
    let newString = unSplitLinesCli . unSplitGeometryLines $ (newHeaderLines, newGeometryLines, newFooterLines)
    
    writeFile outputFile newString
    
    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration



{-
-- More memory-efficient version of the above function
-- Add this more memory-efficient streaming version
myCombineLayersFrom2FilesAndExecuteStreaming :: FilePath -> FilePath -> Int -> FilePath -> IO ()
myCombineLayersFrom2FilesAndExecuteStreaming inputFile1 inputFile2 partStartLayer outputFile = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime
    
    -- Process files in smaller chunks
    putStrLn "Reading file 1..."
    inputString1 <- readFile inputFile1
    let (headerLines1, geometryLines1, footerLines1) = splitGeometryLines . splitLines $ inputString1
    
    putStrLn "Reading file 2..."  
    inputString2 <- readFile inputFile2
    let (headerLines2, geometryLines2, footerLines2) = splitGeometryLines . splitLines $ inputString2
    
    putStrLn $ "File 1 geometry lines: " ++ show (L.length geometryLines1)
    putStrLn $ "File 2 geometry lines: " ++ show (L.length geometryLines2)

    -- Process layer by layer instead of loading everything
    putStrLn "Processing layers incrementally..."
    
    -- Create output header first
    let newHeaderLines = combineHeaderLinesFrom2Files headerLines1 headerLines2
    let headerString = unSplitLinesCli newHeaderLines
    
    -- Write header immediately
    writeFile outputFile headerString
    
    -- Process geometry lines in smaller batches
    let batchSize = 1000  -- Process 1000 lines at a time
    
    putStrLn "Processing geometry lines in batches..."
    processBatchesAndAppend outputFile geometryLines1 geometryLines2 partStartLayer batchSize
    
    -- Append footer
    let newFooterLines = combineFooterLinesFrom2Files footerLines1 footerLines2
    let footerString = unSplitLinesCli newFooterLines
    appendFile outputFile footerString
    
    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration
-}
{-
-- Helper function to process in batches
processBatchesAndAppend :: FilePath -> [String] -> [String] -> Int -> Int -> IO ()
processBatchesAndAppend outputFile geoLines1 geoLines2 partStartLayer batchSize = do
    let totalLines1 = length geoLines1
    let totalLines2 = length geoLines2
    
    putStrLn $ "Total lines to process: " ++ show totalLines1 ++ " and " ++ show totalLines2
    
    -- Process in batches
    processBatchesRecursively outputFile geoLines1 geoLines2 partStartLayer batchSize 0

-- Recursive helper to process batches
processBatchesRecursively :: FilePath -> [String] -> [String] -> Int -> Int -> Int -> IO ()
processBatchesRecursively outputFile geoLines1 geoLines2 partStartLayer batchSize offset = do
    let batch1 = take batchSize (drop offset geoLines1)
    let batch2 = take batchSize (drop offset geoLines2)
    
    if null batch1 || null batch2
        then putStrLn "Finished processing all batches"
        else do
            putStrLn $ "Processing batch starting at offset " ++ show offset ++ " with " ++ show (length batch1) ++ " lines"
            
            let gLines1 = L.map readGeometryLine batch1
            let gLines2 = L.map readGeometryLine batch2
            
            -- Try to accumulate this batch into layers
            let geoData1 = accumulateGLines gLines1
            let geoData2 = accumulateGLines gLines2
            
            -- Only combine if we have complete layers
            if not (null (layers geoData1)) && not (null (layers geoData2))
                then do
                    let combinedGeoData = combineLayersFrom2Files geoData1 geoData2 partStartLayer
                    let newGeometryLines = writeGeometryData combinedGeoData
                    let geoString = unSplitLinesCli newGeometryLines
                    
                    appendFile outputFile geoString
                    putStrLn $ "Batch processed successfully: " ++ show (length (layers combinedGeoData)) ++ " layers"
                else 
                    putStrLn "Batch skipped - incomplete layers"
            
            -- Process next batch
            processBatchesRecursively outputFile geoLines1 geoLines2 partStartLayer batchSize (offset + batchSize)
-}

-- Layer based batching

-- More efficient: process complete layers in batches
processBatchesAndAppendByLayers :: FilePath -> [String] -> [String] -> Int -> Int -> IO ()
processBatchesAndAppendByLayers outputFile geoLines1 geoLines2 partStartLayer layersPerBatch = do
    putStrLn "Converting all geometry lines to layers..."
    
    let gLines1 = L.map readGeometryLine geoLines1
    let gLines2 = L.map readGeometryLine geoLines2
    
    let allGeoData1 = accumulateGLines gLines1
    let allGeoData2 = accumulateGLines gLines2
    
    let allLayers1 = layers allGeoData1
    let allLayers2 = layers allGeoData2

    putStrLn $ "Total layers: " ++ show (L.length allLayers1) ++ " and " ++ show (L.length allLayers2)

    -- Process layers in batches
    processLayerBatches outputFile allLayers1 allLayers2 partStartLayer layersPerBatch 0 initialLaserState

processLayerBatches :: FilePath -> [Layer] -> [Layer] -> Int -> Int -> Int -> LaserState -> IO ()
processLayerBatches outputFile allLayers1 allLayers2 partStartLayer layersPerBatch offset prevState = do
    let batchLayers1 = L.take layersPerBatch (L.drop offset allLayers1)
    let batchLayers2 = L.take layersPerBatch (L.drop offset allLayers2)

    if L.null batchLayers1 || L.null batchLayers2
        then putStrLn "Finished processing all layer batches"
        else do
            putStrLn $ "Processing layer batch " ++ show (offset `div` layersPerBatch + 1) ++ 
                      " with " ++ show (L.length batchLayers1) ++ " layers"

            let batchGeoData1 = GeometryData batchLayers1
            let batchGeoData2 = GeometryData batchLayers2
            
            -- Use prevState as the initial state for this batch
            let (combinedBatch, finalState) = combineLayersFrom2FilesWithState batchGeoData1 batchGeoData2 partStartLayer sampleWaitingLines1 prevState
            let batchGeometryLines = writeGeometryData combinedBatch
            let batchString = unSplitLinesCli batchGeometryLines
            
            appendFile outputFile batchString
            
            -- Write finalState to file for next batch
            writeFile ".last_laser_state" (serializeLaserState finalState)
            
            -- Process next batch with the final state
            processLayerBatches outputFile allLayers1 allLayers2 partStartLayer layersPerBatch (offset + layersPerBatch) finalState


-- Updated streaming version using proper batching
myCombineLayersFrom2FilesAndExecuteStreaming :: FilePath -> FilePath -> Int -> FilePath -> IO ()
myCombineLayersFrom2FilesAndExecuteStreaming inputFile1 inputFile2 partStartLayer outputFile = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime
    
    putStrLn "Reading files..."
    inputString1 <- readFile inputFile1
    inputString2 <- readFile inputFile2
    
    let (headerLines1, geometryLines1, footerLines1) = splitGeometryLines . splitLines $ inputString1
    let (headerLines2, geometryLines2, footerLines2) = splitGeometryLines . splitLines $ inputString2

    putStrLn $ "File 1 geometry lines: " ++ show (L.length geometryLines1)
    putStrLn $ "File 2 geometry lines: " ++ show (L.length geometryLines2)

    -- Write header first
    let newHeaderLines = combineHeaderLinesFrom2Files headerLines1 headerLines2
    let headerString = unSplitLinesCli newHeaderLines
    writeFile outputFile headerString
    
    -- Process in batches of 10 layers at a time
    putStrLn "Processing layers in batches..."
    processBatchesAndAppendByLayers outputFile geometryLines1 geometryLines2 partStartLayer 10
    
    -- Append footer
    let newFooterLines = combineFooterLinesFrom2Files footerLines1 footerLines2
    let footerString = unSplitLinesCli newFooterLines
    appendFile outputFile footerString
    
    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration



------------------------------------------------------
-------------------------------------------------------
-------------------------------------------------------

-- accumulate state from the beginning up to and including that layer.
getStateUpToLayer :: [[GeometryLine]] -> Int -> LaserState
getStateUpToLayer allLayerLines idx =
    let linesUpTo = L.concat $ L.take (idx + 1) allLayerLines
    in execState (mapM_ updateLaserState linesUpTo) initialLaserState

-- In your combineLayersFrom2FilesWithState:
combineLayersFrom2FilesWithState :: GeometryData -> GeometryData -> Int -> [GeometryLine] -> LaserState -> (GeometryData, LaserState)
combineLayersFrom2FilesWithState geoData1 geoData2 partStartLayer additionalLines initialState =
    let
        allLayers1 = layers geoData1
        allLayers2 = layers geoData2

        initialLayers1 = L.take (partStartLayer - 1) allLayers1
        layers1ToCombine = L.drop (partStartLayer - 1) allLayers1
        layers2ToCombine = L.drop (partStartLayer - 1) allLayers2

        allLayer2Lines = L.map layerLines allLayers2

        -- Helper: get state as of the end of the previous layer (or initial if idx==0)
        getPrevState :: [[GeometryLine]] -> Int -> LaserState
        getPrevState allLayerLines idx =
            if idx + partStartLayer - 1 == 0 then initialState
            else getStateUpToLayerWithInitial allLayerLines (idx + partStartLayer - 2) initialState
        
        -- Like getStateUpToLayer but starts from initialState
        getStateUpToLayerWithInitial :: [[GeometryLine]] -> Int -> LaserState -> LaserState
        getStateUpToLayerWithInitial allLayerLines idx st =
            let linesUpTo = L.concat $ L.take (idx + 1) allLayerLines
            in execState (mapM_ updateLaserState linesUpTo) st

        combineLayerWithState :: ([Layer], Int, LaserState) -> Layer -> ([Layer], Int, LaserState)
        combineLayerWithState (accLayers, idx, curState) layer1 =
            let layer2 = layers2ToCombine !! idx
                layer2LinesClean = dropInitialLayerChange (layerLines layer2)
                prevState = getPrevState allLayer2Lines idx
                stateRestorationLines = getStateRestorationLinesWithDefaults prevState
                combinedLayerLines = layerLines layer1 ++ additionalLines ++ stateRestorationLines ++ layer2LinesClean
                combinedLayer = Layer (layerNumber layer1) combinedLayerLines
                -- Update curState to the state at the end of this layer
                newState = execState (mapM_ updateLaserState (layerLines layer2)) prevState
            in (accLayers ++ [combinedLayer], idx + 1, newState)

        (combinedLayers, _, finalState) = L.foldl' combineLayerWithState ([], 0, initialState) layers1ToCombine
        finalLayers = initialLayers1 ++ combinedLayers
    in (GeometryData finalLayers, finalState)

-- Test the state preservation across batches
testStatePersistence :: IO ()
testStatePersistence = do
    putStrLn "=== Testing State Persistence Across Batches ==="
    
    -- Create a state with specific values
    let testState = LaserState (Just (Power 250)) (Just (Speed 1600)) (Just (Focus 4))
    
    -- Serialize and deserialize
    let serialized = serializeLaserState testState
    putStrLn $ "Serialized state: " ++ serialized
    
    let deserialized = deserializeLaserState serialized
    putStrLn $ "Deserialized state: " ++ show deserialized
    
    -- Test restoration lines
    let restorationLines = getStateRestorationLinesWithDefaults deserialized
    putStrLn "Restoration lines:"
    mapM_ (putStrLn . ("  " ++) . show) restorationLines

--------------------------------------------------------
-----------------------------------------------------
--------------------------------------------------------
-- myCombineLayersFrom2FilesAndExecuteStreaming is not working with the big files,
-- looks like no combinng is happening, but the mini files work.
-- seems to work with the mini files.

-- NEW MEMORY-EFFICIENT STREAMING IMPLEMENTATION
-- Processes layers incrementally without loading all into memory

-- Helper: Extract layer lines from geometry lines for a specific layer number
extractLayerLines :: [String] -> Int -> [String]
extractLayerLines geoLines layerNum = 
    let layerStartPattern = "$$LAYER/"
        isLayerStart line = layerStartPattern `L.isPrefixOf` line
        
        -- Find all layer start positions
        layerPositions = [(i, line) | (i, line) <- L.zip [0..] geoLines, isLayerStart line]
        
        -- Find the start and end positions for our target layer
        targetLayer = if layerNum < L.length layerPositions
                     then Just (layerPositions !! layerNum)
                     else Nothing
        nextLayer = if layerNum + 1 < L.length layerPositions  
                   then Just (layerPositions !! (layerNum + 1))
                   else Nothing
    in case targetLayer of
        Nothing -> []
        Just (startPos, _) -> 
            let endPos = case nextLayer of
                    Nothing -> L.length geoLines
                    Just (nextPos, _) -> nextPos
            in L.take (endPos - startPos) (L.drop startPos geoLines)

-- Helper: Parse a batch of layer lines into GeometryData
parseBatchLayers :: [String] -> [String] -> Int -> Int -> (GeometryData, GeometryData)
parseBatchLayers geoLines1 geoLines2 startLayer batchSize = 
    let layerLines1 = L.concatMap (extractLayerLines geoLines1) [startLayer .. startLayer + batchSize - 1]
        layerLines2 = L.concatMap (extractLayerLines geoLines2) [startLayer .. startLayer + batchSize - 1]
        
        gLines1 = L.map readGeometryLine layerLines1
        gLines2 = L.map readGeometryLine layerLines2
        
        geoData1 = accumulateGLines gLines1
        geoData2 = accumulateGLines gLines2
    in (geoData1, geoData2)

-- Helper: Write initial layers (before partStartLayer) to file
writeInitialLayers :: FilePath -> [String] -> Int -> IO ()
writeInitialLayers outputFile geoLines1 partStartLayer = do
    let initialLayerLines = L.concatMap (extractLayerLines geoLines1) [0 .. partStartLayer - 2]
    if not (L.null initialLayerLines) then do
        let initialGLines = L.map readGeometryLine initialLayerLines
        let initialGeoData = accumulateGLines initialGLines
        let initialGeometryLines = writeGeometryData initialGeoData
        let initialString = unSplitLinesCli initialGeometryLines
        appendFile outputFile initialString
        putStrLn $ "Written initial " ++ show (partStartLayer - 1) ++ " layers"
    else
        putStrLn "No initial layers to write"

-- Helper: Get total number of layers from geometry lines
getTotalLayers :: [String] -> Int
getTotalLayers geoLines = 
    let layerStartPattern = "$$LAYER/"
        isLayerStart line = layerStartPattern `L.isPrefixOf` line
    in L.length $ L.filter isLayerStart geoLines

-- Main memory-efficient streaming function
myCombineLayersFrom2FilesStreamingV2 :: FilePath -> FilePath -> Int -> FilePath -> IO ()
myCombineLayersFrom2FilesStreamingV2 inputFile1 inputFile2 partStartLayer outputFile = do
    startTime <- getCurrentTime
    putStrLn $ "Start time: " ++ show startTime
    
    putStrLn "Reading files..."
    inputString1 <- readFile inputFile1
    inputString2 <- readFile inputFile2
    
    let (headerLines1, geometryLines1, footerLines1) = splitGeometryLines . splitLines $ inputString1
    let (headerLines2, geometryLines2, footerLines2) = splitGeometryLines . splitLines $ inputString2

    putStrLn $ "File 1 geometry lines: " ++ show (L.length geometryLines1)
    putStrLn $ "File 2 geometry lines: " ++ show (L.length geometryLines2)

    -- Step 1: Write header
    let newHeaderLines = combineHeaderLinesFrom2Files headerLines1 headerLines2
    let headerString = unSplitLinesCli newHeaderLines
    writeFile outputFile headerString
    putStrLn "Header written"
    
    -- Step 2: Write initial layers (before partStartLayer)
    writeInitialLayers outputFile geometryLines1 partStartLayer
    
    -- Step 2.5: Calculate initial state from the initial layers of file 2
    let initialLayersLines2 = L.concatMap (extractLayerLines geometryLines2) [0 .. partStartLayer - 2]
    let initialGLines2 = L.map readGeometryLine initialLayersLines2
    let accumulatedInitialState = L.foldl' (\laserState gLine -> execState (updateLaserState gLine) laserState) initialLaserState initialGLines2
    
    putStrLn $ "Initial state accumulated from first " ++ show (partStartLayer - 1) ++ " layers of file 2: " ++ show accumulatedInitialState
    
    -- Step 3: Process layers in batches starting from partStartLayer
    let totalLayers1 = getTotalLayers geometryLines1
    let totalLayers2 = getTotalLayers geometryLines2
    let layersToProcess = min totalLayers1 totalLayers2 - (partStartLayer - 1)
    
    putStrLn $ "Total layers: " ++ show totalLayers1 ++ " and " ++ show totalLayers2
    putStrLn $ "Processing " ++ show layersToProcess ++ " layers from layer " ++ show partStartLayer
    
    -- Process in batches of 5 layers (smaller batches for better memory management)
    let batchSize = 5
    processLayerBatchesV2 outputFile geometryLines1 geometryLines2 partStartLayer layersToProcess batchSize 0 accumulatedInitialState
    
    -- Step 4: Write footer
    let newFooterLines = combineFooterLinesFrom2Files footerLines1 footerLines2
    let footerString = unSplitLinesCli newFooterLines
    appendFile outputFile footerString
    putStrLn "Footer written"
    
    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration

-- Main memory-efficient streaming function
myCombineLayersFrom2FilesStreamingV2withoutWaitingLines :: FilePath -> FilePath -> Int -> FilePath -> IO ()
myCombineLayersFrom2FilesStreamingV2withoutWaitingLines inputFile1 inputFile2 partStartLayer outputFile = do
    startTime <- getCurrentTime
    putStrLn $ "Input file 1: " ++ inputFile1
    putStrLn $ "Input file 2: " ++ inputFile2
    putStrLn $ "Output file: " ++ outputFile
    putStrLn $ "Start tprocessLayerBatchesV2withoutWaitingLinesime: " ++ show startTime
    
    putStrLn "Reading files..."
    inputString1 <- readFile inputFile1
    inputString2 <- readFile inputFile2
    
    let (headerLines1, geometryLines1, footerLines1) = splitGeometryLines . splitLines $ inputString1
    let (headerLines2, geometryLines2, footerLines2) = splitGeometryLines . splitLines $ inputString2

    putStrLn $ "File 1 geometry lines: " ++ show (L.length geometryLines1)
    putStrLn $ "File 2 geometry lines: " ++ show (L.length geometryLines2)

    -- Step 1: Write header processLayerBatchesV2withoutWaitingLines
    let newHeaderLines = combineHeaderLinesFrom2Files headerLines1 headerLines2
    let headerString = unSplitLinesCli newHeaderLines
    writeFile outputFile headerString
    putStrLn "Header written"
    
    -- Step 2: Write initial layers (before partStartLayer)
    writeInitialLayers outputFile geometryLines1 partStartLayer
    
    -- Step 2.5: Calculate initial state from the initial layers of file 2
    let initialLayersLines2 = L.concatMap (extractLayerLines geometryLines2) [0 .. partStartLayer - 2]
    let initialGLines2 = L.map readGeometryLine initialLayersLines2
    let accumulatedInitialState = L.foldl' (\laserState gLine -> execState (updateLaserState gLine) laserState) initialLaserState initialGLines2
    
    putStrLn $ "Initial state accumulated from first " ++ show (partStartLayer - 1) ++ " layers of file 2: " ++ show accumulatedInitialState
    
    -- Step 3: Process layers in batches starting from partStartLayer
    let totalLayers1 = getTotalLayers geometryLines1
    let totalLayers2 = getTotalLayers geometryLines2
    let layersToProcess = min totalLayers1 totalLayers2 - (partStartLayer - 1)
    
    putStrLn $ "Total layers: " ++ show totalLayers1 ++ " and " ++ show totalLayers2
    putStrLn $ "Processing " ++ show layersToProcess ++ " layers from layer " ++ show partStartLayer
    
    -- Process in batches of 5 layers (smaller batches for better memory management)
    --let batchSize = 5
    -- Running the executable was killed when combining files for tensile test specimen which were larger
    --let batchSize = 2
    -- When running the executable with the tensile test specimen, when using a batch size of 2 the process was killed much earlier than when using the batch size of 5, 
    -- so we increased the batch size to 10 and it worked
    let batchSize = 10

    processLayerBatchesV2withoutWaitingLines outputFile geometryLines1 geometryLines2 partStartLayer layersToProcess batchSize 0 accumulatedInitialState
    
    -- Step 4: Write footer
    let newFooterLines = combineFooterLinesFrom2Files footerLines1 footerLines2
    let footerString = unSplitLinesCli newFooterLines
    appendFile outputFile footerString
    putStrLn "Footer written"
    
    endTime <- getCurrentTime
    putStrLn $ "End time: " ++ show endTime
    
    let duration = diffUTCTime endTime startTime
    putStrLn $ "Duration: " ++ show duration




-- Process layers in batches with memory management
processLayerBatchesV2 :: FilePath -> [String] -> [String] -> Int -> Int -> Int -> Int -> LaserState -> IO ()
processLayerBatchesV2 outputFile geoLines1 geoLines2 partStartLayer totalLayersToProcess batchSize processed prevState = do
    if processed >= totalLayersToProcess
        then putStrLn "Finished processing all layer batches"
        else do
            let currentBatchSize = min batchSize (totalLayersToProcess - processed)
            let currentStartLayer = partStartLayer - 1 + processed  -- Convert to 0-based indexing
            
            putStrLn $ "Processing batch " ++ show (processed `div` batchSize + 1) ++ 
                      " (layers " ++ show (currentStartLayer + 1) ++ " to " ++ 
                      show (currentStartLayer + currentBatchSize) ++ ")"
            
            -- Step 3: Parse only the current batch of layers
            let (batchGeoData1, batchGeoData2) = parseBatchLayers geoLines1 geoLines2 currentStartLayer currentBatchSize
            
            putStrLn $ "Parsed batch with " ++ show (L.length (layers batchGeoData1)) ++ " and " ++ 
                      show (L.length (layers batchGeoData2)) ++ " layers"
            
            -- Read state from previous batch if available
            currentState <- if processed == 0 
                           then return prevState
                           else do
                               stateExists <- fileExists ".last_laser_state"
                               if stateExists
                               then do
                                   stateContent <- readFile ".last_laser_state"
                                   let restoredState = deserializeLaserState stateContent
                                   putStrLn $ "Restored state from file: " ++ show restoredState
                                   return restoredState
                               else return prevState
            
            -- Step 4: Combine and process this batch with proper state
            let (combinedBatch, finalState) = combineLayersFrom2FilesWithState batchGeoData1 batchGeoData2 1 sampleWaitingLines1 currentState
            
            -- Step 5: Write to output file and clear from memory
            let batchGeometryLines = writeGeometryData combinedBatch
            let batchString = unSplitLinesCli batchGeometryLines
            appendFile outputFile batchString
            
            putStrLn $ "Written batch " ++ show (processed `div` batchSize + 1) ++ " to file"
            putStrLn $ "Final state from batch: " ++ show finalState
            
            -- Write state for next batch
            writeFile ".last_laser_state" (serializeLaserState finalState)
            putStrLn $ "Saved state to file: " ++ serializeLaserState finalState
            
            -- Process next batch (tail recursion for memory efficiency)
            processLayerBatchesV2 outputFile geoLines1 geoLines2 partStartLayer totalLayersToProcess batchSize (processed + currentBatchSize) finalState


-- Process layers in batches with memory management

processLayerBatchesV2withoutWaitingLines :: FilePath -> [String] -> [String] -> Int -> Int -> Int -> Int -> LaserState -> IO ()
processLayerBatchesV2withoutWaitingLines outputFile geoLines1 geoLines2 partStartLayer totalLayersToProcess batchSize processed prevState = do
    if processed >= totalLayersToProcess
        then putStrLn "Finished processing all layer batches"
        else do
            let currentBatchSize = min batchSize (totalLayersToProcess - processed)
            let currentStartLayer = partStartLayer - 1 + processed  -- Convert to 0-based indexing
            
            putStrLn $ "Processing batch " ++ show (processed `div` batchSize + 1) ++ 
                      " (layers " ++ show (currentStartLayer + 1) ++ " to " ++ 
                      show (currentStartLayer + currentBatchSize) ++ ")"
            
            -- Step 3: Parse only the current batch of layers
            let (batchGeoData1, batchGeoData2) = parseBatchLayers geoLines1 geoLines2 currentStartLayer currentBatchSize
            
            putStrLn $ "Parsed batch with " ++ show (L.length (layers batchGeoData1)) ++ " and " ++ 
                      show (L.length (layers batchGeoData2)) ++ " layers"
            
            -- Read state from previous batch if available
            currentState <- if processed == 0 
                           then return prevState
                           else do
                               stateExists <- fileExists ".last_laser_state"
                               if stateExists
                               then do
                                   stateContent <- readFile ".last_laser_state"
                                   let restoredState = deserializeLaserState stateContent
                                   putStrLn $ "Restored state from file: " ++ show restoredState
                                   return restoredState
                               else return prevState
            
            -- Step 4: Combine and process this batch with proper state
            let (combinedBatch, finalState) = combineLayersFrom2FilesWithState batchGeoData1 batchGeoData2 1 [] currentState
            
            -- Step 5: Write to output file and clear from memory
            let batchGeometryLines = writeGeometryData combinedBatch
            let batchString = unSplitLinesCli batchGeometryLines
            appendFile outputFile batchString
            
            putStrLn $ "Written batch " ++ show (processed `div` batchSize + 1) ++ " to file"
            putStrLn $ "Final state from batch: " ++ show finalState
            
            -- Write state for next batch
            writeFile ".last_laser_state" (serializeLaserState finalState)
            putStrLn $ "Saved state to file: " ++ serializeLaserState finalState
            
            -- Process next batch (tail recursion for memory efficiency)
            processLayerBatchesV2withoutWaitingLines outputFile geoLines1 geoLines2 partStartLayer totalLayersToProcess batchSize (processed + currentBatchSize) finalState




-- Test function for the new streaming version
testStreamingV2 :: IO ()
testStreamingV2 = do
    putStrLn "Testing new memory-efficient streaming implementation..."
    myCombineLayersFrom2FilesStreamingV2 myInputFile1Big myInputFile2Big 12 myOutputFileBig

-- Test function to see file usage before and after
testMemoryUsage :: IO ()
testMemoryUsage = do
    putStrLn "Testing memory usage with the new implementation..."
    putStrLn "This will process the big files with reduced memory footprint"
    testStreamingV2