module Rotation where

import CliParse as CP
import Data.List as L

-- Rotation angle in radians
type Angle = Double

-- Convert degrees to radians
degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * pi / 180

-- Round a coordinate to 3 decimal places
roundTo3Decimals :: Double -> Double
roundTo3Decimals x = fromIntegral (round (x * 1000)) / 1000

-- Round a 2D point to 3 decimal places
roundPoint :: (Double, Double) -> (Double, Double)
roundPoint (x, y) = (roundTo3Decimals x, roundTo3Decimals y)

-- Rotate a 2D point by a given angle around the origin
rotatePoint :: Angle -> (Double, Double) -> (Double, Double)
rotatePoint angle (x, y) = roundPoint (x', y')
  where
    cosTheta = cos angle
    sinTheta = sin angle
    x' = x * cosTheta - y * sinTheta
    y' = x * sinTheta + y * cosTheta

-- Rotate a 2D point by a given angle around a center point
rotatePointAroundCenter :: Angle -> (Double, Double) -> (Double, Double) -> (Double, Double)
rotatePointAroundCenter angle (cx, cy) (x, y) = roundPoint (x'' + cx, y'' + cy)
  where
    -- Translate to origin, rotate, then translate back
    (x', y') = (x - cx, y - cy)
    cosTheta = cos angle
    sinTheta = sin angle
    x'' = x' * cosTheta - y' * sinTheta
    y'' = x' * sinTheta + y' * cosTheta

-- Calculate bounding box of a list of points
boundingBox :: [(Double, Double)] -> ((Double, Double), (Double, Double))
boundingBox [] = ((0, 0), (0, 0))
boundingBox points = ((minX, minY), (maxX, maxY))
  where
    xs = map fst points
    ys = map snd points
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys

-- Calculate center point of a bounding box
centerOfBoundingBox :: ((Double, Double), (Double, Double)) -> (Double, Double)
centerOfBoundingBox ((minX, minY), (maxX, maxY)) = 
    ((minX + maxX) / 2, (minY + maxY) / 2)

-- Rotate all points in a polyline around a specified center point
rotatePolylinePointsAroundCenter :: Angle -> (Double, Double) -> Polyline -> Polyline
rotatePolylinePointsAroundCenter angle center (Polyline idPL dirPL numPtsPL pointsPL) = 
    Polyline idPL dirPL numPtsPL rotatedPoints
  where
    rotatedPoints = map (rotatePointAroundCenter angle center) pointsPL

-- Rotate all points in hatches around a specified center point
rotateHatchesPointsAroundCenter :: Angle -> (Double, Double) -> Hatches -> Hatches
rotateHatchesPointsAroundCenter angle center (Hatches idHat numHat pointsHat) = 
    Hatches idHat numHat rotatedPoints
  where
    rotatedPoints = map (rotatePointAroundCenter angle center) pointsHat

-- Legacy functions that rotate around the bounding box center (for backward compatibility)
rotatePolylinePoints :: Angle -> Polyline -> Polyline
rotatePolylinePoints angle polyline@(Polyline _ _ _ pointsPL) = 
    rotatePolylinePointsAroundCenter angle center polyline
  where
    bbox = boundingBox pointsPL
    center = centerOfBoundingBox bbox

rotateHatchesPoints :: Angle -> Hatches -> Hatches
rotateHatchesPoints angle hatches@(Hatches _ _ pointsHat) = 
    rotateHatchesPointsAroundCenter angle center hatches
  where
    bbox = boundingBox pointsHat
    center = centerOfBoundingBox bbox

-- Flip coordinates by multiplying by -1
data FlipAxis = FlipX | FlipY deriving (Show, Eq)

-- Flip a 2D point along the specified axis
flipPoint :: FlipAxis -> (Double, Double) -> (Double, Double)
flipPoint FlipX (x, y) = roundPoint (-x, y)
flipPoint FlipY (x, y) = roundPoint (x, -y)

-- Flip all points in a polyline along the specified axis
flipPolylinePoints :: FlipAxis -> Polyline -> Polyline
flipPolylinePoints axis (Polyline idPL dirPL numPtsPL pointsPL) = 
    Polyline idPL dirPL numPtsPL flippedPoints
  where
    flippedPoints = map (flipPoint axis) pointsPL

-- Flip all points in hatches along the specified axis
flipHatchesPoints :: FlipAxis -> Hatches -> Hatches
flipHatchesPoints axis (Hatches idHat numHat pointsHat) = 
    Hatches idHat numHat flippedPoints
  where
    flippedPoints = map (flipPoint axis) pointsHat

-- Flip a geometry line if it's a polyline or hatches
flipGeometryLine :: FlipAxis -> GeometryLine -> GeometryLine
flipGeometryLine axis (GLPolyline polyline) = GLPolyline (flipPolylinePoints axis polyline)
flipGeometryLine axis (GLHatches hatches) = GLHatches (flipHatchesPoints axis hatches)
flipGeometryLine _ otherLine = otherLine  -- Keep power, speed, focus, layer changes unchanged

-- Flip all geometry lines in a layer
flipLayerLines :: FlipAxis -> Layer -> Layer
flipLayerLines axis (Layer layerNum lines) = 
    Layer layerNum (map (flipGeometryLine axis) lines)

-- Flip all layers in geometry data
flipGeometryData :: FlipAxis -> GeometryData -> GeometryData
flipGeometryData axis (GeometryData layers) = 
    GeometryData (map (flipLayerLines axis) layers)

-- Rotate a geometry line if it's a polyline or hatches (around user-specified center)
rotateGeometryLineAroundCenter :: Angle -> (Double, Double) -> GeometryLine -> GeometryLine
rotateGeometryLineAroundCenter angle center (GLPolyline polyline) = GLPolyline (rotatePolylinePointsAroundCenter angle center polyline)
rotateGeometryLineAroundCenter angle center (GLHatches hatches) = GLHatches (rotateHatchesPointsAroundCenter angle center hatches)
rotateGeometryLineAroundCenter _ _ otherLine = otherLine  -- Keep power, speed, focus, layer changes unchanged

-- Rotate a geometry line if it's a polyline or hatches (around their own centers - legacy)
rotateGeometryLine :: Angle -> GeometryLine -> GeometryLine
rotateGeometryLine angle (GLPolyline polyline) = GLPolyline (rotatePolylinePoints angle polyline)
rotateGeometryLine angle (GLHatches hatches) = GLHatches (rotateHatchesPoints angle hatches)
rotateGeometryLine _ otherLine = otherLine  -- Keep power, speed, focus, layer changes unchanged

-- Rotate all geometry lines in a layer (around user-specified center)
rotateLayerLinesAroundCenter :: Angle -> (Double, Double) -> Layer -> Layer
rotateLayerLinesAroundCenter angle center (Layer layerNum lines) = 
    Layer layerNum (map (rotateGeometryLineAroundCenter angle center) lines)

-- Rotate all geometry lines in a layer (around their own centers - legacy)
rotateLayerLines :: Angle -> Layer -> Layer
rotateLayerLines angle (Layer layerNum lines) = 
    Layer layerNum (map (rotateGeometryLine angle) lines)

-- Rotate all layers in geometry data (around user-specified center)
rotateGeometryDataAroundCenter :: Angle -> (Double, Double) -> GeometryData -> GeometryData
rotateGeometryDataAroundCenter angle center (GeometryData layers) = 
    GeometryData (map (rotateLayerLinesAroundCenter angle center) layers)

-- Rotate all layers in geometry data (around their own centers - legacy)
rotateGeometryData :: Angle -> GeometryData -> GeometryData
rotateGeometryData angle (GeometryData layers) = 
    GeometryData (map (rotateLayerLines angle) layers)

-- Main function to rotate CLI file contents around a specified center point
rotateCLIFileAroundCenter :: Double -> (Double, Double) -> FilePath -> FilePath -> IO ()
rotateCLIFileAroundCenter angleDegrees center inputFile outputFile = do
    putStrLn $ "Rotating scan patterns by " ++ show angleDegrees ++ " degrees around point " ++ show center
    putStrLn $ "Input file: " ++ inputFile
    putStrLn $ "Output file: " ++ outputFile
    
    -- Read input file
    inputString <- readFile inputFile
    let (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
    
    putStrLn $ "Read " ++ show (L.length geometryLines) ++ " geometry lines"
    
    -- Parse geometry lines into data structures
    let gLines = L.map readGeometryLine geometryLines
    let geoData = accumulateGLines gLines
    
    putStrLn $ "Parsed " ++ show (L.length (layers geoData)) ++ " layers"
    
    -- Rotate the geometry data around the specified center
    let angleRadians = degreesToRadians angleDegrees
    let rotatedGeoData = rotateGeometryDataAroundCenter angleRadians center geoData
    
    -- Convert back to geometry lines
    let rotatedGeometryLines = writeGeometryData rotatedGeoData
    
    -- Combine with header and footer
    let newString = unSplitLinesCli . unSplitGeometryLines $ (headerLines, rotatedGeometryLines, footerLines)
    
    -- Write output file
    writeFile outputFile newString
    
    putStrLn $ "Successfully rotated around " ++ show center ++ " and saved to " ++ outputFile

-- Main function to rotate CLI file contents (legacy - rotates around bounding box centers)
rotateCLIFile :: Double -> FilePath -> FilePath -> IO ()
rotateCLIFile angleDegrees inputFile outputFile = do
    putStrLn $ "Rotating scan patterns by " ++ show angleDegrees ++ " degrees (around individual bounding box centers)"
    putStrLn $ "Input file: " ++ inputFile
    putStrLn $ "Output file: " ++ outputFile
    
    -- Read input file
    inputString <- readFile inputFile
    let (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
    
    putStrLn $ "Read " ++ show (L.length geometryLines) ++ " geometry lines"
    
    -- Parse geometry lines into data structures
    let gLines = L.map readGeometryLine geometryLines
    let geoData = accumulateGLines gLines
    
    putStrLn $ "Parsed " ++ show (L.length (layers geoData)) ++ " layers"
    
    -- Rotate the geometry data
    let angleRadians = degreesToRadians angleDegrees
    let rotatedGeoData = rotateGeometryData angleRadians geoData
    
    -- Convert back to geometry lines
    let rotatedGeometryLines = writeGeometryData rotatedGeoData
    
    -- Combine with header and footer
    let newString = unSplitLinesCli . unSplitGeometryLines $ (headerLines, rotatedGeometryLines, footerLines)
    
    -- Write output file
    writeFile outputFile newString
    
    putStrLn $ "Successfully rotated and saved to " ++ outputFile

-- Main function to flip CLI file coordinates
flipCLIFile :: FlipAxis -> FilePath -> FilePath -> IO ()
flipCLIFile axis inputFile outputFile = do
    let axisName = case axis of
            FlipX -> "X"
            FlipY -> "Y"
    putStrLn $ "Flipping " ++ axisName ++ " coordinates"
    putStrLn $ "Input file: " ++ inputFile
    putStrLn $ "Output file: " ++ outputFile
    
    -- Read input file
    inputString <- readFile inputFile
    let (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
    
    putStrLn $ "Read " ++ show (L.length geometryLines) ++ " geometry lines"
    
    -- Parse geometry lines into data structures
    let gLines = L.map readGeometryLine geometryLines
    let geoData = accumulateGLines gLines
    
    putStrLn $ "Parsed " ++ show (L.length (layers geoData)) ++ " layers"
    
    -- Flip the geometry data
    let flippedGeoData = flipGeometryData axis geoData
    
    -- Convert back to geometry lines
    let flippedGeometryLines = writeGeometryData flippedGeoData
    
    -- Combine with header and footer
    let newString = unSplitLinesCli . unSplitGeometryLines $ (headerLines, flippedGeometryLines, footerLines)
    
    -- Write output file
    writeFile outputFile newString
    
    putStrLn $ "Successfully flipped " ++ axisName ++ " coordinates and saved to " ++ outputFile

-- Convenience functions for flipping specific axes
flipXCoordinates :: FilePath -> FilePath -> IO ()
flipXCoordinates = flipCLIFile FlipX

flipYCoordinates :: FilePath -> FilePath -> IO ()
flipYCoordinates = flipCLIFile FlipY

-- Combined rotation and flipping function (around specified center)
rotateAndFlipCLIFileAroundCenter :: Double -> (Double, Double) -> FlipAxis -> FilePath -> FilePath -> IO ()
rotateAndFlipCLIFileAroundCenter angleDegrees center axis inputFile outputFile = do
    let tempFile = outputFile ++ "_temp.cli"
    
    putStrLn $ "Step 1: Rotating by " ++ show angleDegrees ++ " degrees around " ++ show center
    rotateCLIFileAroundCenter angleDegrees center inputFile tempFile
    
    let axisName = case axis of
            FlipX -> "X"
            FlipY -> "Y"
    putStrLn $ "Step 2: Flipping " ++ axisName ++ " coordinates"
    flipCLIFile axis tempFile outputFile
    
    -- Clean up temp file (optional - you might want to keep it for debugging)
    -- removeFile tempFile
    putStrLn $ "Combined transformation complete. Temp file: " ++ tempFile

-- Combined rotation and flipping function (legacy - around bounding box centers)
rotateAndFlipCLIFile :: Double -> FlipAxis -> FilePath -> FilePath -> IO ()
rotateAndFlipCLIFile angleDegrees axis inputFile outputFile = do
    let tempFile = outputFile ++ "_temp.cli"
    
    putStrLn $ "Step 1: Rotating by " ++ show angleDegrees ++ " degrees"
    rotateCLIFile angleDegrees inputFile tempFile
    
    let axisName = case axis of
            FlipX -> "X"
            FlipY -> "Y"
    putStrLn $ "Step 2: Flipping " ++ axisName ++ " coordinates"
    flipCLIFile axis tempFile outputFile
    
    -- Clean up temp file (optional - you might want to keep it for debugging)
    -- removeFile tempFile
    putStrLn $ "Combined transformation complete. Temp file: " ++ tempFile

-- Convenience functions for common rotation centers

-- Rotate around origin (0, 0)
rotateCLIFileAroundOrigin :: Double -> FilePath -> FilePath -> IO ()
rotateCLIFileAroundOrigin angleDegrees = rotateCLIFileAroundCenter angleDegrees (0.0, 0.0)

-- Rotate by 90 degrees around origin for perpendicular remelting strategy
rotate90DegreesAroundOrigin :: FilePath -> FilePath -> IO ()
rotate90DegreesAroundOrigin = rotateCLIFileAroundOrigin 90.0

-- Rotate by 90 degrees for perpendicular remelting strategy (legacy)
rotate90Degrees :: FilePath -> FilePath -> IO ()
rotate90Degrees = rotateCLIFile 90.0

-- Rotate by 45 degrees around origin
rotate45DegreesAroundOrigin :: FilePath -> FilePath -> IO ()
rotate45DegreesAroundOrigin = rotateCLIFileAroundOrigin 45.0

-- Rotate by 45 degrees (legacy) 
rotate45DegreesLegacy :: FilePath -> FilePath -> IO ()
rotate45DegreesLegacy = rotateCLIFile 45.0

testInputFileMini :: FilePath
testInputFileMini = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/Rotation/mini.cli"

testOutputFileMini :: FilePath
testOutputFileMini = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/Rotation/mini_rotated_90.cli"

testInputFileBig1 :: FilePath
testInputFileBig1 = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/Rotation/2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650.cli"

testOutputFileBig1 :: FilePath
testOutputFileBig1 = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/Remelting by combining layer lines from 2 files/Rotation/2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650_rotated_and_flipped_90.cli"

-- Rotate by 45 degrees 
rotate45Degrees :: FilePath -> FilePath -> IO ()
rotate45Degrees = rotateCLIFile 45.0

-- Test function with mini files (around origin)
testRotationAroundOrigin :: FilePath -> FilePath -> IO ()
testRotationAroundOrigin inputFile outputFile = do
    putStrLn "Testing rotation around origin (0,0) with mini file..."
    rotate90DegreesAroundOrigin inputFile outputFile

-- Test function with mini files (legacy - around bounding box centers)
testRotation :: FilePath -> FilePath -> IO ()
testRotation inputFile outputFile = do
    putStrLn "Testing rotation around individual bounding box centers..."
    rotate90Degrees inputFile outputFile

-- Test flipping functionality
testFlipping :: FilePath -> IO ()
testFlipping inputFile = do
    putStrLn "Testing coordinate flipping..."
    
    let baseOutputPath = inputFile ++ "_flipped"
    let xFlippedFile = baseOutputPath ++ "_X.cli"
    let yFlippedFile = baseOutputPath ++ "_Y.cli"
    
    putStrLn "Testing X-coordinate flipping..."
    flipXCoordinates inputFile xFlippedFile
    
    putStrLn "Testing Y-coordinate flipping..."
    flipYCoordinates inputFile yFlippedFile
    
    putStrLn $ "Created flipped files:"
    putStrLn $ "  X-flipped: " ++ xFlippedFile
    putStrLn $ "  Y-flipped: " ++ yFlippedFile

-- Comprehensive test function
testAllTransformations :: FilePath -> IO ()
testAllTransformations inputFile = do
    putStrLn "=== Testing All Coordinate Transformations ==="
    putStrLn $ "Input file: " ++ inputFile
    
    let baseName = inputFile ++ "_test"
    
    -- Test rotation around origin
    putStrLn "\n1. Testing 90-degree rotation around origin (0,0)..."
    let rotated90OriginFile = baseName ++ "_rotated_90_origin.cli"
    rotate90DegreesAroundOrigin inputFile rotated90OriginFile
    
    -- Test rotation around bounding box centers (legacy)
    putStrLn "\n2. Testing 90-degree rotation around bounding box centers..."
    let rotated90File = baseName ++ "_rotated_90_bbox.cli"
    rotate90Degrees inputFile rotated90File
    
    -- Test 45-degree rotation around origin
    putStrLn "\n3. Testing 45-degree rotation around origin..."
    let rotated45OriginFile = baseName ++ "_rotated_45_origin.cli"
    rotate45DegreesAroundOrigin inputFile rotated45OriginFile
    
    -- Test X flipping
    putStrLn "\n4. Testing X-coordinate flipping..."
    let xFlippedFile = baseName ++ "_flipped_X.cli"
    flipXCoordinates inputFile xFlippedFile
    
    -- Test Y flipping
    putStrLn "\n5. Testing Y-coordinate flipping..."
    let yFlippedFile = baseName ++ "_flipped_Y.cli"
    flipYCoordinates inputFile yFlippedFile
    
    -- Test combined transformation (90° rotation around origin + Y flip)
    putStrLn "\n6. Testing combined transformation (90° rotation around origin + Y flip)..."
    let combinedOriginFile = baseName ++ "_rotated_90_origin_flipped_Y.cli"
    rotateAndFlipCLIFileAroundCenter 90.0 (0.0, 0.0) FlipY inputFile combinedOriginFile
    
    -- Test combined transformation (90° rotation around bbox + Y flip) - legacy
    putStrLn "\n7. Testing combined transformation (90° rotation around bbox + Y flip)..."
    let combinedFile = baseName ++ "_rotated_90_bbox_flipped_Y.cli"
    rotateAndFlipCLIFile 90.0 FlipY inputFile combinedFile
    
    -- Test new combined single-step transformation (90° rotation around origin + Y flip)
    putStrLn "\n8. Testing new single-step combined transformation (90° rotation around origin + Y flip)..."
    let singleStepFile = baseName ++ "_single_step_90_origin_flipped_Y.cli"
    rotateAboutPointAndFlipCLIFile 90.0 (0.0, 0.0) FlipY inputFile singleStepFile
    
    -- Test new combined single-step transformation (90° rotation around origin + X flip)
    putStrLn "\n9. Testing new single-step combined transformation (90° rotation around origin + X flip)..."
    let singleStepXFile = baseName ++ "_single_step_90_origin_flipped_X.cli"
    rotateAboutPointAndFlipCLIFile 90.0 (0.0, 0.0) FlipX inputFile singleStepXFile
    
    putStrLn "\n=== All tests completed! ==="
    putStrLn "Generated files:"
    putStrLn $ "  90° rotation (origin): " ++ rotated90OriginFile
    putStrLn $ "  90° rotation (bbox): " ++ rotated90File
    putStrLn $ "  45° rotation (origin): " ++ rotated45OriginFile
    putStrLn $ "  X-flipped: " ++ xFlippedFile
    putStrLn $ "  Y-flipped: " ++ yFlippedFile
    putStrLn $ "  Combined (90° origin + Y flip): " ++ combinedOriginFile
    putStrLn $ "  Combined (90° bbox + Y flip): " ++ combinedFile
    putStrLn $ "  Single-step (90° origin + Y flip): " ++ singleStepFile
    putStrLn $ "  Single-step (90° origin + X flip): " ++ singleStepXFile

-- Combined rotation and flipping for a single point (utility function)
rotateAboutPointAndFlipPoint :: Double -> (Double, Double) -> FlipAxis -> (Double, Double) -> (Double, Double) 
rotateAboutPointAndFlipPoint angleDegrees (cx, cy) flipAxis (x, y) = roundPoint (finalX, finalY)
  where
    -- Convert to radians
    angleRadians = degreesToRadians angleDegrees
    
    -- Rotate point (x, y) around center (cx, cy) by the given angle
    cosA = cos angleRadians
    sinA = sin angleRadians
    translatedX = x - cx
    translatedY = y - cy
    rotatedX = cx + translatedX * cosA - translatedY * sinA
    rotatedY = cy + translatedX * sinA + translatedY * cosA

    -- Apply flipping if necessary
    (finalX, finalY) = case flipAxis of
        FlipX -> (-rotatedX, rotatedY)  -- Flip X coordinate
        FlipY -> (rotatedX, -rotatedY)  -- Flip Y coordinate

-- Apply rotation and flipping to polyline points
rotateAboutPointAndFlipPolyline :: Double -> (Double, Double) -> FlipAxis -> Polyline -> Polyline
rotateAboutPointAndFlipPolyline angleDegrees center flipAxis (Polyline idPL dirPL numPtsPL pointsPL) = 
    Polyline idPL dirPL numPtsPL transformedPoints
  where
    transformedPoints = map (rotateAboutPointAndFlipPoint angleDegrees center flipAxis) pointsPL

-- Apply rotation and flipping to hatches points
rotateAboutPointAndFlipHatches :: Double -> (Double, Double) -> FlipAxis -> Hatches -> Hatches
rotateAboutPointAndFlipHatches angleDegrees center flipAxis (Hatches idHat numHat pointsHat) = 
    Hatches idHat numHat transformedPoints
  where
    transformedPoints = map (rotateAboutPointAndFlipPoint angleDegrees center flipAxis) pointsHat

-- Apply rotation and flipping to a geometry line
rotateAboutPointAndFlipGeometryLine :: Double -> (Double, Double) -> FlipAxis -> GeometryLine -> GeometryLine
rotateAboutPointAndFlipGeometryLine angleDegrees center flipAxis (GLPolyline polyline) = 
    GLPolyline (rotateAboutPointAndFlipPolyline angleDegrees center flipAxis polyline)
rotateAboutPointAndFlipGeometryLine angleDegrees center flipAxis (GLHatches hatches) = 
    GLHatches (rotateAboutPointAndFlipHatches angleDegrees center flipAxis hatches)
rotateAboutPointAndFlipGeometryLine _ _ _ otherLine = otherLine  -- Keep power, speed, focus, layer changes unchanged

-- Apply rotation and flipping to all geometry lines in a layer
rotateAboutPointAndFlipLayer :: Double -> (Double, Double) -> FlipAxis -> Layer -> Layer
rotateAboutPointAndFlipLayer angleDegrees center flipAxis (Layer layerNum lines) = 
    Layer layerNum (map (rotateAboutPointAndFlipGeometryLine angleDegrees center flipAxis) lines)

-- Apply rotation and flipping to all layers in geometry data
rotateAboutPointAndFlipGeometryData :: Double -> (Double, Double) -> FlipAxis -> GeometryData -> GeometryData
rotateAboutPointAndFlipGeometryData angleDegrees center flipAxis (GeometryData layers) = 
    GeometryData (map (rotateAboutPointAndFlipLayer angleDegrees center flipAxis) layers)

-- Main function to rotate and flip CLI file contents around a specified center point
rotateAboutPointAndFlipCLIFile :: Double -> (Double, Double) -> FlipAxis -> FilePath -> FilePath -> IO ()
rotateAboutPointAndFlipCLIFile angleDegrees center flipAxis inputFile outputFile = do
    let axisName = case flipAxis of
            FlipX -> "X"
            FlipY -> "Y"
    putStrLn $ "Rotating scan patterns by " ++ show angleDegrees ++ " degrees around point " ++ show center ++ " and flipping " ++ axisName ++ " coordinates"
    putStrLn $ "Input file: " ++ inputFile
    putStrLn $ "Output file: " ++ outputFile
    
    -- Read input file
    inputString <- readFile inputFile
    let (headerLines, geometryLines, footerLines) = splitGeometryLines . splitLines $ inputString
    
    putStrLn $ "Read " ++ show (L.length geometryLines) ++ " geometry lines"
    
    -- Parse geometry lines into data structures
    let gLines = L.map readGeometryLine geometryLines
    let geoData = accumulateGLines gLines
    
    putStrLn $ "Parsed " ++ show (L.length (layers geoData)) ++ " layers"
    
    -- Apply rotation and flipping to the geometry data
    let transformedGeoData = rotateAboutPointAndFlipGeometryData angleDegrees center flipAxis geoData
    
    -- Convert back to geometry lines
    let transformedGeometryLines = writeGeometryData transformedGeoData
    
    -- Combine with header and footer
    let newString = unSplitLinesCli . unSplitGeometryLines $ (headerLines, transformedGeometryLines, footerLines)
    
    -- Write output file
    writeFile outputFile newString
    
    putStrLn $ "Successfully rotated around " ++ show center ++ " and flipped " ++ axisName ++ " coordinates. Saved to " ++ outputFile
    



-- Convenience function: 90-degree rotation around origin with Y-flip for remelting strategy
createRemeltingFileWithRotationAndFlip :: FilePath -> FilePath -> IO ()
createRemeltingFileWithRotationAndFlip inputFile outputFile = do
    putStrLn "Creating 90-degree rotated and Y-flipped file for remelting strategy..."
    rotateAboutPointAndFlipCLIFile 90.0 (0.0, 0.0) FlipY inputFile outputFile

-- Convenience function: 90-degree rotation around origin with X-flip for remelting strategy
createRemeltingFileWithRotationAndFlipX :: FilePath -> FilePath -> IO ()
createRemeltingFileWithRotationAndFlipX inputFile outputFile = do
    putStrLn "Creating 90-degree rotated and X-flipped file for remelting strategy..."
    rotateAboutPointAndFlipCLIFile 90.0 (0.0, 0.0) FlipX inputFile outputFile

-- Create rotated version of the big file for remelting (around origin for best control)
createRemeltingFileAroundOrigin :: FilePath -> FilePath -> IO ()
createRemeltingFileAroundOrigin inputFile outputFile = do
    putStrLn "Creating 90-degree rotated file (around origin) for remelting strategy..."
    rotate90DegreesAroundOrigin inputFile outputFile

-- Create rotated version of the big file for remelting (legacy - around bounding box centers)
createRemeltingFile :: FilePath -> FilePath -> IO ()
createRemeltingFile inputFile outputFile = do
    putStrLn "Creating 90-degree rotated file for remelting strategy..."
    rotate90Degrees inputFile outputFile

-- Function to create both first melting and remelting files with different power settings
-- This assumes you have separate files with different power assignments
-- Uses rotation around origin for better control
createRemeltingStrategyAroundOrigin :: FilePath -> FilePath -> FilePath -> IO ()
createRemeltingStrategyAroundOrigin firstMeltFile remeltPowerFile outputFile = do
    putStrLn "Creating remelting strategy files (rotation around origin)..."
    
    -- Step 1: Create rotated version of remelting power file around origin
    let rotatedRemeltFile = remeltPowerFile ++ "_rotated_90_origin.cli"
    putStrLn "Step 1: Rotating remelting file by 90 degrees around origin (0,0)..."
    rotate90DegreesAroundOrigin remeltPowerFile rotatedRemeltFile
    
    -- Step 2: Combine the files (you would use your existing combining function)
    putStrLn "Step 2: Use myCombineLayersFrom2FilesStreamingV2 to combine:"
    putStrLn $ "  First melt file: " ++ firstMeltFile
    putStrLn $ "  Rotated remelt file: " ++ rotatedRemeltFile  
    putStrLn $ "  Output: " ++ outputFile
    putStrLn "This ensures perpendicular hatch patterns between first melt and remelt!"
    putStrLn "Rotation around origin provides consistent geometric transformation."

-- Function to create both first melting and remelting files with different power settings
-- This assumes you have separate files with different power assignments
createRemeltingStrategy :: FilePath -> FilePath -> FilePath -> IO ()
createRemeltingStrategy firstMeltFile remeltPowerFile outputFile = do
    putStrLn "Creating remelting strategy files..."
    
    -- Step 1: Create rotated version of remelting power file
    let rotatedRemeltFile = remeltPowerFile ++ "_rotated_90.cli"
    putStrLn "Step 1: Rotating remelting file by 90 degrees..."
    rotate90Degrees remeltPowerFile rotatedRemeltFile
    
    -- Step 2: Combine the files (you would use your existing combining function)
    putStrLn "Step 2: Use myCombineLayersFrom2FilesStreamingV2 to combine:"
    putStrLn $ "  First melt file: " ++ firstMeltFile
    putStrLn $ "  Rotated remelt file: " ++ rotatedRemeltFile  
    putStrLn $ "  Output: " ++ outputFile
    putStrLn "This ensures perpendicular hatch patterns between first melt and remelt!"


