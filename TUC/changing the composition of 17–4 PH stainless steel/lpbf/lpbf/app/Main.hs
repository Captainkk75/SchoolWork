module Main (main) where

--import Lib
import CliParse 

import Rotation as R

main :: IO ()
-- main = myCombineLayersFrom2FilesAndExecute myInputFile1Big myInputFile2Big 12 myOutputFileBig
--main = myCombineLayersFrom2FilesWithStateAndExecute myInputFile1Big myInputFile2Big 12 myOutputFileBig
--main = myCombineLayersFrom2FilesAndExecuteStreaming  myInputFile1Big myInputFile2Big 8 myOutputFileBig
-- main = myCombineLayersFrom2FilesAndExecuteStreaming  myInputFile1Mini myInputFile2Mini 2 myOutputFileMini
--main = testStatePersistence


-- The next line works for combining -- using CliParse.hs
--main = myCombineLayersFrom2FilesStreamingV2withoutWaitingLines experiment4CarbonAngle0File experiment4CarbonAngle90File 84 experiment4CarbonRemeltingCombinedFileWithoutWaitingLines
--main = myCombineLayersFrom2FilesStreamingV2 experiment4CarbonAngle0File experiment4CarbonAngle90File 84 experiment4CarbonRemeltingCombinedFile


-- The following line worked for combining the tensile test files on 15 August 2025
--main = myCombineLayersFrom2FilesStreamingV2withoutWaitingLines ex4TensileFirstMeltingFile2 ex4TensileSecondMeltingFile 84 ex4TensileCombinedFile2
main = myCombineLayersFrom2FilesStreamingV2withoutWaitingLines ex4TensileFirstMeltingFileWithContour100W ex4TensileSecondMeltingFileWithContour100W 84 ex4TensileCombinedFileWithContour100W


-- The following line worked for combining the cubes (after the microscopy, new 2 cubes) on 15 August 2025
--main = myCombineLayersFrom2FilesStreamingV2withoutWaitingLines ex4cubes2FirstMeltingFile ex4cubes2SecondMeltingFile 84 ex4cubes2CombinedFile



-- The next line is for rotating -- using Rotation.hs
--main = R.rotateAboutPointAndFlipCLIFile 90.0 (0.0, 0.0) R.FlipY R.testInputFileMini R.testOutputFileMini
--main = R.rotateAboutPointAndFlipCLIFile 90.0 (0.0, 0.0) R.FlipY R.testInputFileBig1 R.testOutputFileBig1
-- Will need to be checked.