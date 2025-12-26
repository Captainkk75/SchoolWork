module Main where

import CliParse

-- Define your file paths here
myInputFile1Big :: FilePath
myInputFile1Big = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650.cli"

myInputFile2Big :: FilePath  
myInputFile2Big = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/2024-03-06_Ti5553_60mu_Kontur_P220-260_v1450-1650_copy.cli"

myOutputFileBig :: FilePath
myOutputFileBig = "/home/kushik/Kushik/TU Chemnitz/Thesis and Work github/lpbf/lpbf/G code files/cli files/combined_big_files.cli"

main :: IO ()
main = do
    putStrLn "Starting big file combination test..."
    myCombineLayersFrom2FilesAndExecute myInputFile1Big myInputFile2Big 12 myOutputFileBig
    putStrLn "Completed successfully!"
