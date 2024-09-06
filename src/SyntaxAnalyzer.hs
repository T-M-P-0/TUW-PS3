module SyntaxAnalyzer (execute, Result(..), Error(..)) where

import System.Exit
import System.Process
import Text.Read (readMaybe)

data Error = LineError Int String | GeneralError String deriving (Show, Eq) 
data Result = Success | Failure Error deriving (Show, Eq)

execute :: String -> IO Result
execute code = do
  let args = createArgs code
  (exitCode, stdout, stderr) <- readProcessWithExitCode "python" args ""
  let error = parseErrorString stderr
  case exitCode of
    ExitSuccess -> return Success
    ExitFailure _ -> return $ Failure error

createArgs :: String -> [String]
createArgs code = [pathToExecutable, "--code", code]

pathToExecutable :: String
pathToExecutable = "/home/tom/Code/Uni/ProgrammingLanguages/PS3/interpreter/src/main.py"

getFirstLine :: String -> String
getFirstLine = head . lines

getLastElementOfLine :: String -> String
getLastElementOfLine = last . words . getLastLine

getLastLine :: String -> String
getLastLine = last . lines

extractLineNumber :: String -> Maybe Int
extractLineNumber errorString =
  let 
    firstLine = getFirstLine errorString
    lineNumberStr = getLastElementOfLine firstLine
   in readMaybe lineNumberStr

parseErrorString :: String -> Error
parseErrorString errorString =
  let lastLine = getLastLine errorString
      lineNumber = extractLineNumber errorString
   in case lineNumber of
        Just ln -> LineError ln lastLine
        Nothing -> GeneralError lastLine
