import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafeInterleaveIO)

import Utils (stringsFromStatus, Hash(MkHash))
import Data.Maybe (fromMaybe)

import Data.Git (findRepo)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

{- Git commands -}

successOrNothing :: (ExitCode, a, b) -> Maybe a
successOrNothing (exitCode, output, _)
     | exitCode == ExitSuccess = Just output
     | otherwise = Nothing

safeRun :: String -> [String] -> IO (Maybe String)
safeRun cmd args =
        successOrNothing <$> readProcessWithExitCode cmd args mempty

gitrevparse :: IO (Maybe Hash)
gitrevparse = do
        mresult <- safeRun "git" ["rev-parse", "--short", "HEAD"]
        return $ MkHash . init <$> mresult

main :: IO ()
main = do
    repo <- findRepo
    isTTY <- queryTerminal stdInput
    mhash <- unsafeInterleaveIO gitrevparse
    let parse = putStr .  maybe mempty unwords . stringsFromStatus mhash
    if isTTY
        then do -- run status command
            status <- safeRun "git" ["status", "--porcelain", "--branch"]
            case status of
                Nothing -> error "fatal: not a git repository"
                Just s -> parse s
        else do -- get status from stdin
            status <- getContents
            parse status
