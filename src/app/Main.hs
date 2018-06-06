import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafeInterleaveIO)

import Utils (stringsFromStatus, Hash(MkHash))

import Filesystem.Path.Rules (encodeString, posix)
import Data.Git.Storage (findRepoMaybe)
import Data.Git.Named
import Data.Git.Ref (HashAlgorithm, SHA1)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)
import Data.Maybe (fromMaybe)

import qualified Control.Exception.Base as E

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

getStashCount :: String -> IO Int
getStashCount repo = countLinesInFile $ repo ++ "/logs/refs/stash"
    where
        countLinesInFile :: String -> IO Int
        countLinesInFile f = length . lines <$> E.catch (readFile f) readHandler
        readHandler :: IOError -> IO String
        readHandler _ = pure mempty

{- isMergeInProgess :: IO Bool -}
{- isMergeInProgess = do -}
{-     repo <- findRepoMaybe -}

parse status = do
    repo <- findRepoMaybe
    stashCount <- getStashCount . fromMaybe mempty $ encodeString posix <$> repo
    mhash <- unsafeInterleaveIO gitrevparse
    let parseStatus =  maybe mempty unwords . stringsFromStatus mhash
    let echo x = " " ++ show x
    putStr $ parseStatus status
    putStr $ echo stashCount

main :: IO ()
main = do
    isTTY <- queryTerminal stdInput
    if isTTY
        then do -- run status command
            status <- safeRun "git" ["status", "--porcelain", "--branch"]
            case status of
                Nothing -> error "fatal: not a git repository"
                Just s -> parse s
        else getContents >>= parse -- get status from stdin
