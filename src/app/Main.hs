import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafeInterleaveIO)

import Utils (stringsFromStatus, Hash(MkHash))

{- import Filesystem.Path.Rules (encodeString, posix) -}
import Filesystem.Path.CurrentOS (encodeString)
import Data.Git.Storage (findRepoMaybe)
import Data.Git.Named
import Data.Git.Ref (HashAlgorithm, SHA1)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
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

getStashCount :: FilePath -> IO Int
getStashCount repo = countLinesInFile $ repo </> "logs" </> "refs" </> "stash"
    where
        countLinesInFile :: String -> IO Int
        countLinesInFile f = length . lines <$> E.catch (readFile f) readHandler
        readHandler :: IOError -> IO String
        readHandler _ = pure mempty

isMergeInProgess :: FilePath -> IO Bool
isMergeInProgess repo = doesFileExist $ repo </> "MERGE_HEAD"

fromBool :: Num a => Bool -> a
fromBool False  = 0
fromBool True   = 1

parse :: String -> IO ()
parse status = do
    maybeRepo <- (fmap . fmap) encodeString findRepoMaybe
    let repo = fromMaybe mempty maybeRepo
    stashCount <- getStashCount repo
    merge <- isMergeInProgess  repo
    mhash <- unsafeInterleaveIO gitrevparse
    let parseStatus =  maybe mempty unwords . stringsFromStatus mhash
    let echo = putStr . (' ' :) . show
    -- 0. The branch.
    -- 1. If branch is tracked upstream, number of commits behind.
    -- 2. If branch is tracked upstream, number of commits ahead.
    -- 3. The number of staged files.
    -- 4. The number of conflict files (i.e. unmerged).
    -- 5. The number of changed files (i.e. tracked and edited)
    -- 6. The number of untracked files (does not include ignored).
    putStr $ parseStatus status -- e.g. master 0 0 0 0 0 0
    -- 7. The number of stashes on the current repository.
    echo stashCount
    -- 8. 1 iff we are on a branch and that branch has no upstream tracking set.
    -- TODO implement
    -- 9. The name of the upstream remote/branch set to track, e.g. origin/master
    -- TODO implement
    -- 10. 1 if and only if engaged in a merge operation. 0 otherwise.
    echo $ fromBool merge
    -- 11. rebase indicator, format m/n, m is the current commit we are checked
    --     out on to resolve /  n is the total no of commits to, 0 otherwise.
    -- TODO implement

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
