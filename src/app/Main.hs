import Utils (safeRun, stringsFromStatus, readHandler,
  fromBool, strip, Hash(MkHash))

import Data.Git.Ref()
import Data.Git.Storage (findRepoMaybe)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Filesystem.Path.CurrentOS (encodeString)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

import qualified Control.Exception.Base as E

gitrevparse :: IO (Maybe Hash)
gitrevparse = do
    mresult <- safeRun "git" ["rev-parse", "--short", "HEAD"]
    return $ MkHash . init <$> mresult

getStashCount :: FilePath -> IO Int
getStashCount repo =
    countLinesInFile $ repo </> "logs" </> "refs" </> "stash"
    where
        countLinesInFile :: String -> IO Int
        countLinesInFile f = length . lines <$> E.catch (readFile f) readHandler

-- | Determine the rebase status of this repostitory and return it.
-- | Args: git root
-- | Returns:
-- |    - "0": No active rebase
-- |    - "1/4": Rebase in progress, commit 1 of 4
rebaseProgess :: FilePath -> IO String
rebaseProgess repo = do
    let readRebase = readFile . ((repo </> "rebase-apply") </>)
    next <- E.catch (readRebase "next") readHandler
    last' <- E.catch (readRebase "last") readHandler
    if length (last' ++ next) < 1
        then pure "0"
        else pure $ strip last' ++ "/" ++ strip next

isMergeInProgess :: FilePath -> IO Bool
isMergeInProgess = doesFileExist . (</> "MERGE_HEAD")

parseBranch :: String -> FilePath -> IO (String, String, Int)
parseBranch status repo
    | "..." `isInfixOf` status = do
            let getBranch = splitOn "..." . head . tail $ words status
            let branch = head getBranch
            let upstream = last getBranch
            pure (branch, upstream, 0)
    | "no branch" `isInfixOf` status = do
            let readHead = readFile $ repo </> "HEAD"
            head' <- E.catch readHead readHandler
            let hashPrefixEnv = "ZSH_THEME_GIT_PROMPT_HASH_PREFIX"
            sym_prehash <-  fromMaybe ":" <$> lookupEnv hashPrefixEnv
            pure (sym_prehash ++ take 7 head', "..", 0)
    | "Initial Commit" `isPrefixOf` status
        || isPrefixOf "No commits yet" status = do
            let branch = last $ words status
            pure (branch, "..", 1)
    | otherwise = pure (head . tail $ words status, "..", 1)

parse :: String -> IO ()
parse status = do
    maybeRepo <- (fmap . fmap) encodeString findRepoMaybe
    let repo = fromMaybe mempty maybeRepo
    stashCount <- getStashCount repo
    merge <- isMergeInProgess  repo
    rebase <- rebaseProgess repo
    mhash <- unsafeInterleaveIO gitrevparse
    (_, upstream, local) <- parseBranch status repo
    let parseStatus =  maybe mempty unwords . stringsFromStatus mhash
    let echo = putStr . (' ' :)

    -- 0. The branch.
    -- 1. If branch is tracked upstream, number of commits behind.
    -- 2. If branch is tracked upstream, number of commits ahead.
    -- 3. The number of staged files.
    -- 4. The number of conflict files (i.e. unmerged).
    -- 5. The number of changed files (i.e. tracked and edited)
    -- 6. The number of untracked files (does not include ignored).
    putStr $ parseStatus status -- e.g. master 0 0 0 0 0 0
    -- 7. The number of stashes on the current repository.
    echo $ show stashCount
    -- 8. 1 iff we are on a branch and that branch has no upstream tracking set.
    echo $ show local
    -- 9. The name of the upstream remote/branch set to track, e.g. origin/master
    echo upstream
    -- 10. 1 iff engaged in a merge operation. 0 otherwise.
    echo . show $ fromBool merge
    -- 11. rebase indicator, format m/n, m is the current commit we are checked
    --     out on to resolve /  n is the total no of commits to, 0 otherwise.
    echo rebase

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
