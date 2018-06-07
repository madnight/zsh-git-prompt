module Utils where

import BranchParse (Branch(MkBranch), MBranchInfo, BranchInfo(MkBranchInfo), branchInfo, getDistance, pairFromDistance, Remote)
import StatusParse (Status(MakeStatus), processStatus)

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Char (isSpace)

{- Type aliases -}

newtype Hash = MkHash {getHash :: String}

data GitInfo = MkGitInfo MBranchInfo (Status Int)

fromBool :: Bool -> Integer
fromBool False  = 0
fromBool True   = 1

readHandler :: IOError -> IO String
readHandler _ = pure mempty

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse

successOrNothing :: (ExitCode, a, b) -> Maybe a
successOrNothing (exitCode, output, _)
    | exitCode == ExitSuccess = Just output
    | otherwise = Nothing

safeRun :: String -> [String] -> IO (Maybe String)
safeRun cmd args =
    successOrNothing <$> readProcessWithExitCode cmd args mempty

rightOrNothing :: Either a b -> Maybe b
rightOrNothing = either (const Nothing) Just

processBranch :: String -> Maybe MBranchInfo
processBranch = rightOrNothing . branchInfo

processGitStatus :: [String] -> Maybe GitInfo
processGitStatus [] = Nothing
processGitStatus (branchLine:statusLines) = do
            mbranch <- processBranch branchLine
            status <- processStatus statusLines
            return (MkGitInfo mbranch status)

showStatusNumbers :: Status Int -> [String]
showStatusNumbers (MakeStatus s x c t) = show <$> [s, x, c, t]

showRemoteNumbers :: Maybe Remote -> [String]
showRemoteNumbers mremote = show <$> [ahead, behind]
    where -- the script needs some value, (0,0) means no display
        (ahead, behind) =
            maybe (0, 0) pairFromDistance (getDistance =<< mremote)


showBranchInfo :: BranchInfo -> [String]
showBranchInfo (MkBranchInfo branch mremote) =
    show branch : showRemoteNumbers mremote

{- Combine status info, branch info and hash -}

branchOrHashWith :: Char -> Maybe Hash -> Maybe BranchInfo -> BranchInfo
branchOrHashWith _ _ (Just bi) = bi
branchOrHashWith c (Just hash) Nothing =
    MkBranchInfo (MkBranch (c : getHash hash)) Nothing
branchOrHashWith _ Nothing _ = MkBranchInfo (MkBranch "") Nothing

showGitInfo :: Maybe Hash
            -> GitInfo
            -> [String]
showGitInfo mhash (MkGitInfo bi stat) =
    branchInfoString ++ showStatusNumbers stat
    where
        branchInfoString = showBranchInfo (branchOrHashWith ':' mhash bi)

stringsFromStatus :: Maybe Hash
                    -> String -- status
                    -> Maybe [String]
stringsFromStatus hash = (showGitInfo hash <$>) . processGitStatus . lines
