import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafeInterleaveIO)

import Utils (stringsFromStatus, Hash(MkHash))
import Data.Maybe (fromMaybe)

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
    status <- getContents
    -- defer the execution until we know we need the hash
    mhash <- unsafeInterleaveIO gitrevparse
    putStr . fromMaybe mempty $ unwords <$> stringsFromStatus mhash status
