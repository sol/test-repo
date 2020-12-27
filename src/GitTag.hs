{-# LANGUAGE LambdaCase #-}
module GitTag (
  main
, run
, extractVersion
) where

import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Version
import           System.Environment
import           System.Exit hiding (die)
import           System.Process
import           System.Directory
import           System.FilePath
import           Text.ParserCombinators.ReadP

main :: IO ()
main = run createTag

die :: String -> IO a
die err = throwIO (ErrorCall err)

run :: CreateTag -> IO ()
run create = do
  dir <- fromMaybe "." <$> lookupEnv "PACKAGE_PATH"

  tagPreReleases <- (== Just "true") <$> lookupEnv "TAG_PRE_RELEASES"
  dryRun <- (== Just "true") <$> lookupEnv "DRY_RUN"

  v@(Version branch tags) <- packageVersion dir

  prefix <- fromMaybe "v" <$> lookupEnv "TAG_PREFIX"
  let version = showVersion (makeVersion branch)
      versionWithTags = showVersion v
      tagName = prefix <> versionWithTags

  when (null tags || tagPreReleases) $ do
    created <- if dryRun then return True else create tagName
    when created $ do
      setOutput "created" "true"
  setOutput "tag-name" tagName
  setOutput "version" version
  setOutput "version-tags" (intercalate "-" tags)
  setOutput "version-with-tags" versionWithTags

packageVersion :: FilePath -> IO Version
packageVersion dir = do
  name <- findCabalFile dir
  input <- readFile name
  case extractVersion input of
    Just v -> return v
    Nothing -> die $ "Couldn't extract a version from " <> name

setOutput :: String -> String -> IO ()
setOutput name value = putStrLn $ "::set-output name=" <> name <> "::" <> value

extractVersion :: String -> Maybe Version
extractVersion = extract >=> parse

extract :: String -> Maybe String
extract = mconcat . map foo . lines . foobar . dropComments
  where
    foo = stripPrefix "version:" . filter (/= ' ')

foobar :: String -> String
foobar input = case input of
  '\n' : x : xs | isSpace x -> foobar (x : xs)
  x : xs -> x : foobar xs
  [] -> []

dropComments :: String -> String
dropComments = unlines . filter (not . isComment) . lines
  where
    isComment = isPrefixOf "--" . dropWhile isSpace

parse :: String -> Maybe Version
parse input = case reverse $ readP_to_S parseVersion input of
  (v, "") : _ -> Just v
  _ -> Nothing

findCabalFile :: FilePath -> IO FilePath
findCabalFile dir = do
  cabalFiles <- filter isCabalFile <$> listDirectory dir
  case cabalFiles of
    [cabalFile] -> return (dir </> cabalFile)
    [] -> die $ "Couldn't find a .cabal file in " <> dir
    _ -> die $ "Multiple cabal files found in " <> dir
  where
    isCabalFile :: FilePath -> Bool
    isCabalFile name = ".cabal" `isSuffixOf` name && (not . isDotFile) name

    isDotFile :: FilePath -> Bool
    isDotFile name = "." `isPrefixOf` name

type CreateTag = String -> IO Bool

createTag :: CreateTag
createTag name = withGitTag $ withGitPushTags $ return True
  where
    withGitTag :: IO Bool -> IO Bool
    withGitTag action = callProcessWithExitCode "git" ["tag", name] >>= \ case
      ExitSuccess -> action
      ExitFailure 128 -> return False
      e -> throwIO e

    withGitPushTags :: IO Bool -> IO Bool
    withGitPushTags action = callProcessWithExitCode "git" ["push", "--tags"] >>= \ case
      ExitSuccess -> action
      ExitFailure 1 -> return False
      e -> throwIO e

    callProcessWithExitCode :: FilePath -> [String] -> IO ExitCode
    callProcessWithExitCode cmd args = withCreateProcess process wait
      where
        process = (proc cmd args) {delegate_ctlc = True}
        wait _ _ _ = waitForProcess
