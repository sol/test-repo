module PackageVersion where

import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Version
import           System.Environment
import           System.Directory
import           System.FilePath
import           Text.ParserCombinators.ReadP

main :: IO ()
main = do
  args <- getArgs
  let dir = fromMaybe "." (listToMaybe args)
  packageVersion dir >>= putStr

die :: String -> IO a
die err = throwIO (ErrorCall err)

packageVersion :: FilePath -> IO String
packageVersion dir = do
  name <- findCabalFile dir
  input <- readFile name
  case extractVersion input of
    Just v -> return (formatVersion v)
    Nothing -> die $ "Couldn't extract a version from " <> name

formatVersion :: Version -> String
formatVersion (Version branch tags) = unlines $ v : t
  where
    v = setOutput "branch" (showVersion $ makeVersion branch)
    t | null tags = []
      | otherwise = [setOutput "tags" (intercalate "-" tags)]

setOutput :: String -> String -> String
setOutput name value = "::set-output name=" <> name <> "::" <> value

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
    [] -> die $ "Couldn't find .cabal file in " <> dir
    _ -> die $ "Multiple cabal files found in " <> dir
  where
    isCabalFile :: FilePath -> Bool
    isCabalFile name = ".cabal" `isSuffixOf` name && (not . isDotFile) name

    isDotFile :: FilePath -> Bool
    isDotFile name = "." `isPrefixOf` name
