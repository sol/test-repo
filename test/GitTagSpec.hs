module GitTagSpec (spec) where

import           Test.Hspec
import           Test.Mockery.Directory
import           Test.Mockery.Environment
import           System.Environment.Blank
import           System.IO.Silently
import           Data.IORef

import           Control.Monad

import           Data.Version

import           GitTag

shouldCreateTag :: HasCallStack => String -> [String] -> Expectation
shouldCreateTag name outputs = do
  ref <- newIORef ""
  let createTag = writeIORef ref >=> const (return True)
  capture_ (run createTag) `shouldReturn` unlines (
      setOutput "created" "true"
    : setOutput "tag-name" name
    : outputs
    )
  readIORef ref `shouldReturn` name

shouldNotCreateTag :: HasCallStack => [String] -> Expectation
shouldNotCreateTag outputs = capture_ (run undefined) `shouldReturn` unlines outputs

setOutput :: String -> String -> String
setOutput name value = "::set-output name=" <> name <> "::" <> value

spec :: Spec
spec = do
  describe "run" $ around_ inTempDirectory $ do

    context "with a .cabal file" $ before_ (writeFile "package.cabal" "version: 0.1.0") $ do
      let versionOutputs :: String -> [String]
          versionOutputs version = [
              setOutput "version" version
            , setOutput "version-tags" ""
            , setOutput "version-with-tags" version
            ]

      it "creates a tag" $ do
        shouldCreateTag "v0.1.0" $ versionOutputs "0.1.0"

      context "when tag already exists" $ do
        it "does not set any outputs" $ do
          let createTag _ = return False
          capture_ (run createTag) `shouldReturn` unlines (
              setOutput "tag-name" "v0.1.0"
            : versionOutputs "0.1.0"
            )

      context "with PACKAGE_PATH" $ do
        it "extracts the version from the specified package" $ do
          withEnvironment [("PACKAGE_PATH", "./foo/")] $ do
            touch "foo/package.cabal"
            writeFile "foo/package.cabal" "version: 0.2.0"
            shouldCreateTag "v0.2.0" $ versionOutputs "0.2.0"

      context "with TAG_PREFIX=" $ do
        it "XXXX" $ do
          withEnvironment [] $ do
            setEnv "TAG_PREFIX" "" True
            shouldCreateTag "0.1.0" $ versionOutputs "0.1.0"

      context "with TAG_PREFIX" $ do
        it "XXXXX" $ do
          withEnvironment [("TAG_PREFIX", "version-")] $ do
            shouldCreateTag "version-0.1.0" $ versionOutputs "0.1.0"

      context "with DRY_RUN=true" $ do
        it "does not create any tag" $ do
          withEnvironment [("DRY_RUN", "true")] $ do
            shouldNotCreateTag $
                setOutput "created" "true"
              : setOutput "tag-name" "v0.1.0"
              : versionOutputs "0.1.0"

    context "with a .cabal file with version tags" $ before_ (writeFile "package.cabal" "version: 0.1.0-pre-alpha") $ do

      let versionOutputs = [
              setOutput "version" "0.1.0"
            , setOutput "version-tags" "pre-alpha"
            , setOutput "version-with-tags" "0.1.0-pre-alpha"
            ]

      it "does not create a tag" $ do
        shouldNotCreateTag $
            setOutput "tag-name" "v0.1.0-pre-alpha"
          : versionOutputs

      context "with TAG_PRE_RELEASES=true" $ do
        it "creates a tag" $ do
          withEnvironment [("TAG_PRE_RELEASES", "true")] $ do
            shouldCreateTag "v0.1.0-pre-alpha" versionOutputs

    context "without a .cabal" $ do
      it "fails" $ do
        run undefined `shouldThrow` errorCall "Couldn't find a .cabal file in ."

    context "with a .cabal file without a version" $ before_ (touch "package.cabal") $ do
      it "fails" $ do
        run undefined `shouldThrow` errorCall "Couldn't extract a version from ./package.cabal"

    context "with multiple .cabal files" $ do
      it "fails" $ do
        writeFile "foo.cabal" "version: 0.1.0"
        writeFile "bar.cabal" "version: 0.1.0"
        run undefined `shouldThrow` errorCall "Multiple cabal files found in ."

  describe "extractVersion" $ do
    it "extracts version" $ do
      extractVersion "version:0.1.0" `shouldBe` Just (makeVersion [0,1,0])

    it "ignores other values" $ do
      extractVersion "foo:23\nversion:0.1.0\nbar:42" `shouldBe` Just (makeVersion [0,1,0])

    it "accepts spaces" $ do
      extractVersion "version  :  0.1.0" `shouldBe` Just (makeVersion [0,1,0])

    it "accepts line continuations" $ do
      extractVersion "version:\n  0.1.0" `shouldBe` Just (makeVersion [0,1,0])

    it "ignores comments" $ do
      extractVersion "version:\n  -- foo\n  0.1.0" `shouldBe` Just (makeVersion [0,1,0])
