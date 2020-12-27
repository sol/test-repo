module PackageVersionSpec (spec) where

import           Test.Hspec
import           Test.Mockery.Directory

import           Data.Version

import           PackageVersion

spec :: Spec
spec = do
  describe "packageVersion" $ around_ inTempDirectory $ do
    it "" $ do
      writeFile "package.cabal" "version: 0.1.0"
      packageVersion "." `shouldReturn` unlines [
          "::set-output name=branch::0.1.0"
        ]

    it "" $ do
      writeFile "package.cabal" "version: 0.1.0-pre"
      packageVersion "." `shouldReturn` unlines [
          "::set-output name=branch::0.1.0"
        , "::set-output name=tags::pre"
        ]

    it "" $ do
      touch "foo/package.cabal"
      writeFile "foo/package.cabal" "version: 0.1.0"
      packageVersion "foo" `shouldReturn` unlines [
          "::set-output name=branch::0.1.0"
        ]

    it "" $ do
      touch "package.cabal"
      packageVersion "." `shouldThrow` errorCall "Couldn't extract a version from ./package.cabal"

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
