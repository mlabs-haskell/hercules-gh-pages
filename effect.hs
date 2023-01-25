{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import qualified Data.Text as Text
import System.Environment
import Text.URI
import Turtle (fromText)
import Turtle.Prelude
import Turtle.Line (lineToText)
import Turtle.Shell (FoldShell(FoldShell), foldShell, liftIO, sh)

need' e f = do
  mb <- need e
  case mb of
    Nothing -> fail "need': Nothing"
    Just s -> f s

fromRightM _ (Right ok) = pure ok
fromRightM e (Left _) = fail e

main :: IO ()
main = sh do
  branchName <- need' "branchName" pure
  ghPages <- need' "gh-pages" pure
  owner <- need' "owner" mkUsername
  remoteHttpUrl <- need' "remoteHttpUrl" mkURI
  rewriteHistory <- need' "rewriteHistory" (pure . (== "1"))

  token <- inshell "readSecretString git .token" mempty >>= mkPassword . lineToText
  authority <- fromRightM "uriAuthority is Left _" $ uriAuthority remoteHttpUrl
  let origin = remoteHttpUrl { uriAuthority = Right authority { authUserInfo = Just UserInfo { uiUsername = owner, uiPassword = Just token } } }
  if rewriteHistory then do
    mkdir "gh-pages"
    cd "gh-pages"
    procs "git" ["init", "--initial-branch", branchName] mempty
    procs "git" ["remote", "add", "origin", render origin] mempty
  else do
    procs "git" ["clone", "--branch", branchName, "--single-branch", render origin, "gh-pages"] mempty
    cd "gh-pages"
  ls "." >>= \f ->
    when (f /= ".git") do
      rmtree f
  -- procs "cp" ["-r", "--no-preserve=mode", "-T", ghPages, "."] mempty
  cptreeL (fromText ghPages) "."
  hasChanges <-
    foldShell (shells "git status --porcelain" mempty) $
      FoldShell
        (\_ _ -> pure True)
        False
        pure
  if hasChanges then do
    shells "git add ." mempty
    procs "git" ["commit", "-m", "Deploy to " <> branchName] mempty
    procs "git" ["push", "-f", "origin", branchName] mempty
  else echo "Nothing to commit"
