{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module Main (main) where

import Prelude hiding (FilePath)

import Data.Aeson (FromJSON(parseJSON), eitherDecodeFileStrict', withObject, (.:))
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Text.URI
import Turtle (FilePath, MonadIO, toText)
import Turtle.Prelude
import Turtle.Shell (FoldShell(FoldShell), foldShell, sh, liftIO)

need' :: (MonadIO m, MonadFail m) => Text -> m Text
need' e = do
  mb <- need e
  case mb of
    Nothing -> fail "need': Nothing"
    Just s -> pure s

fromRightM :: (MonadFail m, Show e) => Either e a -> m a
fromRightM (Right ok) = pure ok
fromRightM (Left e) = fail $ "fromRightM: " <> show e

newtype Secrets = Secrets { gitToken :: Text }

instance FromJSON Secrets where
  parseJSON =
    withObject "secrets" \secrets ->
      secrets .: "git" >>= withObject "git" \git ->
        git .: "data" >>= withObject "data" \data_ ->
          Secrets <$> data_ .: "token"

shellStdoutNonEmpty :: (Eq a, Monoid a) => FoldShell a Bool
shellStdoutNonEmpty =
  FoldShell
    (\acc line -> pure (acc || line /= mempty))
    False
    pure

ghPagesDir :: FilePath
ghPagesDir = "gh-pages"

main :: IO ()
main = sh do
  secretsPath <- need' "HERCULES_CI_SECRETS_JSON"
  Secrets {gitToken} <- liftIO $ eitherDecodeFileStrict' (Text.unpack secretsPath) >>= \case
    Left e -> fail $ show e
    Right ok -> pure ok

  branchName <- need' "branchName"
  ghPages <- need' "gh-pages"
  owner <- need' "owner" >>= mkUsername
  remoteHttpUrl <- need' "remoteHttpUrl" >>= mkURI
  rewriteHistory <- (== "1") <$> need' "rewriteHistory"

  gitTokenPassword <- mkPassword gitToken
  authority <- fromRightM $ uriAuthority remoteHttpUrl
  let origin = remoteHttpUrl { uriAuthority = Right authority { authUserInfo = Just UserInfo { uiUsername = owner, uiPassword = Just gitTokenPassword } } }
  pwd >>= liftIO . print

  if rewriteHistory then do
    mkdir ghPagesDir
    cd ghPagesDir
    procs "git" ["init", "--initial-branch", branchName] mempty
    procs "git" ["remote", "add", "origin", render origin] mempty
  else do
    procs "git" ["clone", "--branch", branchName, "--single-branch", render origin, fromRight (error "impossible") (toText ghPagesDir)] mempty
    cd ghPagesDir

  let currentGit = ".git"
      backupGit = "/build/git-backup"

  pwd >>= liftIO . print
  ls "." >>= liftIO . print

  mv currentGit backupGit
  ls "." >>= rmtree

  echo "Removed trees"

  procs "cp" ["-r", "--no-preserve=mode", "-T", ghPages, "."] mempty
  -- cptreeL (fromText ghPages) "."
  mv backupGit currentGit
  hasChanges <-
    foldShell (inproc "git" ["status", "--porcelain"] mempty) shellStdoutNonEmpty
  if hasChanges then do
    procs "git" ["add", "."] mempty
    procs "git" ["commit", "-m", "Deploy to " <> branchName] mempty
    procs "git" ["push", "-f", "origin", branchName] mempty
  else echo "Nothing to commit"
