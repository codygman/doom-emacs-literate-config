#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (ps: [ps.turtle])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/ae6bdcc53584aaf20211ce1814bea97ece08a248.tar.gz
{-# LANGUAGE OverloadedStrings #-}
-- NOTE: nixpkgs is pinned to nixos-unstable 2020-03-28
import Turtle
import Data.Time
import Control.Monad


{-
ln -rs ~/.config/doom/nixpkgs ~/.config/nixpkgs
nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-channel --update
echo "installing"
nix-shell '<home-manager>' -A install
home-manager switch
-}

-- echoTxt =  echo . unsafeTextToLine
main :: IO ()
main = do
    userHome <- home
    doomDir <- pwd
    let doomDirNixpkgs = doomDir </> decodeString "nixpkgs"
    let nixpkgConfigPath = userHome </> decodeString ".config/nixpkgs"
    let bootstrapBackups = userHome </> decodeString ".bootstrap-backups"
    testdir nixpkgConfigPath >>= \there -> do
      echoTxt $ format ("NOTE: moving current config to " % fp) bootstrapBackups
      -- create backup directory
      mktree bootstrapBackups
      -- move old folder with timestamp into backup directory
      date >>= \now -> let timestamp = (fromString $formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H_%M_%S") now ) :: Text
                           nixpkgsPrefix = ("nixpkgs_" :: Text)
                           backupPath = bootstrapBackups </> fromText (format (s % s) nixpkgsPrefix timestamp)
                         in do
                            -- TODO don't want to do this every time unless there's a change
                            echoTxt $ format ("moving '" %fp% "' to '" %fp% "'") nixpkgConfigPath backupPath
                            mv nixpkgConfigPath backupPath
      -- symlink file (or copy on android, then rename home.nix to nix-on-droid.nix)
      hostname >>= \hn -> case hn of
                            "localhost" -> do
                                           echo "android detected, using copying instead of symlinks"
                                           cp doomDirNixpkgs nixpkgConfigPath
                                           mv (nixpkgConfigPath </> decodeString "home.nix") (nixpkgConfigPath </> decodeString "nix-on-droid.nix")
                                           echo "renamed home.nix to nix-on-droid.nix"
                            "nixos" -> symlink doomDirNixpkgs nixpkgConfigPath
                            unknown -> error $ "unkown system, not sure what to do: " <> show unknown
    dirsExist <- sequenceA [ testdir doomDirNixpkgs
                           , testdir nixpkgConfigPath
                           ]
    if all (== True) dirsExist then pure () else exit (ExitFailure 1)
    view $ inproc "nix-channel" ["--add", "https://github.com/rycee/home-manager/archive/master.tar.gz", "home-manager"] empty
    view $ inproc "nix-channel" ["--update"] empty
    view $ inshell "nix-shell '<home-manager>' -A install" empty

echoTxt :: Text -> IO ()
echoTxt = echo . unsafeTextToLine
