#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (ps: [ps.turtle])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/ae6bdcc53584aaf20211ce1814bea97ece08a248.tar.gz
{-# LANGUAGE OverloadedStrings #-}
-- NOTE: nixpkgs is pinned to nixos-unstable 2020-03-28
import Turtle
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
    -- guard True $ pure 1
    userHome <- home
    doomDir <- pwd
    let doomDirNixpkgs = doomDir </> decodeString "nixpkgs"
    let nixpkgConfigPath = userHome </> decodeString ".config/nixpkgs"
    testdir nixpkgConfigPath >>= \there -> do
      if there then do
        echo "WARNING: ~/.config/nixpkgs symlink already exists, using current nix files"
      else do
        echoTxt $ format ("Symlinked "%fp%" to "%fp) doomDirNixpkgs nixpkgConfigPath 
        symlink doomDirNixpkgs nixpkgConfigPath
    dirsExist <- sequenceA [ testdir doomDirNixpkgs
                           , testdir nixpkgConfigPath
                           ]
    if all (== True) dirsExist then pure () else exit (ExitFailure 1)
    view $ inproc "nix-channel" ["--add", "https://github.com/rycee/home-manager/archive/master.tar.gz", "home-manager"] empty
    view $ inproc "nix-channel" ["--update"] empty
    view $ inshell "nix-shell '<home-manager>' -A install" empty

echoTxt :: Text -> IO ()
echoTxt = echo . unsafeTextToLine
