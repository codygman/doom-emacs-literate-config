{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  core = import ./core.nix { config = config; pkgs=pkgs; home=home; };
  # figure out currentEnvironment and import it
  homeNixDir = builtins.toPath (builtins.getEnv "HOME") + ("/.config/nixpkgs/");
  myNixEnv = builtins.getEnv "MY_NIX_ENV";
  #environmentSpecificNixFileName = if (myNixEnv == "WORK") then "work.nix" else throw "only WORK environment currently supported" ;
  environmentSpecificNixFileName = if (myNixEnv == "PERSONAL") then "personal.nix" else throw "only PERSONAL environment currently supported" ;
  environmentSpecificNixFile = homeNixDir + environmentSpecificNixFileName;
  currentEnvironment = import environmentSpecificNixFile { config = config; pkgs=pkgs; home=home; };
in
{
  home = {
    packages = lib.mkMerge [
      []
      core.home.packages
      currentEnvironment.home.packages
    ];
  };

  nixpkgs.config = {allowUnfree = true;}; # allowBroken = true;};

  programs = lib.mkMerge [
    # TODO move this into core.nix
    {
      home-manager.enable = true; # Let Home Manager install and manage itself.
      # TODO move git back into core only when I get merges working
      git = {
        enable = true;
        userName = "codygman";
        userEmail = "codygman.consulting@gmail.com";
      };
    }
    core.programs
    currentEnvironment.programs
  ];

  services = lib.mkMerge [
    # TODO move this into core.nix
    {
    }
    currentEnvironment.services
  ];
}
