{ config, pkgs, home, ... }:
with import <nixpkgs> {};
with lib;

let
  core = import ./core.nix { config = config; pkgs=pkgs; home=home; };
  personal = import ./personal.nix { config = config; pkgs=pkgs; home=home; };
in
{

  home = {
    packages = lib.mkMerge [
      core.home.packages
      personal.home.packages
    ];
  };

  programs = lib.mkMerge [
    core.programs
    personal.programs
  ];

  services = lib.mkMerge [
    core.services
    personal.services
  ];
}
