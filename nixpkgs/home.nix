{ config, pkgs, home, ... }:
with import <nixpkgs> {};
with lib;
let
  core = import ./core.nix { config = config; pkgs=pkgs; home=home; };
  personal = import ./personal.nix { config = config; pkgs=pkgs; home=home; };
  work = import ./work.nix { config = config; pkgs=pkgs; home=home; };
  empty = {
    programs = {};
    home = {packages = [];};
    services = {};
  };
  currentEnv = if builtins.getEnv "MYENV" == "HOME"
               then
                 lib.info "loading HOME.nix home manager environment" personal
               else
                 lib.warn "MYENV not specified, ONLY core home environment will be available!" empty;
in
{
  imports = if builtins.getEnv "MYENV" == "HOME"
               then
                 lib.info "loading HOME.nix home manager environment"
                   [ ./personal.nix ]
               else
                 lib.warn "MYENV not specified, ONLY core home environment will be available!"
                   [ ./core.nix ];

  home = lib.mkMerge [ core.home currentEnv.home ];
  programs = lib.mkMerge [
    core.programs
    currentEnv.programs
  ];
  services = lib.mkMerge [
    core.services
    currentEnv.services
  ];
}
