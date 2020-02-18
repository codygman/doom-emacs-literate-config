{ config, pkgs, home, ... }:
with import <nixpkgs> {};
with lib;
let
  myEnv = builtins.getEnv "MYENV";
  emacsHEAD = import ./emacs.nix;
in
{
  imports = if myEnv != ""
               then if myEnv == "personal" then
                 lib.info "loading PERSONAL home manager environment"
                   [ ~/Sync/nix-home-manager-config/personal.nix ]
                    else
                      if myEnv == "work" then
                        lib.info "loading WORK home manager environment"
                          [ ~/Sync/nix-home-manager-config/work.nix ]
                      else
                        lib.warn "MYENV is not one of 'personal' or 'work', ONLY core home environment will be available!" []
               else
                 lib.warn "MYENV not specified, ONLY core home environment will be available!" [];
  nixpkgs.config.allowUnfree = true;
  programs = {
    home-manager.enable = true;
    emacs = {
      enable = true;
      package = emacsHEAD;
    };
    git = {
      enable = true;
      userName = "Cody Goodman";
    };
  };

  home = {
    packages = with pkgs; [
      ripgrep
      fd

      source-code-pro

      psensor
    ];
  };

  services = {
    redshift = {
      enable = true;
      provider = "geoclue2";
    };
    syncthing = {
      enable = true;
    };
  };
}
