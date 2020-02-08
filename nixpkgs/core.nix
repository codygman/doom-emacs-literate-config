{ config, pkgs, home, ... }:
with import <nixpkgs> {};
with lib;
{
  programs = {
    home-manager.enable = true;
    emacs.enable = true;
    git.enable = true;
  };
  home = {
    packages = with pkgs; [
      ripgrep
      source-code-pro
    ];
  };
  services = {
    redshift = {
      enable = true;
      provider = "geoclue2";
    };
  };
}
