{ config, pkgs, home, ... }:
with import <nixpkgs> {};
with lib;

{
  programs = [
    {
      pidgin.enable = true;
      obs-studio.enable = true;
    }
  ];

  home = {
    packages = [ steam ];
  };

  services = {
    spotifyd.enable = true;
  };
}
