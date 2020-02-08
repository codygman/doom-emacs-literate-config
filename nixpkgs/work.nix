{ config, pkgs, home, ... }:
with import <nixpkgs> {};
with lib;
{
  programs =
    {
      mercurial.enable = true;
    };
  home = {
    packages = [ teams ];
  };
  services = {
  };
}
