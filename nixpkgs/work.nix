{ config, pkgs, home, ... }:
with import <nixpkgs> {};
with lib;
{
  imports = [
    ./core.nix
  ];
  programs =
    {
      git = {
        userEmail = "cody@workEmail.com";
      };
    };
  home = {
    packages = [ teams ];
  };
  services = {
  };
}
