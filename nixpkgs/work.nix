{ config, pkgs, home, ... }:
with import <nixpkgs> {};
with lib;
{
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
