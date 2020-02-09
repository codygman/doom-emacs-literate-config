{ config, pkgs, home, ... }:
with import <nixpkgs> {};
with lib;

{
  programs =
    {
      pidgin.enable = true;
      obs-studio.enable = true;
      git = {
        userEmail = "cody@codygman.dev";
      };
      astroid.enable = true;
    };
  home = {
    packages = [ pong3d ];
    file = {
      ".bashrc" = {
        text = ''
        export MYENV="HOME"
        '';
      };
    };
  };
  services = {
    spotifyd.enable = true;
  };
}
