{ config, pkgs, home, ... }:
with import <nixpkgs> {};
with lib;
{
  nixpkgs.config.allowUnfree = true;
  imports = [ ~/Sync/nix-home-manager-config/work.nix ];
  programs = {home-manager.enable = true;};
  home = {packages = with pkgs; [];};
}
