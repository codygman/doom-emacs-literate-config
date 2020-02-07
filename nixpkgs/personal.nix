{ config, pkgs, home, ... }:


with import <nixpkgs> {};
with lib;

{
  home = {
    packages = lib.mkMerge [
      []
      []
    ];
  };

  programs = lib.mkMerge [
    {}
    {}
  ];

  services = {
    redshift = {
      enable = true;
      temperature = {
        day = 6500;
        night = 3500;
      };
      brightness = {
        day = "1";
        night = "0.45";
      };
      # Dallas: 32.7763, -96.7969
      latitude = "33.7763";
      longitude = "-96.7969";
    };
  };

}
