{ config, pkgs, home, ... }:

with import <nixpkgs> {};
with lib;

let
  emacsHEAD = import ./emacs.nix;
  # TODO can't we get this from home.nix since this is being called from there?
  homeNixDir = builtins.toPath (builtins.getEnv "HOME") + ("/.config/nixpkgs/");
  myNixEnv = builtins.getEnv "MY_NIX_ENV";
  environmentSpecificNixFileName = if (myNixEnv == "WORK") then "work.nix" else throw "only WORK environment currently supported" ;
  environmentSpecificNixFile = homeNixDir + environmentSpecificNixFileName;
  currentEnvironment = import environmentSpecificNixFile { config = config; pkgs=pkgs; home=home; };
in
{
  nixpkgs.overlays = lib.mkMerge [
    [
      (self: super: {
        emacs = emacsHEAD;
        emacsWithPackages = (pkgs.emacsPackagesNgGen emacsHEAD).emacsWithPackages;
      } )
    ]
    currentEnvironment.overlays
  ];

  programs = {
    emacs = {
      enable = true;
      package = emacsHEAD;
    };
    git = {
      enable = true;
      userName = "Cody Goodman";
      extraConfig = {
        http = {
          sslCAinfo = "${ca-bundle_crt}";
          sslverify = true;
        };
      };
    };
  } // mkIf (builtins.getEnv "TRAVIS_OS_NAME" == "") {
    ssh = {
      enable = true;
      controlPath = "~/.ssh/master-%C";
    };
    vim = {enable = true;};
    jq = {enable = true;};
    gpg = {enable = true;};
    htop =  {enable = true;};
    firefox = {enable = true;};
    # doesn't exist anymore?
    # mu = {
    #   enable = true;
    # };
  };

  home = {
    packages = with pkgs; [

      ripgrep
      direnv
      pinentry
      fd
      gnumake
      file
    ] ++ (if builtins.getEnv "TRAVIS_OS_NAME" == "" then [

      dmenu
      feh
      mu
      pwgen
      source-code-pro

      haskellPackages.brittany
      haskellPackages.ghcid
      haskellPackages.hlint
      haskellPackages.hpack
      haskellPackages.lens
      haskellPackages.pandoc
      haskellPackages.stack

      python
      pkgs.python36Packages.virtualenv

      shellcheck
    ] else []);
  };

  # not needed since I'm not supporting non linux? wait... work mac?
  systemd.user.startServices = if stdenv.isLinux then true else false;

  home = {
    keyboard = mkIf stdenv.isLinux {
      # TODO test to see if this works on osx too
      layout = "us";
      options = [
        "ctrl:nocaps"
      ];
    };

    sessionVariables = {
      EDITOR = "emacsclient --create-frame --alternate-editor emacs";
    };
    file = {
      ".direnvrc" = {
        text = ''
        use nix
        layout_haskell() {
          PATH_add ~/.cabal/bin
          [ -d .cabal-sandbox ] || cabal sandbox init
          PATH_add .cabal-sandbox/bin
          export GHC_PACKAGE_PATH=$(cabal exec -- sh -c "echo \$GHC_PACKAGE_PATH")
        }
      '';
      };
    };
  };

  # email stuff is in work.nix/personal.nix appropriately
  # accounts.email.accounts = {};

  services = mkIf (builtins.getEnv "TRAVIS_OS_NAME" == ""  && stdenv.isLinux) {
    gpg-agent =  {
      enable = true;
      defaultCacheTtl = 600;
      enableSshSupport = true;
      extraConfig = ''
          allow-emacs-pinentry
          allow-loopback-pinentry
          pinentry-program ${pkgs.pinentry}/bin/pinentry
      '';
    };
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
    syncthing = { enable = true; };
  };

}
