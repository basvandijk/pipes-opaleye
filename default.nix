{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation
      , base
      , opaleye
      , pipes
      , pipes-safe
      , product-profunctors
      , exceptions
      , postgresql-simple
      , stdenv
      }:
      mkDerivation {
        pname = "pipes-opaleye";
        version = "0.1.0.0";
        sha256 = "";
        libraryHaskellDepends = [
          base
          opaleye
          pipes
          pipes-safe
          product-profunctors
          exceptions
          postgresql-simple
        ];
        homepage = "https://github.com/basvandijk/pipes-opaleye";
        description = "Stream opaleye query results using pipes";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
