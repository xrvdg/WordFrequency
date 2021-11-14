{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, lib, servant, servant-server, text
      , unordered-containers, wai, warp, zlib, hspec
      }:
      mkDerivation {
        pname = "ordina";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        librarySystemDepends = [zlib];
        executableHaskellDepends = [
          base servant servant-server text unordered-containers wai warp hspec
          
        ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
