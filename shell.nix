{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, binary, bytestring, containers, mtl, stdenv, stm, text, http-types, cabal-install }:
      mkDerivation {
        pname = "web-plugins";
        version = "0.2.9.1";
        src = ./.;
        libraryHaskellDepends = [ base binary bytestring containers mtl stm text http-types cabal-install ];
        homepage = "https://github.com/clckwrks/web-plugins";
        description = "dynamic plugin system for web applications";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
