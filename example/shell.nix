{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, base, containers, happstack-hsp
      , happstack-server, hsp, hsx2hs, mtl, safecopy, stdenv, text
      , web-routes, web-routes-th, cabal-install
      }:
      mkDerivation {
        pname = "web-plugins-example";
        version = "0.1.0.0";
        src = ./.;
        configureFlags = [ "-fexample" ];
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          acid-state base containers happstack-hsp happstack-server hsp
          hsx2hs mtl safecopy text web-routes web-routes-th cabal-install
        ];
        description = "A demo of the web-plugins system";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
