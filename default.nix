{ mkDerivation, nixpkgs, base, containers, mtl, stdenv, stm, text }:
mkDerivation {
  pname = "web-plugins";
  version = "0.2.7";
  src = nixpkgs.haskellFilterSource [] ./.;
  buildDepends = [ base containers mtl stm text ];
  homepage = "http://www.happstack.com/";
  description = "dynamic plugin system for web applications";
  license = stdenv.lib.licenses.bsd3;
}
