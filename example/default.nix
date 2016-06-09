{ mkDerivation, acid-state, base, containers, happstack-hsp
, happstack-server, hsp, hsx2hs, mtl, safecopy, stdenv, text
, web-plugins, web-routes, web-routes-th
}:
mkDerivation {
  pname = "web-plugins-example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    acid-state base containers happstack-hsp happstack-server hsp
    hsx2hs mtl safecopy text web-plugins web-routes web-routes-th
  ];
  description = "A demo of the web-plugins system";
  license = stdenv.lib.licenses.bsd3;
}
