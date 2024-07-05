{ mkDerivation, aeson, base, bytestring, conduit, containers
, http-client, http-client-tls, http-types, lib, lrucache
, monad-logger, mtl, network-uri, opt-env-conf, path, path-io
, retry, stm, tagsoup, text, unliftio
}:
mkDerivation {
  pname = "linkcheck";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring conduit containers http-client
    http-client-tls http-types lrucache monad-logger mtl network-uri
    opt-env-conf path path-io retry stm tagsoup text unliftio
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/linkcheck#readme";
  description = "Check for broken links in CI";
  license = lib.licenses.mit;
  mainProgram = "linkcheck";
}
