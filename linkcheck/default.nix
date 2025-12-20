{ mkDerivation, aeson, base, bytestring, conduit, containers
, http-client, http-client-tls, http-types, lib, list-t, lrucache
, monad-logger, mtl, network-uri, opt-env-conf, path, path-io
, retry, stm, stm-containers, tagsoup, text, unliftio
, validity-network-uri
}:
mkDerivation {
  pname = "linkcheck";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring conduit containers http-client
    http-client-tls http-types list-t lrucache monad-logger mtl
    network-uri opt-env-conf path path-io retry stm stm-containers
    tagsoup text unliftio validity-network-uri
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/linkcheck#readme";
  description = "Check for broken links in CI";
  license = lib.licenses.mit;
  mainProgram = "linkcheck";
}
