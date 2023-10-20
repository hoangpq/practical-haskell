{ mkDerivation, attoparsec, base, bytestring, containers
, contravariant-extras, errors, hasql, hasql-th, hslua
, http-client-tls, http-conduit, http-types, lens, lib
, monad-control, mtl, network-simple, random, sockets-and-pipes
, stm, string-conversions, time, transformers, vector, wai
, wai-cors, wai-extra, warp, websockets, list-transformer
}:
mkDerivation {
  pname = "practical-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring containers contravariant-extras errors
    hasql hasql-th hslua http-client-tls http-conduit http-types lens
    monad-control mtl network-simple random sockets-and-pipes stm
    string-conversions time transformers vector wai wai-cors wai-extra
    warp websockets list-transformer
  ];
  license = "unknown";
  mainProgram = "practical-haskell";
}
