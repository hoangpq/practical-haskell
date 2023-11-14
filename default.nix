{ mkDerivation, attoparsec, base, bytestring, containers
, contravariant-extras, errors, hasql, hasql-th, hslua
, http-client-tls, http-conduit, http-types, lens, lib
, monad-control, mtl, network-simple, random, sockets-and-pipes
, stm, string-conversions, time, transformers, vector, wai
, wai-cors, wai-extra, warp, websockets, list-transformer 
, foldl, unordered-containers, hashable, regex-tdfa
, regex-applicative, aeson, deriving-aeson, binary, validation
, megaparsec, parser-combinators
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
    warp websockets list-transformer foldl unordered-containers hashable 
    regex-tdfa regex-applicative aeson deriving-aeson binary validation 
    megaparsec parser-combinators
  ];
  license = "unknown";
  mainProgram = "practical-haskell";
}
