let
  pkgs = import <nixpkgs> { };
in
  pkgs.mkShell {
    inputsFrom = [ (pkgs.haskellPackages.callCabal2nix "practical-haskell" ./. { }).env ];
    buildInputs = (with pkgs.haskellPackages; [ 
      cabal-install haskell-language-server hlint 
    ]);
  }