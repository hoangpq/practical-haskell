let
  pkgs = import <nixpkgs> { };
  pgrest = pkgs.haskellPackages.callPackage ./default.nix { };
  hsTools = with pkgs.haskellPackages; [
    cabal-install hpack 
    haskell-language-server 
    haskell-dap ghci-dap haskell-debug-adapter phoityne-vscode 
    hlint fmt random text-show cassava phoityne-vscode 
    sqlite-simple exceptions network-simple time
    attoparsec transformers string-conversions http-conduit http-types 
    bytestring errors hslua monad-control lens random hasql containers
    hasql-th vector warp wai wai-extra wai-cors http-types
    contravariant-extras stm websockets
  ];
in
  pkgs.lib.overrideDerivation pgrest.env (old: {
    nativeBuildInputs = with pkgs; [] ++ (with llvmPackages_14; [ clang clang-unwrapped lld llvm ]);
    buildInputs = old.buildInputs ++ [ 
      pkgs.pkg-config pkgs.zlib pkgs.zlib.dev pkgs.sqlite 
      pkgs.llvmPackages_14.clang-unwrapped
    ] ++ hsTools;
  })
