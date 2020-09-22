# our packages overlay
pkgs: _: with pkgs; {
  cardanoRTViewHaskellPackages = import ./haskell.nix {
    inherit config
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
