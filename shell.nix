# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, sourcesOverride ? {}
, minimal ? false
, withHoogle ? (! minimal)
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
}:
with pkgs;
let
  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = cardanoRTViewHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    packages = _: lib.attrValues cardanoRTViewHaskellPackages.projectPackages;

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      cabal-install
      stylish-haskell
      nix
      niv
      pkgconfig
    ] ++ lib.optionals (! minimal) [
      ghcid
      pkgs.git
      hlint
      stylish-haskell
      weeder
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    inherit withHoogle;
  };

  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      niv
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';
  };

in

 shell // { inherit devops; }
