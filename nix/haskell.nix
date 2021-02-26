############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ pkgs
, lib
, stdenv
, haskell-nix
, buildPackages
, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc8102"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
# Enable asserts for given packages
, assertedPackages ? []
# Version info, to be passed when not building from a git work tree
, gitrev ? null
, libsodium ? pkgs.libsodium
}:
let

  src = haskell-nix.haskellLib.cleanGit {
      name = "cardano-rt-view-src";
      src = ../.;
  };

  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject {
      inherit src;
      compiler-nix-name = compiler;
    }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    inherit src;
    compiler-nix-name = compiler;
    modules = [
      # Allow reinstallation of Win32
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
       nonReinstallablePkgs =
        [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          # ghcjs custom packages
          "ghcjs-prim" "ghcjs-th"
          "ghc-boot"
          "ghc" "array" "binary" "bytestring" "containers"
          "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          # "ghci" "haskeline"
          "hpc"
          "mtl" "parsec" "text" "transformers"
          "xhtml"
          # "stm" "terminfo"
        ];
      })
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;
      }
      # TODO: Compile all local packages with -Werror:
      { packages.cardano-rt-view.configureFlags = [
          "--ghc-option=-Wall"
        ];
      }
      {
        # systemd not needed here
        packages.cardano-config.flags.systemd = false;
      }
      # Musl libc fully static build
      (lib.optionalAttrs stdenv.hostPlatform.isMusl (let
        # Module options which adds GHC flags and libraries for a fully static build
        fullyStaticOptions = {
          enableShared = false;
          enableStatic = true;
        };
      in
        {
          packages = lib.genAttrs (projectPackages) (name: fullyStaticOptions);

          # Haddock not working and not needed for cross builds
          doHaddock = false;
        }
      ))

      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];
        packages.network-bsd.components.library.build-tools = lib.mkForce [];
      })
    ];
    # TODO add flags to packages (like cs-ledger) so we can turn off tests that will
    # not build for windows on a per package bases (rather than using --disable-tests).
    # configureArgs = lib.optionalString stdenv.hostPlatform.isWindows "--disable-tests";
  };

  # setGitRev is a postInstall script to stamp executables with
  # version info. It uses the "gitrev" argument, if set. Otherwise,
  # the revision is sourced from the local git work tree.
  gitrev' = if (gitrev == null)
    then buildPackages.commonLib.commitIdFromGitRepoOrZero ../.git
    else gitrev;
  haskellBuildUtils = buildPackages.haskellBuildUtils.package;
in
  pkgSet // {
    projectPackages = lib.getAttrs projectPackages pkgSet;
  }
