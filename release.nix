############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-rt-view ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    inherit sourcesOverride;
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-rt-view.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling (default: linux)
, supportedCrossSystems ? [ (builtins.head supportedSystems) ]

# Cross compilation to Windows is currently only supported on linux.
, linuxBuild ? builtins.elem "x86_64-linux" supportedCrossSystems

# Cross compilation to Windows is currently only supported on linux.
, windowsBuild ? linuxBuild

, darwinBuild ? builtins.elem "x86_64-darwin" supportedCrossSystems

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

}:

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-rt-view;
  gitrev = cardano-rt-view.rev;
};

with pkgs.lib;

let

  # restrict supported systems to a subset where tests (if exist) are required to pass:
  testsSupportedSystems = intersectLists supportedSystems [ "x86_64-linux" "x86_64-darwin" ];
  # Recurse through an attrset, returning all derivations in a list matching test supported systems.
  collectJobs' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectJobs = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectJobs' package)
    ) ds);

  nonDefaultBuildSystems = tail supportedSystems;

  # Paths or prefixes of paths of derivations to build only on the default system (ie. linux on hydra):
  onlyBuildOnDefaultSystem = [ ["checks"] ];
  # Paths or prefix of paths for which cross-builds (mingwW64, musl64) are disabled:
  noCrossBuild = [ ["shell"] ]
    ++ onlyBuildOnDefaultSystem;
  noMusl64Build = [ ["checks"] ["tests"] ["benchmarks"] ["haskellPackages"] ]
    ++ noCrossBuild;

  # Remove build jobs for which cross compiling does not make sense.
  filterProject = noBuildList: mapAttrsRecursiveCond (a: !(isDerivation a)) (path: value:
    if (isDerivation value && (any (p: take (length p) path == p) noBuildList)) then null
    else value
  ) project;

  inherit (systems.examples) mingwW64 musl64;

  jobs = {
    native =
      let filteredBuilds = mapAttrsRecursiveCond (a: !(isList a)) (path: value:
        if (any (p: take (length p) path == p) onlyBuildOnDefaultSystem) then filter (s: !(elem s nonDefaultBuildSystems)) value else value)
        (packagePlatforms project);
      in (mapTestOn (__trace (__toJSON filteredBuilds) filteredBuilds));
    musl64 = mapTestOnCross musl64 (packagePlatformsCross (filterProject noMusl64Build));
    "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross (filterProject noCrossBuild));
    cardano-rt-view-service-win64-release = import ./nix/windows-release.nix {
      inherit pkgs project;
      exes = collectJobs jobs.${mingwW64.config}.exes.cardano-rt-view;
      staticDir = ./static;
    };
    cardano-rt-view-service-linux-release = import ./nix/linux-release.nix {
      inherit pkgs project;
      exes = collectJobs jobs.musl64.exes.cardano-rt-view;
      staticDir = ./static;
    };
    cardano-rt-view-service-darwin-release = import ./nix/darwin-release.nix {
      inherit (pkgsFor "x86_64-darwin") pkgs;
      inherit project;
      exes = filter (p: p.system == "x86_64-darwin") (collectJobs jobs.native.exes.cardano-rt-view);
      staticDir = ./static;
    };
  } // (mkRequiredJob (concatLists [
      (collectJobs jobs.native.checks)
      (collectJobs jobs.native.benchmarks)
      (collectJobs jobs.native.exes)
      (optional windowsBuild jobs.cardano-rt-view-service-win64-release)
      (optional linuxBuild jobs.cardano-rt-view-service-linux-release)
      (optional darwinBuild jobs.cardano-rt-view-service-darwin-release)
    ]));

in jobs
