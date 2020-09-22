############################################################################
# Darwin release cardano-rt-view-*.zip
#
# This bundles up the macOS executable with its dependencies
# and static directory. Result is *.zip file.
#
############################################################################

{ pkgs
, project
, exes
, staticDir
}:

let
  lib = pkgs.lib;
  name = "cardano-rt-view-${project.version}-darwin";
  rtViewServiceExe = lib.head (lib.filter (exe: lib.hasInfix "cardano-rt-view" exe.name) exes);

in pkgs.runCommand name {
    buildInputs = with pkgs.buildPackages; [
      binutils
      darwin.cctools
      nix
      haskellBuildUtils.package
      zip
    ];
  } ''
  mkdir -p $out release

  cd release
  mkdir ./static
  cp -R ${staticDir}/* ./static/

  cp ${rtViewServiceExe}/bin/* .

  chmod -R +w .

  rewrite-libs ./ ${rtViewServiceExe}/bin/*

  dist_file=$out/${name}.zip
  zip -r $dist_file .

  mkdir -p $out/nix-support
  echo "file binary-dist $dist_file" > $out/nix-support/hydra-build-products
''
