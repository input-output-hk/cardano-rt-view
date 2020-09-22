############################################################################
# Linux release cardano-rt-view-service-*.AppImage
#
# This bundles up the linux executable with its dependencies
# and static directory. Result is *.AppImage file.
#
############################################################################

{ pkgs
, project
, exes
, staticDir
}:

let
  lib = pkgs.lib;
  name = "cardano-rt-view-service-${project.version}-linux-x86_64";
  rtViewServiceExe = lib.head (lib.filter (exe: lib.hasInfix "cardano-rt-view-service" exe.name) exes);

in pkgs.runCommand name {
    buildInputs = with pkgs.buildPackages; [
      haskellBuildUtils.package
    ];
  } ''
  mkdir -p $out release

  cd release
  mkdir ./static
  cp -R ${staticDir}/* ./static/

  cp -n --remove-destination -v ${rtViewServiceExe}/bin/* ./

  chmod -R +w .

  dist_file=$out/${name}.tar.gz
  tar -czf $dist_file .

  mkdir -p $out/nix-support
  echo "file binary-dist $dist_file" > $out/nix-support/hydra-build-products
''
