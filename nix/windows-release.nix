############################################################################
# Windows release cardano-rt-view-*.zip
#
# This bundles up the windows executable with its dependencies
# and static directory.
#
############################################################################

{ pkgs
, project
, exes
, staticDir
}:

let
  lib = pkgs.lib;
  name = "cardano-rt-view-${project.version}-win64";
  rtViewServiceExe = lib.head (lib.filter (exe: lib.hasInfix "cardano-rt-view" exe.name) exes);

in pkgs.runCommand name {
    buildInputs = with pkgs.buildPackages; [
      zip
      haskellBuildUtils.package
    ];
  } ''
  mkdir -p $out release
  cd release

  cp -n --remove-destination -v ${rtViewServiceExe}/bin/* ./

  mkdir ./static
  cp -R ${staticDir}/* ./static/

  chmod -R +w .

  zip -r $out/${name}.zip .

  cd ..

  #dist_file=$(ls $out)
  mkdir -p $out/nix-support
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products

  cd /tmp
  mkdir -p output
  cp -vf "$out/${name}.zip" output/
  ls -lh output/
  pwd
''
