########################################################################
# default.nix -- The top-level nix build file for animaliens.
#
# This file defines various attributes that are used for building and
# developing animaliens.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   animaliens: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix;

  inherit (packages) pkgs animaliens;
  project = animaliens.haskell.project;
in
{
  inherit pkgs animaliens;

  inherit project;
}
