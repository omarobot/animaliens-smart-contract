let
  packages = import ./.;
  inherit (packages) pkgs animaliens;
  inherit (animaliens) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with animaliens; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
    ];
  }
