let
  hsPkgs = import ./default.nix {};
in
  hsPkgs.reanimate.components.all
