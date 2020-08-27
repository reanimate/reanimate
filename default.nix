let
  haskellNixSrc = fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/tarball/878121ff8e5a8a51ffb46f57d4b3b6d8e32bfb2e";
    sha256 = "0p18izavpwmhxz5x3crdcgk16rjz6ghm6cr136h52wa1nwahk1jk";
  };
  # haskellSrcMaster = builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;
  haskellNix = import haskellNixSrc {};

  all-hies = fetchTarball {
    # Insert the desired all-hies commit here
    url = "https://github.com/infinisil/all-hies/tarball/534ac517b386821b787d1edbd855b9966d0c0775";
    # Insert the correct hash after the first evaluation
    sha256 = "0bw1llpwxbh1dnrnbxkj2l0j58s523hjivszf827c3az5i4py1i2";
  };

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      (import all-hies {}).overlay
    ];
  });

  set = pkgs.haskell-nix.stackProject {
    name = "reanimate";
    compiler-nix-name = "ghc883";
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "reanimate";
      src = ./.;
    };
  };
in set.reanimate.components.library // {
  env = set.shellFor {
    packages = p: [ p.reanimate ];
    # exactDeps = true;
    nativeBuildInputs = [ pkgs.stack
                          pkgs.zlib.dev
                          pkgs.zlib.out
                          pkgs.gmp
                          pkgs.gnome3.librsvg
                          pkgs.blender
                          pkgs.povray
                          pkgs.ffmpeg
                          pkgs.texlive.combined.scheme-full
                        ];
    tools = {
      cabal = "3.2.0.0";
      hie = "unstable";
    };
    shellHook = ''
      export HIE_HOOGLE_DATABASE=$(realpath "$(dirname "$(realpath "$(which hoogle)")")/../share/doc/hoogle/default.hoo")
    '';
  };
}
