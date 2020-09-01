let
  haskellNixSrc = fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/tarball/403b1a1b848295b907f4ceaeb0b6508c3f97e3a3";
    sha256 = "1m32hz6zalfhywrzjir4gn81xbwp9vmxqgv3x746gb1zzmjqdhhs";
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
