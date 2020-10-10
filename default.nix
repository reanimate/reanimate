let
  hackageSrc = fetchTarball {
    url = "https://github.com/input-output-hk/hackage.nix/tarball/f95145a966a728f203c42b45bd7a101d067d5ec5";
    sha256 = "1grr6l68j1sn79ig6qgj19agqyb24j7iibrim4r6vk0pr9i104za";
  };
  # Stackage isn't used right now.
  stackageSrc = fetchTarball {
    url = "https://github.com/input-output-hk/stackage.nix/tarball/f319b88ce4d3c0880287a96b9c54626802df1eb5";
    sha256 = "1shwq0faphycs6b3i2phvj0ram242vh02wmr5h2spxin1lda2995";
  };
  haskellSrc = fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/tarball/b406bb1d5bed581f651ae18a5d3ed07f47ace2b9";
    sha256 = "0lcb5p14097b1ya2flwkcbq65crc3k2fzb376bq8ck84i707shqi";
  };
  # haskellSrcMaster = builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;
  haskellNix = import haskellSrc {
    sourcesOverride = {
      hackage = hackageSrc;
      stackage = stackageSrc;
    };
  };

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

  set = pkgs.haskell-nix.cabalProject {
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
