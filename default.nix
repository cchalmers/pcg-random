{ sources ? import nix/sources.nix }:

let pkgs = import sources.nixpkgs {};
    lib = pkgs.lib;
    hpkgs = pkgs.haskell.packages.ghc8102;

    cleanSource = name: type: let
      baseName = baseNameOf (toString name);
    in lib.cleanSourceFilter name type && !(
      (type == "directory" && (builtins.elem baseName [ ".stack-work" "dist" "dist-newstyle"])) ||
      builtins.any (lib.flip lib.hasSuffix baseName) [ ".hi" ".ipynb" ".nix" ".sock" ".yaml" ".yml" ".local"]
    );
    mySourceFilter = src: name: type: let
      relPath = lib.removePrefix (toString src + "/") (toString name);
    in cleanSource name type && ( builtins.any (lib.flip lib.hasPrefix relPath) [
        "src" "c" "test" "Setup.hs" "pcg-random.cabal" "LICENSE"
    ]);

    src = builtins.filterSource (mySourceFilter ./.) ./.;

in  hpkgs.callCabal2nix "pcg-random" src {}
