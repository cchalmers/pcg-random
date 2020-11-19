{ sources ? import nix/sources.nix }:

let pcg-random = import ./. { inherit sources; };
    pkgs = import sources.nixpkgs {};
    lib = pkgs.lib;
    hpkgs = pkgs.haskell.packages.ghc8102;

in hpkgs.shellFor {
  packages = ps: [ pcg-random ];
  buildInputs = [ hpkgs.cabal-install ];
}
