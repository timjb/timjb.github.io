{ nixpkgs ? import ./nix/nixpkgs.nix { config = { allowUnfree = true; }; }, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  generator = haskellPackages.callCabal2nix "timbaumann" (./.) { };

in if pkgs.lib.inNixShell then generator.env else generator