with import <nixpkgs> {};

{
  octar = haskellPackages.callPackage ./pkg.nix {};
}
