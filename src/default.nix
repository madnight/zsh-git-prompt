{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, directory, filepath, git, HUnit, parsec
      , process, QuickCheck, split, stdenv, system-filepath, unix
      }:
      mkDerivation {
        pname = "git-prompt";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base directory filepath git parsec process QuickCheck split
          system-filepath unix
        ];
        executableHaskellDepends = [
          base directory filepath git parsec process QuickCheck split
          system-filepath unix
        ];
        testHaskellDepends = [ base HUnit parsec process QuickCheck ];
        homepage = "http://github.com/olivierverdier/zsh-git-prompt#readme";
        description = "Informative git prompt for zsh";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
