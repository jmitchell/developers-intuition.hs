#! /usr/bin/env nix-shell
#! nix-shell -i bash --pure -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [cabal2nix])"

ROOT="$(dirname $0)/../.."

pushd "${ROOT}/nix"
cabal2nix ../ > devs-intuit.nix
popd
