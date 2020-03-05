{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.09.tar.gz) {}
, stdenv ? pkgs.overrideCC pkgs.stdenv pkgs.gcc7
}:

with pkgs;

stdenv.mkDerivation {
  name = "pbbsEnv";
  buildInputs = [ gcc ];
}
