let derivation = import ./default.nix; in

with (import (builtins.fetchTarball {
  name = "nixpkgs-21.11";
  # Tarball of tagged release of Nixpkgs 21.11
  url = "https://github.com/NixOS/nixpkgs/archive/22.05.tar.gz";
  # Tarball hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik";
}) {});

stdenvNoCC.mkDerivation rec {
  name = "elpd-env";

  cmdstanr = pkgs.rPackages.buildRPackage {
    name = "cmdstanr";
    # src = fetchTarball "https://github.com/stan-dev/cmdstanr/archive/refs/tags/v0.4.0.tar.gz";
    src = fetchTarball "https://github.com/stan-dev/cmdstanr/archive/refs/tags/v0.5.2.tar.gz";
    propagatedBuildInputs = [];
    nativeBuildInputs = with pkgs.rPackages; [
      gcc
      checkmate
      jsonlite
      processx
      R6
      vroom
      data_table
      posterior
    ];
  };

  buildInputs = with pkgs.rPackages; [
    tidyverse
    loo
    abind
    distributional
    tensorA
    jsonlite
    data_table
    posterior
  ] ++ [
    cmdstanr
    R
    gcc
    python3
    gfortran
  ];
}
