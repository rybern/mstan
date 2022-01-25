let derivation = import ./default.nix; in

with (import (builtins.fetchTarball {
  name = "nixpkgs-21.11";
  # Tarball of tagged release of Nixpkgs 21.11
  url = "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
  # Tarball hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
}) {});

stdenvNoCC.mkDerivation rec {
  name = "elpd-env";

  cmdstanr = pkgs.rPackages.buildRPackage {
    name = "cmdstanr";
    src = fetchTarball "https://github.com/stan-dev/cmdstanr/archive/refs/tags/v0.4.0.tar.gz";
    propagatedBuildInputs = [];
    nativeBuildInputs = with pkgs.rPackages; [
      gcc9
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
    gcc9
    python3
  ];
}
