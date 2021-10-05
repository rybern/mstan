let derivation = import ./default.nix; in

# with (import (builtins.fetchTarball {
#   name = "nixpkgs-19.09";
#   # Tarball of tagged release of Nixpkgs 19.09
#   url = "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz";
#   # Tarball hash obtained using `nix-prefetch-url --unpack <url>`
#   sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
# }) {});

with (import (builtins.fetchTarball {
  name = "nixpkgs-21.05";
  # Tarball of tagged release of Nixpkgs 21.05
  url = "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";
  # Tarball hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";
}) {});

stdenvNoCC.mkDerivation rec {
  name = "elpd-env";

  # rlang = pkgs.rPackages.buildRPackage {
  #      name = "rlang";
  #      src = fetchTarball "https://cran.r-project.org/src/contrib/rlang_0.4.7.tar.gz";
  #      propagatedBuildInputs = [];
  #      nativeBuildInputs = [];
  # };

  posterior = pkgs.rPackages.buildRPackage {
    name = "posterior";
    # src = fetchTarball  "https://mc-stan.org/r-packages/src/contrib/posterior_0.1.2.tar.gz";
    src = fetchTarball  "https://cran.r-project.org/src/contrib/posterior_1.1.0.tar.gz";
    propagatedBuildInputs = [];
    nativeBuildInputs = with pkgs.rPackages; [
      gcc9
      checkmate
      abind
      rlang
      tibble
      tensorA
      matrixStats
      distributional
      abind
    ];
  };

  cmdstanr = pkgs.rPackages.buildRPackage {
    name = "cmdstanr";
    # src = fetchTarball "https://mc-stan.org/r-packages/src/contrib/cmdstanr_0.1.0.tar.gz";
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
    ] ++ [
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
  ] ++ [
    cmdstanr
    posterior
    R
    gcc9
    python3
  ];

  CMDSTAN = "~/projects/cmdstan-g++10/";
}
