This repository contains:
 * `mstan`, a compiler that implements "swappable modules" for the Stan language. See [this blog post](https://statmodeling.stat.columbia.edu/2021/11/19/drawing-maps-of-model-space-with-modular-stan/) for an introduction, and check out [the website](http://ryanbe.me/modular-stan.html) for online interactive visualizations of modular programs.
   `mstan` is built from the `lib` and `mstan` directories with `stack` (instructions below).
 * `mstan-server`, a backend WebSocket server for the http://ryanbe.me/modular-stan.html website. `mstan-server` is built from the `lib` and `mstan-server` directories with `stack` (instructions below).
 * `model_search.py`, a simple proof-of-concept model search for the network of models. `model_search.py` has its own command-line interface and requires `mstan` to operate.

# Installation
 1. Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
 2. Make sure [Graphviz](https://graphviz.org/) is installed.
 3. Run `stack install`. This should build `mstan` and `mstan-server` and copy them to your PATH.
 4. To use `model_search.py` with ELPD evaluation, you'll need `Rscript`, `python`, `cmdstan`, and the following R dependencies: 
    * tidyverse
    * loo
    * abind
    * distributional
    * tensorA
    * jsonlite
    * data_table
    * cmdstanr
    * posterior
    If `cmdstanr` has trouble finding `cmdstan`, make sure the `CMDSTAN` environment variable is set correctly.
    You can use [`nix`](https://nixos.org/download.html) to manage these dependencies by running everything inside a `nix-shell`. `nix-shell` will default to the appropriate environment by reading the `default.nix` file. You may still need to install `cmdstan` separately from `nix`.
 
# Using the `mstan` command-line interface
You can explore `mstan` usage with `mstan --help`. You can also get help on commands, like `mstan concrete-model --help`.

Here is the output of `mstan --help`:
```
Usage: mstan (-f|--modular-stan-file FILE) [-v|--debug-parse] 
             [-o|--output-file FILE] COMMAND
  Execute model network command

Available options:
  -f,--modular-stan-file FILE
                           File path of the input modular Stan file
  -v,--debug-parse         Show parsed modular program data structure
  -o,--output-file FILE    Output file path
  -h,--help                Show this help text

Available commands:
  neighbors                Return the model IDs of the neighbors of the given
                           model
  concrete-model           Return the concrete Stan model given a model ID
  module-graph             Produce Graphviz image and text files of the module
                           graph of the modular Stan program.
  model-graph              Produce Graphviz image and text files of the model
                           graph of the modular Stan program.
  first-model              Return an arbitrary model ID
  list-models              Return all model IDs
```

# Running the "Bernoulli" Example

Basic example with this modular stan program:
```
data {
  int<lower=0> N;
  array[N] int<lower=0,upper=1> y; // or int<lower=0,upper=1> y[N];
}
parameters {
  real<lower=0,upper=1> theta;
}
model {
  theta ~ ThetaPrior();  // uniform prior on interval 0,1
  y ~ bernoulli(theta);
}
generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = bernoulli_lpmf(y[i] | theta);
  }
}

module "informative" ThetaPrior(theta) {
  // Bias theta towards zero
  theta ~ beta(1, 4);
}
module "uninformative" ThetaPrior(theta) {
  theta ~ beta(1, 1);
}
```

From root, run `python graph_search.py examples/bernoulli.m.stan examples/bernoulli_data.json`.
The first argument is the modular stan file and the second is input data.

# Running the "Birthday" Example

My translation of the birthday case study into a modular Stan program for can be found at: `examples/birthday/birthday.m.stan`.

The data for the birthday problem has been pre-processed. You can replicate the pre-processing by running `Rscript prepare_birthday_data.R` inside `examples/birthday`. This converts the original `.csv` file into a `.json` file and adds all of the extra information expected by the Stan models (e.g. holidays).

There is also a simpler translation of the birthday problem at `examples/birthday/birthday-trivial-translation.m.stan` To run the graph search on this trivial example, execute `python graph_search.py examples/birthday/birthday-trivial-translation.m.stan examples/birthday/births_usa_1969.json`.

Here are example results:
```
Visiting:
	Model ID:	 Model:model1
	Score:		 6718.1204643854
	Push neighbor:	 Model:model2 5298.35671164131
	Push neighbor:	 Model:model3 13127.6477332358
	Push neighbor:	 Model:model4 13586.8301927714
	Push neighbor:	 Model:model5 -167840.2708031
	Push neighbor:	 Model:model6 13973.7607351042
	Push neighbor:	 Model:model7 10402.3775114932
	Push neighbor:	 Model:model8 15306.1033233817
	Push neighbor:	 Model:model8rhs 15391.7673968433
Visiting:
	Model ID:	 Model:model8rhs
	Score:		 15391.7673968433
Visiting:
	Model ID:	 Model:model8
	Score:		 15306.1033233817

Winner:
	Model ID:	 Model:model8rhs
	Score:		 15391.7673968433
  
9 scores
2 expands
```

The search selected the model with ID `Model:model8rhs`. To get the concrete Stan program with this `ID`, run `mstan exec -f examples/birthday/birthday.m.stan get-model -s Model:model8rhs`.

# Troubleshooting
 * To see the command line calls that are being made to `elpd.R` and `mstan`, set `DEBUG_IO` in `graph_search.py` to `True`
