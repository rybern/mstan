This repository contains:
 * `mstan`, a compiler that implements "swappable modules" for the Stan language. See [this blog post](https://statmodeling.stat.columbia.edu/2021/11/19/drawing-maps-of-model-space-with-modular-stan/) for an introduction, and check out [the website](http://ryanbe.me/modular-stan.html) for online interactive visualizations of modular programs.
   `mstan` is built from the `lib` and `mstan` directories with `stack` (instructions below).
 * `mstan-server`, a backend WebSocket server for the http://ryanbe.me/modular-stan.html website. `mstan-server` is built from the `lib` and `mstan-server` directories with `stack` (instructions below).
 * `model_search.py`, a simple proof-of-concept model search for the network of models. `model_search.py` has its own command-line interface and requires `mstan` to operate.

# Installation
 1. Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
 3. Build the `mstan` compiler. [Graphviz](https://graphviz.org/) is a dependency. Easiest is to run `stack install`, which builds the program and tries to add the executable to your PATH. Another option is to use `stack build`, which will print the installation location, and then add that location to your PATH or copy the binary to where you need it.
 4. To use ELPD evaluation, you'll need R dependencies: Rscript, cmdstanr, and some other dependencies (see `default.nix` if you're having trouble finding dependencies). If you want, you can use `nix` to install the R dependencies by running everything inside a `nix-shell` (it'll default to setting up the appropriate environment using `default.nix`). If you use nix, you should comment out the last line of `default.nix` that sets the `CMDSTAN` variable - sorry about that.
 5. You'll also need to have `cmdstan` installed and have the `CMDSTAN` environment variable set so that `cmdstanr` can find it. I don't think `nix` currently properly installs `cmdstan`.
 
# Using the `mstan` command-line interface
You can explore `mstan` commands with `mstan --help`. You can also use help on partial commands, like `mstan --help`.

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
  get-neighbors            Get model IDs of the neighbors of a model
  get-model                Get the concrete Stan model given a model ID
  get-module-graph         Generate a representation of the module graph of the
                           modular Stan program.
  get-model-graph          Generate a representation of the model graph of the
                           modular Stan program.
  get-first-model          Get an arbitrary model ID
  get-all-models           Get all model IDs
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
