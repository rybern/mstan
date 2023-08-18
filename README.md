## This repository contains:
 * **`mstan`**, a compiler that implements a "swappable module" system for [Stan](https://mc-stan.org/). See [this blog post](https://statmodeling.stat.columbia.edu/2021/11/19/drawing-maps-of-model-space-with-modular-stan/) or [paper Multi-Model Probabilistic Programming](https://arxiv.org/pdf/2208.06329.pdf) for an introduction and check out [the website](http://ryanbe.me/modular-stan.html) for interactive visualizations of modular programs.
 * **`model_search.py`**, a simple proof-of-concept model search for the network of models.
 * **`mstan-server`**, the backend server for [the Modular Stan website](http://ryanbe.me/modular-stan.html).
 
**Please keep in mind that this is a research prototype. You are likely to encounter bugs and unimplemented features.**

# Installation
 1. Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
 2. Make sure [Graphviz](https://graphviz.org/) is installed.
 3. Run `stack install`. This should build `mstan` and `mstan-server` and copy them to your PATH.
 4. To use `model_search.py` with ELPD evaluation, you'll need `Rscript`, `python`, `cmdstan`, and the following R dependencies: tidyverse, loo, abind, distributional, tensorA, jsonlite, data_table, cmdstanr, posterior.

    You can use [`nix`](https://nixos.org/download.html) to manage these dependencies with the `nix-shell` command. `nix-shell` will default to the appropriate environment specified by the `default.nix` file. You may still need to install `cmdstan` separately from `nix`.
 
# The `mstan` command-line interface
You can explore `mstan` usage by running `mstan --help`:
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
  concrete-model           Return the concrete Stan model given a model ID
  model-graph              Produce Graphviz image and text files of the model
                           graph of the modular Stan program.
  module-graph             Produce Graphviz image and text files of the module
                           graph of the modular Stan program.
  model-neighbors          Return the model IDs of the neighbors of the given
                           model
  any-model                Return an arbitrary model ID
  list-all-models          Return all model IDs
```

You can also use `--help` for individual commands, like `mstan concrete-model --help`.

### Model IDs
*Model IDs* are strings that are used to uniquely reference individual models in a network. For example, the following string is the ID of a model from the ["simple" example](http://ryanbe.me/modular-stan.html?example=simple) network:
```
Mean:standard,Stddev:lognormal,StddevInformative:yes
```
Model IDs are comma-separated lists of the module selections that make up that model; in this case `Mean` is implemented by `standard` and so on.

You can also find model ID of a selected model on the website in the text box above the module tree.

### Usage examples
These command-line examples will use the ["simple"](http://ryanbe.me/modular-stan.html?example=simple) example modular program.

Get an arbitrary model ID:
```
> mstan -f examples/simple.m.stan any-model
Mean:standard,Stddev:standard
```

Get the concrete Stan program for the model ID `Mean:standard,Stddev:standard`:
```
> mstan -f examples/simple.m.stan concrete-model -s Mean:standard,Stddev:standard
data {
  int N;
  vector[N] x;
}
model {
  x ~ normal(0, 1);
}
```

Get IDs of the models that neighbor `Mean:standard,Stddev:standard`:
```
> mstan -f examples/simple.m.stan model-neighbors -s Mean:standard,Stddev:standard
Mean:normal,Stddev:standard
Mean:standard,Stddev:lognormal,StddevInformative:no
Mean:standard,Stddev:lognormal,StddevInformative:yes
```

Get all model IDs and write them to "models.txt":
```
> mstan -f examples/simple.m.stan list-all-models -o models.txt
> cat models.txt
Mean:normal,Stddev:lognormal,StddevInformative:no
Mean:normal,Stddev:lognormal,StddevInformative:yes
Mean:normal,Stddev:standard
Mean:standard,Stddev:lognormal,StddevInformative:no
Mean:standard,Stddev:lognormal,StddevInformative:yes
Mean:standard,Stddev:standard
```

Produce an image of the model graph:
```
> mstan -f examples/simple.m.stan model-graph
model_graph.svg
```

Produce an image of the module tree:
```
> mstan -f examples/simple.m.stan module-tree
module_tree.svg
```

Print out diagnostics and produce an image of the module tree:
```
> mstan -f examples/simple.m.stan module-tree -v
============== Parsed program: ===============
signatures: ...
...
============== Modular tree:     ===============
(root)
  [Mean]
    (normal)
    (standard)
  [Stddev]
    (lognormal)
      [StddevInformative]
        (no)
        (yes)
    (standard)
============== Results:      ===============
module_tree.svg
```

# Using `graph_search.py`

## Basic example: "Bernoulli problem"

This small modular Stan program can be found at `example/bernoulli.m.stan`:
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

To run a greedy ELPD-maximizing graph search, from the project root directory run `python graph_search.py examples/bernoulli.m.stan examples/bernoulli_data.json`.

The first argument to `graph_search.py` is the modular stan file and the second is input data.

## Running the "Birthday" example

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

The search selected the model with ID `Model:model8rhs`. To get the concrete Stan program with this `ID`, run `mstan exec -f examples/birthday/birthday-trivial-translation.m.stan get-model -s Model:model8rhs`.

## Troubleshooting
 * To see the command line calls that are being made to `elpd.R` and `mstan`, set `DEBUG_IO` in `graph_search.py` to `True`
