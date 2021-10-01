# Installation
 * You'll have to have [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed and run `stack build` to compile `mstan`. When that finishes it should tell you where the binary was installed. `mstan` should be a symbolic link to that binary.
 * You can use `nix` to install the R dependencies by running everything inside a `nix-shell` (because it'll default to using `default.nix`).
 * You'll need to have Rscript, cmdstanr, and some other dependencies (see `default.nix` if you're having trouble finding dependencies).
 * You'll need to have `cmdstan` installed and have the `CMDSTAN` environment variable set so that `cmdstanr` can find it.
 
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
} generated quantities {
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

From root, run `python graph_search.py examples/bernoulli.m.stan examples/bernoulli_data.json`
The first argument is the modular stan file and the second is input data.

# Running the "Birthday" Example

I stuffed the 9 versions of the birthday model into a modular file: `examples/birthday/birthday.m.stan`. This is just to show that it works; we should do a more reasonable translation later.

The data for the birthday problem has been preprocessed. You can replicate this by running `Rscript prepare_birthday_data.R` inside examples/birthday. This converts the original `.csv` file into a `.json` file and adds all of the extra information expected by the Stan models (e.g. holidays).

To run the graph search, run `python graph_search.py examples/birthday/birthday.m.stan examples/birthday/births_usa_1969.json`.

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

The search selected the model with ID `Model:model8rhs`. To get the concrete Stan program with this `ID`, run `./mstan exec -f examples/birthday/birthday.m.stan get-model -s Model:model8rhs`.

# Troubleshooting
 * To see the command line calls that are being made to `elpd.R` and `mstan`, set `debugIO` in `graph_search.py` to `True`
