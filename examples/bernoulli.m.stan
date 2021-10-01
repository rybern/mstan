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
