data {
  int N;
  vector[N] x;
}
model {
  x ~ normal(Mean(), Stddev());
}

module "standard" Mean() {
  return 0;
}

module "standard" Stddev() {
  return 1;
}

module "normal" Mean() {
  parameters {
    real mu;
  }
  mu ~ normal(0, 1);
  return mu;
}

module "lognormal" Stddev() {
  parameters {
    real<lower=0> sigma;
  }
  sigma ~ lognormal(0, StddevInformative());
  return sigma;
}

module "yes" StddevInformative() {
  return 1;
}

module "no" StddevInformative() {
  return 100;
}
