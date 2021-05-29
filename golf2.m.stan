model {
  y ~ Successes(n, P(x));
}
module "Binomial" Successes(y | n, p) {
  y ~ binomial(n, p);
}
module "Logit" P(x) {
  parameters {
    real a;
    real b;
  }
  return logit(a + b*x);
}
