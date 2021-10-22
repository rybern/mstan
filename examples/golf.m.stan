data {
  int J; // Number of distances
  int n[J]; // Number of shots at each distance
  vector[J] x; // Distances
  int y[J]; // Number of successes at each distance
}
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
module "Angular" P(x) {
  return PAngle(x);
}
module "PAngle" PAngle(x) {
  parameters {
    real sigma_angle;
  }
  real r = (1.68 / 2) / 12;
  real R = (4.25 / 2) / 12;
  vector[J] threshold_angle = asin((R-r) ./ x);
  vector[J] p_angle = 2*Phi(threshold_angle / sigma_angle) - 1;
  return p_angle;
}
module "AngleAndDistance" P (x) {
  return PAngle(x) + PDistance(x);
}
module "PDistance" PDistance(x) {
  parameters {
    real sigma_distance;
  }
  (real overshot, real distance_tolerance) = DistanceModel();
  sigma_distance ~ normal(0, 1);
  vector[J] p_distance = Phi((distance_tolerance - overshot)
                             ./ ((x + overshot)*sigma_distance))
    - Phi((- overshot) ./ ((x + overshot)*sigma_distance));
  return p_distance;
}
module "Fixed" DistanceModel() {
  real overshot = 1;
  real distance_tolerance 3;
  return (overshot, distance_tolerance);
}
module "Proportional" Successes(y | n, p) {
  parameters {
    real<lower=0> sigma_y;
  }
  sigma_y ~ normal(0, 1);

  vector[J] raw_proportions = to_vector(y) ./ to_vector(n);
  raw_proportions ~ normal(p, sqrt(p .* (1-p) ./ to_vector(n) + sigma_y^2));
}
module "Parametric" DistanceModel() {
  parameters {
    real overshot;
    real distance_tolerance;
  }
  overshot ~ normal(1, 5);
  distance_tolerance ~ normal(3, 5);
  return (overshot, distance_tolerance);
}
