// This example is borrowed from Andrew Gelman's case study: https://mc-stan.org/users/documentation/case-studies/golf.html
data {
  int J;        // Number of distances
  vector[J] x;  // Distances
  int n[J];     // Number of shots at each distance
  int y[J];     // Number of successful shots at each distance
}
model {
  y ~ NSuccesses(n, PSuccess(x));
}

module "binomial" NSuccesses(y | n, p) {
  y ~ binomial(n, p);
}

module "logistic" PSuccess(x) {
  parameters {
    real a;
    real b;
  }
  return logit(a + b*x);
}

// A shot is successful if its angle is good.
module "angle_only" PSuccess(x) {
  return PAngleSuccess(x);
}

// A shot's angle is good if the center of the ball would roll over the hole.
module "angle_success" PAngleSuccess(x) {
  parameters {
    real sigma_angle;
  }
  real r = (1.68 / 2) / 12;
  real R = (4.25 / 2) / 12;
  vector[J] threshold_angle = asin((R-r) ./ x);
  vector[J] p_angle = 2*Phi(threshold_angle / sigma_angle) - 1;
  return p_angle;
}

// A shot is successful if both its angle and its distance are good.
module "angle_and_distance" PSuccess(x) {
  return PAngleSuccess(x) + PDistanceSuccess(x);
}

// A shot's distance is good if it would roll to the hole or a little farther.
module "distance_success" PDistanceSuccess(x) {
  parameters {
    real sigma_distance;
  }
  real overshot = OvershootModel.Overshot();
  real distance_tolerance = OvershootModel.DistanceTolerance();
  sigma_distance ~ normal(0, 1);
  vector[J] p_distance = Phi((distance_tolerance - overshot)
                             ./ ((x + overshot)*sigma_distance))
    - Phi((- overshot) ./ ((x + overshot)*sigma_distance));
  return p_distance;
}

// Let's just guess the parameters of our overshooting model for now.
// The OvershootModel hole uses a feature called 'module fields'. It has two fields: "Overshot" and "DistanceTolerance". They can be called separately but they are chosen together.
module "fixed" OvershootModel {
  Overshot() {
    return 1;
  }
  DistanceTolerance() {
    return 3;
  }
}

// The binomial error model tries harder to fit distances with more shots.
// Let's modeling the proportion of successes as normal.
module "proportional" NSuccesses(y | n, p) {
  parameters {
    real<lower=0> sigma_y;
  }
  sigma_y ~ normal(0, 1);

  vector[J] raw_proportions = to_vector(y) ./ to_vector(n);
  raw_proportions ~ normal(p, sqrt(p .* (1-p) ./ to_vector(n) + sigma_y^2));
}

// What if we estimate the overshot parameters?
module "parametric" OvershootModel {
  parameters {
    real overshot_var;
    real distance_tolerance_var;
  }
  Overshot() {
    overshot_var ~ normal(1, 5);
    return overshot_var;
  }
  DistanceTolerance() {
    distance_tolerance_var ~ normal(3, 5);
    return distance_tolerance_var;
  }
}
