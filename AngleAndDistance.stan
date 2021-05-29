/*
  selectionsAngleAndDistanceBinomial :: Map SigName ImplName
  selectionsAngleAndDistanceBinomial = Map.fromList [
      ("Successes", "Binomial")
    , ("P", "AngleAndDistance")
    , ("PAngle", "PAngle")
    , ("PDistance", "PDistance")
    , ("DistanceModel", "Fixed")
    ]
*/

data {
  int J; // Number of distances
  int n[J]; // Number of shots at each distance
  vector[J] x; // Distances
  int y[J]; // Number of successes at each distance
}
parameters {
  real sigma_angle;
  real sigma_distance;
}
model {
  real overshot = 1;
  real distance_tolerance 3;

  x = x;
  (real overshot, real distance_tolerance) = (overshot, distance_tolerance);
  sigma_distance ~ normal(0, 1);
  vector[J] p_distance = Phi((distance_tolerance - overshot)
                             ./ ((x + overshot)*sigma_distance))
    - Phi((- overshot) ./ ((x + overshot)*sigma_distance));

  x = x;
  real r = (1.68 / 2) / 12;
  real R = (4.25 / 2) / 12;
  vector[J] threshold_angle = asin((R-r) ./ x);
  vector[J] p_angle = 2*Phi(threshold_angle / sigma_angle) - 1;

  p = p_angle + p_distance;
  y ~ binomial(n, p);
}
