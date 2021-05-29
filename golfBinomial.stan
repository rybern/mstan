/*
  selectionsBinomialLogit :: Map SigName ImplName
  selectionsBinomialLogit = Map.fromList [
      ("Successes", "Binomial")
    , ("P", "Logit")
    ]
 */

data {
  int J; // Number of distances
  int n[J]; // Number of shots at each distance
  vector[J] x; // Distances
  int y[J]; // Number of successes at each distance
    real a;
    real b;
}
model {
  p = logit(a + b*x);
  y ~ binomial(n, p);
}
