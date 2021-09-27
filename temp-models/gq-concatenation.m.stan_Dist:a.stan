data {
  matrix[3, 3] x;
}
parameters {
}
model {
  x ~ poisson(1);
}
generated quantities {
  println(x);
  matrix[3, 3] log_lik;
  for (r in 1:3) {
    for (c in 1:3) {
      log_lik[r, c] = poisson_lpdf(x[r, c] | 1);
    }
  }
}
