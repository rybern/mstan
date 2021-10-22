data {
  matrix[3, 3] x;
}
model {
  x ~ Dist.Dist();
}
generated quantities {
  matrix[3, 3] log_lik;
  for (r in 1:3) {
    for (c in 1:3) {
      log_lik[r, c] = Dist.LogLikelihood(r, c);
    }
  }
}

module "a" Dist {
  Dist(matrix x) {
    x ~ poisson(1);
  }
  LogLikelihood(int r, int c) {
    return poisson_lpdf(x[r, c] | 1);
  }
}

module "b" Dist {
  Dist(matrix x) {
    x ~ normal(0, 1);
  }
  LogLikelihood(int r, int c) {
    return normal_lpdf(x[r, c] | 0, 1);
  }
}
