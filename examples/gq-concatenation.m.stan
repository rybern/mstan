data {
  matrix[3, 3] x;
}
model {
  x ~ Dist();
}
generated quantities {
  println(x);
}

module "a" Dist(matrix x) {
  generated quantities {
    matrix[3, 3] log_lik;
    for (r in 1:3) {
      for (c in 1:3) {
        log_lik[r, c] = poisson_lpdf(x[r, c] | 1);
      }
    }
  }
  x ~ poisson(1);
}

module "b" Dist(matrix x) {
  generated quantities {
    matrix[3, 3] log_lik;
    for (r in 1:3) {
      for (c in 1:3) {
        log_lik[r, c] = normal_lpdf(x[r, c] | 0, 1);
      }
    }
  }
  x ~ normal(0, 1);
}
