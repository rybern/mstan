functions {
vector diagSPD_EQ(real alpha, real rho, real L, int M) {
  return sqrt((alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho*pi()/2/L)^2 * linspaced_vector(M, 1, M)^2));
}
/* real spd_Matt(real alpha, real rho, real w) { */
/*   real S = 4*alpha^2 * (sqrt(3)/rho)^3 * 1/((sqrt(3)/rho)^2 + w^2)^2; */
/*   return sqrt(S); */
/* } */
vector diagSPD_periodic(real alpha, real rho, int M) {
  real a = 1/rho^2;
  int one_to_M[M];
  for (m in 1:M) one_to_M[m] = m;
  vector[M] q = sqrt(alpha^2 * 2 / exp(a) * to_vector(modified_bessel_first_kind(one_to_M, a)));
  return append_row(q,q);
}
matrix PHI_EQ(int N, int M, real L, vector x) {
//  return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L);
  matrix[N,M] PHI = sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L);
  for (m in 1:M)
    PHI[,m] = PHI[,m] - mean(PHI[,m]);
  return PHI;
}
matrix PHI_periodic(int N, int M, real w0, vector x) {
  matrix[N,M] mw0x = diag_post_multiply(rep_matrix(w0*x, M), linspaced_vector(M, 1, M));
  matrix[N,M] PHI = append_col(cos(mw0x), sin(mw0x));
  for (m in 1:M)
    PHI[,m] = PHI[,m] - mean(PHI[,m]);
  return PHI;
}
}
data {
  int<lower=1> N;      // number of observations
  vector[N] x;         // univariate covariate
  vector[N] y;         // target variable
        
  real<lower=0> c_f1;  // factor c to determine the boundary value L
  int<lower=1> M_f1;   // number of basis functions for smooth function
  int<lower=1> J_f2;   // number of cos and sin functions for periodic
}
transformed data {
  // Normalize data
  real xmean = mean(x);
  real ymean = mean(y);
  real xsd = sd(x);
  real ysd = sd(y);
  vector[N] xn = (x - xmean)/xsd;
  vector[N] yn = (y - ymean)/ysd;
  // Basis functions for f1
  real L_f1 = c_f1*max(xn);
  matrix[N,M_f1] PHI_f1 = PHI_EQ(N, M_f1, L_f1, xn);
  // Basis functions for f2
  real period_year = 365.25/xsd;
  matrix[N,2*J_f2] PHI_f2 = PHI_periodic(N, J_f2, 2*pi()/period_year, xn);
  // Concatenated basis functions
  matrix[N,M_f1+2*J_f2] PHI_f = append_col(PHI_f1, PHI_f2);
}
parameters {
  real intercept;
  vector[M_f1] beta_f1;         // the basis functions coefficients for f1
  vector[2*J_f2] beta_f2;       // the basis functions coefficients for f2
  real<lower=0> lengthscale_f1; //
  real<lower=0> lengthscale_f2; //
  real<lower=0> sigma_f1;       // scale of f1
  real<lower=0> sigma_f2;       // scale of f2
  real<lower=0> sigma;          // residual scale
}
model {
  // spectral densities for f1 and f2
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
  // priors
  intercept ~ normal(0, 1);
  beta_f1 ~ normal(0, 1);
  beta_f2 ~ normal(0, 1);
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  lengthscale_f2 ~ normal(0, .1);
  sigma_f1 ~ normal(0, 1);
  sigma_f2 ~ normal(0, 1);
  sigma ~ normal(0, .5);
  // model
  yn ~ normal_id_glm(PHI_f,
		     intercept,
		     append_row(diagSPD_f1 .* beta_f1, diagSPD_f2 .* beta_f2),
		     sigma); 
}
generated quantities {
  vector[N] f1;
  vector[N] f2;
  vector[N] f;
  vector[N] log_lik;
  {
    // spectral densities for f1
    vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
    vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
    // functions scaled back to original scale
    f1 = (intercept + PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd;
    f2 = (PHI_f2 * (diagSPD_f2 .* beta_f2))*ysd;
    f = f1 + f2 + ymean;
    // log_liks for loo
    for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
  }
}
