// Probably should do binary implementations and unique signatures
// #3 needs to be an addition to intercept
// https://avehtari.github.io/casestudies/Birthdays/birthdays.html
// Need to go back and do generated quantities
// Need module fields

/* model 2 functions */
functions {
  vector diagSPD_EQ(real alpha, real rho, real L, int M) {
    return sqrt((alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho*pi()/2/L)^2 * linspaced_vector(M, 1, M)^2));
  }
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

/* model 1 functions */
/* functions { */
/*   vector diagSPD_EQ(real alpha, real rho, real L, int M) { */
/*     return sqrt((alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho*pi()/2/L)^2 * linspaced_vector(M, 1, M)^2)); */
/*   } */
/*   vector diagSPD_periodic(real alpha, real rho, int M) { */
/*     real a = 1/rho^2; */
/*     int one_to_M[M]; */
/*     for (m in 1:M) one_to_M[m] = m; */
/*     vector[M] q = sqrt(alpha^2 * 2 / exp(a) * to_vector(modified_bessel_first_kind(one_to_M, a))); */
/*     return append_row(q,q); */
/*   } */
/*   matrix PHI_EQ(int N, int M, real L, vector x) { */
/*     return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L); */
/*   } */
/*   matrix PHI_periodic(int N, int M, real w0, vector x) { */
/*     matrix[N,M] mw0x = diag_post_multiply(rep_matrix(w0*x, M), linspaced_vector(M, 1, M)); */
/*     return append_col(cos(mw0x), sin(mw0x)); */
/*   } */
/* } */

data {
  int<lower=1> N;      // number of observations
  vector[N] x;         // univariate covariate
  vector[N] y;         // target variable
  int day_of_week[N];
  int day_of_year[N];
  int memorial_days[20];
  int labor_days[40];
  int thanksgiving_days[40];

  real<lower=0> c_f1;  // factor c to determine the boundary value L
  int<lower=1> M_f1;   // number of basis functions for smooth function
  int<lower=1> J_f2;   // number of cos and sin functions for periodic
  real<lower=0> c_g3;  // factor c to determine the boundary value L
  int<lower=1> M_g3;   // number of basis functions for smooth function
  real scale_global;   // scale for the half-t prior for tau
}
transformed data {
  // Normalize data
  real xmean = mean(x);
  real ymean = mean(y);
  real xsd = sd(x);
  real ysd = sd(y);
  vector[N] xn = (x - xmean)/xsd;
  vector[N] yn = (y - ymean)/ysd;
}
model {
  yn ~ Regression(xn);
}

module "glm" Regression(yn | xn) {
  transformed data {
    int basis_size =
      LongTermTrend.BasisLength() +
      SeasonalTrend.BasisLength();
    matrix[N, basis_size] x_in_basis =
      SeasonalTrend.AddBasis(LongTermTrend.AddBasis(rep_matrix(0, N, 0)));
  }
  parameters {
    real sigma;
    real intercept;
  }
  generated quantities {
    vector[N] log_lik;
    vector[N] f = ymean +
      LongTermTrend.OriginalScaleFunction() +
      SeasonalTrend.OriginalScaleFunction();
    for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
  }
  intercept ~ normal(0, 1);
  sigma ~ normal(0, 0.5);
  yn ~ normal_id_glm(x_in_basis,
                     intercept,
                     SeasonalTrend.AddWeights(LongTermTrend.AddWeights(rep_matrix(0, 0, N)),
                     sigma);
}

module "yes" LongTermTrend {
  parameters {
    vector[M_f1] beta_f1;         // the basis functions coefficients
    real<lower=0> lengthscale_f1; // lengthscale of f1
    real<lower=0> sigma_f1;       // scale of f1
  }
  transformed parameters {
    vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  }
  AddBasis(basis) {
    real L_f1 = c_f1*max(xn);
    matrix[N,M_f1] PHI_f1 = PHI_EQ(N, M_f1, L_f1, xn);
    return (num_elements(basis) == 0) ? PHI_f1 : append_col(basis, PHI_f1);
  }
  BasisLength() {
    return M_f1;
  }
  AddWeights(weights) {
    lengthscale_f1 ~ lognormal(log(700/xsd), 1);
    sigma_f1 ~ normal(0, .5);
    beta_f1 ~ normal(0, 1);
    return (num_elements(weights) == 0) ? diagSPD_f1 .* beta_f1 : append_row(weights, diagSPD_f1 .* beta_f1);
  }
  OriginalScaleFunction() {
    return (intercept + PHI_f1 * (diagSPD_f1 .* beta_f1)) * ysd;
  }
}

module "no" LongTermTrend {
  AddBasis(basis) {
    return basis;
  }
  BasisLength() {
    return 0;
  }
  AddWeights(weights) {
    return weights;
  }
  OriginalScaleFunction() {
    vector[0] no_longterm_scale_function;
    return no_longterm_scale_function;
  }
}

module "yes" SeasonalTrend {
  parameters {
    vector[2*J_f2] beta_f2;       // the basis functions coefficients for f2
    real<lower=0> lengthscale_f2; //
    real<lower=0> sigma_f2;       // scale of f2
  }
  transformed parameters {
    vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
  }
  AddBasis(basis) {
    real period_year = 365.25/xsd;
    matrix[N,2*J_f2] PHI_f2 = PHI_periodic(N, J_f2, 2*pi()/period_year, xn);
    return (num_elements(basis) == 0) ? PHI_f2 : append_col(basis, PHI_f2);
  }
  BasisLength() {
    return 2*J_f2;
  }
  AddWeights(weights) {
    lengthscale_f2 ~ normal(0, .1);
    sigma_f2 ~ normal(0, 1);
    beta_f2 ~ normal(0, 1);
    return (num_elements(weights) == 0) ? diagSPD_f2 .* beta_f2 : append_row(weights, diagSPD_f2 .* beta_f2);
  }
  OriginalScaleFunction() {
    return (PHI_f2 * (diagSPD_f2 .* beta_f2))*ysd;
  }
}

module "no" SeasonalTrend {
  AddBasis(basis) {
    return basis;
  }
  BasisLength() {
    return 0;
  }
  AddWeights(weights) {
    return weights;
  }
  OriginalScaleFunction() {
    vector[0] no_seasonal_scale_function;
    return no_seasonal_scale_function;
  }
}
