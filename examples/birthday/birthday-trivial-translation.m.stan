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
model {
  Model();
}

module "model1" Model() {
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
      return sin(diag_post_multiply(rep_matrix(pi()/(2*L) * (x+L), M), linspaced_vector(M, 1, M)))/sqrt(L);
    }
    matrix PHI_periodic(int N, int M, real w0, vector x) {
      matrix[N,M] mw0x = diag_post_multiply(rep_matrix(w0*x, M), linspaced_vector(M, 1, M));
      return append_col(cos(mw0x), sin(mw0x));
    }
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
  }
  parameters {
    real intercept;               //
    vector[M_f1] beta_f1;         // the basis functions coefficients
    real<lower=0> lengthscale_f1; // lengthscale of f1
    real<lower=0> sigma_f1;       // scale of f1
    real<lower=0> sigma;          // residual scale
  }
  generated quantities {
    vector[N] f;
    vector[N] log_lik;
    {
      // spectral densities for f1
      vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
      // function scaled back to original scale
      f = (intercept + PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd + ymean;
      // log_liks for loo
      for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
    }
  }
  // spectral densities for f1
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  // priors
  intercept ~ normal(0, 1);
  beta_f1 ~ normal(0, 1);
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  sigma_f1 ~ normal(0, .5);
  sigma ~ normal(0, .5);
  // model
  yn ~ normal_id_glm(PHI_f1, intercept, diagSPD_f1 .* beta_f1, sigma); 
}

module "model2" Model() {
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

module "model3" Model() {
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
    real intercept0;
    vector[M_f1] beta_f1;         // the basis functions coefficients for f1
    vector[2*J_f2] beta_f2;       // the basis functions coefficients for f2
    vector[6] beta_f3;            // day of week effect
    real<lower=0> lengthscale_f1; //
    real<lower=0> lengthscale_f2; //
    real<lower=0> sigma_f1;       // scale of f1
    real<lower=0> sigma_f2;       // scale of f2
    real<lower=0> sigma;          // residual scale
  }
  generated quantities {
    vector[N] f1;
    vector[N] f2;
    vector[N] f;
    vector[7] f_day_of_week;
    vector[N] log_lik;
    {
      // spectral densities for f1 and f2
      vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
      vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
      // day of week effect with Monday effect set to 0
      vector[7] f_day_of_week_n = append_row(0, beta_f3);
      vector[N] intercept = intercept0 + f_day_of_week_n[day_of_week];
      // functions scaled back to original scale
      f_day_of_week = f_day_of_week_n*ysd;
      f1 = (intercept0 + PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd;
      f2 = (PHI_f2 * (diagSPD_f2 .* beta_f2))*ysd;
      f = f1 + f2 + (intercept-intercept0)*ysd + ymean;
      // log_liks for loo
      for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
    }
  }
  // spectral densities for f1 and f2
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
  // day of week effect with Monday effect set to 0
  vector[7] f_day_of_week = append_row(0, beta_f3);
  vector[N] intercept = intercept0 + f_day_of_week[day_of_week];
  // priors
  intercept0 ~ normal(0, 1);
  beta_f1 ~ normal(0, 1);
  beta_f2 ~ normal(0, 1);
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  lengthscale_f2 ~ normal(0, .1);
  sigma_f1 ~ normal(0, 1);
  sigma_f2 ~ normal(0, 1);
  beta_f3 ~ normal(0, 1);
  sigma ~ normal(0, 0.5);
  // model
  yn ~ normal_id_glm(PHI_f,
                     intercept,
                     append_row(diagSPD_f1 .* beta_f1, diagSPD_f2 .* beta_f2),
                     sigma);
}

module "model4" Model() {
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
  transformed data {
    // Normalize data
    real xmean = mean(x);
    real ymean = mean(y);
    real xsd = sd(x);
    real ysd = sd(y);
    vector[N] xn = (x - xmean)/xsd;
    vector[N] yn = (y - ymean)/ysd;
    real xmax = max(x);
    vector[N] x1 = x/xmax;
    // Basis functions for f1
    real L_f1 = c_f1*max(xn);
    matrix[N,M_f1] PHI_f1 = PHI_EQ(N, M_f1, L_f1, xn);
    // Basis functions for f2
    real period_year = 365.25/xsd;
    matrix[N,2*J_f2] PHI_f2 = PHI_periodic(N, J_f2, 2*pi()/period_year, xn);
    // Basis functions for g3
    real L_g3= c_g3*max(xn);
    matrix[N,M_g3] PHI_g3 = PHI_EQ(N, M_g3, L_g3, xn);
    // Concatenated basis functions for f1 and f2
    matrix[N,M_f1+2*J_f2] PHI_f = append_col(PHI_f1, PHI_f2);
  }
  parameters {
    real intercept0;
    vector[M_f1] beta_f1;         // the basis functions coefficients for f1
    vector[2*J_f2] beta_f2;       // the basis functions coefficients for f2
    vector[6] beta_f3;            // day of week effect
    vector[M_g3] beta_g3;         // the basis functions coefficients for g3
    real<lower=0> lengthscale_f1; //
    real<lower=0> lengthscale_f2; //
    real<lower=0> lengthscale_g3; //
    real<lower=0> sigma_f1;       // scale of f1
    real<lower=0> sigma_f2;       // scale of f2
    real<lower=0> sigma_g3;       // scale of g3
    real<lower=0> sigma;          // residual scale
  }
  generated quantities {
    vector[N] f1;
    vector[N] f2;
    vector[N] f3;
    vector[N] f;
    vector[7] f_day_of_week = append_row(0, beta_f3);
    vector[N] log_lik;
    {
      // spectral densities
      vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
      vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
      vector[M_g3] diagSPD_g3 = diagSPD_EQ(sigma_g3, lengthscale_g3, L_g3, M_g3);
      // day of week effect with increasing magnitude (Monday set to 0)
      vector[N] g3 = PHI_g3 * (diagSPD_g3 .* beta_g3);
      // functions scaled back to original scale
      f1 = (intercept0 + PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd;
      f2 = (PHI_f2 * (diagSPD_f2 .* beta_f2))*ysd;
      f3 = exp(g3).*f_day_of_week[day_of_week]*ysd;
      f_day_of_week = append_row(0, beta_f3)*ysd;
      f = f1 + f2 + f3 + ymean;
      // log_liks for loo
      for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
    }
  }
  // spectral densities for f1, f2, and g3
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
  vector[M_g3] diagSPD_g3 = diagSPD_EQ(sigma_g3, lengthscale_g3, L_g3, M_g3);
  // day of week effect with increasing magnitude (Monday set to 0)
  vector[7] f_day_of_week = append_row(0, beta_f3);
  vector[N] g3 = PHI_g3 * (diagSPD_g3 .* beta_g3);
  vector[N] intercept = intercept0 + exp(g3).*f_day_of_week[day_of_week];
  // priors
  intercept0 ~ normal(0, 1);
  beta_f1 ~ normal(0, 1);
  beta_f2 ~ normal(0, 1);
  beta_f3 ~ normal(0, 1);
  beta_g3 ~ normal(0, 1);
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  lengthscale_f2 ~ normal(0, .1);
  lengthscale_g3 ~ lognormal(log(7000/xsd), 1);
  sigma_f1 ~ normal(0, 1);
  sigma_f2 ~ normal(0, 1);
  sigma_g3 ~ normal(0, 0.1);
  sigma ~ normal(0, 0.5);
  // model
  yn ~ normal_id_glm(PHI_f,
                     intercept,
                     append_row(diagSPD_f1 .* beta_f1, diagSPD_f2 .* beta_f2),
                     sigma);
}

module "model5" Model() {
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
  transformed data {
    // Normalize data
    real xmean = mean(x);
    real ymean = mean(y);
    real xsd = sd(x);
    real ysd = sd(y);
    vector[N] xn = (x - xmean)/xsd;
    vector[N] yn = (y - ymean)/ysd;
    real xmax = max(x);
    vector[N] x1 = x/xmax;
    real nu_global = 1;	   // degrees of freedom for the half-t priors for tau
    real nu_local = 1;       // for the regularized horseshoe
    real slab_scale = 1;     // for the regularized horseshoe
    real slab_df = 100;      // for the regularized horseshoe
    // Basis functions for f1
    real L_f1 = c_f1*max(xn);
    matrix[N,M_f1] PHI_f1 = PHI_EQ(N, M_f1, L_f1, xn);
    // Basis functions for f2
    real period_year = 365.25/xsd;
    matrix[N,2*J_f2] PHI_f2 = PHI_periodic(N, J_f2, 2*pi()/period_year, xn);
    // Basis functions for g3
    real L_g3= c_g3*max(xn);
    matrix[N,M_g3] PHI_g3 = PHI_EQ(N, M_g3, L_g3, xn);
    // Concatenated basis functions for f1 and f2
    matrix[N,M_f1+2*J_f2] PHI_f = append_col(PHI_f1, PHI_f2);
  }
  parameters {
    real intercept0;
    vector[M_f1] beta_f1;         // the basis functions coefficients for f1
    vector[2*J_f2] beta_f2;       // the basis functions coefficients for f2
    vector[6] beta_f3;            // day of week effect
    vector[M_g3] beta_g3;         // the basis functions coefficients for g3
    real<lower=0> lengthscale_f1; //
    real<lower=0> lengthscale_f2; //
    real<lower=0> lengthscale_g3; //
    real<lower=0> sigma_f1;       // scale of f1
    real<lower=0> sigma_f2;       // scale of f2
    real<lower=0> sigma_g3;       // scale of g3
    // regularized horseshoe
    real <lower=0> tau_f4;         // global shrinkage parameter
    vector <lower=0>[366] lambda_f4; // local shrinkage parameter
    real<lower=0> caux_f4;
    vector<multiplier=sqrt( (slab_scale * sqrt(caux_f4))^2 * square(lambda_f4) ./ ((slab_scale * sqrt(caux_f4))^2 + tau_f4^2*square(lambda_f4)))*tau_f4>[366] beta_f4;
    real<lower=0> sigma;          // residual scale
  }
  generated quantities {
    vector[N] f1;
    vector[N] f2;
    vector[N] f3;
    vector[N] f;
    vector[7] f_day_of_week;
    vector[N] log_lik;
    {
      // spectral densities
      vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
      vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
      vector[M_g3] diagSPD_g3 = diagSPD_EQ(sigma_g3, lengthscale_g3, L_g3, M_g3);
      // day of week effect with increasing magnitude (Monday set to 0)
      vector[N] g3 = PHI_g3 * (diagSPD_g3 .* beta_g3);
      vector[7] f_day_of_week_n = append_row(0, beta_f3);
      // functions scaled back to original scale
      f1 = (intercept0 + PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd;
      f2 = (PHI_f2 * (diagSPD_f2 .* beta_f2))*ysd;
      f3 = exp(g3).*f_day_of_week_n[day_of_week]*ysd;
      f_day_of_week = f_day_of_week_n*ysd;
      f = f1 + f2 + f3 + beta_f4[day_of_year] + ymean;
      // log_liks for loo
      for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
    }
  }
  // spectral densities
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
  vector[M_g3] diagSPD_g3 = diagSPD_EQ(sigma_g3, lengthscale_g3, L_g3, M_g3);
  // day of week effect with increasing magnitude (Monday set to 0)
  vector[7] f_day_of_week = append_row(0, beta_f3);
  vector[N] g3 = PHI_g3 * (diagSPD_g3 .* beta_g3);
  vector[N] intercept = intercept0 + exp(g3).*f_day_of_week[day_of_week];
  intercept += beta_f4[day_of_year];
  real c_f4 = slab_scale * sqrt(caux_f4); // slab scale
  // priors
  intercept0 ~ normal(0, 1);
  beta_f1 ~ normal(0, 1);
  beta_f2 ~ normal(0, 1);
  beta_f3 ~ normal(0, 1);
  beta_g3 ~ normal(0, 1);
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  lengthscale_f2 ~ normal(0, .1);
  lengthscale_g3 ~ lognormal(log(7000/xsd), 1);
  sigma_f1 ~ normal(0, 1);
  sigma_f2 ~ normal(0, 1);
  sigma_g3 ~ normal(0, 0.1);
  // regularized horseshoe
  beta_f4 ~ normal(0, sqrt( c_f4^2 * square(lambda_f4) ./ (c_f4^2 + tau_f4^2*square(lambda_f4)))*tau_f4);
  lambda_f4 ~ student_t(nu_local, 0, 1);
  tau_f4 ~ student_t(nu_global, 0, scale_global*2);
  caux_f4 ~ inv_gamma(0.5*slab_df, 0.5*slab_df);
  sigma ~ normal(0, 0.5);
  // model
  yn ~ normal_id_glm(PHI_f,
                     intercept,
                     append_row(diagSPD_f1 .* beta_f1, diagSPD_f2 .* beta_f2),
                     sigma);
}

module "model6" Model() {
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
    // Concatenated basis functions for f1 and f2
    matrix[N,M_f1+2*J_f2] PHI_f = append_col(PHI_f1, PHI_f2);
  }
  parameters {
    real intercept0;
    vector[M_f1] beta_f1;         // the basis functions coefficients for f1
    vector[2*J_f2] beta_f2;       // the basis functions coefficients for f2
    vector[6] beta_f3;            // day of week effect
    vector[366] beta_f4;          // day of year effect
    real<lower=0> lengthscale_f1; //
    real<lower=0> lengthscale_f2; //
    real<lower=0> sigma_f1;       // scale of f1
    real<lower=0> sigma_f2;       // scale of f2
    real<lower=0> sigma_f4;       // scale of day of year effect
    real<lower=0> sigma;          // residual scale
  }
  generated quantities {
    vector[N] f1;
    vector[N] f2;
    vector[N] f;
    vector[7] f_day_of_week;
    vector[N] log_lik;
    {
      // spectral densities for f1 and f2
      vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
      vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
      // day of week and day of year effects
      vector[7] f_day_of_week_n = append_row(0, beta_f3);
      vector[N] intercept = intercept0 + f_day_of_week_n[day_of_week] + beta_f4[day_of_year];
      f_day_of_week = f_day_of_week_n*ysd;
      // functions scaled back to original scale
      f1 = (intercept0 + PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd;
      f2 = (PHI_f2 * (diagSPD_f2 .* beta_f2))*ysd;
      f = f1 + f2 + (intercept-intercept0)*ysd + ymean;
      // log_liks for loo
      for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
    }
  }
  // spectral densities for f1 and f2
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
  // day of week and day of year effects
  vector[7] f_day_of_week = append_row(0, beta_f3);
  vector[N] intercept = intercept0 + f_day_of_week[day_of_week] + beta_f4[day_of_year];
  // priors
  intercept0 ~ normal(0, 1);
  beta_f1 ~ normal(0, 1);
  beta_f2 ~ normal(0, 1);
  beta_f3 ~ normal(0, 1);
  beta_f4 ~ normal(0, sigma_f4);
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  lengthscale_f2 ~ normal(0, .1);
  sigma_f1 ~ normal(0, 1);
  sigma_f2 ~ normal(0, 1);
  sigma_f4 ~ normal(0, 0.1);
  sigma ~ normal(0, 0.5);
  // model
  yn ~ normal_id_glm(PHI_f,
                     intercept,
                     append_row(diagSPD_f1 .* beta_f1, diagSPD_f2 .* beta_f2),
                     sigma);
}

module "model7" Model() {
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
    // Concatenated basis functions for f1 and f2
    matrix[N,M_f1+2*J_f2] PHI_f = append_col(PHI_f1, PHI_f2);
  }
  parameters {
    real intercept0;
    vector[M_f1] beta_f1;         // the basis functions coefficients for f1
    vector[2*J_f2] beta_f2;       // the basis functions coefficients for f2
    vector[6] beta_f3;            // day of week effect
    vector[366] beta_f4;          // day of year effect
    vector[3] beta_f5;            // floating special days effects
    real<lower=0> lengthscale_f1; //
    real<lower=0> lengthscale_f2; //
    real<lower=0> sigma_f1;       // scale of f1
    real<lower=0> sigma_f2;       // scale of f2
    real<lower=0> sigma_f4;       // scale of day of year effect
    real<lower=0> sigma;          // residual scale
  }
  generated quantities {
    vector[N] f1;
    vector[N] f2;
    vector[N] f;
    vector[7] f_day_of_week;
    vector[N] log_lik;
    {
      // spectral densities for f1 and f2
      vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
      vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
      // day of week and day of year effects
      vector[7] f_day_of_week_n = append_row(0, beta_f3);
      vector[N] intercept = f_day_of_week_n[day_of_week] + beta_f4[day_of_year];
      // these floating days overrule day_of_week and day_of_year
      intercept[memorial_days] = rep_vector(beta_f5[1], size(memorial_days));
      intercept[labor_days] = rep_vector(beta_f5[2], size(labor_days));
      intercept[thanksgiving_days] = rep_vector(beta_f5[3], size(thanksgiving_days));
      intercept += intercept0;
      // functions scaled back to original scale
      f_day_of_week = f_day_of_week_n*ysd;
      f1 = (intercept0 + PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd;
      f2 = (PHI_f2 * (diagSPD_f2 .* beta_f2))*ysd;
      f = f1 + f2 + (intercept-intercept0)*ysd + ymean;
      // log_liks for loo
      for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
    }
  }
  // spectral densities for f1 and f2
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
  // day of week and day of year effects
  vector[7] f_day_of_week = append_row(0, beta_f3);
  vector[N] intercept = f_day_of_week[day_of_week] + beta_f4[day_of_year];
  // these floating days overrule day_of_week and day_of_year
  intercept[memorial_days] = rep_vector(beta_f5[1], size(memorial_days));
  intercept[labor_days] = rep_vector(beta_f5[2], size(labor_days));
  intercept[thanksgiving_days] = rep_vector(beta_f5[3], size(thanksgiving_days));
  intercept += intercept0;
  // priors
  intercept0 ~ normal(0, 1);
  beta_f1 ~ normal(0, 1);
  beta_f2 ~ normal(0, 1);
  beta_f3 ~ normal(0, 1);
  beta_f4 ~ normal(0, sigma_f4);
  beta_f5 ~ normal(0, 1);
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  lengthscale_f2 ~ normal(0, .1);
  sigma_f1 ~ normal(0, 1);
  sigma_f2 ~ normal(0, 1);
  sigma_f4 ~ normal(0, 0.1);
  sigma ~ normal(0, 0.5);
  // model
  yn ~ normal_id_glm(PHI_f,
                     intercept,
                     append_row(diagSPD_f1 .* beta_f1, diagSPD_f2 .* beta_f2),
                     sigma);
}

module "model8" Model() {
  functions {
    // #include gpbasisfun_functions.stan
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
    // Basis functions for g3
    real L_g3= c_g3*max(xn);
    matrix[N,M_g3] PHI_g3 = PHI_EQ(N, M_g3, L_g3, xn);
    // Concatenated basis functions for f1 and f2
    matrix[N,M_f1+2*J_f2] PHI_f = append_col(PHI_f1, PHI_f2);
  }
  parameters {
    real intercept0;
    vector[M_f1] beta_f1;         // the basis functions coefficients for f1
    vector[M_f1] beta_f1;         // the basis functions coefficients for f1
    vector[2*J_f2] beta_f2;       // the basis functions coefficients for f2
    vector[6] beta_f3;            // day of week effect
    vector[M_g3] beta_g3;         // the basis functions coefficients for g3
    vector[366] beta_f4;          // day of year effect
    vector[3] beta_f5;            // floating special days effects
    real<lower=0> lengthscale_f1; //
    real<lower=0> lengthscale_f2; //
    real<lower=0> lengthscale_g3; //
    real<lower=0> sigma_f1;       // scale of f1
    real<lower=0> sigma_f2;       // scale of f2
    real<lower=0> sigma_g3;       // scale of g3
    real<lower=0> sigma_f4;       // scale of day of year effect
    real<lower=0> sigma;          // residual scale
  }
  generated quantities {
    vector[N] f1;
    vector[N] f2;
    vector[N] f3;
    vector[N] f;
    vector[7] f_day_of_week;
    vector[N] log_lik;
    {
      // spectral densities for f1, f2, and g3
      vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
      vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
      vector[M_g3] diagSPD_g3 = diagSPD_EQ(sigma_g3, lengthscale_g3, L_g3, M_g3);
      // day of week and day of year effects
      vector[7] f_day_of_week_n = append_row(0, beta_f3);
      vector[N] eg3 = exp(PHI_g3 * (diagSPD_g3 .* beta_g3));
      vector[N] intercept = eg3.*f_day_of_week_n[day_of_week] + beta_f4[day_of_year];
      // these floating days overrule day_of_week and day_of_year
      intercept[memorial_days] = rep_vector(beta_f5[1], size(memorial_days));
      intercept[labor_days] = rep_vector(beta_f5[2], size(labor_days));
      intercept[thanksgiving_days] = rep_vector(beta_f5[3], size(thanksgiving_days));
      // functions scaled back to original scale
      f3 = eg3.*f_day_of_week_n[day_of_week]*ysd;
      f_day_of_week = f_day_of_week_n*ysd;
      f1 = (intercept0 + PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd;
      f2 = (PHI_f2 * (diagSPD_f2 .* beta_f2))*ysd;
      f = f1 + f2 + intercept*ysd + ymean;
      for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
    }
  }

  // spectral densities for f1, f2, and g3
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
  vector[M_g3] diagSPD_g3 = diagSPD_EQ(sigma_g3, lengthscale_g3, L_g3, M_g3);
  // day of week and day of year effects
  vector[7] f_day_of_week = append_row(0, beta_f3);
  vector[N] eg3 = exp(PHI_g3 * (diagSPD_g3 .* beta_g3));
  vector[N] intercept = eg3.*f_day_of_week[day_of_week] + beta_f4[day_of_year];
  // these floating days overrule day_of_week and day_of_year
  intercept[memorial_days] = rep_vector(beta_f5[1], size(memorial_days));
  intercept[labor_days] = rep_vector(beta_f5[2], size(labor_days));
  intercept[thanksgiving_days] = rep_vector(beta_f5[3], size(thanksgiving_days));
  intercept += intercept0;
  // priors
  intercept0 ~ normal(0, 1);
  beta_f1 ~ normal(0, 1);
  beta_f2 ~ normal(0, 1);
  beta_f3 ~ normal(0, 1);
  beta_g3 ~ normal(0, 1);
  beta_f4 ~ normal(0, sigma_f4);
  beta_f5 ~ normal(0, 1);
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  lengthscale_f2 ~ normal(0, .1);
  lengthscale_g3 ~ lognormal(log(7000/xsd), 1);
  sigma_f1 ~ normal(0, 1);
  sigma_f2 ~ normal(0, 1);
  sigma_g3 ~ normal(0, 0.1);
  sigma_f4 ~ normal(0, 0.1);
  sigma ~ normal(0, 0.5);
  // model
  yn ~ normal_id_glm(PHI_f,
                     intercept,
                     append_row(diagSPD_f1 .* beta_f1, diagSPD_f2 .* beta_f2),
                     sigma);
}

module "model8rhs" Model() {
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
    // Basis functions for g3
    real L_g3= c_g3*max(xn);
    matrix[N,M_g3] PHI_g3 = PHI_EQ(N, M_g3, L_g3, xn);
    // Concatenated basis functions for f1 and f2
    matrix[N,M_f1+2*J_f2] PHI_f = append_col(PHI_f1, PHI_f2);
    // Horseshoe
    real nu_global = 100;	   // degrees of freedom for the half-t priors for tau
    real nu_local = 1;       // for the regularized horseshoe
    real slab_scale = 2;     // for the regularized horseshoe
    real slab_df = 100;      // for the regularized horseshoe
  }
  parameters {
    real intercept0;
    vector[M_f1] beta_f1;         // the basis functions coefficients for f1
    vector[2*J_f2] beta_f2;       // the basis functions coefficients for f2
    vector[6] beta_f3;            // day of week effect
    vector[M_g3] beta_g3;         // the basis functions coefficients for g3
    vector[366] beta_f4;          // day of year effect
    vector[3] beta_f5;            // floating special days effects
    real<lower=0> lengthscale_f1; //
    real<lower=0> lengthscale_f2; //
    real<lower=0> lengthscale_g3; //
    real<lower=0> sigma_f1;       // scale of f1
    real<lower=0> sigma_f2;       // scale of f2
    real<lower=0> sigma_g3;       // scale of g3
    real<lower=0> sigma_f4;       // scale of day of year effect
    real<lower=0> sigma;          // residual scale
    // horseshoe
    real <lower=0> tau_f4;           // global shrinkage parameter
    vector <lower=0>[366] lambda_f4; // local shrinkage parameter
    real<lower=0> caux_f4;           // auxiliary parameter
  }
  generated quantities {
    vector[N] f1;
    vector[N] f2;
    vector[N] f3;
    vector[N] f;
    vector[7] f_day_of_week;
    vector[N] log_lik;
    {
      // spectral densities for f1, f2, and g3
      vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
      vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
      vector[M_g3] diagSPD_g3 = diagSPD_EQ(sigma_g3, lengthscale_g3, L_g3, M_g3);
      // day of week and day of year effects
      vector[7] f_day_of_week_n = append_row(0, beta_f3);
      vector[N] eg3 = exp(PHI_g3 * (diagSPD_g3 .* beta_g3));
      vector[N] intercept = eg3.*f_day_of_week_n[day_of_week] + beta_f4[day_of_year];
      // these floating days overrule day_of_week and day_of_year
      intercept[memorial_days] = rep_vector(beta_f5[1], size(memorial_days));
      intercept[labor_days] = rep_vector(beta_f5[2], size(labor_days));
      intercept[thanksgiving_days] = rep_vector(beta_f5[3], size(thanksgiving_days));
      // functions scaled back to original scale
      f3 = eg3.*f_day_of_week_n[day_of_week]*ysd;
      f_day_of_week = f_day_of_week_n*ysd;
      f1 = (intercept0 + PHI_f1 * (diagSPD_f1 .* beta_f1))*ysd;
      f2 = (PHI_f2 * (diagSPD_f2 .* beta_f2))*ysd;
      f = f1 + f2 + intercept*ysd + ymean;
      for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | f[n], sigma*ysd);
    }
  }
  // spectral densities for f1, f2, and g3
  vector[M_f1] diagSPD_f1 = diagSPD_EQ(sigma_f1, lengthscale_f1, L_f1, M_f1);
  vector[2*J_f2] diagSPD_f2 = diagSPD_periodic(sigma_f2, lengthscale_f2, J_f2);
  vector[M_g3] diagSPD_g3 = diagSPD_EQ(sigma_g3, lengthscale_g3, L_g3, M_g3);
  // day of week and day of year effects
  vector[7] f_day_of_week = append_row(0, beta_f3);
  vector[N] eg3 = exp(PHI_g3 * (diagSPD_g3 .* beta_g3));
  vector[N] intercept = eg3.*f_day_of_week[day_of_week] + beta_f4[day_of_year];
  // these floating days overrule day_of_week and day_of_year
  intercept[memorial_days] = rep_vector(beta_f5[1], size(memorial_days));
  intercept[labor_days] = rep_vector(beta_f5[2], size(labor_days));
  intercept[thanksgiving_days] = rep_vector(beta_f5[3], size(thanksgiving_days));
  intercept += intercept0;
  // horseshoe
  real c_f4 = slab_scale * sqrt(caux_f4); // slab scale
  // priors
  intercept0 ~ normal(0, 1);
  beta_f1 ~ normal(0, 1);
  beta_f2 ~ normal(0, 1);
  beta_f3 ~ normal(0, 1);
  beta_g3 ~ normal(0, 1);
  beta_f4 ~ normal(0, sigma_f4);
  // horseshoe
  beta_f4 ~ normal(0, sqrt( c_f4^2 * square(lambda_f4) ./ (c_f4^2 + tau_f4^2*square(lambda_f4)))*tau_f4);
  lambda_f4 ~ student_t(nu_local, 0, 1);
  tau_f4 ~ student_t(nu_global, 0, scale_global*2);
  caux_f4 ~ inv_gamma(0.5*slab_df, 0.5*slab_df);
  //
  beta_f5 ~ normal(0, 1);
  lengthscale_f1 ~ lognormal(log(700/xsd), 1);
  lengthscale_f2 ~ normal(0, .1);
  lengthscale_g3 ~ lognormal(log(7000/xsd), 1);
  sigma_f1 ~ normal(0, 1);
  sigma_f2 ~ normal(0, 1);
  sigma_g3 ~ normal(0, 0.1);
  sigma_f4 ~ normal(0, 0.1);
  sigma ~ normal(0, 0.5);
  // model
  yn ~ normal_id_glm(PHI_f,
                     intercept,
                     append_row(diagSPD_f1 .* beta_f1, diagSPD_f2 .* beta_f2),
                     sigma);
}


/* named modular: */
/*
Chain 8 finished in 162.7 seconds.
Chain 2 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 2 finished in 159.1 seconds.
Chain 3 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 3 finished in 176.3 seconds.
Chain 4 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 4 finished in 174.5 seconds.
Chain 7 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 7 finished in 181.0 seconds.
Chain 10 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 1 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 10 finished in 183.3 seconds.
Chain 1 finished in 177.9 seconds.
Chain 5 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 5 finished in 183.9 seconds.
Chain 6 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 6 finished in 180.6 seconds.

All 10 chains finished successfully.
Mean chain execution time: 158.0 seconds.
Total execution time: 184.6 seconds.

Warning: 50 of 500 (10.0%) transitions ended with a divergence.
This may indicate insufficient exploration of the posterior distribution.
Possible remedies include: 
  * Increasing adapt_delta closer to 1 (default is 0.8) 
  * Reparameterizing the model (e.g. using a non-centered parameterization)
  * Using informative or weakly informative prior distributions 

Warning message:
Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
 
6849.98275614093
*/


/* model modular: */
/*
Chain 6 finished in 45.7 seconds.
Chain 5 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 5 finished in 46.4 seconds.
Chain 3 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 3 finished in 49.4 seconds.
Chain 10 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 10 finished in 50.3 seconds.
Chain 8 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 4 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 8 finished in 50.9 seconds.
Chain 4 finished in 51.1 seconds.
Chain 2 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 7 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 2 finished in 48.0 seconds.
Chain 7 finished in 51.5 seconds.
Chain 1 Iteration: 51 / 100 [ 51%]  (Sampling) 
Chain 1 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 1 finished in 59.9 seconds.

All 10 chains finished successfully.
Mean chain execution time: 45.8 seconds.
Total execution time: 65.3 seconds.

Warning: 50 of 500 (10.0%) transitions ended with a divergence.
This may indicate insufficient exploration of the posterior distribution.
Possible remedies include: 
  * Increasing adapt_delta closer to 1 (default is 0.8) 
  * Reparameterizing the model (e.g. using a non-centered parameterization)
  * Using informative or weakly informative prior distributions 

50 of 500 (10.0%) transitions hit the maximum treedepth limit of 10 or 2^10-1 leapfrog steps.
Trajectories that are prematurely terminated due to this limit will result in slow exploration.
Increasing the max_treedepth limit can avoid this at the expense of more computation.
If increasing max_treedepth does not remove warnings, try to reparameterize the model.

Warning message:
Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
 
6033.43866687506
 */


/* named modular with TD  */
/*
Chain 3 finished in 21.6 seconds.
Chain 5 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 5 finished in 25.1 seconds.
Chain 1 Iteration: 51 / 100 [ 51%]  (Sampling) 
Chain 10 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 10 finished in 26.1 seconds.
Chain 4 Iteration: 51 / 100 [ 51%]  (Sampling) 
Chain 2 Iteration: 51 / 100 [ 51%]  (Sampling) 
Chain 6 Iteration: 51 / 100 [ 51%]  (Sampling) 
Chain 8 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 8 finished in 35.1 seconds.
Chain 7 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 7 finished in 37.8 seconds.
Chain 1 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 1 finished in 33.1 seconds.
Chain 2 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 2 finished in 33.8 seconds.
Chain 4 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 4 finished in 36.2 seconds.
Chain 6 Iteration: 100 / 100 [100%]  (Sampling) 
Chain 6 finished in 36.2 seconds.

All 10 chains finished successfully.
Mean chain execution time: 28.6 seconds.
Total execution time: 40.2 seconds.

Warning: 50 of 500 (10.0%) transitions ended with a divergence.
This may indicate insufficient exploration of the posterior distribution.
Possible remedies include:
  * Increasing adapt_delta closer to 1 (default is 0.8)
  * Reparameterizing the model (e.g. using a non-centered parameterization)
  * Using informative or weakly informative prior distributions

Warning message:
Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.

6849.98275614093
 */

/* named seed 1 */
/* 6765.39734251076 */

/* ref seed 1 */
/* 7251.31920543013 */
