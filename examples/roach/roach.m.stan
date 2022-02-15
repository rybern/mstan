data {
    # y roach1 treatment senior exposure2
    int N;
    int y[N];
    real roach1[N];
    int treatment[N];
    int senior[N];
    real exposure2[N];
}

transformed data {
    vector[N] roach1_vec = to_vector(roach1);
    #roach1_vec = roach1_vec ./ 100
    vector[N] treatment_vec = to_vector(treatment);
    vector[N] senior_vec = to_vector(senior);
    vector[N] exposure2_vec = to_vector(exposure2);
}

model {
  y ~ Regression();
}

module "glm" Regression() {
  transformed parameters {
      vector[N] offset = OffsetType();
      vector[N] mu = Roach() + Treatment() + Senior();
  }
  Likelihood();
}

module "Poisson" Likelihood() {
    generated quantities {
        vector[N] log_lik;
        for (n in 1:N) log_lik[n] = poisson_lpmf(y[n] | exp(mu + RandomEffects() + offset));
    }
    y ~ poisson(exp(mu + RandomEffects() + offset));
}

module "NegBinomial" Likelihood() {
    parameters {
        real<lower=0> phi;
    }
    generated quantities {
        vector[N] log_lik;
        for (n in 1:N) log_lik[n] = neg_binomial_2_log_lpmf(y[n] | mu + offset, phi);
        
    }
    PhiPrior();
    y ~ neg_binomial_2_log(exp(mu + offset), phi);
}

module "normal" PhiPrior() {
    phi ~ normal(0, 3);
}

module "cauchy" PhiPrior() {
    phi ~ cauchy(0, 3);
}

module "log" OffsetType() {
    return log(exposure2_vec);
}

module "identity" OffsetType() {
    return exposure2_vec;
}

module "sqrt" Roach() {
    parameters {
        real roach_coeff;
    }
    return roach_coeff * sqrt(roach1_vec);
}

module "identity" Roach() {
    parameters {
        real roach_coeff;
    }
    return roach_coeff * roach1_vec;
}

module "no" Roach() {
    return rep_vector(0, N);
}

module "yes" Treatment() {
    parameters {
        real treatment_coeff;
    }
    return treatment_coeff * treatment_vec;
}

module "no" Treatment() {
    return rep_vector(0, N);
}

module "yes" Senior() {
    parameters {
        real senior_coeff;
    }
    return senior_coeff * senior_vec;
}

module "no" Senior() {
    return rep_vector(0, N);
}

module "yes" RandomEffects() {
    parameters {
        vector[N] random_effect;
    }
    model {
        random_effect ~ normal(0, 5);
    }
    return random_effect;
}

module "no" RandomEffects() {
    return rep_vector(0, N) ;
}