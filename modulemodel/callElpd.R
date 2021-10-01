
#' Workflow for iterative building of a time series model.

# THINGS TO CONSIDER FOR MODULARIZATION (by Hyunji)
# Network node is equivalent to one pure dgp, represented with DAG (N model signatures and N+1 implelmentations(root included)) 
#' computation choices happens within one node using `set_init`, `set_iter_sampling`
#' model averaging happens between many nodes `loo_model_weights`

# 1. optimizing vs sampling 
# Using results from optimizaing to set initial values and sample numbers
#' `set_init`: initial values: Sample short chains using the optimization result as initial values
#' `set_iter_sampling`: set sample numbers: # Could lp for optimization and sampling having the same value be seen as "enough sample"?
# The original notebook compares sampling with optimizing, so we might want to analyze them more carefully, especially w.r.t. targets' distribution

# 2. Dynamic graph search standards
# Available scores:
#' - between node: `node.elpd` 
#' - within node: `node.set_iter_sampling` using `lp_dist(node.optimizing, node.sampling)`, 
#' - edge: `sbc_dist(node, node)`,`loo_dist(node, node)`
# `elpd` measures accuracy, `lp__` helps efficiency (min #sample s.t.lp_dist(opt, sample) < epsilon), `sbc_dist` helps escaping local min.

# 3. Input datahanding
# Additional data mutation operation (i.e. graph search based feature creation) e.g. from standata6 to 7 (holiday feautres) might be needed

# 4. Classify change i.e.edge types 
# - prior change: Aki used `gpbf8`, `gpbf8rhs`, `gpbf8tnu` for prior change; computational related if we think no -> flat -> zero-avoiding prior for tau in eight school model. 
# - likelihood model change: include adding hierarchy, new predictors, 
# - computation change: HMC, ADVI, INLA, SMC (hope to include this in the ends)

# 5. Stacking shouldn't be viewed as new node.
# 


theme_set(bayesplot::theme_default(base_family = "sans"))
set1 <- RColorBrewer::brewer.pal(7, "Set1")
library("rprojroot")
root<-has_file(".Workflow-Examples-root")$make_fix_file()
library(tidyverse)
library(cmdstanr)
library(posterior)
options(pillar.neg = FALSE, pillar.subtle=FALSE, pillar.sigfig=2)
library(loo)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))
library(patchwork)

#' ## Load data
#' 
#' Load birthdays per day in USA 1969-1988:
data <- read_csv(root("births_usa_1969.csv"))

# Read stanfile names within the directory - where can I get supplies for signature-implementation string?

#' Data to be passed to Stan
standata <- list()
standata[[1]] <- list(x=data$id,
                  y=log(data$births_relative100),
                  N=length(data$id),
                  c_f1=1.5, # factor c of basis functions for GP for f1
                  M_f1=10)  # number of basis functions for GP for f1
standata[[2]]  <- list(x=data$id,
                  y=log(data$births_relative100),
                  N=length(data$id),
                  c_f1=1.5, # factor c of basis functions for GP for f1
                  M_f1=10,  # number of basis functions for GP for f1
                  J_f2=20)  # number of basis functions for periodic f2
standata[[3]] <- list(x=data$id,
                  y=log(data$births_relative100),
                  N=length(data$id),
                  c_f1=1.5, # factor c of basis functions for GP for f1
                  M_f1=10,  # number of basis functions for GP for f1
                  J_f2=20,  # number of basis functions for periodic f2
                  day_of_week=data$day_of_week)
standata[[4]] <- list(x=data$id,
                  y=log(data$births_relative100),
                  N=length(data$id),
                  c_f1=1.5, # factor c of basis functions for GP for f1
                  M_f1=10,  # number of basis functions for GP for f1
                  J_f2=20,  # number of basis functions for periodic f2
                  c_g3=1.5, # factor c of basis functions for GP for g3
                  M_g3=5,   # number of basis functions for GP for g3
                  day_of_week=data$day_of_week) 
standata[[5]] <- list(x=data$id,
                  y=log(data$births_relative100),
                  N=length(data$id),
                  c_f1=1.5, # factor c of basis functions for GP for f1
                  M_f1=10,  # number of basis functions for GP for f1
                  J_f2=20,  # number of basis functions for periodic f2
                  c_g3=1.5, # factor c of basis functions for GP for g3
                  M_g3=5,   # number of basis functions for GP for g3
                  scale_global=0.1, # gloval scale for RHS prior
                  day_of_week=data$day_of_week,
                  day_of_year=data$day_of_year2) # 1st March = 61 every year
standata[[6]] <- list(x=data$id,
                  y=log(data$births_relative100),
                  N=length(data$id),
                  c_f1=1.5, # factor c of basis functions for GP for f1
                  M_f1=10, # number of basis functions for GP for f1
                  J_f2=20, # number of basis functions for periodic f2
                  day_of_week=data$day_of_week,
                  day_of_year=data$day_of_year2) # 1st March = 61 every year

# Need extra data mutate operation
memorial_days <- with(data,which(month==5&day_of_week==1&day>=25))
labor_days <- with(data,which(month==9&day_of_week==1&day<=7))
labor_days <- c(labor_days, labor_days+1)
thanksgiving_days <- with(data,which(month==11&day_of_week==4&day>=22&day<=28))
thanksgiving_days <- c(thanksgiving_days, thanksgiving_days+1)
standata[[7]] <- list(x=data$id,
                  y=log(data$births_relative100),
                  N=length(data$id),
                  c_f1=1.5, # factor c of basis functions for GP for f1
                  M_f1=10,  # number of basis functions for GP for f1
                  J_f2=20,  # number of basis functions for periodic f2
                  day_of_week=data$day_of_week,
                  day_of_year=data$day_of_year2, # 1st March = 61 every year
                  memorial_days=memorial_days,
                  labor_days=labor_days,
                  thanksgiving_days=thanksgiving_days)
standata[[8]] <- list(x=data$id,
                  y=log(data$births_relative100),
                  N=length(data$id),
                  c_f1=1.5, # factor c of basis functions for GP for f1
                  M_f1=10,  # number of basis functions for GP for f1
                  J_f2=20,  # number of basis functions for periodic f2
                  c_g3=1.5, # factor c of basis functions for GP for g3
                  M_g3=5,   # number of basis functions for GP for g3
                  day_of_week=data$day_of_week,
                  day_of_year=data$day_of_year2, # 1st March = 61 every year
                  memorial_days=memorial_days,
                  labor_days=labor_days,
                  thanksgiving_days=thanksgiving_days)

# gpdf8[, rhs, tnu] all use the same data
#' Add a global scale for RHS prior
standata[[8]] <- c(standata[[8]], scale_global=0.1) # global scale for RHS prior

# Model and fit
model <- list()
fit <- list()
loo <- list()
for (i in seq(1:8)){ 
  model[[i]] <- cmdstan_model(stan_file = root("stanfiles", paste0("gpbf",i, ".stan")),
                              include_paths = root("stanfiles"))
  fit[[i]] <- model[[i]]$sample(data = standata[[i]], iter_warmup=100, iter_sampling=100,
                            chains=4, parallel_chains=4, seed=3891)
  loo[[i]] <- fit[[i]]$loo()
}

saveRDS(fit, "1to8fit.RDS") # 8th model with different priors excluded

#' `set_init` with optimizing
#' 
model8tnu <- cmdstan_model(stan_file = root("stanfiles", "gpbf8tnu.stan"),
                           include_paths = root("stanfiles"))
opt8tnu <- model8tnu$optimize(data=standata8, init=0.1, algorithm='lbfgs',
                              history=100, tol_obj=10)
odraws8tnu <- opt8tnu$draws()
init8tnu <- sapply(c('intercept0','lengthscale_f1','lengthscale_f2','lengthscale_g3',
                     'sigma_f1','sigma_f2','sigma_g3','sigma_f4','nu_f4','sigma',
                     'beta_f1','beta_f2','beta_f3','beta_g3','beta_f4','beta_f5'),
                   function(variable) {as.numeric(subset(odraws8tnu, variable=variable))})

fit8tnu <- model8tnu$sample(data=standata8, iter_warmup=100, iter_sampling=100,
                           chains=4, parallel_chains=4,
                           init=function() { init8tnu }, refresh=10)
loo8tnu <- fit8tnu$loo()
model8rhs <- cmdstan_model(stan_file = root("stanfiles", "gpbf8rhs.stan"),
                           include_paths = root("stanfiles"))
opt8rhs <- model8rhs$optimize(data=standata8, init=0.1, algorithm='lbfgs',
                              history=100, tol_obj=10)
odraws8rhs <- opt8rhs$draws()

#' Sample short chains using the optimization result as initial values
#' (although the result from short chains can be useful in a quick
#' workflow, the result should not be used as the final result).
init8rhs <- sapply(c('intercept0','lengthscale_f1','lengthscale_f2','lengthscale_g3',
                     'sigma_f1','sigma_f2','sigma_g3','sigma_f4','sigma',
                     'beta_f1','beta_f2','beta_f3','beta_g3','beta_f4','beta_f5',
                     'tau_f4','lambda_f4','caux_f4'),
                   function(variable) {as.numeric(subset(odraws8rhs, variable=variable))})

fit8rhs <- model8rhs$sample(data=standata8, iter_warmup=100, iter_sampling=100,
                            chains=4, parallel_chains=4,
                            init=function() { init8rhs }, refresh=10)

loo8tnu <- fit8rhs$loo()
#' Visually we get quite similar result as with Cauchy prior. When we
#' compare the models with LOO-CV, Cauchy is favored instead of RHS.

#' `node$elpd`
print(loo[[i]])

# stacking
loo_model_weights(list(`Model 8 Students t`=loo[[8]],`Model 8 RHS`=loo8tnu))
#   weight
# Model 8 Students t 0.065 
# Model 8 RHS        0.935 

#' `node[i].set_iter_sampling` calls `lp_dist(optimizing, sampling)`
lp_dist <- function(fit, opt){
  opt_lp <- subset_draws(opt$draws(), variable = "lp__")
  # Initial log joint probability = -3657.53 
  # Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
  # 94      -2165.49    0.00173561     0.0289089           1           1      100    
  fit_lp <- subset_draws(fit$draws(), variable = "lp__")
  #mean(fit_lp) ~ -2389 +_ (have variation)
  return(mean(fit_lp) - opt_lp)
}
lp_dist(fit8tnu, opt8tnu) # 7529 - 7609  = - 81 -> `set_iter_sampling` finds `iter_sampling` of fit where this value first becomes positive

#' `loo_dist(node,node)`
i = 1
j = 8
loo_dist <- function(i, j){
  return (loo[[j]]$elpd_loo -  loo[[i]]$elpd_loo)
}
loo_dist(i, j) # 9344.809 : j is better!
# can use `loo_compare` but this mixes the node order
print(loo_compare(list(`Model 1`=loo[[i]],`Model 8`=loo[[j]]))[, "elpd_diff"])

#' `sbc_dist(node,node)`
# IMPLEMENTING

######################## EOD ########################
# Future use for expert inspection such as predictive check
#' Add date type column for plotting
data <- data %>%
  mutate(date = as.Date("1968-12-31") + id,
         births_relative100 = births/mean(births)*100)

#' ### Plot all births
#'
#' We can see slow variation in trend, yearly pattern, and especially
#' in the later years spread to lower and higher values.
data %>%
  ggplot(aes(x=date, y=births)) + geom_point(color=set1[2]) +
  labs(x="Date", y="Relative number of births")


oEf <- exp(as.numeric(subset(odraws1, variable='f')))
data %>%
  mutate(oEf = oEf) %>%
  ggplot(aes(x=date, y=births_relative100)) +
  geom_point(color=set1[2], alpha=0.2) +
  geom_line(aes(y=oEf), color=set1[1]) +
  geom_hline(yintercept=100, color='gray') +
  labs(x="Date", y="Relative number of births")

# Questions of the meaning:
#' If we look at the LOO-stacking model weights, the predictive
#' performance can be improved by combining Cauchy and RHS priors on
#' day of year effect which indicates that neither of them is very
#' close to true distribution.

#' #' Sample short chains using the early stopped optimization result as
#' initial values (although the result from short chains can be useful
#' in a quick workflow, the result should not be used as the final
#' result).
