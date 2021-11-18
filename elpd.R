args = commandArgs(trailingOnly=TRUE)
stanfile <- args[[1]]
datafile <- args[[2]]
print(paste("Using stan file:", stanfile))
print(paste("Using data file:", datafile))

#' Workflow for iterative building of a time series model.

## library("rprojroot")
## root<-has_file(".Workflow-Examples-root")$make_fix_file()
library(tidyverse)
library(cmdstanr)
library(posterior)
options(pillar.neg = FALSE, pillar.subtle=FALSE, pillar.sigfig=2)
library(loo)
library(jsonlite)


#' ## Load data
#'
#' Load birthdays per day in USA 1969-1988:
standata <- fromJSON(datafile)
print("   ...with names:")
print(names(standata))
print(cmdstan_version())

#' Model and fit
model <- cmdstan_model(stan_file = stanfile, quiet=TRUE)

fit <- model$sample(data = standata, iter_warmup=200, iter_sampling=200,
                          chains=10, parallel_chains=10, seed=1)
loo <- fit$loo()

elpd_estimate <- loo$estimates['elpd_loo', 'Estimate']
## elpd_se <- loo$estimates['elpd_loo', 'SE']

sink(stdout(), type = "message")
message(elpd_estimate)
