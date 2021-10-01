
library("rprojroot")
root<-has_file(".Workflow-Examples-root")$make_fix_file()
library(tidyverse)
library(cmdstanr)
library(posterior)
options(pillar.neg = FALSE, pillar.subtle=FALSE, pillar.sigfig=2)
library(loo)

#' ## Load data
#'
#' Load birthdays per day in USA 1969-1988:
data <- read_csv(root("births_usa_1969.csv"))

data <- data %>%
  mutate(date = as.Date("1968-12-31") + id,
         births_relative100 = births/mean(births)*100)

#' Data to be passed to Stan
memorial_days <- with(data,which(month==5&day_of_week==1&day>=25))
labor_days <- with(data,which(month==9&day_of_week==1&day<=7))
labor_days <- c(labor_days, labor_days+1)
thanksgiving_days <- with(data,which(month==11&day_of_week==4&day>=22&day<=28))
thanksgiving_days <- c(thanksgiving_days, thanksgiving_days+1)
birthday_data <- list(x=data$id,
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
birthday_data <- c(birthday_data, scale_global=0.1) # global scale for RHS prior

write_stan_json(birthday_data, "births_usa_1969.json")
