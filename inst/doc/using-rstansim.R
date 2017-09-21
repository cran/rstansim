## ----echo=FALSE,results='asis'-------------------------------------------
knitr::opts_chunk$set(collapse = T)

## ------------------------------------------------------------------------
# set up independent covariance matrix
omega_uncorr <- diag(5)
omega_uncorr

# set up correlated covariance matrix
omega_corr <- matrix(rbind(c(1, 0, .2, .3, .4),
                           c(0, 1, .3, .4, .2),
                           c(.2, .3, 1, 0, .4), 
                           c(.3, .4, 0, 1, .2), 
                           c(.4, .2, .4, .2, 1)), 5, 5)
omega_corr

regression_input_data <- list("N" = 1000, "K" = 5)

regression_uncorr_params <- list("alpha" = 5, "beta" = c(0, 0, .8, .6, .4),
                                 "sigma" = 1, "omega" = omega_uncorr)

regression_corr_params <-list("alpha" = 5, "beta" = c(0, 0, .8, .6, .4),
                              "sigma" = 1, "omega" = omega_corr)


## ---- results = "hide", message = F, warning = F-------------------------
library(rstansim)

# simulate data with uncorrelated predictors
uncorrelated_data <- simulate_data(
  file = "sim_data_model.stan",
  data_name = "uncorrelated multi-regression",
  input_data = regression_input_data,
  param_values = regression_uncorr_params,
  vars = c("N", "K", "sim_x", "sim_y"),
  nsim = 50,
  path = "reg_data/uncorrelated", 
  seed = 1234
  )

# simulate data with correlated predictors
correlated_data <- simulate_data(
  file = "sim_data_model.stan",
  data_name = "correlated multi-regression",
  input_data = regression_input_data,
  param_values = regression_corr_params, 
  vars = c("N", "K", "sim_x", "sim_y"),
  nsim = 50,
  path = "reg_data/correlated", 
  seed = 1234
  )


## ------------------------------------------------------------------------
# see folders inside reg_data/
dir("reg_data")

# see first five files in reg_data/correlated/
dir("reg_data/correlated")[1:5]


## ------------------------------------------------------------------------
str(readRDS("reg_data/correlated/correlated multi-regression_1.rds"))


## ---- results = "hide", message = F, warning = FALSE---------------------
# set the number of available cores to use
# core_num <- parallel::detectCores() - 1

test_data <- readRDS("reg_data/uncorrelated/uncorrelated multi-regression_1.rds")

test_fit <- rstan::stan(file = "simple_reg_model.stan", data = test_data)

## ------------------------------------------------------------------------
rstan::summary(test_fit)$summary

## ---- echo=FALSE, results='asis'-----------------------------------------
stansim_output1 <- readRDS("stansim_output1.rds")
stansim_output2 <- readRDS("stansim_output2.rds")
stansim_output3 <- readRDS("stansim_output3.rds")
stansim_output4 <- readRDS("stansim_output4.rds")

## ---- eval = F-----------------------------------------------------------
#  # fit simple model to uncorrelated data
#  stansim_output1 <- fit_models(sim_name = "simple - uncorrelated simulation",
#                                sim_data = uncorrelated_data,
#                                stan_args = list(file = "simple_reg_model.stan"),
#                                seed = 1234)
#  
#  # fit simple model to correlated data
#  stansim_output2 <- fit_models(sim_name = "simple - correlated simulation",
#                                sim_data = correlated_data,
#                                stan_args = list(file = "simple_reg_model.stan"),
#                                seed = 1234)
#  
#  # fit QR model to uncorrelated data
#  stansim_output3 <- fit_models(sim_name = "QR - uncorrelated simulation",
#                                sim_data = uncorrelated_data,
#                                stan_args = list(file = "qr_reg_model.stan"),
#                                seed = 1234)
#  
#  # fit QR model to correlated data
#  stansim_output4 <- fit_models(sim_name = "QR - correlated simulation",
#                                sim_data = correlated_data,
#                                stan_args = list(file = "qr_reg_model.stan"),
#                                seed = 1234)
#  

## ------------------------------------------------------------------------
reg_study_col <- collect_simulations(
  collection_name = "regression study collection",
  stansim_output1,
  stansim_output2,
  stansim_output3,
  stansim_output4)

## ------------------------------------------------------------------------
extract_data(
  reg_study_col,
  estimates = "Rhat",
  values = function(x)
  x > 1.1
  )


## ---- message = F, warning = F, fig.width=7------------------------------
# extract estimates for correlated datasets
correlated_beta_estimates <- extract_data(
  reg_study_col,
  sim_names = c("QR - correlated simulation", "simple - correlated simulation"), 
  parameters = c("beta"), 
  estimates = "mean")

# extract estimates for uncorrelated datasets
uncorrelated_beta_estimates <- extract_data(
  reg_study_col,
  sim_names = c("QR - uncorrelated simulation", "simple - uncorrelated simulation"), 
  parameters = c("beta"), 
  estimates = "mean")

# plot beta estimates for correlated datasets
library(ggplot2)
library(ggjoy)

ggplot(correlated_beta_estimates, aes(x=value, y = parameter)) + 
  geom_joy() + 
  facet_wrap(~sim_name)

# plot beta estimates for uncorrelated datasets
ggplot(uncorrelated_beta_estimates, aes(x=value, y = parameter)) + 
  geom_joy() + 
  facet_wrap(~sim_name)

## ---- message = F, warning = F, fig.width=7------------------------------
# extract total times (warmup and sampling) for correlated datasets
correlated_time <- extract_time_elapsed(
  reg_study_col,
  sim_names = c("QR - correlated simulation", 
                "simple - correlated simulation"),
  stages = "total"
  )
  
# extract total times (warmup and sampling) for correlated datasets
uncorrelated_time <- extract_time_elapsed(
  reg_study_col,
  sim_names = c("QR - uncorrelated simulation",
                "simple - uncorrelated simulation"),
  stages = "total"
  )

# plot total elapsed time for correlated datasets
ggplot(correlated_time, aes(x=elapsed, y = sim_name)) + geom_joy()

# plot total elapsed time for uncorrelated datasets
ggplot(uncorrelated_time, aes(x=elapsed, y = sim_name)) + geom_joy()


## ---- message = F, warning = F, fig.width=7, fig.height = 7--------------
# extract the effective number of parameters samples for correlated datasets
n_eff_correlated <- extract_data(reg_study_col,
  sim_names = c("QR - correlated simulation", "simple - correlated simulation"), 
  parameters = c("beta", "sigma", "lp__"),
  estimates = "n_eff")

# extract the effective number of parameters samples for uncorrelated datasets
n_eff_uncorrelated <- extract_data(reg_study_col,
  sim_names = c("QR - uncorrelated simulation", "simple - uncorrelated simulation"),
  parameters = c("beta", "sigma", "lp__"),
  estimates = "n_eff")

# plot correlated datasets effective samples by parameter
ggplot(n_eff_correlated, aes(x = value, y = sim_name)) + 
  geom_jitter() + 
  facet_wrap(~parameter)

# plot uncorrelated datasets effective samples by parameter
ggplot(n_eff_uncorrelated, aes(x = value, y = sim_name)) + 
  geom_jitter() + 
  facet_wrap(~parameter)

## ---- echo=FALSE, results='asis'-----------------------------------------
## clean up all created files from vignetes/
# remove stan models
if(file.exists("simple_reg_model.stan"))
  unlink("simple_reg_model.stan")

if(file.exists("qr_reg_model.stan"))
  unlink("qr_reg_model.stan")

if(file.exists("sim_data_model.stan"))
  unlink("sim_data_model.stan")

# if simulated data dir has been created, delete it
if(dir.exists("reg_data"))
  unlink("reg_data", recursive = T)

