# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Item response theory model script
# April 2019
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./data/analysis/sample-processed'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/analysis/sample_irt'
'./code/twopl_irt.stan'
'./data/models/fit_twopl_irt*'
'./data/models/fit_twopl_irt_posterior'
'./data/models/fit_twopl_irt_posterior_array'
'./data/models/fit_twopl_irt_summary'
'./data/analysis/sample-processed'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 35 - PREPARATIONS
Line 52 - PREPARE DATA FOR ESTIMATION OF ITEM RESPONSE THEORY MODEL
Line 84 - ESTIMATION OF ITEM RESPONSE THEORY MODEL
Line 211 - MODEL DIAGNOSTICS
Line 254 - JOIN ESTIMATES WITH SAMPLE
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")

# read data from disk -------------------------------------------------------------------
# import processed sample
sample_processed <- readRDS("./data/analysis/sample_processed")


#### PREPARE DATA FOR ESTIMATION OF ITEM RESPONSE THEORY MODEL ==========================

# melt voting histories to long format --------------------------------------------------
# melt turnout
turnout <- melt(data = select(sample_processed, voter_id, voted_gen_2006:voted_pri_2018),
                id.vars = "voter_id", variable.name = "election", value.name = "turnout")
# format election variable
turnout$election <- str_remove(turnout$election, "^voted_")
# melt eligibility
eligibility <- melt(data = select(sample_processed, voter_id, 
                                  eligible_gen_2006:eligible_pri_2018), 
                    id.vars = "voter_id", variable.name = "election", 
                    value.name = "eligible")
# format election variable
eligibility$election <- str_remove(eligibility$election, "^eligible_")
# join long format turnout and eligibility
sample_irt <- left_join(x = turnout, y = eligibility, by = c("voter_id", "election"))
# format sample_irt
sample_irt$turnout <- ifelse(sample_irt$turnout == TRUE, 1, 0)
sample_irt$eligible <- ifelse(sample_irt$eligible == TRUE, 1, 0)
sample_irt$eligible <- ifelse(sample_irt$eligible == 0, NA, sample_irt$eligible)
# remove NAs (not eligible), cannot be handled by Stan and should not be considered 
# in IRT model anyway
sample_irt <- sample_irt[-which(is.na(sample_irt$eligible)),]
# convert columns to integers, arrange as consecutive integers
sample_irt$election_int <- as.integer(as.factor(sample_irt$election))
sample_irt$voter_id_int <- as.integer(as.factor(sample_irt$voter_id))
sample_irt <- arrange(sample_irt, voter_id_int, election_int)
# save sample in arranged order to later merge irt estimates to sample
saveRDS(sample_irt, "./data/models/sample_irt")


#### ESTIMATION OF ITEM RESPONSE THEORY MODEL ===========================================

# specify parallel estimation by distributing chains over processor cores ---------------
options(mc.cores = parallel::detectCores())

# specficy Stan program -----------------------------------------------------------------
twopl_irt <- "// TWO-PARAMETER LOGISTIC ITEM RESPONSE THEORY MODEL

// DATA BLOCK
data {
  int<lower=1> I; // number of elections (items)
  int<lower=1> J; // number of voters (respondents)
  int<lower=1> N; // number of observations
  int<lower=1, upper=I> ii[N]; // observed election (item) for observation n
  int<lower=1, upper=J> jj[N]; // observed voter (respondent) for observation n
  int<lower=0, upper=1> y[N]; // sobserved turnout (response) for observation n
}

// PARAMETERS BLOCK
parameters {
  vector<lower=0>[I] alpha; // discrimination parameter for election (item) i
  vector[I] beta; // difficulty parameter for election (item) i
  vector[J] theta; // ability for voter (respondent) j
  real mu_beta; // average election difficulty (item difficulty)
  real<lower=0> sigma_alpha; // scale of (log) discrimination
  real<lower=0> sigma_beta; // scale of difficulties
}

// MODEL BLOCK
model{
  vector[N] pi;

  // PRIORS ON HYPERPARAMETERS
  mu_beta ~ cauchy(0, 5);
  sigma_alpha ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);

  // PRIORS ON PARAMETERS
  alpha ~ lognormal(0, sigma_alpha);
  beta ~ normal(mu_beta, sigma_beta); // centered parameterization
  theta ~ std_normal();

  // MODEL
  for (n in 1:N)
    pi[n] = alpha[ii[n]] * (theta[jj[n]] - beta[ii[n]]); // centered parameterization
  y ~ bernoulli_logit(pi);
}"
write(twopl_irt, "./code/twopl_irt.stan")

# compile Stan model --------------------------------------------------------------------
compiled_twopl_irt <- stan_model(file = "./code/twopl_irt.stan")

# compile data for Stan model -----------------------------------------------------------
data_twopl_irt <- list(I = length(unique(sample_irt$election)),
                       J = length(unique(sample_irt$voter_id)),
                       N = nrow(sample_irt),
                       ii = sample_irt$election_int,
                       jj = sample_irt$voter_id_int,
                       y = as.integer(sample_irt$turnout))
rm(sample_irt)

# estimate model ------------------------------------------------------------------------
fit_twopl_irt <- sampling(object = compiled_twopl_irt,
                          data = data_twopl_irt,
                          chains = 4,
                          iter = 2000,
                          warmup = 1000,
                          save_warmup = FALSE,
                          sample_file = "./data/models/fit_twopl_irt",
                          init = "random")

# save model, posterior, and summary ----------------------------------------------------
# save model
saveRDS(fit_twopl_irt, "./data/models/fit_twopl_irt")
# given the size of the fitted model, the posterior cannot simply be extracted due
# to memory limits, hence the posterior is build in steps
# extract parameter names
fit_twopl_irt_pars <- names(fit_twopl_irt@sim$samples[[1]])
# extract posterior for half of the parameters as data frame
fit_twopl_irt_posterior_a <- as.data.frame(fit_twopl_irt, 
                                           pars = fit_twopl_irt_pars[1:50000])
# save half of the posterior to disk
saveRDS(fit_twopl_irt_posterior_a, "./data/models/fit_twopl_irt_posterior_a")
# remove half of the posterior from memory
rm(fit_twopl_irt_posterior_a)
# extract posterior for half of the parameters as array
fit_twopl_irt_posterior_a2 <- as.array(fit_twopl_irt, 
                                       pars = fit_twopl_irt_pars[1:50000])
# save half of the posterior to disk
saveRDS(fit_twopl_irt_posterior_a2, "./data/models/fit_twopl_irt_posterior_a2")
# remove half of the posterior from memory
rm(fit_twopl_irt_posterior_a2)
# extract posterior for other half of the parameters 
fit_twopl_irt_posterior_b <- as.data.frame(fit_twopl_irt, 
                                           pars = fit_twopl_irt_pars[51000:102273])
# save other half of the posterior to disk
saveRDS(fit_twopl_irt_posterior_b, "./data/models/fit_twopl_irt_posterior_b")
# extract posterior for half of the parameters as array
fit_twopl_irt_posterior_b2 <- as.array(fit_twopl_irt, 
                                       pars = fit_twopl_irt_pars[51000:102273])
# save half of the posterior to disk
saveRDS(fit_twopl_irt_posterior_b2, "./data/models/fit_twopl_irt_posterior_b2")
# remove other half of the posterior, the fitted model, and the parameter names
rm(fit_twopl_irt_posterior_b, fit_twopl_irt_posterior_b2, fit_twopl_irt, 
   fit_twopl_irt_pars)
# import and merge both halfes of the posterior
fit_twopl_irt_posterior <- cbind(readRDS("./data/models/fit_twopl_irt_posterior_a"),
                                 readRDS("./data/models/fit_twopl_irt_posterior_b"))
# save posterior
saveRDS(fit_twopl_irt_posterior, "./data/models/fit_twopl_irt_posterior")
# remove halfes of the posterior from disk
file.remove("./data/models/fit_twopl_irt_posterior_a")
file.remove("./data/models/fit_twopl_irt_posterior_b")
# import and merge both halfes of the posterior
fit_twopl_irt_posterior_array <- cbind(readRDS("./data/models/fit_twopl_irt_posterior_a2"),
                                       readRDS("./data/models/fit_twopl_irt_posterior_b2"))
# save posterior
saveRDS(fit_twopl_irt_posterior_array, "./data/models/fit_twopl_irt_posterior_array")
# remove halfes of the posterior from disk
file.remove("./data/models/fit_twopl_irt_posterior_a2")
file.remove("./data/models/fit_twopl_irt_posterior_b2")
# extract and save model summary
fit_twopl_irt_summary <- as.data.frame(summary(fit_twopl_irt,
                                 probs = c(0.025,0.05,0.1,0.5,0.9,0.95,0.975))$summary)
saveRDS(fit_twopl_irt_summary, "./data/models/fit_twopl_irt_summary")


#### MODEL DIAGNOSTICS ==================================================================

# split Rhat: potential scale reduction statistic ---------------------------------------
fit_twopl_irt_rhats <- rhat(fit_twopl_irt)
mcmc_rhat_hist(fit_twopl_irt_rhats, binwidth = 0.00001)

# effective sample size -----------------------------------------------------------------
fit_twopl_irt_neffs <- neff_ratio(fit_twopl_irt)
mcmc_neff_hist(fit_twopl_irt_neffs, binwidth = 0.01)
which(fit_twopl_irt_neffs < 0.1)

# autocorrelation -----------------------------------------------------------------------
# examine autocorrelation for all parameters and a random sample of theta parameters
fit_twopl_irt_posterior_1_1 <- as.array(fit_twopl_irt, pars = c("alpha"))
fit_twopl_irt_posterior_1_2 <- as.array(fit_twopl_irt, pars = c("beta"))
fit_twopl_irt_posterior_1_3 <- as.array(fit_twopl_irt, pars = c("mu_beta"))
fit_twopl_irt_posterior_1_4 <- as.array(fit_twopl_irt, pars = c("sigma_alpha"))
fit_twopl_irt_posterior_1_5 <- as.array(fit_twopl_irt, pars = c("sigma_beta"))
mcmc_acf(fit_twopl_irt_posterior_1_1, lags = 10)
mcmc_acf(fit_twopl_irt_posterior_1_2, lags = 10)
mcmc_acf(fit_twopl_irt_posterior_1_3, lags = 10)
mcmc_acf(fit_twopl_irt_posterior_1_4, lags = 10)
mcmc_acf(fit_twopl_irt_posterior_1_5, lags = 10)
fit_twopl_irt_posterior_2 <- as.array(fit_twopl_irt, 
                                pars = sample(names(fit_twopl_irt@sim$samples[[1]]), 15))
mcmc_acf(fit_twopl_irt_posterior_2, lags = 15)

# traceplots ----------------------------------------------------------------------------
# examine traceplots for all parameters and a random sample of theta parameters
fit_twopl_irt_posterior_1 <- as.array(fit_twopl_irt, 
                                      pars = c("alpha", "beta", "mu_beta", 
                                               "sigma_alpha", "sigma_beta"))
mcmc_trace(fit_twopl_irt_posterior_1)
mcmc_trace(fit_twopl_irt_posterior_2)

# divergent transitions -----------------------------------------------------------------
mcmc_nuts_divergence(fit_twopl_irt_nuts, fit_twopl_irt_lg)
check_divergences(fit_twopl_irt)

# energy plots --------------------------------------------------------------------------
mcmc_nuts_energy(fit_twopl_irt_nuts)


##### JOIN ESTIMATES WITH SAMPLE ========================================================

# reimport sample used for irt estimation and model summary -----------------------------
sample_irt <- readRDS("./data/analysis/sample_irt")
fit_twopl_irt_summary <- readRDS("./data/models/fit_twopl_irt_summary")

# extract theta parameters --------------------------------------------------------------
thetas <- subset(fit_twopl_irt_summary, 
                 subset = !str_detect(row.names(fit_twopl_irt_summary), "alpha|beta|lp"))

# bind thetas to sample -----------------------------------------------------------------
sample_irt <- distinct(sample_irt, voter_id)
sample_irt$theta_median <- thetas$`50%`
sample_irt$theta_mean <- thetas$mean
sample_processed <- left_join(x = sample_processed, y = sample_irt, by = "voter_id")

# statistics ----------------------------------------------------------------------------
cor(x= sample_processed$turnout_rate, y = sample_processed$theta_median, use = "complete.obs")

# save updated sample -------------------------------------------------------------------
saveRDS(sample_processed, "./data/analysis/sample_processed")
