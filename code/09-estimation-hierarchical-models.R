# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Hierarchical model estimation script
# July 2019
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./data/analysis/sample_analysis'
'./data/models/fit_twopl_irt_summary'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/analysis/sample_ml'
'./code/ml_logit*.stan'
'./data/models/fit_ml_logit*_chain'
'./data/models/fit_ml_logit*'
'./data/models/fit_ml_logit_posterior*'
'./data/models/fit_ml_logit_posterior_array*'
'./data/models/pred_*'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 37 - PREPARATIONS
Line 61 - PREPARE DATA FOR ESTIMATION OF HIERARCHICAL MODEL
Line 151 - ESTIMATION OF HIERARCHICAL MODEL
Line 682 - MODEL DIAGNOSTICS
Line 749 - POSTESTIMATION
Line 1782 - ESTIMATION AND POSTESTIMATION FOR VOTER TYPES
Line 2605 - ESTIMATION AND POSTESTIMATION FOR ALTERNATIVE MODEL SPECIFICATIONS
Line 4408 - ESTIMATION AND POSTESTIMATION FOR SUBGROUP INTERACTIONS AND VOTER TYPES
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
# import sample for analysis
sample_analysis <- readRDS("./data/sample_analysis")
# remove terminated/protected accounts
sample_analysis <- filter(sample_analysis, !terminated == TRUE)
# remove voters with missing values (e.g., recorded income, theta)
sample_analysis <- filter(sample_analysis, !is.na(income_pci))
# remove NAs (not eligible in 2016, or registered in 2018), cannot be handled by Stan and 
# should not be considered in model anyway, no theta
sample_analysis <- sample_analysis[-which(is.na(sample_analysis$theta_median)),]


#### PREPARE DATA FOR ESTIMATION OF HIERARCHICAL MODEL ==================================

# initialize data frame for multilevel regression with Stan -----------------------------
sample_ml <- data.frame(id = sample_analysis$voter_id)

# prepare conversion of columns and their interactions to integers ----------------------
key_sex <- c("female" = 1L, "male" = 2L)
key_party <- c("dem" = 1L, "rep" = 2L, "npa" = 3L, "other" = 3L) 
key_race <- c("white" = 1L, "black" = 2L, "hispanic" = 3L, "multi" = 4L,
              "asian" = 4L, "native" = 4L, "other" = 4L)
key_sex_race <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, 
                  "2_1" = 5L, "2_2" = 6L, "2_3" = 7L, "2_4" = 8L)
key_sex_party <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, 
                   "2_1" = 4L, "2_2" = 5L, "2_3" = 6L)
key_sex_age <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, 
                 "2_1" = 5L, "2_2" = 6L, "2_3" = 7L, "2_4" = 8L)
key_sex_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L, 
                    "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L)
key_race_party <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, 
                    "2_1" = 4L, "2_2" = 5L, "2_3" = 6L, 
                    "3_1" = 7L, "3_2" = 8L, "3_3" = 9L,
                    "4_1" = 10L, "4_2" = 11L, "4_3" = 12L)
key_race_age <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, 
                  "2_1" = 5L, "2_2" = 6L, "2_3" = 7L, "2_4" = 8L, 
                  "3_1" = 9L, "3_2" = 10L, "3_3" = 11L, "3_4" = 12L,
                  "4_1" = 13L, "4_2" = 14L, "4_3" = 15L, "4_4" = 16L)
key_race_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L,
                     "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L,
                     "3_1" = 11L, "3_2" = 12L, "3_3" = 13L, "3_4" = 14L, "3_5" = 15L,
                     "4_1" = 16L, "4_2" = 17L, "4_3" = 18L, "4_4" = 19L, "4_5" = 20L)
key_age_party <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, 
                   "2_1" = 4L, "2_2" = 5L, "2_3" = 6L, 
                   "3_1" = 7L, "3_2" = 8L, "3_3" = 9L,
                   "4_1" = 10L, "4_2" = 11L, "4_3" = 12L)
key_age_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L,
                    "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L,
                    "3_1" = 11L, "3_2" = 12L, "3_3" = 13L, "3_4" = 14L, "3_5" = 15L,
                    "4_1" = 16L, "4_2" = 17L, "4_3" = 18L, "4_4" = 19L, "4_5" = 20L)
key_party_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L,
                      "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L,
                      "3_1" = 11L, "3_2" = 12L, "3_3" = 13L, "3_4" = 14L, "3_5" = 15L)

# add relevant integer coded vars to Stan data ------------------------------------------
sample_ml$sex <- recode(sample_analysis$sex, !!!key_sex)
sample_ml$party <- recode(sample_analysis$party, !!!key_party)
sample_ml$race <- recode(sample_analysis$race, !!!key_race)
sample_ml$age <- ifelse(sample_analysis$age <= 29, 1L,
                 ifelse(sample_analysis$age > 29 & sample_analysis$age <= 44, 2L, 
                 ifelse(sample_analysis$age > 44 & sample_analysis$age <= 64, 3L, 
                 ifelse(sample_analysis$age > 64, 4L, 
                        NA)))) 
sample_ml$income <- ifelse(sample_analysis$income_pci <= 15000, 1L,
                    ifelse(sample_analysis$income_pci > 15000 & 
                           sample_analysis$income_pci <= 30000, 2L,
                    ifelse(sample_analysis$income_pci > 30000 & 
                           sample_analysis$income_pci <= 50000, 3L,
                    ifelse(sample_analysis$income_pci > 50000 & 
                           sample_analysis$income_pci <= 75000, 4L,
                    ifelse(sample_analysis$income_pci > 75000, 5L, 
                           NA)))))
sample_ml$sex_race <- interaction(sample_ml$sex, sample_ml$race, sep = "_")
sample_ml$sex_race <- recode(sample_ml$sex_race, !!!key_sex_race)
sample_ml$sex_party <- interaction(sample_ml$sex, sample_ml$party, sep = "_")
sample_ml$sex_party <- recode(sample_ml$sex_party, !!!key_sex_party)
sample_ml$sex_age <- interaction(sample_ml$sex, sample_ml$age, sep = "_")
sample_ml$sex_age <- recode(sample_ml$sex_age, !!!key_sex_age)
sample_ml$sex_income <- interaction(sample_ml$sex, sample_ml$income, sep = "_")
sample_ml$sex_income <- recode(sample_ml$sex_income, !!!key_sex_income)
sample_ml$race_party <- interaction(sample_ml$race, sample_ml$party, sep = "_")
sample_ml$race_party <- recode(sample_ml$race_party, !!!key_race_party)
sample_ml$race_age <- interaction(sample_ml$race, sample_ml$age, sep = "_")
sample_ml$race_age <- recode(sample_ml$race_age, !!!key_race_age)
sample_ml$race_income <- interaction(sample_ml$race, sample_ml$income, sep = "_")
sample_ml$race_income <- recode(sample_ml$race_income, !!!key_race_income)
sample_ml$age_party <- interaction(sample_ml$age, sample_ml$party, sep = "_")
sample_ml$age_party <- recode(sample_ml$age_party, !!!key_age_party)
sample_ml$age_income <- interaction(sample_ml$age, sample_ml$income, sep = "_")
sample_ml$age_income <- recode(sample_ml$age_income, !!!key_age_income)
sample_ml$party_income <- interaction(sample_ml$party, sample_ml$income, sep = "_")
sample_ml$party_income <- recode(sample_ml$party_income, !!!key_party_income)
sample_ml$theta <- sample_analysis$theta_median
sample_ml$polact <- as.integer(sample_analysis$polact_full_a1)
sample_ml$vote_gen_2018 <- as.integer(sample_analysis$voted_gen_2018)
sample_ml$vote_pri_2018 <- as.integer(sample_analysis$voted_pri_2018)
# arrange as consecutive integers
sample_ml <- arrange(sample_ml, sex, race, age, party, income)
# save to disk
saveRDS(sample_ml, "./data/sample_ml")


#### ESTIMATION OF HIERARCHICAL MODEL ===================================================

# allow parallel estimation by distributing chains over processor cores -----------------
options(mc.cores = parallel::detectCores())

# specficy Stan program for models ------------------------------------------------------
ml_logit <- "// HIERARCHICAL LOGISTIC REGRESSION WITH VARYING INTERCEPTS AND SLOPES 
// (WITH VOTING PROPENSITIES, APPENDIX MODEL)

// DATA BLOCK
data {
  int<lower=1> N; // number of observations
  int<lower=1> n_sex; // number of sexes
  int<lower=1> n_race; // number of racial groups
  int<lower=1> n_party; // number of party affiliations
  int<lower=1> n_age; // number of age groups
  int<lower=1> n_income; // number of income groups
  int<lower=1> n_sex_race; // number of sexes by racial group
  int<lower=1> n_sex_party; // number of sexes by party affiliation
  int<lower=1> n_sex_age; // number of sexes by age group
  int<lower=1> n_sex_income; // number of sexes by income group
  int<lower=1> n_race_party; // number of racial groups by party affiliation
  int<lower=1> n_race_age; // number of racial groups by age
  int<lower=1> n_race_income; // number of racial groups by income group
  int<lower=1> n_age_party; // number of age groups by party affiliation
  int<lower=1> n_age_income; // number of age groups by income group
  int<lower=1> n_party_income; // number of party affiliations by income group 
  int<lower=1, upper=n_sex> sex[N]; // observed sex for observations
  int<lower=1, upper=n_race> race[N]; // observed racial group for observations
  int<lower=1, upper=n_party> party[N]; // observed party affiliation for observations
  int<lower=1, upper=n_age> age[N]; // observed age group for observations
  int<lower=1, upper=n_income> income[N]; // observed income group for observations
  int<lower=1, upper=n_sex_race> sex_race[N]; // observed racial group with specific sex for observations
  int<lower=1, upper=n_sex_party> sex_party[N]; // observed party affiliation with specific sex for observations
  int<lower=1, upper=n_sex_age> sex_age[N]; // observed age group with specific sex for observations
  int<lower=1, upper=n_sex_income> sex_income[N]; // observed income group with specific sex for obeservations
  int<lower=1, upper=n_race_party> race_party[N]; // observed party affiliation with specific race for observations
  int<lower=1, upper=n_race_age> race_age[N]; // observed age group with specific racial race for observations
  int<lower=1, upper=n_race_income> race_income[N]; // observed income group with specific race for observations
  int<lower=1, upper=n_age_party> age_party[N]; // observed party affiliation with specific age for observations
  int<lower=1, upper=n_age_income> age_income[N]; // observed income group with specific age for observations
  int<lower=1, upper=n_party_income> party_income[N]; // observed income group with specific party affiliation for observations
  real theta[N]; // observed turnout propensity for observations
  int<lower=0,upper=1> y[N]; // observed social media-based political engagement for observations
}

// PARAMETERS BLOCK
parameters {
  real mu_alpha_raw; // global/fixed intercept, location parameter for varying-intercept priors 
  real mu_beta_raw; // global/fixed effect for theta, location parameter for varying-slope priors
  vector[n_sex] alpha_sex_raw; // varying intercept for sexes
  vector[n_race] alpha_race_raw; // varying intercept for racial groups
  vector[n_party] alpha_party_raw; // varying intercept for party groups
  vector[n_age] alpha_age_raw; // varying intercept for age groups
  vector[n_income] alpha_income_raw; // varying intercept for income groups
  vector[n_sex_race] alpha_sex_race_raw; // varying intercept for sex-race interaction
  vector[n_sex_party] alpha_sex_party_raw; // varying intercept for sex-party interaction
  vector[n_sex_age] alpha_sex_age_raw; // varying intercept for sex-age interaction
  vector[n_sex_income] alpha_sex_income_raw; // varying intercept for sex-income interaction
  vector[n_race_party] alpha_race_party_raw; // varying intercept for race-party interaction
  vector[n_race_age] alpha_race_age_raw; // varying intercept for race-age interaction
  vector[n_race_income] alpha_race_income_raw; // varying intercept for race-income interaction
  vector[n_age_party] alpha_age_party_raw; // varying intercept for age-party interaction
  vector[n_age_income] alpha_age_income_raw; // varying intercept for age-income interaction
  vector[n_party_income] alpha_party_income_raw; // varying intercept for party-income interaction
  vector[n_sex] beta_sex_raw; // varying slope for 'theta' among sexes
  vector[n_race] beta_race_raw; // varying slope for 'theta' among racial groups
  vector[n_party] beta_party_raw; // varying slope for 'theta' among party affiliations
  vector[n_age] beta_age_raw; // varying slope for 'theta' among age groups
  vector[n_income] beta_income_raw; // varying slope for 'theta' among income groups
  real<lower=0> sigma_alpha_sex; // variance/scale parameter for the prior on alpha_sex
  real<lower=0> sigma_alpha_race; // variance/scale parameter for the prior on alpha_race
  real<lower=0> sigma_alpha_party; // variance/scale parameter for the prior on alpha_party
  real<lower=0> sigma_alpha_age; // variance/scale parameter for the prior on alpha_age
  real<lower=0> sigma_alpha_income; // variance/scale parameter for the prior on alpha_income
  real<lower=0> sigma_alpha_sex_race; // variance/scale parameter for the prior on alpha_sex_race
  real<lower=0> sigma_alpha_sex_party; // variance/scale parameter for the prior on alpha_sex_party
  real<lower=0> sigma_alpha_sex_age; // variance/scale parameter for the prior on alpha_sex_age
  real<lower=0> sigma_alpha_sex_income; // variance/scale parameter for the prior on alpha_sex_income
  real<lower=0> sigma_alpha_race_party; // variance/scale parameter for the prior on alpha_race_party
  real<lower=0> sigma_alpha_race_age; // variance/scale parameter for the prior on alpha_race_age
  real<lower=0> sigma_alpha_race_income; // variance/scale parameter for the prior on alpha_race_income
  real<lower=0> sigma_alpha_age_party; // variance/scale parameter for the prior on alpha_age_party
  real<lower=0> sigma_alpha_age_income; // variance/scale parameter for the prior on alpha_age_income
  real<lower=0> sigma_alpha_party_income; // variance/scale parameter for the prior on alpha_party_income
  real<lower=0> sigma_beta_sex; // variance/scale parameter for the prior on beta_sex
  real<lower=0> sigma_beta_race; // variance/scale parameter for the prior on beta_race
  real<lower=0> sigma_beta_party; // variance/scale parameter for the prior on beta_party
  real<lower=0> sigma_beta_age; // variance/scale parameter for the prior on beta_age
  real<lower=0> sigma_beta_income; // variance/scale parameter for the prior on beta_income
}

// TRANSFORMED PARAMETERS BLOCK
transformed parameters {
  real mu_alpha;
  real mu_beta;
  vector[n_sex] alpha_sex;
  vector[n_race] alpha_race;
  vector[n_party] alpha_party;
  vector[n_age] alpha_age;
  vector[n_income] alpha_income;
  vector[n_sex_race] alpha_sex_race;
  vector[n_sex_party] alpha_sex_party;
  vector[n_sex_age] alpha_sex_age;
  vector[n_sex_income] alpha_sex_income;
  vector[n_race_party] alpha_race_party;
  vector[n_race_age] alpha_race_age;
  vector[n_race_income] alpha_race_income;
  vector[n_age_party] alpha_age_party;
  vector[n_age_income] alpha_age_income;
  vector[n_party_income] alpha_party_income;
  vector[n_sex] beta_sex;
  vector[n_race] beta_race;
  vector[n_party] beta_party;
  vector[n_age] beta_age;
  vector[n_income] beta_income;

  // REPARAMETERIZATION 
  // centered parameterization makes the model nonidentifiable
  // reparameterized in terms of identifiable combinations of parameters
  mu_alpha = mean(alpha_sex_raw) + mean(alpha_race_raw) + mean(alpha_party_raw) +
             mean(alpha_age_raw) + mean(alpha_income_raw) + mean(alpha_sex_race_raw) +
             mean(alpha_sex_party_raw) + mean(alpha_sex_age_raw) + mean(alpha_sex_income_raw) +
             mean(alpha_race_party_raw) + mean(alpha_race_age_raw) + mean(alpha_race_income_raw) +
             mean(alpha_age_party_raw) + mean(alpha_age_income_raw) + mean(alpha_party_income_raw);
  mu_beta = mean(beta_sex_raw) + mean(beta_race_raw) + mean(beta_party_raw) +
            mean(beta_age_raw) + mean(beta_income_raw);
  alpha_sex = alpha_sex_raw - mean(alpha_sex_raw);
  alpha_race = alpha_race_raw - mean(alpha_race_raw);
  alpha_party = alpha_party_raw - mean(alpha_party_raw);
  alpha_age = alpha_age_raw - mean(alpha_age_raw);
  alpha_income = alpha_income_raw - mean(alpha_income_raw);
  alpha_sex_race = alpha_sex_race_raw - mean(alpha_sex_race_raw);
  alpha_sex_party = alpha_sex_party_raw - mean(alpha_sex_party_raw);
  alpha_sex_age = alpha_sex_age_raw - mean(alpha_sex_age_raw);
  alpha_sex_income = alpha_sex_income_raw - mean(alpha_sex_income_raw);
  alpha_race_party = alpha_race_party_raw - mean(alpha_race_party_raw);
  alpha_race_age = alpha_race_age_raw - mean(alpha_race_age_raw);
  alpha_race_income = alpha_race_income_raw - mean(alpha_race_income_raw);
  alpha_age_party = alpha_age_party_raw - mean(alpha_age_party_raw);
  alpha_age_income = alpha_age_income_raw - mean(alpha_age_income_raw);
  alpha_party_income = alpha_party_income_raw - mean(alpha_party_income_raw);
  beta_sex = beta_sex_raw - mean(beta_sex_raw);
  beta_race = beta_race_raw - mean(beta_race_raw);
  beta_party = beta_party_raw - mean(beta_party_raw);
  beta_age = beta_age_raw - mean(beta_age_raw);
  beta_income = beta_income_raw - mean(beta_income_raw);
}

// MODEL BLOCK
model{
  vector[N] pi;

  // PRIORS
  // weakly informative (main)
  mu_alpha_raw ~ student_t(5, 0, 3); // 5
  mu_beta_raw ~ student_t(5, 0, 1); // 2.5
  sigma_alpha_sex ~ student_t(4, 0, 2); // 5
  sigma_alpha_race ~ student_t(4, 0, 2);
  sigma_alpha_party ~ student_t(4, 0, 2);
  sigma_alpha_age ~ student_t(4, 0, 2);
  sigma_alpha_income ~ student_t(4, 0, 2);
  sigma_alpha_sex_race ~ student_t(4, 0, 2);
  sigma_alpha_sex_party ~ student_t(4, 0, 2);
  sigma_alpha_sex_age ~ student_t(4, 0, 2);
  sigma_alpha_sex_income ~ student_t(4, 0, 2);
  sigma_alpha_race_party ~ student_t(4, 0, 2);
  sigma_alpha_race_age ~ student_t(4, 0, 2);
  sigma_alpha_race_income ~ student_t(4, 0, 2);
  sigma_alpha_age_party ~ student_t(4, 0, 2);
  sigma_alpha_age_income ~ student_t(4, 0, 2);
  sigma_alpha_party_income ~ student_t(4, 0, 2);
  sigma_beta_sex ~ student_t(4, 0, 2);
  sigma_beta_race ~ student_t(4, 0, 2);
  sigma_beta_party ~ student_t(4, 0, 2);
  sigma_beta_age ~ student_t(4, 0, 2);
  sigma_beta_income ~ student_t(4, 0, 2);


/*  // weakly informative - less regularizing (rc)
  mu_alpha_raw ~ student_t(5, 0, 5); // 5
  mu_beta_raw ~ student_t(5, 0, 2.5); // 2.5
  sigma_alpha_sex ~ student_t(4, 0, 5); // 5
  sigma_alpha_race ~ student_t(4, 0, 5);
  sigma_alpha_party ~ student_t(4, 0, 5);
  sigma_alpha_age ~ student_t(4, 0, 5);
  sigma_alpha_income ~ student_t(4, 0, 5);
  sigma_alpha_sex_race ~ student_t(4, 0, 5);
  sigma_alpha_sex_party ~ student_t(4, 0, 5);
  sigma_alpha_sex_age ~ student_t(4, 0, 5);
  sigma_alpha_sex_income ~ student_t(4, 0, 5);
  sigma_alpha_race_party ~ student_t(4, 0, 5);
  sigma_alpha_race_age ~ student_t(4, 0, 5);
  sigma_alpha_race_income ~ student_t(4, 0, 5);
  sigma_alpha_age_party ~ student_t(4, 0, 5);
  sigma_alpha_age_income ~ student_t(4, 0, 5);
  sigma_alpha_party_income ~ student_t(4, 0, 5);
  sigma_beta_sex ~ student_t(4, 0, 5);
  sigma_beta_race ~ student_t(4, 0, 5);
  sigma_beta_party ~ student_t(4, 0, 5);
  sigma_beta_age ~ student_t(4, 0, 5);
  sigma_beta_income ~ student_t(4, 0, 5);
*/

  // centered parameterization
  // applied to speed computation
  alpha_sex_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex); 
  alpha_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race);
  alpha_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party);
  alpha_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age);
  alpha_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_income);
  alpha_sex_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_race); 
  alpha_sex_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_party);
  alpha_sex_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_age);
  alpha_sex_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_income);
  alpha_race_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_party);
  alpha_race_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_age);
  alpha_race_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_income);
  alpha_age_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age_party);
  alpha_age_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age_income);
  alpha_party_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party_income);
  beta_sex_raw ~ student_t(5, mu_beta_raw, sigma_beta_sex);
  beta_race_raw ~ student_t(5, mu_beta_raw, sigma_beta_race);
  beta_party_raw ~ student_t(5, mu_beta_raw, sigma_beta_party);
  beta_age_raw ~ student_t(5, mu_beta_raw, sigma_beta_age);
  beta_income_raw ~ student_t(5, mu_beta_raw, sigma_beta_income);

  // LIKELIHOOD
  for (n in 1:N)
    pi[n] = alpha_sex_raw[sex[n]] + alpha_race_raw[race[n]] +  alpha_party_raw[party[n]] + 
            alpha_age_raw[age[n]] +  alpha_income_raw[income[n]] + 
            alpha_sex_race_raw[sex_race[n]] + alpha_sex_party_raw[sex_party[n]] + 
            alpha_sex_age_raw[sex_age[n]] + alpha_sex_income_raw[sex_income[n]] + 
            alpha_race_party_raw[race_party[n]] + alpha_race_age_raw[race_age[n]] +
            alpha_race_income_raw[race_income[n]] + alpha_age_party_raw[age_party[n]] + 
            alpha_age_income_raw[age_income[n]] + alpha_party_income_raw[party_income[n]] + 
            beta_sex_raw[sex[n]] * theta[n] + beta_race_raw[race[n]] * theta[n] +  
            beta_party_raw[party[n]] * theta[n] + beta_age_raw[age[n]] * theta[n] +  
            beta_income_raw[income[n]] * theta[n];
  y ~ bernoulli_logit(pi);
}"
write(ml_logit, "./code/ml_logit.stan")

ml_logit_2 <- "// HIERARCHICAL LOGISTIC REGRESSION WITH VARYING INTERCEPTS 
// (PAPER MODEL)

// DATA BLOCK
data {
  int<lower=1> N; // number of observations
  int<lower=1> n_sex; // number of sexes
  int<lower=1> n_race; // number of racial groups
  int<lower=1> n_party; // number of party affiliations
  int<lower=1> n_age; // number of age groups
  int<lower=1> n_income; // number of income groups
  int<lower=1> n_sex_race; // number of sexes by racial group
  int<lower=1> n_sex_party; // number of sexes by party affiliation
  int<lower=1> n_sex_age; // number of sexes by age group
  int<lower=1> n_sex_income; // number of sexes by income group
  int<lower=1> n_race_party; // number of racial groups by party affiliation
  int<lower=1> n_race_age; // number of racial groups by age
  int<lower=1> n_race_income; // number of racial groups by income group
  int<lower=1> n_age_party; // number of age groups by party affiliation
  int<lower=1> n_age_income; // number of age groups by income group
  int<lower=1> n_party_income; // number of party affiliations by income group 
  int<lower=1, upper=n_sex> sex[N]; // observed sex for observations
  int<lower=1, upper=n_race> race[N]; // observed racial group for observations
  int<lower=1, upper=n_party> party[N]; // observed party affiliation for observations
  int<lower=1, upper=n_age> age[N]; // observed age group for observations
  int<lower=1, upper=n_income> income[N]; // observed income group for observations
  int<lower=1, upper=n_sex_race> sex_race[N]; // observed racial group with specific sex for observations
  int<lower=1, upper=n_sex_party> sex_party[N]; // observed party affiliation with specific sex for observations
  int<lower=1, upper=n_sex_age> sex_age[N]; // observed age group with specific sex for observations
  int<lower=1, upper=n_sex_income> sex_income[N]; // observed income group with specific sex for obeservations
  int<lower=1, upper=n_race_party> race_party[N]; // observed party affiliation with specific race for observations
  int<lower=1, upper=n_race_age> race_age[N]; // observed age group with specific racial race for observations
  int<lower=1, upper=n_race_income> race_income[N]; // observed income group with specific race for observations
  int<lower=1, upper=n_age_party> age_party[N]; // observed party affiliation with specific age for observations
  int<lower=1, upper=n_age_income> age_income[N]; // observed income group with specific age for observations
  int<lower=1, upper=n_party_income> party_income[N]; // observed income group with specific party affiliation for observations
  int<lower=0,upper=1> y[N]; // observed social media-based political engagement for observations
}

// PARAMETERS BLOCK
parameters {
  real mu_alpha_raw; // global/fixed intercept, location parameter for varying-intercept priors 
  vector[n_sex] alpha_sex_raw; // varying intercept for sexes
  vector[n_race] alpha_race_raw; // varying intercept for racial groups
  vector[n_party] alpha_party_raw; // varying intercept for party groups
  vector[n_age] alpha_age_raw; // varying intercept for age groups
  vector[n_income] alpha_income_raw; // varying intercept for income groups
  vector[n_sex_race] alpha_sex_race_raw; // varying intercept for sex-race interaction
  vector[n_sex_party] alpha_sex_party_raw; // varying intercept for sex-party interaction
  vector[n_sex_age] alpha_sex_age_raw; // varying intercept for sex-age interaction
  vector[n_sex_income] alpha_sex_income_raw; // varying intercept for sex-income interaction
  vector[n_race_party] alpha_race_party_raw; // varying intercept for race-party interaction
  vector[n_race_age] alpha_race_age_raw; // varying intercept for race-age interaction
  vector[n_race_income] alpha_race_income_raw; // varying intercept for race-income interaction
  vector[n_age_party] alpha_age_party_raw; // varying intercept for age-party interaction
  vector[n_age_income] alpha_age_income_raw; // varying intercept for age-income interaction
  vector[n_party_income] alpha_party_income_raw; // varying intercept for party-income interaction
  real<lower=0> sigma_alpha_sex; // variance/scale parameter for the prior on alpha_sex
  real<lower=0> sigma_alpha_race; // variance/scale parameter for the prior on alpha_race
  real<lower=0> sigma_alpha_party; // variance/scale parameter for the prior on alpha_party
  real<lower=0> sigma_alpha_age; // variance/scale parameter for the prior on alpha_age
  real<lower=0> sigma_alpha_income; // variance/scale parameter for the prior on alpha_income
  real<lower=0> sigma_alpha_sex_race; // variance/scale parameter for the prior on alpha_sex_race
  real<lower=0> sigma_alpha_sex_party; // variance/scale parameter for the prior on alpha_sex_party
  real<lower=0> sigma_alpha_sex_age; // variance/scale parameter for the prior on alpha_sex_age
  real<lower=0> sigma_alpha_sex_income; // variance/scale parameter for the prior on alpha_sex_income
  real<lower=0> sigma_alpha_race_party; // variance/scale parameter for the prior on alpha_race_party
  real<lower=0> sigma_alpha_race_age; // variance/scale parameter for the prior on alpha_race_age
  real<lower=0> sigma_alpha_race_income; // variance/scale parameter for the prior on alpha_race_income
  real<lower=0> sigma_alpha_age_party; // variance/scale parameter for the prior on alpha_age_party
  real<lower=0> sigma_alpha_age_income; // variance/scale parameter for the prior on alpha_age_income
  real<lower=0> sigma_alpha_party_income; // variance/scale parameter for the prior on alpha_party_income
}

// TRANSFORMED PARAMETERS BLOCK
transformed parameters {
  real mu_alpha;
  vector[n_sex] alpha_sex;
  vector[n_race] alpha_race;
  vector[n_party] alpha_party;
  vector[n_age] alpha_age;
  vector[n_income] alpha_income;
  vector[n_sex_race] alpha_sex_race;
  vector[n_sex_party] alpha_sex_party;
  vector[n_sex_age] alpha_sex_age;
  vector[n_sex_income] alpha_sex_income;
  vector[n_race_party] alpha_race_party;
  vector[n_race_age] alpha_race_age;
  vector[n_race_income] alpha_race_income;
  vector[n_age_party] alpha_age_party;
  vector[n_age_income] alpha_age_income;
  vector[n_party_income] alpha_party_income;
  
  // REPARAMETERIZATION 
  // centered parameterization makes the model nonidentifiable
  // reparameterized in terms of identifiable combinations of parameters
  mu_alpha = mean(alpha_sex_raw) + mean(alpha_race_raw) + mean(alpha_party_raw) +
    mean(alpha_age_raw) + mean(alpha_income_raw) + mean(alpha_sex_race_raw) +
    mean(alpha_sex_party_raw) + mean(alpha_sex_age_raw) + mean(alpha_sex_income_raw) +
    mean(alpha_race_party_raw) + mean(alpha_race_age_raw) + mean(alpha_race_income_raw) +
    mean(alpha_age_party_raw) + mean(alpha_age_income_raw) + mean(alpha_party_income_raw);
  alpha_sex = alpha_sex_raw - mean(alpha_sex_raw);
  alpha_race = alpha_race_raw - mean(alpha_race_raw);
  alpha_party = alpha_party_raw - mean(alpha_party_raw);
  alpha_age = alpha_age_raw - mean(alpha_age_raw);
  alpha_income = alpha_income_raw - mean(alpha_income_raw);
  alpha_sex_race = alpha_sex_race_raw - mean(alpha_sex_race_raw);
  alpha_sex_party = alpha_sex_party_raw - mean(alpha_sex_party_raw);
  alpha_sex_age = alpha_sex_age_raw - mean(alpha_sex_age_raw);
  alpha_sex_income = alpha_sex_income_raw - mean(alpha_sex_income_raw);
  alpha_race_party = alpha_race_party_raw - mean(alpha_race_party_raw);
  alpha_race_age = alpha_race_age_raw - mean(alpha_race_age_raw);
  alpha_race_income = alpha_race_income_raw - mean(alpha_race_income_raw);
  alpha_age_party = alpha_age_party_raw - mean(alpha_age_party_raw);
  alpha_age_income = alpha_age_income_raw - mean(alpha_age_income_raw);
  alpha_party_income = alpha_party_income_raw - mean(alpha_party_income_raw);
}

// MODEL BLOCK
model{
  vector[N] pi;
  
  // PRIORS
  // weakly informative (main)
  mu_alpha_raw ~ student_t(5, 0, 3); // 5
  sigma_alpha_sex ~ student_t(4, 0, 2); // 5
  sigma_alpha_race ~ student_t(4, 0, 2);
  sigma_alpha_party ~ student_t(4, 0, 2);
  sigma_alpha_age ~ student_t(4, 0, 2);
  sigma_alpha_income ~ student_t(4, 0, 2);
  sigma_alpha_sex_race ~ student_t(4, 0, 2);
  sigma_alpha_sex_party ~ student_t(4, 0, 2);
  sigma_alpha_sex_age ~ student_t(4, 0, 2);
  sigma_alpha_sex_income ~ student_t(4, 0, 2);
  sigma_alpha_race_party ~ student_t(4, 0, 2);
  sigma_alpha_race_age ~ student_t(4, 0, 2);
  sigma_alpha_race_income ~ student_t(4, 0, 2);
  sigma_alpha_age_party ~ student_t(4, 0, 2);
  sigma_alpha_age_income ~ student_t(4, 0, 2);
  sigma_alpha_party_income ~ student_t(4, 0, 2);

  // centered parameterization
  // applied to speed computation
  alpha_sex_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex); 
  alpha_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race);
  alpha_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party);
  alpha_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age);
  alpha_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_income);
  alpha_sex_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_race); 
  alpha_sex_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_party);
  alpha_sex_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_age);
  alpha_sex_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_income);
  alpha_race_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_party);
  alpha_race_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_age);
  alpha_race_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_income);
  alpha_age_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age_party);
  alpha_age_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age_income);
  alpha_party_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party_income);
  
  // LIKELIHOOD
  for (n in 1:N)
    pi[n] = alpha_sex_raw[sex[n]] + alpha_race_raw[race[n]] +  alpha_party_raw[party[n]] + 
    alpha_age_raw[age[n]] +  alpha_income_raw[income[n]] + 
    alpha_sex_race_raw[sex_race[n]] + alpha_sex_party_raw[sex_party[n]] + 
    alpha_sex_age_raw[sex_age[n]] + alpha_sex_income_raw[sex_income[n]] + 
    alpha_race_party_raw[race_party[n]] + alpha_race_age_raw[race_age[n]] +
    alpha_race_income_raw[race_income[n]] + alpha_age_party_raw[age_party[n]] + 
    alpha_age_income_raw[age_income[n]] + alpha_party_income_raw[party_income[n]];
  y ~ bernoulli_logit(pi);
}"
write(ml_logit_2, "./code/ml_logit_2.stan")

# compile Stan model --------------------------------------------------------------------
compiled_ml_logit <- stan_model(file = "./code/ml_logit.stan")
compiled_ml_logit_2 <- stan_model(file = "./code/ml_logit_2.stan")

# compile data for Stan model -----------------------------------------------------------
data_ml_logit <- list(N = nrow(sample_ml),
                      n_sex = length(unique(sample_ml$sex)),
                      n_race = length(unique(sample_ml$race)),
                      n_party = length(unique(sample_ml$party)),
                      n_age = length(unique(sample_ml$age)),
                      n_income = length(unique(sample_ml$income)),
                      n_sex_race = length(unique(sample_ml$sex_race)),
                      n_sex_party = length(unique(sample_ml$sex_party)),
                      n_sex_age = length(unique(sample_ml$sex_age)),
                      n_sex_income = length(unique(sample_ml$sex_income)),
                      n_race_party = length(unique(sample_ml$race_party)),
                      n_race_age = length(unique(sample_ml$race_age)),
                      n_race_income = length(unique(sample_ml$race_income)),
                      n_age_party = length(unique(sample_ml$age_party)),
                      n_age_income = length(unique(sample_ml$age_income)),
                      n_party_income = length(unique(sample_ml$party_income)),
                      sex = sample_ml$sex,
                      race = sample_ml$race,
                      party = sample_ml$party,
                      age = sample_ml$age,
                      income = sample_ml$income,
                      sex_race = sample_ml$sex_race,
                      sex_party = sample_ml$sex_party,
                      sex_age = sample_ml$sex_age,
                      sex_income = sample_ml$sex_income,
                      race_party = sample_ml$race_party,
                      race_age = sample_ml$race_age,
                      race_income = sample_ml$race_income,
                      age_party = sample_ml$age_party,
                      age_income = sample_ml$age_income,
                      party_income = sample_ml$party_income,
                      theta = sample_ml$theta,
                      y = sample_ml$polact)

# specifiy parameters of interest to be stored in fitted results ------------------------
pars_return <- c("mu_alpha", "mu_beta", 
                 "alpha_sex", "alpha_race", "alpha_party", "alpha_age", 
                 "alpha_income", "alpha_sex_race", "alpha_sex_party", 
                 "alpha_sex_age", "alpha_sex_income", 
                 "alpha_race_party", "alpha_race_age", 
                 "alpha_race_income", "alpha_age_party",
                 "alpha_age_income", "alpha_party_income", 
                 "beta_sex", "beta_race", "beta_party", "beta_age", 
                 "beta_income",
                 "sigma_alpha_sex", "sigma_alpha_race", "sigma_alpha_party", 
                 "sigma_alpha_age", "sigma_alpha_income", "sigma_alpha_sex_race", 
                 "sigma_alpha_sex_party", "sigma_alpha_sex_age", 
                 "sigma_alpha_sex_income", "sigma_alpha_race_party", 
                 "sigma_alpha_race_age", "sigma_alpha_race_income", 
                 "sigma_alpha_age_party","sigma_alpha_age_income", 
                 "sigma_alpha_party_income", 
                 "sigma_beta_sex", "sigma_beta_race", "sigma_beta_party", 
                 "sigma_beta_age", "sigma_beta_income")
pars_return2 <- c("mu_alpha",
                 "alpha_sex", "alpha_race", "alpha_party", "alpha_age", 
                 "alpha_income", "alpha_sex_race", "alpha_sex_party", 
                 "alpha_sex_age", "alpha_sex_income", 
                 "alpha_race_party", "alpha_race_age", 
                 "alpha_race_income", "alpha_age_party",
                 "alpha_age_income", "alpha_party_income", 
                 "sigma_alpha_sex", "sigma_alpha_race", "sigma_alpha_party", 
                 "sigma_alpha_age", "sigma_alpha_income", "sigma_alpha_sex_race", 
                 "sigma_alpha_sex_party", "sigma_alpha_sex_age", 
                 "sigma_alpha_sex_income", "sigma_alpha_race_party", 
                 "sigma_alpha_race_age", "sigma_alpha_race_income", 
                 "sigma_alpha_age_party","sigma_alpha_age_income", 
                 "sigma_alpha_party_income")

# estimate model ------------------------------------------------------------------------
fit_ml_logit <- sampling(object = compiled_ml_logit,
                         data = data_ml_logit,
                         pars = pars_return,
                         chains = 4,
                         iter = 2000,
                         warmup = 1000,
                         control = list(max_treedepth = 12),
                         save_warmup = FALSE,
                         init = "random",
                         sample_file = "./data/models/fit_ml_logit_chain")

fit_ml_logit_2 <- sampling(object = compiled_ml_logit_2,
                           data = data_ml_logit,
                           pars = pars_return2,
                           chains = 4,
                           iter = 2000,
                           warmup = 1000,
                           control = list(max_treedepth = 12),
                           save_warmup = FALSE,
                           init = "random",
                           sample_file = "./data/models/fit_ml_logit_2_chain")

# save model, posterior, and summary ----------------------------------------------------
# save model
saveRDS(fit_ml_logit, "./data/models/fit_ml_logit")
# extract posterior as data.frame and array
fit_ml_logit_posterior <- as.data.frame(fit_ml_logit)
fit_ml_logit_posterior_array <- as.array(fit_ml_logit)
# save posterior
saveRDS(fit_ml_logit_posterior, "./data/models/fit_ml_logit_posterior")
saveRDS(fit_ml_logit_posterior_array, "./data/models/fit_ml_logit_posterior_array")

# save model 2
saveRDS(fit_ml_logit_2, "./data/models/fit_ml_logit_2")
# extract posterior as data.frame and array
fit_ml_logit_posterior_2 <- as.data.frame(fit_ml_logit_2)
fit_ml_logit_posterior_array_2 <- as.array(fit_ml_logit_2)
# save posterior
saveRDS(fit_ml_logit_posterior_2, "./data/models/fit_ml_logit_posterior_2")
saveRDS(fit_ml_logit_posterior_array_2, "./data/models/fit_ml_logit_posterior_array_2")


#### MODEL DIAGNOSTICS ==================================================================

# split Rhat: potential scale reduction statistic ---------------------------------------
fit_ml_logit_rhats <- rhat(fit_ml_logit)
mcmc_rhat_hist(fit_ml_logit_rhats, binwidth = 0.00001)
fit_ml_logit_rhats_2 <- rhat(fit_ml_logit_2)
mcmc_rhat_hist(fit_ml_logit_rhats_2, binwidth = 0.00001)

# effective sample size -----------------------------------------------------------------
fit_ml_logit_neffs <- neff_ratio(fit_ml_logit)
mcmc_neff_hist(fit_ml_logit_neffs, binwidth = 0.01)
which(fit_ml_logit_neffs < 0.1)
fit_ml_logit_neffs_2 <- neff_ratio(fit_ml_logit_2)
mcmc_neff_hist(fit_ml_logit_neffs_2, binwidth = 0.01)
which(fit_ml_logit_neffs_2 < 0.1)

# autocorrelation -----------------------------------------------------------------------
# examine autocorrelation for all parameters, adjust parameter names after "_"
fit_ml_logit_posterior_1_1 <- as.array(fit_ml_logit, pars = c("alpha_"))
fit_ml_logit_posterior_1_2 <- as.array(fit_ml_logit, pars = c("beta_"))
fit_ml_logit_posterior_1_3 <- as.array(fit_ml_logit, pars = c("mu_alpha"))
fit_ml_logit_posterior_1_4 <- as.array(fit_ml_logit, pars = c("mu_beta"))
fit_ml_logit_posterior_1_5 <- as.array(fit_ml_logit, pars = c("sigma_alpha_"))
fit_ml_logit_posterior_1_6 <- as.array(fit_ml_logit, pars = c("sigma_beta_"))
mcmc_acf(fit_ml_logit_posterior_1_1, lags = 10)
mcmc_acf(fit_ml_logit_posterior_1_2, lags = 10)
mcmc_acf(fit_ml_logit_posterior_1_3, lags = 10)
mcmc_acf(fit_ml_logit_posterior_1_4, lags = 10)
mcmc_acf(fit_ml_logit_posterior_1_5, lags = 10)
mcmc_acf(fit_ml_logit_posterior_1_6, lags = 10)
fit_ml_logit_posterior_2 <- as.array(fit_ml_logit, 
                                      pars = sample(names(fit_ml_logit@sim$samples[[1]]), 15))
mcmc_acf(fit_ml_logit, lags = 15)

fit_ml_logit_2_posterior_1_1 <- as.array(fit_ml_logit_2, pars = c("alpha_"))
fit_ml_logit_2_posterior_1_2 <- as.array(fit_ml_logit_2, pars = c("mu_alpha"))
fit_ml_logit_2_posterior_1_3 <- as.array(fit_ml_logit_2, pars = c("sigma_alpha_"))
mcmc_acf(fit_ml_logit_2_posterior_1_1, lags = 10)
mcmc_acf(fit_ml_logit_2_posterior_1_2, lags = 10)
mcmc_acf(fit_ml_logit_2_posterior_1_3, lags = 10)
fit_ml_logit_2_posterior_2 <- as.array(fit_ml_logit_2, 
                                     pars = sample(names(fit_ml_logit_2@sim$samples[[1]]), 15))
mcmc_acf(fit_ml_logit_2, lags = 15)

# traceplots ----------------------------------------------------------------------------
# examine traceplots for all parameters
# adjust parameter names after "_"
fit_ml_logit_posterior <- as.array(fit_ml_logit, 
                                      pars = c("alpha_", "beta_", "mu_beta", 
                                               "sigma_alpha_", "sigma_beta_"))
fit_ml_logit_2_posterior <- as.array(fit_ml_logit_2, 
                                     pars = c("alpha_", "beta_", "mu_beta", 
                                              "sigma_alpha_", "sigma_beta_"))
mcmc_trace(fit_ml_logit_posterior)
mcmc_trace(fit_ml_logit_2_posterior)

# divergent transitions -----------------------------------------------------------------
mcmc_nuts_divergence(nuts_params(fit_ml_logit), log_posterior(fit_ml_logit))
check_divergences(fit_ml_logit)
mcmc_nuts_divergence(nuts_params(fit_ml_logit_2), log_posterior(fit_ml_logit_2))
check_divergences(fit_ml_logit_2)

# energy plots --------------------------------------------------------------------------
mcmc_nuts_energy(nuts_params(fit_ml_logit))
mcmc_nuts_energy(nuts_params(fit_ml_logit_2))


#### POSTESTIMATION =====================================================================

# create new datasets with specific variables fixed at values of interest ---------------
# sex subgroups fixed 
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) %>%
  dplyr::select(-c(id, theta, polact))
# party subgroups fixed 
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_npa <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
# racial subgroups fixed 
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_otherrace <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
# age subgroups fixed 
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                               !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
# income subgroups fixed 
postest_to_15 <- sample_ml %>% 
  mutate(income = 1L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                           !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_15_30 <- sample_ml %>% 
  mutate(income = 2L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_30_50 <- sample_ml %>% 
  mutate(income = 3L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_50_75 <- sample_ml %>% 
  mutate(income = 4L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_75plus <- sample_ml %>% 
  mutate(income = 5L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
parameters <- fit_ml_logit@model_pars[43:64]
# collect respective posteriors in a list
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit, pars = .x)
  }) %>%
  setNames(parameters)
parameters <- fit_ml_logit_2@model_pars[c(32:47)]
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_2, pars = .x)
  }) %>%
  setNames(parameters)

# specify the linear predictor ----------------------------------------------------------
linear_predictor <-  "mu_alpha[n] + mu_beta[n] * x[k] + 
                      alpha_sex[n, sex] + alpha_race[n, race] +  alpha_party[n, party] + 
                      alpha_age[n, age] +  alpha_income[n, income] + 
                      alpha_sex_race[n, sex_race] + alpha_sex_party[n, sex_party] + 
                      alpha_sex_age[n, sex_age] + alpha_sex_income[n, sex_income] + 
                      alpha_race_party[n, race_party] + alpha_race_age[n, race_age] +
                      alpha_race_income[n, race_income] + alpha_age_party[n, age_party] + 
                      alpha_age_income[n, age_income] + alpha_party_income[n, party_income] + 
                      beta_sex[n, sex] * x[k] + beta_race[n, race] * x[k] +  
                      beta_party[n, party] * x[k] + beta_age[n, age] * x[k] +  
                      beta_income[n, income] * x[k]"
linear_predictor_2 <- "mu_alpha[n] + alpha_sex[n, sex] + alpha_race[n, race] +  
                       alpha_party[n, party] + alpha_age[n, age] +  
                       alpha_income[n, income] + alpha_sex_race[n, sex_race] + 
                       alpha_sex_party[n, sex_party] + alpha_sex_age[n, sex_age] + 
                       alpha_sex_income[n, sex_income] + alpha_race_party[n, race_party] + 
                       alpha_race_age[n, race_age] + alpha_race_income[n, race_income] + 
                       alpha_age_party[n, age_party] + alpha_age_income[n, age_income] + 
                       alpha_party_income[n, party_income]"

# specify levels along x ----------------------------------------------------------------
levels <- list(x = seq(from = -2.2, to = 2.2, length.out = 50))
levels_2 <- list(x = 1)

# generate population-averaged predictions ----------------------------------------------
# register parallel backend
registerDoParallel(6) 
# collect names of datasets
postest_datasets <- c("postest_female", "postest_male", "postest_dem", "postest_rep",
                      "postest_npa", "postest_white", "postest_black", "postest_hispanic",
                      "postest_otherrace", "postest_18_29", "postest_30_44", 
                      "postest_45_64", "postest_65plus", "postest_to_15", 
                      "postest_15_30", "postest_30_50", "postest_50_75", 
                      "postest_75plus")
# generate predictions and save to disk
for (i in 1:length(postest_datasets)) {
   pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels, 
         draws = sum(fit_ml_logit@sim$n_save), posterior = posteriors, 
         linear_predictor = linear_predictor, type = "logit")
   saveRDS(pred, str_c("./data/models/", 
                       str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)

postest_datasets <- c("postest_female", "postest_male", "postest_dem", "postest_rep",
                      "postest_npa", "postest_white", "postest_black", "postest_hispanic",
                      "postest_otherrace", "postest_18_29", "postest_30_44", 
                      "postest_45_64", "postest_65plus", "postest_to_15", 
                      "postest_15_30", "postest_30_50", "postest_50_75", 
                      "postest_75plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_2")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for race -----
# all subgroup interactions for race and income
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_white_1 <- as.list(replicate(5, postest_white, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_white_1[[i]] <- postest_white_1[[i]] %>% 
           mutate(income = i) %>%
           mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
                  race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
                  age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
                  party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                        !!!key_party_income))
}
postest_white_to_15 <- postest_white_1[[1]]
postest_white_15_30 <- postest_white_1[[2]]
postest_white_30_50 <- postest_white_1[[3]]
postest_white_50_75 <- postest_white_1[[4]]
postest_white_75_plus <- postest_white_1[[5]]

postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_black_1 <- as.list(replicate(5, postest_black, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_black_1[[i]] <- postest_black_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_black_to_15 <- postest_black_1[[1]]
postest_black_15_30 <- postest_black_1[[2]]
postest_black_30_50 <- postest_black_1[[3]]
postest_black_50_75 <- postest_black_1[[4]]
postest_black_75_plus <- postest_black_1[[5]]

postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_hispanic_1 <- as.list(replicate(5, postest_hispanic, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_hispanic_1[[i]] <- postest_hispanic_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_hispanic_to_15 <- postest_hispanic_1[[1]]
postest_hispanic_15_30 <- postest_hispanic_1[[2]]
postest_hispanic_30_50 <- postest_hispanic_1[[3]]
postest_hispanic_50_75 <- postest_hispanic_1[[4]]
postest_hispanic_75_plus <- postest_hispanic_1[[5]]

postest_other <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_other_1 <- as.list(replicate(5, postest_other, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_other_1[[i]] <- postest_other_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_other_to_15 <- postest_other_1[[1]]
postest_other_15_30 <- postest_other_1[[2]]
postest_other_30_50 <- postest_other_1[[3]]
postest_other_50_75 <- postest_other_1[[4]]
postest_other_75_plus <- postest_other_1[[5]]

postest_datasets <- c("postest_white_to_15", "postest_white_15_30", "postest_white_30_50", 
                      "postest_white_50_75", "postest_white_75_plus",
                      "postest_black_to_15", "postest_black_15_30", "postest_black_30_50", 
                      "postest_black_50_75", "postest_black_75_plus",
                      "postest_hispanic_to_15", "postest_hispanic_15_30", "postest_hispanic_30_50", 
                      "postest_hispanic_50_75", "postest_hispanic_75_plus",
                      "postest_other_to_15", "postest_other_15_30", "postest_other_30_50", 
                      "postest_other_50_75", "postest_other_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)

# all subgroup interactions for race and age
postest_white_2 <- as.list(replicate(5, postest_white, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_white_2[[i]] <- postest_white_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_white_18_29 <- postest_white_2[[1]]
postest_white_30_44 <- postest_white_2[[2]]
postest_white_45_64 <- postest_white_2[[3]]
postest_white_60plus <- postest_white_2[[4]]

postest_black_2 <- as.list(replicate(5, postest_black, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_black_2[[i]] <- postest_black_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_black_18_29 <- postest_black_2[[1]]
postest_black_30_44 <- postest_black_2[[2]]
postest_black_45_64 <- postest_black_2[[3]]
postest_black_60plus <- postest_black_2[[4]]

postest_hispanic_2 <- as.list(replicate(5, postest_hispanic, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_hispanic_2[[i]] <- postest_hispanic_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_hispanic_18_29 <- postest_hispanic_2[[1]]
postest_hispanic_30_44 <- postest_hispanic_2[[2]]
postest_hispanic_45_64 <- postest_hispanic_2[[3]]
postest_hispanic_60plus <- postest_hispanic_2[[4]]

postest_other_2 <- as.list(replicate(4, postest_other, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_other_2[[i]] <- postest_other_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_other_18_29 <- postest_other_2[[1]]
postest_other_30_44 <- postest_other_2[[2]]
postest_other_45_64 <- postest_other_2[[3]]
postest_other_60plus <- postest_other_2[[4]]

postest_datasets <- c("postest_white_18_29", "postest_white_30_44", "postest_white_45_64", 
                      "postest_white_60plus",
                      "postest_black_18_29", "postest_black_30_44", "postest_black_45_64", 
                      "postest_black_60plus", 
                      "postest_hispanic_18_29", "postest_hispanic_30_44", "postest_hispanic_45_64", 
                      "postest_hispanic_60plus", 
                      "postest_other_18_29", "postest_other_30_44", "postest_other_45_64", 
                      "postest_other_60plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)

# all subgroup interactions for race and sex
postest_white_3 <- as.list(replicate(2, postest_white, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_white_3[[i]] <- postest_white_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_white_female <- postest_white_3[[1]]
postest_white_male <- postest_white_3[[2]]

postest_black_3 <- as.list(replicate(2, postest_black, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_black_3[[i]] <- postest_black_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_black_female <- postest_black_3[[1]]
postest_black_male <- postest_black_3[[2]]

postest_hispanic_3 <- as.list(replicate(2, postest_hispanic, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_hispanic_3[[i]] <- postest_hispanic_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_hispanic_female <- postest_hispanic_3[[1]]
postest_hispanic_male <- postest_hispanic_3[[2]]

postest_other_3 <- as.list(replicate(2, postest_other, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_other_3[[i]] <- postest_other_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_other_female <- postest_other_3[[1]]
postest_other_male <- postest_other_3[[2]]
postest_datasets <- c("postest_white_female", "postest_white_male", 
                      "postest_black_female", "postest_black_male", 
                      "postest_hispanic_female", "postest_hispanic_male",
                      "postest_other_female", "postest_other_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)

# all subgroup interactions for race and party
postest_white_4 <- as.list(replicate(3, postest_white, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_white_4[[i]] <- postest_white_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_white_dem <- postest_white_4[[1]]
postest_white_rep <- postest_white_4[[2]]
postest_white_none <- postest_white_4[[3]]

postest_black_4 <- as.list(replicate(3, postest_black, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_black_4[[i]] <- postest_black_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_black_dem <- postest_black_4[[1]]
postest_black_rep <- postest_black_4[[2]]
postest_black_none <- postest_black_4[[3]]

postest_hispanic_4 <- as.list(replicate(3, postest_hispanic, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_hispanic_4[[i]] <- postest_hispanic_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_hispanic_dem <- postest_hispanic_4[[1]]
postest_hispanic_rep <- postest_hispanic_4[[2]]
postest_hispanic_none <- postest_hispanic_4[[3]]

postest_other_4 <- as.list(replicate(3, postest_other, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_other_4[[i]] <- postest_other_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_other_dem <- postest_other_4[[1]]
postest_other_rep <- postest_other_4[[2]]
postest_other_none <- postest_other_4[[3]]

postest_datasets <- c("postest_white_dem", "postest_white_rep", "postest_white_none", 
                      "postest_black_dem", "postest_black_rep", "postest_black_none", 
                      "postest_hispanic_dem", "postest_hispanic_rep", "postest_hispanic_none",
                      "postest_other_dem", "postest_other_rep", "postest_other_none")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)


# create new datasets with specific variables fixed at interacted subgroups for age -----
# all subgroup interactions for race and income
# age_race is already included in race subgroup interactions, here only income, party, and sex
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_18_29_1 <- as.list(replicate(5, postest_18_29, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_18_29_1[[i]] <- postest_18_29_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_18_29_to_15 <- postest_18_29_1[[1]]
postest_18_29_15_30 <- postest_18_29_1[[2]]
postest_18_29_30_50 <- postest_18_29_1[[3]]
postest_18_29_50_75 <- postest_18_29_1[[4]]
postest_18_29_75_plus <- postest_18_29_1[[5]]

postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_30_44_1 <- as.list(replicate(5, postest_30_44, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_30_44_1[[i]] <- postest_30_44_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_30_44_to_15 <- postest_30_44_1[[1]]
postest_30_44_15_30 <- postest_30_44_1[[2]]
postest_30_44_30_50 <- postest_30_44_1[[3]]
postest_30_44_50_75 <- postest_30_44_1[[4]]
postest_30_44_75_plus <- postest_30_44_1[[5]]

postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_45_64_1 <- as.list(replicate(5, postest_45_64, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_45_64_1[[i]] <- postest_45_64_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_45_64_to_15 <- postest_45_64_1[[1]]
postest_45_64_15_30 <- postest_45_64_1[[2]]
postest_45_64_30_50 <- postest_45_64_1[[3]]
postest_45_64_50_75 <- postest_45_64_1[[4]]
postest_45_64_75_plus <- postest_45_64_1[[5]]

postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_65plus_1 <- as.list(replicate(5, postest_65plus, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_65plus_1[[i]] <- postest_65plus_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_65plus_to_15 <- postest_65plus_1[[1]]
postest_65plus_15_30 <- postest_65plus_1[[2]]
postest_65plus_30_50 <- postest_65plus_1[[3]]
postest_65plus_50_75 <- postest_65plus_1[[4]]
postest_65plus_75_plus <- postest_65plus_1[[5]]

postest_datasets <- c("postest_18_29_to_15", "postest_18_29_15_30", "postest_18_29_30_50", 
                      "postest_18_29_50_75", "postest_18_29_75_plus",
                      "postest_30_44_to_15", "postest_30_44_15_30", "postest_30_44_30_50", 
                      "postest_30_44_50_75", "postest_30_44_75_plus",
                      "postest_45_64_to_15", "postest_45_64_15_30", "postest_45_64_30_50", 
                      "postest_45_64_50_75", "postest_45_64_75_plus",
                      "postest_65plus_to_15", "postest_65plus_15_30", "postest_65plus_30_50", 
                      "postest_65plus_50_75", "postest_65plus_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)

postest_18_29_2 <- as.list(replicate(3, postest_18_29, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_18_29_2[[i]] <- postest_18_29_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_18_29_dem <- postest_18_29_2[[1]]
postest_18_29_rep <- postest_18_29_2[[2]]
postest_18_29_none <- postest_18_29_2[[3]]

postest_30_44_2 <- as.list(replicate(3, postest_30_44, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_30_44_2[[i]] <- postest_30_44_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_30_44_dem <- postest_30_44_2[[1]]
postest_30_44_rep <- postest_30_44_2[[2]]
postest_30_44_none <- postest_30_44_2[[3]]

postest_45_64_2 <- as.list(replicate(3, postest_45_64, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_45_64_2[[i]] <- postest_45_64_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_45_64_dem <- postest_45_64_2[[1]]
postest_45_64_rep <- postest_45_64_2[[2]]
postest_45_64_none <- postest_45_64_2[[3]]

postest_65plus_2 <- as.list(replicate(3, postest_65plus, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_65plus_2[[i]] <- postest_65plus_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_65plus_dem <- postest_65plus_2[[1]]
postest_65plus_rep <- postest_65plus_2[[2]]
postest_65plus_none <- postest_65plus_2[[3]]

postest_datasets <- c("postest_18_29_dem", "postest_18_29_rep", "postest_18_29_none",
                      "postest_30_44_dem", "postest_30_44_rep", "postest_30_44_none", 
                      "postest_45_64_dem", "postest_45_64_rep", "postest_45_64_none",
                      "postest_65plus_dem", "postest_65plus_rep", "postest_65plus_none")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)

postest_18_29_3 <- as.list(replicate(2, postest_18_29, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_18_29_3[[i]] <- postest_18_29_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_18_29_female <- postest_18_29_3[[1]]
postest_18_29_male <- postest_18_29_3[[2]]

postest_30_44_3 <- as.list(replicate(2, postest_30_44, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_30_44_3[[i]] <- postest_30_44_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_30_44_female <- postest_30_44_3[[1]]
postest_30_44_male <- postest_30_44_3[[2]]


postest_45_64_3 <- as.list(replicate(2, postest_45_64, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_45_64_3[[i]] <- postest_45_64_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_45_64_female <- postest_45_64_3[[1]]
postest_45_64_male <- postest_45_64_3[[2]]

postest_65plus_3 <- as.list(replicate(2, postest_65plus, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_65plus_3[[i]] <- postest_65plus_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_65plus_female <- postest_65plus_3[[1]]
postest_65plus_male <- postest_65plus_3[[2]]

postest_datasets <- c("postest_18_29_female", "postest_18_29_male",
                      "postest_30_44_female", "postest_30_44_male", 
                      "postest_45_64_female", "postest_45_64_male",
                      "postest_65plus_female", "postest_65plus_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for party -----
# all subgroup interactions for race and income
# party_race, party_age already included, here only income, and sex
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_dem_1 <- as.list(replicate(5, postest_dem, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_dem_1[[i]] <- postest_dem_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_dem_to_15 <- postest_dem_1[[1]]
postest_dem_15_30 <- postest_dem_1[[2]]
postest_dem_30_50 <- postest_dem_1[[3]]
postest_dem_50_75 <- postest_dem_1[[4]]
postest_dem_75_plus <- postest_dem_1[[5]]

postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_rep_1 <- as.list(replicate(5, postest_rep, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_rep_1[[i]] <- postest_rep_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_rep_to_15 <- postest_rep_1[[1]]
postest_rep_15_30 <- postest_rep_1[[2]]
postest_rep_30_50 <- postest_rep_1[[3]]
postest_rep_50_75 <- postest_rep_1[[4]]
postest_rep_75_plus <- postest_rep_1[[5]]

postest_none <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_none_1 <- as.list(replicate(5, postest_none, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_none_1[[i]] <- postest_none_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_none_to_15 <- postest_none_1[[1]]
postest_none_15_30 <- postest_none_1[[2]]
postest_none_30_50 <- postest_none_1[[3]]
postest_none_50_75 <- postest_none_1[[4]]
postest_none_75_plus <- postest_none_1[[5]]

postest_datasets <- c("postest_dem_to_15", "postest_dem_15_30", "postest_dem_30_50", 
                      "postest_dem_50_75", "postest_dem_75_plus",
                      "postest_rep_to_15", "postest_rep_15_30", "postest_rep_30_50", 
                      "postest_rep_50_75", "postest_rep_75_plus",
                      "postest_none_to_15", "postest_none_15_30", "postest_none_30_50", 
                      "postest_none_50_75", "postest_none_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)


postest_dem_2 <- as.list(replicate(2, postest_dem, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_dem_2[[i]] <- postest_dem_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_dem_female <- postest_dem_2[[1]]
postest_dem_male <- postest_dem_2[[2]]

postest_rep_2 <- as.list(replicate(2, postest_rep, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_rep_2[[i]] <- postest_rep_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_rep_female <- postest_rep_2[[1]]
postest_rep_male <- postest_rep_2[[2]]


postest_none_2 <- as.list(replicate(2, postest_none, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_none_2[[i]] <- postest_none_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_none_female <- postest_none_2[[1]]
postest_none_male <- postest_none_2[[2]]


postest_datasets <- c("postest_dem_female", "postest_dem_male",
                      "postest_rep_female", "postest_rep_male", 
                      "postest_none_female", "postest_none_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for sex -----
# all subgroup interactions for race and party, age already included, here only income
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income))
postest_female_1 <- as.list(replicate(5, postest_female, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_female_1[[i]] <- postest_female_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_female_to_15 <- postest_female_1[[1]]
postest_female_15_30 <- postest_female_1[[2]]
postest_female_30_50 <- postest_female_1[[3]]
postest_female_50_75 <- postest_female_1[[4]]
postest_female_75_plus <- postest_female_1[[5]]

postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income))
postest_male_1 <- as.list(replicate(5, postest_male, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_male_1[[i]] <- postest_male_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_male_to_15 <- postest_male_1[[1]]
postest_male_15_30 <- postest_male_1[[2]]
postest_male_30_50 <- postest_male_1[[3]]
postest_male_50_75 <- postest_male_1[[4]]
postest_male_75_plus <- postest_male_1[[5]]

postest_datasets <- c("postest_female_to_15", "postest_female_15_30", "postest_female_30_50", 
                      "postest_female_50_75", "postest_female_75_plus",
                      "postest_male_to_15", "postest_male_15_30", "postest_male_30_50", 
                      "postest_male_50_75", "postest_male_75_plus")

for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred")))
}
rm(pred)


#### ESTIMATION AND POSTESTIMATION FOR VOTER TYPES ======================================

fit_twopl_irt_summary <- readRDS("./data/models/fit_twopl_irt_summary")
item_parameters <- subset(fit_twopl_irt_summary, 
                          subset = str_detect(row.names(fit_twopl_irt_summary), "alpha\\[|beta\\["))
icc_data <- data.frame(alpha = item_parameters[1:14,]$mean,
                       beta = item_parameters[15:28,]$mean,
                       type = c("midterm", "presidential", "midterm", 
                                "presidential", "midterm", "presidential",
                                "midterm", "mprimary", "gprimary", "mprimary", 
                                "gprimary", "mprimary", "gprimary", "mprimary"))
sample_ml$midterm_2006 <- arm::invlogit(icc_data$alpha[1]*(sample_ml$theta - icc_data$beta[1]))
sample_ml$presidential_2008 <- arm::invlogit(icc_data$alpha[2]*(sample_ml$theta - icc_data$beta[2]))
sample_ml$midterm_2010 <- arm::invlogit(icc_data$alpha[3]*(sample_ml$theta - icc_data$beta[3]))
sample_ml$presidential_2012 <- arm::invlogit(icc_data$alpha[4]*(sample_ml$theta - icc_data$beta[4]))
sample_ml$midterm_2014 <- arm::invlogit(icc_data$alpha[5]*(sample_ml$theta - icc_data$beta[5]))
sample_ml$presidential_2016 <- arm::invlogit(icc_data$alpha[6]*(sample_ml$theta - icc_data$beta[6]))
sample_ml$midterm_2018 <- arm::invlogit(icc_data$alpha[7]*(sample_ml$theta - icc_data$beta[7]))
sample_ml$mprimary_2006 <- arm::invlogit(icc_data$alpha[8]*(sample_ml$theta - icc_data$beta[8]))
sample_ml$pprimary_2008 <- arm::invlogit(icc_data$alpha[9]*(sample_ml$theta - icc_data$beta[9]))
sample_ml$mprimary_2010 <- arm::invlogit(icc_data$alpha[10]*(sample_ml$theta - icc_data$beta[10]))
sample_ml$pprimary_2012 <- arm::invlogit(icc_data$alpha[11]*(sample_ml$theta - icc_data$beta[11]))
sample_ml$mprimary_2014 <- arm::invlogit(icc_data$alpha[12]*(sample_ml$theta - icc_data$beta[12]))
sample_ml$pprimary_2016 <- arm::invlogit(icc_data$alpha[13]*(sample_ml$theta - icc_data$beta[13]))
sample_ml$mprimary_2018 <- arm::invlogit(icc_data$alpha[14]*(sample_ml$theta - icc_data$beta[14]))
sample_ml$pr_midterm <- rowMeans(sample_ml[,c("midterm_2006", "midterm_2010", 
                                             "midterm_2014", "midterm_2018")])
sample_ml$pr_presidential <- rowMeans(sample_ml[,c("presidential_2008", "presidential_2012", 
                                              "presidential_2016")])
sample_ml$pr_primary <- rowMeans(sample_ml[,c("mprimary_2006", "pprimary_2008", 
                                              "mprimary_2010", "pprimary_2012",
                                              "mprimary_2014", "pprimary_2016",
                                              "mprimary_2018")])
sample_ml$type <- ifelse(sample_ml$pr_primary > 0.5 & 
                           sample_ml$pr_midterm > 0.5 & 
                           sample_ml$pr_presidential > 0.5, "highly_engaged",
                   ifelse(sample_ml$pr_primary < 0.5 & 
                            sample_ml$pr_midterm > 0.5 & 
                            sample_ml$pr_presidential > 0.5, "regular",
                   ifelse(sample_ml$pr_primary < 0.5 & 
                            sample_ml$pr_midterm < 0.5 & 
                            sample_ml$pr_presidential > 0.5, "irregular",
                   ifelse(sample_ml$pr_primary < 0.5 & 
                            sample_ml$pr_midterm < 0.5 & 
                            sample_ml$pr_presidential < 0.5, "low_propensity",NA))))
sample_ml_original <- sample_ml
sample_ml_lo <- sample_ml_original[sample_ml_original$type == "low_propensity",]
sample_ml <- dplyr::select(sample_ml, sex:party_income, id, theta, polact)
sample_ml_ir <- sample_ml_original[sample_ml_original$type == "irregular",]
sample_ml_re <- sample_ml_original[sample_ml_original$type == "regular",]
sample_ml_he <- sample_ml_original[sample_ml_original$type == "highly_engaged",]

# iterate over
sample_ml <- sample_ml_original[sample_ml_original$type == "low_propensity",]
sample_ml <- sample_ml_original[sample_ml_original$type == "regular",]
sample_ml <- sample_ml_original[sample_ml_original$type == "irregular",]
sample_ml <- sample_ml_original[sample_ml_original$type == "highly_engaged",]
data_ml_logit <- list(N = nrow(sample_ml),
                      n_sex = length(unique(sample_ml$sex)),
                      n_race = length(unique(sample_ml$race)),
                      n_party = length(unique(sample_ml$party)),
                      n_age = length(unique(sample_ml$age)),
                      n_income = length(unique(sample_ml$income)),
                      n_sex_race = length(unique(sample_ml$sex_race)),
                      n_sex_party = length(unique(sample_ml$sex_party)),
                      n_sex_age = length(unique(sample_ml$sex_age)),
                      n_sex_income = length(unique(sample_ml$sex_income)),
                      n_race_party = length(unique(sample_ml$race_party)),
                      n_race_age = length(unique(sample_ml$race_age)),
                      n_race_income = length(unique(sample_ml$race_income)),
                      n_age_party = length(unique(sample_ml$age_party)),
                      n_age_income = length(unique(sample_ml$age_income)),
                      n_party_income = length(unique(sample_ml$party_income)),
                      sex = sample_ml$sex,
                      race = sample_ml$race,
                      party = sample_ml$party,
                      age = sample_ml$age,
                      income = sample_ml$income,
                      sex_race = sample_ml$sex_race,
                      sex_party = sample_ml$sex_party,
                      sex_age = sample_ml$sex_age,
                      sex_income = sample_ml$sex_income,
                      race_party = sample_ml$race_party,
                      race_age = sample_ml$race_age,
                      race_income = sample_ml$race_income,
                      age_party = sample_ml$age_party,
                      age_income = sample_ml$age_income,
                      party_income = sample_ml$party_income,
                      theta = sample_ml$theta,
                      y = sample_ml$polact)
fit_ml_logit_he <- sampling(object = compiled_ml_logit_2,
                           data = data_ml_logit,
                           pars = pars_return2,
                           chains = 4,
                           iter = 2000,
                           warmup = 1000,
                           control = list(max_treedepth = 12),
                           save_warmup = FALSE,
                           init = "random",
                           sample_file = "./data/models/fit_ml_logit_he_chain")
fit_ml_logit_re <- sampling(object = compiled_ml_logit_2,
                            data = data_ml_logit,
                            pars = pars_return2,
                            chains = 4,
                            iter = 2000,
                            warmup = 1000,
                            control = list(max_treedepth = 12),
                            save_warmup = FALSE,
                            init = "random",
                            sample_file = "./data/models/fit_ml_logit_re_chain")
fit_ml_logit_ir <- sampling(object = compiled_ml_logit_2,
                            data = data_ml_logit,
                            pars = pars_return2,
                            chains = 4,
                            iter = 2000,
                            warmup = 1000,
                            control = list(max_treedepth = 12),
                            save_warmup = FALSE,
                            init = "random",
                            sample_file = "./data/models/fit_ml_logit_ir_chain")
fit_ml_logit_lo <- sampling(object = compiled_ml_logit_2,
                            data = data_ml_logit,
                            pars = pars_return2,
                            chains = 4,
                            iter = 2000,
                            warmup = 1000,
                            control = list(max_treedepth = 12),
                            save_warmup = FALSE,
                            init = "random",
                            sample_file = "./data/models/fit_ml_logit_lo_chain")


saveRDS(fit_ml_logit_lo, "./data/models/fit_ml_logit_lo")
# extract posterior as data.frame and array
fit_ml_logit_posterior_lo <- as.data.frame(fit_ml_logit_lo)
fit_ml_logit_posterior_array_lo <- as.array(fit_ml_logit_lo)
# save posterior
saveRDS(fit_ml_logit_posterior_lo, "./data/models/fit_ml_logit_posterior_lo")
saveRDS(fit_ml_logit_posterior_array_lo, "./data/models/fit_ml_logit_posterior_array_lo")

saveRDS(fit_ml_logit_re, "./data/models/fit_ml_logit_re")
# extract posterior as data.frame and array
fit_ml_logit_posterior_re <- as.data.frame(fit_ml_logit_re)
fit_ml_logit_posterior_array_re <- as.array(fit_ml_logit_re)
# save posterior
saveRDS(fit_ml_logit_posterior_re, "./data/models/fit_ml_logit_posterior_re")
saveRDS(fit_ml_logit_posterior_array_re, "./data/models/fit_ml_logit_posterior_array_re")

saveRDS(fit_ml_logit_ir, "./data/models/fit_ml_logit_ir")
# extract posterior as data.frame and array
fit_ml_logit_posterior_ir <- as.data.frame(fit_ml_logit_ir)
fit_ml_logit_posterior_array_ir <- as.array(fit_ml_logit_ir)
# save posterior
saveRDS(fit_ml_logit_posterior_ir, "./data/models/fit_ml_logit_posterior_ir")
saveRDS(fit_ml_logit_posterior_array_ir, "./data/models/fit_ml_logit_posterior_array_ir")

saveRDS(fit_ml_logit_he, "./data/models/fit_ml_logit_he")
# extract posterior as data.frame and array
fit_ml_logit_posterior_he <- as.data.frame(fit_ml_logit_he)
fit_ml_logit_posterior_array_he <- as.array(fit_ml_logit_he)
# save posterior
saveRDS(fit_ml_logit_posterior_he, "./data/models/fit_ml_logit_posterior_he")
saveRDS(fit_ml_logit_posterior_array_he, "./data/models/fit_ml_logit_posterior_array_he")

# low propensity
sample_ml <- sample_ml_original[sample_ml_original$type == "low_propensity",]
sample_ml <- dplyr::select(sample_ml, sex:party_income)

# create new datasets with specific variables fixed at values of interest ---------------
# sex subgroups fixed 
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) 
postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) 
# party subgroups fixed 
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_npa <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
# racial subgroups fixed 
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income))
postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) 
postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) 
postest_otherrace <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income))
# age subgroups fixed 
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
# income subgroups fixed 
postest_to_15 <- sample_ml %>% 
  mutate(income = 1L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_15_30 <- sample_ml %>% 
  mutate(income = 2L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_30_50 <- sample_ml %>% 
  mutate(income = 3L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
postest_50_75 <- sample_ml %>% 
  mutate(income = 4L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
postest_75plus <- sample_ml %>% 
  mutate(income = 5L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
fit_ml_logit_lo <- readRDS("./data/models/fit_ml_logit_lo")
parameters <- fit_ml_logit_lo@model_pars[c(32:47)]
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_lo, pars = .x)
  }) %>%
  setNames(parameters)



# specify the linear predictor ----------------------------------------------------------
linear_predictor_2 <- "mu_alpha[n] + alpha_sex[n, sex] + alpha_race[n, race] +  
alpha_party[n, party] + alpha_age[n, age] +  
alpha_income[n, income] + alpha_sex_race[n, sex_race] + 
alpha_sex_party[n, sex_party] + alpha_sex_age[n, sex_age] + 
alpha_sex_income[n, sex_income] + alpha_race_party[n, race_party] + 
alpha_race_age[n, race_age] + alpha_race_income[n, race_income] + 
alpha_age_party[n, age_party] + alpha_age_income[n, age_income] + 
alpha_party_income[n, party_income]"

# specify levels along x ----------------------------------------------------------------
levels_2 <- list(x = 1)

# generate population-averaged predictions ----------------------------------------------
# register parallel backend
registerDoParallel(6) 
# collect names of datasets
postest_datasets <- c("postest_female", "postest_male", "postest_dem", "postest_rep",
                      "postest_npa", "postest_white", "postest_black", "postest_hispanic",
                      "postest_otherrace", "postest_18_29", "postest_30_44", 
                      "postest_45_64", "postest_65plus", "postest_to_15", 
                      "postest_15_30", "postest_30_50", "postest_50_75", 
                      "postest_75plus")
# generate predictions and save to disk
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_low")))
}
rm(pred)


# irregular
sample_ml <- sample_ml_original[sample_ml_original$type == "irregular",]
sample_ml <- dplyr::select(sample_ml, sex:party_income)

# create new datasets with specific variables fixed at values of interest ---------------
# sex subgroups fixed 
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) 
postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) 
# party subgroups fixed 
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_npa <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
# racial subgroups fixed 
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income))
postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) 
postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) 
postest_otherrace <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income))
# age subgroups fixed 
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
# income subgroups fixed 
postest_to_15 <- sample_ml %>% 
  mutate(income = 1L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_15_30 <- sample_ml %>% 
  mutate(income = 2L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_30_50 <- sample_ml %>% 
  mutate(income = 3L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
postest_50_75 <- sample_ml %>% 
  mutate(income = 4L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
postest_75plus <- sample_ml %>% 
  mutate(income = 5L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
fit_ml_logit_ir <- readRDS("./data/models/fit_ml_logit_ir")
parameters <- fit_ml_logit_ir@model_pars[c(32:47)]
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_ir, pars = .x)
  }) %>%
  setNames(parameters)

# generate predictions and save to disk
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ire")))
}
rm(pred)


# regular
sample_ml <- sample_ml_original[sample_ml_original$type == "regular",]
sample_ml <- dplyr::select(sample_ml, sex:party_income)

# create new datasets with specific variables fixed at values of interest ---------------
# sex subgroups fixed 
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) 
postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) 
# party subgroups fixed 
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_npa <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
# racial subgroups fixed 
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income))
postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) 
postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) 
postest_otherrace <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income))
# age subgroups fixed 
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
# income subgroups fixed 
postest_to_15 <- sample_ml %>% 
  mutate(income = 1L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_15_30 <- sample_ml %>% 
  mutate(income = 2L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_30_50 <- sample_ml %>% 
  mutate(income = 3L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
postest_50_75 <- sample_ml %>% 
  mutate(income = 4L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
postest_75plus <- sample_ml %>% 
  mutate(income = 5L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
fit_ml_logit_re <- readRDS("./data/models/fit_ml_logit_re")
parameters <- fit_ml_logit_re@model_pars[c(32:47)]
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_re, pars = .x)
  }) %>%
  setNames(parameters)

# generate predictions and save to disk
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)


# highly engaged
sample_ml <- sample_ml_original[sample_ml_original$type == "highly_engaged",]
sample_ml <- dplyr::select(sample_ml, sex:party_income)

# create new datasets with specific variables fixed at values of interest ---------------
# sex subgroups fixed 
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) 
postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) 
# party subgroups fixed 
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_npa <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
# racial subgroups fixed 
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income))
postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) 
postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) 
postest_otherrace <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income))
# age subgroups fixed 
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) 
# income subgroups fixed 
postest_to_15 <- sample_ml %>% 
  mutate(income = 1L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_15_30 <- sample_ml %>% 
  mutate(income = 2L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_30_50 <- sample_ml %>% 
  mutate(income = 3L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
postest_50_75 <- sample_ml %>% 
  mutate(income = 4L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) 
postest_75plus <- sample_ml %>% 
  mutate(income = 5L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
fit_ml_logit_he <- readRDS("./data/models/fit_ml_logit_he")
parameters <- fit_ml_logit_he@model_pars[c(32:47)]
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_he, pars = .x)
  }) %>%
  setNames(parameters)

# generate predictions and save to disk
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)


#### ESTIMATION AND POSTESTIMATION FOR ALTERNATIVE MODEL SPECIFICATIONS =================

# allow parallel estimation by distributing chains over processor cores -----------------
options(mc.cores = parallel::detectCores())

# import data prepared for multilevel model ---------------------------------------------
sample_ml <- readRDS("./data/models/sample_ml")

# add squared version of theta ----------------------------------------------------------
sample_ml$theta_sq <- sample_ml$theta^2

# model without interactions ------------------------------------------------------------
ml_logit_rc_1 <- "// HIERARCHICAL LOGISTIC REGRESSION WITH VARYING INTERCEPTS AND SLOPES 
 // (WITHOUT INTERACTIONS)

// DATA BLOCK
data {
  int<lower=1> N; // number of observations
  int<lower=1> n_sex; // number of sexes
  int<lower=1> n_race; // number of racial groups
  int<lower=1> n_party; // number of party affiliations
  int<lower=1> n_age; // number of age groups
  int<lower=1> n_income; // number of income groups
  int<lower=1, upper=n_sex> sex[N]; // observed sex for observations
  int<lower=1, upper=n_race> race[N]; // observed racial group for observations
  int<lower=1, upper=n_party> party[N]; // observed party affiliation for observations
  int<lower=1, upper=n_age> age[N]; // observed age group for observations
  int<lower=1, upper=n_income> income[N]; // observed income group for observations
  real theta[N]; // observed turnout propensity for observations
  int<lower=0,upper=1> y[N]; // observed social media-based political engagement for observations
}

// PARAMETERS BLOCK
parameters {
  real mu_alpha_raw; // global/fixed intercept, location parameter for varying-intercept priors 
  real mu_beta_raw; // global/fixed effect for theta, location parameter for varying-slope priors
  vector[n_sex] alpha_sex_raw; // varying intercept for sexes
  vector[n_race] alpha_race_raw; // varying intercept for racial groups
  vector[n_party] alpha_party_raw; // varying intercept for party groups
  vector[n_age] alpha_age_raw; // varying intercept for age groups
  vector[n_income] alpha_income_raw; // varying intercept for income groups
  vector[n_sex] beta_sex_raw; // varying slope for 'theta' among sexes
  vector[n_race] beta_race_raw; // varying slope for 'theta' among racial groups
  vector[n_party] beta_party_raw; // varying slope for 'theta' among party affiliations
  vector[n_age] beta_age_raw; // varying slope for 'theta' among age groups
  vector[n_income] beta_income_raw; // varying slope for 'theta' among income groups
  real<lower=0> sigma_alpha_sex; // variance/scale parameter for the prior on alpha_sex
  real<lower=0> sigma_alpha_race; // variance/scale parameter for the prior on alpha_race
  real<lower=0> sigma_alpha_party; // variance/scale parameter for the prior on alpha_party
  real<lower=0> sigma_alpha_age; // variance/scale parameter for the prior on alpha_age
  real<lower=0> sigma_alpha_income; // variance/scale parameter for the prior on alpha_income
  real<lower=0> sigma_beta_sex; // variance/scale parameter for the prior on beta_sex
  real<lower=0> sigma_beta_race; // variance/scale parameter for the prior on beta_race
  real<lower=0> sigma_beta_party; // variance/scale parameter for the prior on beta_party
  real<lower=0> sigma_beta_age; // variance/scale parameter for the prior on beta_age
  real<lower=0> sigma_beta_income; // variance/scale parameter for the prior on beta_income
}

// TRANSFORMED PARAMETERS BLOCK
transformed parameters {
  real mu_alpha;
  real mu_beta;
  vector[n_sex] alpha_sex;
  vector[n_race] alpha_race;
  vector[n_party] alpha_party;
  vector[n_age] alpha_age;
  vector[n_income] alpha_income;
  vector[n_sex] beta_sex;
  vector[n_race] beta_race;
  vector[n_party] beta_party;
  vector[n_age] beta_age;
  vector[n_income] beta_income;
  
  // REPARAMETERIZATION 
  // centered parameterization makes the model nonidentifiable
  // reparameterized in terms of identifiable combinations of parameters
  mu_alpha = mean(alpha_sex_raw) + mean(alpha_race_raw) + mean(alpha_party_raw) +
    mean(alpha_age_raw) + mean(alpha_income_raw);
  mu_beta = mean(beta_sex_raw) + mean(beta_race_raw) + mean(beta_party_raw) +
    mean(beta_age_raw) + mean(beta_income_raw);
  alpha_sex = alpha_sex_raw - mean(alpha_sex_raw);
  alpha_race = alpha_race_raw - mean(alpha_race_raw);
  alpha_party = alpha_party_raw - mean(alpha_party_raw);
  alpha_age = alpha_age_raw - mean(alpha_age_raw);
  alpha_income = alpha_income_raw - mean(alpha_income_raw);
  beta_sex = beta_sex_raw - mean(beta_sex_raw);
  beta_race = beta_race_raw - mean(beta_race_raw);
  beta_party = beta_party_raw - mean(beta_party_raw);
  beta_age = beta_age_raw - mean(beta_age_raw);
  beta_income = beta_income_raw - mean(beta_income_raw);
}

// MODEL BLOCK
model{
  vector[N] pi;
  
  // PRIORS
  // weakly informative (as main model)
  mu_alpha_raw ~ student_t(5, 0, 3);
  mu_beta_raw ~ student_t(5, 0, 1);
  sigma_alpha_sex ~ student_t(4, 0, 2);
  sigma_alpha_race ~ student_t(4, 0, 2);
  sigma_alpha_party ~ student_t(4, 0, 2);
  sigma_alpha_age ~ student_t(4, 0, 2);
  sigma_alpha_income ~ student_t(4, 0, 2);
  sigma_beta_sex ~ student_t(4, 0, 2);
  sigma_beta_race ~ student_t(4, 0, 2);
  sigma_beta_party ~ student_t(4, 0, 2);
  sigma_beta_age ~ student_t(4, 0, 2);
  sigma_beta_income ~ student_t(4, 0, 2);
    
  // centered parameterization
  // applied to speed computation
  alpha_sex_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex); 
  alpha_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race);
  alpha_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party);
  alpha_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age);
  alpha_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_income);
  beta_sex_raw ~ student_t(5, mu_beta_raw, sigma_beta_sex);
  beta_race_raw ~ student_t(5, mu_beta_raw, sigma_beta_race);
  beta_party_raw ~ student_t(5, mu_beta_raw, sigma_beta_party);
  beta_age_raw ~ student_t(5, mu_beta_raw, sigma_beta_age);
  beta_income_raw ~ student_t(5, mu_beta_raw, sigma_beta_income);
  
  // LIKELIHOOD
  for (n in 1:N)
    pi[n] = alpha_sex_raw[sex[n]] + alpha_race_raw[race[n]] +  alpha_party_raw[party[n]] + 
            alpha_age_raw[age[n]] +  alpha_income_raw[income[n]] + 
            beta_sex_raw[sex[n]] * theta[n] + beta_race_raw[race[n]] * theta[n] +  
            beta_party_raw[party[n]] * theta[n] + beta_age_raw[age[n]] * theta[n] +  
            beta_income_raw[income[n]] * theta[n];
  y ~ bernoulli_logit(pi);
}"

write(ml_logit_rc_1, "./code/ml_logit_rc_1.stan")
compiled_ml_logit_rc_1 <- stan_model(file = "./code/ml_logit_rc_1.stan")
data_ml_logit_rc_1 <- list(N = nrow(sample_ml),
                           n_sex = length(unique(sample_ml$sex)),
                           n_race = length(unique(sample_ml$race)),
                           n_party = length(unique(sample_ml$party)),
                           n_age = length(unique(sample_ml$age)),
                           n_income = length(unique(sample_ml$income)),
                           sex = sample_ml$sex,
                           race = sample_ml$race,
                           party = sample_ml$party,
                           age = sample_ml$age,
                           income = sample_ml$income,
                           theta = sample_ml$theta,
                           y = sample_ml$polact)
pars_return_rc_1 <- c("mu_alpha", "mu_beta", 
                      "alpha_sex", "alpha_race", "alpha_party", "alpha_age", 
                      "alpha_income",
                      "beta_sex", "beta_race", "beta_party", "beta_age", 
                      "beta_income",
                      "sigma_alpha_sex", "sigma_alpha_race", "sigma_alpha_party", 
                      "sigma_alpha_age", "sigma_alpha_income",
                      "sigma_beta_sex", "sigma_beta_race", "sigma_beta_party", 
                      "sigma_beta_age", "sigma_beta_income")
fit_ml_logit_rc_1 <- sampling(object = compiled_ml_logit_rc_1,
                              data = data_ml_logit_rc_1,
                              pars = pars_return_rc_1,
                              chains = 4,
                              iter = 2000,
                              warmup = 1000,
                              control = list(max_treedepth = 12),
                              save_warmup = FALSE,
                              init = "random",
                              sample_file = "./data/models/fit_ml_logit_rc_1_chain")
saveRDS(fit_ml_logit_rc_1, "./data/models/fit_ml_logit_rc_1")
fit_ml_logit_posterior_rc_1 <- as.data.frame(fit_ml_logit_rc_1)
fit_ml_logit_posterior_array_rc_1 <- as.array(fit_ml_logit_rc_1)
saveRDS(fit_ml_logit_posterior_rc_1, "./data/models/fit_ml_logit_posterior_rc_1")
saveRDS(fit_ml_logit_posterior_array_rc_1, "./data/models/fit_ml_logit_posterior_array_rc_1")

# create new datasets with specific variables fixed at values of interest ---------------
# sex subgroups fixed 
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  dplyr::select(-c(id, theta, polact))
postest_male <- sample_ml %>% 
  mutate(sex = 2L)  %>%
  dplyr::select(-c(id, theta, polact))
# party subgroups fixed 
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  dplyr::select(-c(id, theta, polact))
postest_rep <- sample_ml %>% 
  mutate(party = 2L)  %>%
  dplyr::select(-c(id, theta, polact))
postest_npa <- sample_ml %>% 
  mutate(party = 3L)  %>%
  dplyr::select(-c(id, theta, polact))
# racial subgroups fixed 
postest_white <- sample_ml %>% 
  mutate(race = 1L)  %>%
  dplyr::select(-c(id, theta, polact))
postest_black <- sample_ml %>% 
  mutate(race = 2L)  %>%
  dplyr::select(-c(id, theta, polact))
postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  dplyr::select(-c(id, theta, polact))
postest_otherrace <- sample_ml %>% 
  mutate(race = 4L)  %>%
  dplyr::select(-c(id, theta, polact))
# age subgroups fixed 
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L)  %>%
  dplyr::select(-c(id, theta, polact))
postest_30_44 <- sample_ml %>% 
  mutate(age = 2L)  %>%
  dplyr::select(-c(id, theta, polact))
postest_45_64 <- sample_ml %>% 
  mutate(age = 3L)  %>%
  dplyr::select(-c(id, theta, polact))
postest_65plus <- sample_ml %>% 
  mutate(age = 4L)  %>%
  dplyr::select(-c(id, theta, polact))
# income subgroups fixed 
postest_to_15 <- sample_ml %>% 
  mutate(income = 1L)  %>%
  dplyr::select(-c(id, theta, polact))
postest_15_30 <- sample_ml %>% 
  mutate(income = 2L) %>%
  dplyr::select(-c(id, theta, polact))
postest_30_50 <- sample_ml %>% 
  mutate(income = 3L) %>%
  dplyr::select(-c(id, theta, polact))
postest_50_75 <- sample_ml %>% 
  mutate(income = 4L)  %>%
  dplyr::select(-c(id, theta, polact))
postest_75plus <- sample_ml %>% 
  mutate(income = 5L)  %>%
  dplyr::select(-c(id, theta, polact))

fit_ml_logit_rc_1 <- readRDS("./data/models/fit_ml_logit_rc_1")

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
parameters <- fit_ml_logit_rc_1@model_pars[23:34]
# collect respective posteriors in a list
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_rc_1, pars = .x)
  }) %>%
  setNames(parameters)

# specify the linear predictor ----------------------------------------------------------
linear_predictor <- "mu_alpha[n] + mu_beta[n] * x[k] + alpha_sex[n, sex] + 
                     alpha_race[n, race] +  alpha_party[n, party] + 
                     alpha_age[n, age] +  alpha_income[n, income] + 
                     beta_sex[n, sex] * x[k] + beta_race[n, race] * x[k] +  
                     beta_party[n, party] * x[k] + beta_age[n, age] * x[k] +  
                     beta_income[n, income] * x[k]"

# specify levels along x ----------------------------------------------------------------
levels <- list(x = seq(from = -2.2, to = 2.2, length.out = 50))

# generate population-averaged predictions ----------------------------------------------
# register parallel backend
registerDoParallel(6) 
# collect names of datasets
postest_datasets <- c("postest_female", "postest_male", "postest_dem", "postest_rep",
                      "postest_npa", "postest_white", "postest_black", "postest_hispanic",
                      "postest_otherrace", "postest_18_29", "postest_30_44", 
                      "postest_45_64", "postest_65plus", "postest_to_15", 
                      "postest_15_30", "postest_30_50", "postest_50_75", 
                      "postest_75plus")
# generate predictions and save to disk
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels, 
                 draws = sum(fit_ml_logit_rc_1@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_rc_1")))
}
rm(pred)

# model with cubic predictor ------------------------------------------------------------
ml_logit_rc_2 <- "// HIERARCHICAL LOGISTIC REGRESSION WITH VARYING INTERCEPTS AND SLOPES
 // (WITH CUBIC THETA)

// DATA BLOCK
data {
  int<lower=1> N; // number of observations
  int<lower=1> n_sex; // number of sexes
  int<lower=1> n_race; // number of racial groups
  int<lower=1> n_party; // number of party affiliations
  int<lower=1> n_age; // number of age groups
  int<lower=1> n_income; // number of income groups
  int<lower=1> n_sex_race; // number of sexes by racial group
  int<lower=1> n_sex_party; // number of sexes by party affiliation
  int<lower=1> n_sex_age; // number of sexes by age group
  int<lower=1> n_sex_income; // number of sexes by income group
  int<lower=1> n_race_party; // number of racial groups by party affiliation
  int<lower=1> n_race_age; // number of racial groups by age
  int<lower=1> n_race_income; // number of racial groups by income group
  int<lower=1> n_age_party; // number of age groups by party affiliation
  int<lower=1> n_age_income; // number of age groups by income group
  int<lower=1> n_party_income; // number of party affiliations by income group 
  int<lower=1, upper=n_sex> sex[N]; // observed sex for observations
  int<lower=1, upper=n_race> race[N]; // observed racial group for observations
  int<lower=1, upper=n_party> party[N]; // observed party affiliation for observations
  int<lower=1, upper=n_age> age[N]; // observed age group for observations
  int<lower=1, upper=n_income> income[N]; // observed income group for observations
  int<lower=1, upper=n_sex_race> sex_race[N]; // observed racial group with specific sex for observations
  int<lower=1, upper=n_sex_party> sex_party[N]; // observed party affiliation with specific sex for observations
  int<lower=1, upper=n_sex_age> sex_age[N]; // observed age group with specific sex for observations
  int<lower=1, upper=n_sex_income> sex_income[N]; // observed income group with specific sex for obeservations
  int<lower=1, upper=n_race_party> race_party[N]; // observed party affiliation with specific race for observations
  int<lower=1, upper=n_race_age> race_age[N]; // observed age group with specific racial race for observations
  int<lower=1, upper=n_race_income> race_income[N]; // observed income group with specific race for observations
  int<lower=1, upper=n_age_party> age_party[N]; // observed party affiliation with specific age for observations
  int<lower=1, upper=n_age_income> age_income[N]; // observed income group with specific age for observations
  int<lower=1, upper=n_party_income> party_income[N]; // observed income group with specific party affiliation for observations
  real theta[N]; // observed turnout propensity for observations
  real theta_sq[N]; // squared observed turnout propensity for observations
  int<lower=0,upper=1> y[N]; // observed social media-based political engagement for observations
}

// PARAMETERS BLOCK
parameters {
  real mu_alpha_raw; // global/fixed intercept, location parameter for varying-intercept priors 
  real mu_beta_raw; // global/fixed effect for theta, location parameter for varying-slope priors
  vector[n_sex] alpha_sex_raw; // varying intercept for sexes
  vector[n_race] alpha_race_raw; // varying intercept for racial groups
  vector[n_party] alpha_party_raw; // varying intercept for party groups
  vector[n_age] alpha_age_raw; // varying intercept for age groups
  vector[n_income] alpha_income_raw; // varying intercept for income groups
  vector[n_sex_race] alpha_sex_race_raw; // varying intercept for sex-race interaction
  vector[n_sex_party] alpha_sex_party_raw; // varying intercept for sex-party interaction
  vector[n_sex_age] alpha_sex_age_raw; // varying intercept for sex-age interaction
  vector[n_sex_income] alpha_sex_income_raw; // varying intercept for sex-income interaction
  vector[n_race_party] alpha_race_party_raw; // varying intercept for race-party interaction
  vector[n_race_age] alpha_race_age_raw; // varying intercept for race-age interaction
  vector[n_race_income] alpha_race_income_raw; // varying intercept for race-income interaction
  vector[n_age_party] alpha_age_party_raw; // varying intercept for age-party interaction
  vector[n_age_income] alpha_age_income_raw; // varying intercept for age-income interaction
  vector[n_party_income] alpha_party_income_raw; // varying intercept for party-income interaction
  vector[n_sex] beta_sex_raw; // varying slope for 'theta' among sexes
  vector[n_race] beta_race_raw; // varying slope for 'theta' among racial groups
  vector[n_party] beta_party_raw; // varying slope for 'theta' among party affiliations
  vector[n_age] beta_age_raw; // varying slope for 'theta' among age groups
  vector[n_income] beta_income_raw; // varying slope for 'theta' among income groups
  vector[n_sex] beta_sex_raw_sq; // varying slope for 'squared theta' among sexes
  vector[n_race] beta_race_raw_sq; // varying slope for 'squared theta' among racial groups
  vector[n_party] beta_party_raw_sq; // varying slope for 'squared theta' among party affiliations
  vector[n_age] beta_age_raw_sq; // varying slope for 'squared theta' among age groups
  vector[n_income] beta_income_raw_sq; // varying slope for 'squared theta' among income groups
  real<lower=0> sigma_alpha_sex; // variance/scale parameter for the prior on alpha_sex
  real<lower=0> sigma_alpha_race; // variance/scale parameter for the prior on alpha_race
  real<lower=0> sigma_alpha_party; // variance/scale parameter for the prior on alpha_party
  real<lower=0> sigma_alpha_age; // variance/scale parameter for the prior on alpha_age
  real<lower=0> sigma_alpha_income; // variance/scale parameter for the prior on alpha_income
  real<lower=0> sigma_alpha_sex_race; // variance/scale parameter for the prior on alpha_sex_race
  real<lower=0> sigma_alpha_sex_party; // variance/scale parameter for the prior on alpha_sex_party
  real<lower=0> sigma_alpha_sex_age; // variance/scale parameter for the prior on alpha_sex_age
  real<lower=0> sigma_alpha_sex_income; // variance/scale parameter for the prior on alpha_sex_income
  real<lower=0> sigma_alpha_race_party; // variance/scale parameter for the prior on alpha_race_party
  real<lower=0> sigma_alpha_race_age; // variance/scale parameter for the prior on alpha_race_age
  real<lower=0> sigma_alpha_race_income; // variance/scale parameter for the prior on alpha_race_income
  real<lower=0> sigma_alpha_age_party; // variance/scale parameter for the prior on alpha_age_party
  real<lower=0> sigma_alpha_age_income; // variance/scale parameter for the prior on alpha_age_income
  real<lower=0> sigma_alpha_party_income; // variance/scale parameter for the prior on alpha_party_income
  real<lower=0> sigma_beta_sex; // variance/scale parameter for the prior on beta_sex
  real<lower=0> sigma_beta_race; // variance/scale parameter for the prior on beta_race
  real<lower=0> sigma_beta_party; // variance/scale parameter for the prior on beta_party
  real<lower=0> sigma_beta_age; // variance/scale parameter for the prior on beta_age
  real<lower=0> sigma_beta_income; // variance/scale parameter for the prior on beta_income
  real<lower=0> sigma_beta_sex_sq; // variance/scale parameter for the prior on beta_sex_sq
  real<lower=0> sigma_beta_race_sq; // variance/scale parameter for the prior on beta_race_sq
  real<lower=0> sigma_beta_party_sq; // variance/scale parameter for the prior on beta_party_sq
  real<lower=0> sigma_beta_age_sq; // variance/scale parameter for the prior on beta_age_sq
  real<lower=0> sigma_beta_income_sq; // variance/scale parameter for the prior on beta_income_sq
}

// TRANSFORMED PARAMETERS BLOCK
transformed parameters {
  real mu_alpha;
  real mu_beta;
  vector[n_sex] alpha_sex;
  vector[n_race] alpha_race;
  vector[n_party] alpha_party;
  vector[n_age] alpha_age;
  vector[n_income] alpha_income;
  vector[n_sex_race] alpha_sex_race;
  vector[n_sex_party] alpha_sex_party;
  vector[n_sex_age] alpha_sex_age;
  vector[n_sex_income] alpha_sex_income;
  vector[n_race_party] alpha_race_party;
  vector[n_race_age] alpha_race_age;
  vector[n_race_income] alpha_race_income;
  vector[n_age_party] alpha_age_party;
  vector[n_age_income] alpha_age_income;
  vector[n_party_income] alpha_party_income;
  vector[n_sex] beta_sex;
  vector[n_race] beta_race;
  vector[n_party] beta_party;
  vector[n_age] beta_age;
  vector[n_income] beta_income;
  vector[n_sex] beta_sex_sq;
  vector[n_race] beta_race_sq;
  vector[n_party] beta_party_sq;
  vector[n_age] beta_age_sq;
  vector[n_income] beta_income_sq;
  
  // REPARAMETERIZATION 
  // centered parameterization makes the model nonidentifiable
  // reparameterized in terms of identifiable combinations of parameters
  mu_alpha = mean(alpha_sex_raw) + mean(alpha_race_raw) + mean(alpha_party_raw) +
    mean(alpha_age_raw) + mean(alpha_income_raw) + mean(alpha_sex_race_raw) +
    mean(alpha_sex_party_raw) + mean(alpha_sex_age_raw) + mean(alpha_sex_income_raw) +
    mean(alpha_race_party_raw) + mean(alpha_race_age_raw) + mean(alpha_race_income_raw) +
    mean(alpha_age_party_raw) + mean(alpha_age_income_raw) + mean(alpha_party_income_raw);
  mu_beta = mean(beta_sex_raw) + mean(beta_race_raw) + mean(beta_party_raw) +
    mean(beta_age_raw) + mean(beta_income_raw) + mean(beta_sex_raw_sq) + 
    mean(beta_race_raw_sq) + mean(beta_party_raw_sq) + mean(beta_age_raw_sq) + 
    mean(beta_income_raw_sq);
  alpha_sex = alpha_sex_raw - mean(alpha_sex_raw);
  alpha_race = alpha_race_raw - mean(alpha_race_raw);
  alpha_party = alpha_party_raw - mean(alpha_party_raw);
  alpha_age = alpha_age_raw - mean(alpha_age_raw);
  alpha_income = alpha_income_raw - mean(alpha_income_raw);
  alpha_sex_race = alpha_sex_race_raw - mean(alpha_sex_race_raw);
  alpha_sex_party = alpha_sex_party_raw - mean(alpha_sex_party_raw);
  alpha_sex_age = alpha_sex_age_raw - mean(alpha_sex_age_raw);
  alpha_sex_income = alpha_sex_income_raw - mean(alpha_sex_income_raw);
  alpha_race_party = alpha_race_party_raw - mean(alpha_race_party_raw);
  alpha_race_age = alpha_race_age_raw - mean(alpha_race_age_raw);
  alpha_race_income = alpha_race_income_raw - mean(alpha_race_income_raw);
  alpha_age_party = alpha_age_party_raw - mean(alpha_age_party_raw);
  alpha_age_income = alpha_age_income_raw - mean(alpha_age_income_raw);
  alpha_party_income = alpha_party_income_raw - mean(alpha_party_income_raw);
  beta_sex = beta_sex_raw - mean(beta_sex_raw);
  beta_race = beta_race_raw - mean(beta_race_raw);
  beta_party = beta_party_raw - mean(beta_party_raw);
  beta_age = beta_age_raw - mean(beta_age_raw);
  beta_income = beta_income_raw - mean(beta_income_raw);
  beta_sex_sq = beta_sex_raw_sq - mean(beta_sex_raw_sq);
  beta_race_sq = beta_race_raw_sq - mean(beta_race_raw_sq);
  beta_party_sq = beta_party_raw_sq - mean(beta_party_raw_sq);
  beta_age_sq = beta_age_raw_sq - mean(beta_age_raw_sq);
  beta_income_sq = beta_income_raw_sq - mean(beta_income_raw_sq);
}

// MODEL BLOCK
model{
  vector[N] pi;
  
  // PRIORS
  // weakly informative (main)
  mu_alpha_raw ~ student_t(5, 0, 3); // 5
  mu_beta_raw ~ student_t(5, 0, 1); // 2.5
  sigma_alpha_sex ~ student_t(4, 0, 2); // 5
  sigma_alpha_race ~ student_t(4, 0, 2);
  sigma_alpha_party ~ student_t(4, 0, 2);
  sigma_alpha_age ~ student_t(4, 0, 2);
  sigma_alpha_income ~ student_t(4, 0, 2);
  sigma_alpha_sex_race ~ student_t(4, 0, 2);
  sigma_alpha_sex_party ~ student_t(4, 0, 2);
  sigma_alpha_sex_age ~ student_t(4, 0, 2);
  sigma_alpha_sex_income ~ student_t(4, 0, 2);
  sigma_alpha_race_party ~ student_t(4, 0, 2);
  sigma_alpha_race_age ~ student_t(4, 0, 2);
  sigma_alpha_race_income ~ student_t(4, 0, 2);
  sigma_alpha_age_party ~ student_t(4, 0, 2);
  sigma_alpha_age_income ~ student_t(4, 0, 2);
  sigma_alpha_party_income ~ student_t(4, 0, 2);
  sigma_beta_sex ~ student_t(4, 0, 2);
  sigma_beta_race ~ student_t(4, 0, 2);
  sigma_beta_party ~ student_t(4, 0, 2);
  sigma_beta_age ~ student_t(4, 0, 2);
  sigma_beta_income ~ student_t(4, 0, 2);
  sigma_beta_sex_sq ~ student_t(4, 0, 2);
  sigma_beta_race_sq ~ student_t(4, 0, 2);
  sigma_beta_party_sq ~ student_t(4, 0, 2);
  sigma_beta_age_sq ~ student_t(4, 0, 2);
  sigma_beta_income_sq ~ student_t(4, 0, 2);
    
  // centered parameterization
  // applied to speed computation
  alpha_sex_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex); 
  alpha_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race);
  alpha_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party);
  alpha_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age);
  alpha_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_income);
  alpha_sex_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_race); 
  alpha_sex_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_party);
  alpha_sex_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_age);
  alpha_sex_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_income);
  alpha_race_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_party);
  alpha_race_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_age);
  alpha_race_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_income);
  alpha_age_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age_party);
  alpha_age_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age_income);
  alpha_party_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party_income);
  beta_sex_raw ~ student_t(5, mu_beta_raw, sigma_beta_sex);
  beta_race_raw ~ student_t(5, mu_beta_raw, sigma_beta_race);
  beta_party_raw ~ student_t(5, mu_beta_raw, sigma_beta_party);
  beta_age_raw ~ student_t(5, mu_beta_raw, sigma_beta_age);
  beta_income_raw ~ student_t(5, mu_beta_raw, sigma_beta_income);
  beta_sex_raw_sq ~ student_t(5, mu_beta_raw, sigma_beta_sex_sq);
  beta_race_raw_sq ~ student_t(5, mu_beta_raw, sigma_beta_race_sq);
  beta_party_raw_sq ~ student_t(5, mu_beta_raw, sigma_beta_party_sq);
  beta_age_raw_sq ~ student_t(5, mu_beta_raw, sigma_beta_age_sq);
  beta_income_raw_sq ~ student_t(5, mu_beta_raw, sigma_beta_income_sq);
  
  // LIKELIHOOD
  for (n in 1:N)
    pi[n] = alpha_sex_raw[sex[n]] + alpha_race_raw[race[n]] +  alpha_party_raw[party[n]] + 
            alpha_age_raw[age[n]] +  alpha_income_raw[income[n]] + 
            alpha_sex_race_raw[sex_race[n]] + alpha_sex_party_raw[sex_party[n]] + 
            alpha_sex_age_raw[sex_age[n]] + alpha_sex_income_raw[sex_income[n]] + 
            alpha_race_party_raw[race_party[n]] + alpha_race_age_raw[race_age[n]] +
            alpha_race_income_raw[race_income[n]] + alpha_age_party_raw[age_party[n]] + 
            alpha_age_income_raw[age_income[n]] + alpha_party_income_raw[party_income[n]] + 
            beta_sex_raw[sex[n]] * theta[n] + beta_race_raw[race[n]] * theta[n] +  
            beta_party_raw[party[n]] * theta[n] + beta_age_raw[age[n]] * theta[n] +  
            beta_income_raw[income[n]] * theta[n] +
            beta_sex_raw_sq[sex[n]] * theta_sq[n] + beta_race_raw_sq[race[n]] * theta_sq[n] +  
            beta_party_raw_sq[party[n]] * theta_sq[n] + beta_age_raw_sq[age[n]] * theta_sq[n] +  
            beta_income_raw_sq[income[n]] * theta_sq[n];
  y ~ bernoulli_logit(pi);
}"

write(ml_logit_rc_2, "./code/ml_logit_rc_2.stan")
compiled_ml_logit_rc_2 <- stan_model(file = "./code/ml_logit_rc_2.stan")
data_ml_logit_rc_2 <- list(N = nrow(sample_ml),
                           n_sex = length(unique(sample_ml$sex)),
                           n_race = length(unique(sample_ml$race)),
                           n_party = length(unique(sample_ml$party)),
                           n_age = length(unique(sample_ml$age)),
                           n_income = length(unique(sample_ml$income)),
                           n_sex_race = length(unique(sample_ml$sex_race)),
                           n_sex_party = length(unique(sample_ml$sex_party)),
                           n_sex_age = length(unique(sample_ml$sex_age)),
                           n_sex_income = length(unique(sample_ml$sex_income)),
                           n_race_party = length(unique(sample_ml$race_party)),
                           n_race_age = length(unique(sample_ml$race_age)),
                           n_race_income = length(unique(sample_ml$race_income)),
                           n_age_party = length(unique(sample_ml$age_party)),
                           n_age_income = length(unique(sample_ml$age_income)),
                           n_party_income = length(unique(sample_ml$party_income)),
                           sex = sample_ml$sex,
                           race = sample_ml$race,
                           party = sample_ml$party,
                           age = sample_ml$age,
                           income = sample_ml$income,
                           sex_race = sample_ml$sex_race,
                           sex_party = sample_ml$sex_party,
                           sex_age = sample_ml$sex_age,
                           sex_income = sample_ml$sex_income,
                           race_party = sample_ml$race_party,
                           race_age = sample_ml$race_age,
                           race_income = sample_ml$race_income,
                           age_party = sample_ml$age_party,
                           age_income = sample_ml$age_income,
                           party_income = sample_ml$party_income,
                           theta = sample_ml$theta,
                           theta_sq = sample_ml$theta_sq,
                           y = sample_ml$polact)
pars_return_rc_2 <- c("mu_alpha", "mu_beta", 
                      "alpha_sex", "alpha_race", "alpha_party", "alpha_age", 
                      "alpha_income", "alpha_sex_race", "alpha_sex_party", 
                      "alpha_sex_age", "alpha_sex_income", 
                      "alpha_race_party", "alpha_race_age", 
                      "alpha_race_income", "alpha_age_party",
                      "alpha_age_income", "alpha_party_income", 
                      "beta_sex", "beta_race", "beta_party", "beta_age", 
                      "beta_income",
                      "beta_sex_sq", "beta_race_sq", "beta_party_sq", "beta_age_sq", 
                      "beta_income_sq",
                      "sigma_alpha_sex", "sigma_alpha_race", "sigma_alpha_party", 
                      "sigma_alpha_age", "sigma_alpha_income", "sigma_alpha_sex_race", 
                      "sigma_alpha_sex_party", "sigma_alpha_sex_age", 
                      "sigma_alpha_sex_income", "sigma_alpha_race_party", 
                      "sigma_alpha_race_age", "sigma_alpha_race_income", 
                      "sigma_alpha_age_party","sigma_alpha_age_income", 
                      "sigma_alpha_party_income", 
                      "sigma_beta_sex", "sigma_beta_race", "sigma_beta_party", 
                      "sigma_beta_age", "sigma_beta_income", 
                      "sigma_beta_sex_sq", "sigma_beta_race_sq", "sigma_beta_party_sq", 
                      "sigma_beta_age_sq", "sigma_beta_income_sq")
fit_ml_logit_rc_2 <- sampling(object = compiled_ml_logit_rc_2,
                              data = data_ml_logit_rc_2,
                              pars = pars_return_rc_2,
                              chains = 4,
                              iter = 2000,
                              warmup = 1000,
                              control = list(max_treedepth = 12),
                              save_warmup = FALSE,
                              init = "random",
                              sample_file = "./data/models/fit_ml_logit_rc_2_chain")

saveRDS(fit_ml_logit_rc_2, "./data/models/fit_ml_logit_rc_2")
fit_ml_logit_posterior_rc_2 <- as.data.frame(fit_ml_logit_rc_2)
fit_ml_logit_posterior_array_rc_2 <- as.array(fit_ml_logit_rc_2)
saveRDS(fit_ml_logit_posterior_rc_2, "./data/models/fit_ml_logit_posterior_rc_2")
saveRDS(fit_ml_logit_posterior_array_rc_2, "./data/models/fit_ml_logit_posterior_array_rc_2")




key_sex <- c("female" = 1L, "male" = 2L)
key_party <- c("dem" = 1L, "rep" = 2L, "npa" = 3L, "other" = 3L) 
key_race <- c("white" = 1L, "black" = 2L, "hispanic" = 3L, "multi" = 4L,
              "asian" = 4L, "native" = 4L, "other" = 4L)
key_sex_race <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, 
                  "2_1" = 5L, "2_2" = 6L, "2_3" = 7L, "2_4" = 8L)
key_sex_party <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, 
                   "2_1" = 4L, "2_2" = 5L, "2_3" = 6L)
key_sex_age <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, 
                 "2_1" = 5L, "2_2" = 6L, "2_3" = 7L, "2_4" = 8L)
key_sex_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L, 
                    "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L)
key_race_party <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, 
                    "2_1" = 4L, "2_2" = 5L, "2_3" = 6L, 
                    "3_1" = 7L, "3_2" = 8L, "3_3" = 9L,
                    "4_1" = 10L, "4_2" = 11L, "4_3" = 12L)
key_race_age <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, 
                  "2_1" = 5L, "2_2" = 6L, "2_3" = 7L, "2_4" = 8L, 
                  "3_1" = 9L, "3_2" = 10L, "3_3" = 11L, "3_4" = 12L,
                  "4_1" = 13L, "4_2" = 14L, "4_3" = 15L, "4_4" = 16L)
key_race_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L,
                     "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L,
                     "3_1" = 11L, "3_2" = 12L, "3_3" = 13L, "3_4" = 14L, "3_5" = 15L,
                     "4_1" = 16L, "4_2" = 17L, "4_3" = 18L, "4_4" = 19L, "4_5" = 20L)
key_age_party <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, 
                   "2_1" = 4L, "2_2" = 5L, "2_3" = 6L, 
                   "3_1" = 7L, "3_2" = 8L, "3_3" = 9L,
                   "4_1" = 10L, "4_2" = 11L, "4_3" = 12L)
key_age_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L,
                    "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L,
                    "3_1" = 11L, "3_2" = 12L, "3_3" = 13L, "3_4" = 14L, "3_5" = 15L,
                    "4_1" = 16L, "4_2" = 17L, "4_3" = 18L, "4_4" = 19L, "4_5" = 20L)
key_party_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L,
                      "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L,
                      "3_1" = 11L, "3_2" = 12L, "3_3" = 13L, "3_4" = 14L, "3_5" = 15L)



# create new datasets with specific variables fixed at values of interest ---------------
# sex subgroups fixed 
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) %>%
  dplyr::select(-c(id, theta, polact))
# party subgroups fixed 
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_npa <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
# racial subgroups fixed 
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_otherrace <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
# age subgroups fixed 
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
# income subgroups fixed 
postest_to_15 <- sample_ml %>% 
  mutate(income = 1L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_15_30 <- sample_ml %>% 
  mutate(income = 2L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_30_50 <- sample_ml %>% 
  mutate(income = 3L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_50_75 <- sample_ml %>% 
  mutate(income = 4L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_75plus <- sample_ml %>% 
  mutate(income = 5L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))

fit_ml_logit_rc_2 <- readRDS("./data/models/fit_ml_logit_rc_2")

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
parameters <- fit_ml_logit_rc_2@model_pars[53:79]
# collect respective posteriors in a list
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_rc_2, pars = .x)
  }) %>%
  setNames(parameters)

# specify the linear predictor ----------------------------------------------------------
linear_predictor <- "mu_alpha[n] + mu_beta[n] * x[k] + 
                    alpha_sex[n, sex] + alpha_race[n, race] +  alpha_party[n, party] + 
                    alpha_age[n, age] +  alpha_income[n, income] + 
                    alpha_sex_race[n, sex_race] + alpha_sex_party[n, sex_party] + 
                    alpha_sex_age[n, sex_age] + alpha_sex_income[n, sex_income] + 
                    alpha_race_party[n, race_party] + alpha_race_age[n, race_age] +
                    alpha_race_income[n, race_income] + alpha_age_party[n, age_party] + 
                    alpha_age_income[n, age_income] + alpha_party_income[n, party_income] + 
                    beta_sex[n, sex] * x[k] + beta_race[n, race] * x[k] +  
                    beta_party[n, party] * x[k] + beta_age[n, age] * x[k] +  
                    beta_income[n, income] * x[k] + beta_sex_sq[n, sex] * x_sq[k] + beta_race_sq[n, race] * x_sq[k] +  
                    beta_party_sq[n, party] * x_sq[k] + beta_age_sq[n, age] * x_sq[k] +  
                    beta_income_sq[n, income] * x_sq[k]"

# specify levels along x ----------------------------------------------------------------
levels <- list(x = seq(from = -2.2, to = 2.2, length.out = 50),
               x_sq = seq(from = -2.2, to = 2.2, length.out = 50)^2)

# generate population-averaged predictions ----------------------------------------------
# register parallel backend
registerDoParallel(6) 
# collect names of datasets
postest_datasets <- c("postest_female", "postest_male", "postest_dem", "postest_rep",
                      "postest_npa", "postest_white", "postest_black", "postest_hispanic",
                      "postest_otherrace", "postest_18_29", "postest_30_44", 
                      "postest_45_64", "postest_65plus", "postest_to_15", 
                      "postest_15_30", "postest_30_50", "postest_50_75", 
                      "postest_75plus")
# generate predictions and save to disk
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels, 
                 draws = sum(fit_ml_logit_rc_2@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_rc_2")))
}
rm(pred)

# model with less regularizing priors ---------------------------------------------------
ml_logit_rc_3 <- "// HIERARCHICAL LOGISTIC REGRESSION WITH VARYING INTERCEPTS AND SLOPES 
 // (LESS REGULARIZING PRIORS)

// DATA BLOCK
data {
  int<lower=1> N; // number of observations
  int<lower=1> n_sex; // number of sexes
  int<lower=1> n_race; // number of racial groups
  int<lower=1> n_party; // number of party affiliations
  int<lower=1> n_age; // number of age groups
  int<lower=1> n_income; // number of income groups
  int<lower=1> n_sex_race; // number of sexes by racial group
  int<lower=1> n_sex_party; // number of sexes by party affiliation
  int<lower=1> n_sex_age; // number of sexes by age group
  int<lower=1> n_sex_income; // number of sexes by income group
  int<lower=1> n_race_party; // number of racial groups by party affiliation
  int<lower=1> n_race_age; // number of racial groups by age
  int<lower=1> n_race_income; // number of racial groups by income group
  int<lower=1> n_age_party; // number of age groups by party affiliation
  int<lower=1> n_age_income; // number of age groups by income group
  int<lower=1> n_party_income; // number of party affiliations by income group 
  int<lower=1, upper=n_sex> sex[N]; // observed sex for observations
  int<lower=1, upper=n_race> race[N]; // observed racial group for observations
  int<lower=1, upper=n_party> party[N]; // observed party affiliation for observations
  int<lower=1, upper=n_age> age[N]; // observed age group for observations
  int<lower=1, upper=n_income> income[N]; // observed income group for observations
  int<lower=1, upper=n_sex_race> sex_race[N]; // observed racial group with specific sex for observations
  int<lower=1, upper=n_sex_party> sex_party[N]; // observed party affiliation with specific sex for observations
  int<lower=1, upper=n_sex_age> sex_age[N]; // observed age group with specific sex for observations
  int<lower=1, upper=n_sex_income> sex_income[N]; // observed income group with specific sex for obeservations
  int<lower=1, upper=n_race_party> race_party[N]; // observed party affiliation with specific race for observations
  int<lower=1, upper=n_race_age> race_age[N]; // observed age group with specific racial race for observations
  int<lower=1, upper=n_race_income> race_income[N]; // observed income group with specific race for observations
  int<lower=1, upper=n_age_party> age_party[N]; // observed party affiliation with specific age for observations
  int<lower=1, upper=n_age_income> age_income[N]; // observed income group with specific age for observations
  int<lower=1, upper=n_party_income> party_income[N]; // observed income group with specific party affiliation for observations
  real theta[N]; // observed turnout propensity for observations
  int<lower=0,upper=1> y[N]; // observed social media-based political engagement for observations
}

// PARAMETERS BLOCK
parameters {
  real mu_alpha_raw; // global/fixed intercept, location parameter for varying-intercept priors 
  real mu_beta_raw; // global/fixed effect for theta, location parameter for varying-slope priors
  vector[n_sex] alpha_sex_raw; // varying intercept for sexes
  vector[n_race] alpha_race_raw; // varying intercept for racial groups
  vector[n_party] alpha_party_raw; // varying intercept for party groups
  vector[n_age] alpha_age_raw; // varying intercept for age groups
  vector[n_income] alpha_income_raw; // varying intercept for income groups
  vector[n_sex_race] alpha_sex_race_raw; // varying intercept for sex-race interaction
  vector[n_sex_party] alpha_sex_party_raw; // varying intercept for sex-party interaction
  vector[n_sex_age] alpha_sex_age_raw; // varying intercept for sex-age interaction
  vector[n_sex_income] alpha_sex_income_raw; // varying intercept for sex-income interaction
  vector[n_race_party] alpha_race_party_raw; // varying intercept for race-party interaction
  vector[n_race_age] alpha_race_age_raw; // varying intercept for race-age interaction
  vector[n_race_income] alpha_race_income_raw; // varying intercept for race-income interaction
  vector[n_age_party] alpha_age_party_raw; // varying intercept for age-party interaction
  vector[n_age_income] alpha_age_income_raw; // varying intercept for age-income interaction
  vector[n_party_income] alpha_party_income_raw; // varying intercept for party-income interaction
  vector[n_sex] beta_sex_raw; // varying slope for 'theta' among sexes
  vector[n_race] beta_race_raw; // varying slope for 'theta' among racial groups
  vector[n_party] beta_party_raw; // varying slope for 'theta' among party affiliations
  vector[n_age] beta_age_raw; // varying slope for 'theta' among age groups
  vector[n_income] beta_income_raw; // varying slope for 'theta' among income groups
  real<lower=0> sigma_alpha_sex; // variance/scale parameter for the prior on alpha_sex
  real<lower=0> sigma_alpha_race; // variance/scale parameter for the prior on alpha_race
  real<lower=0> sigma_alpha_party; // variance/scale parameter for the prior on alpha_party
  real<lower=0> sigma_alpha_age; // variance/scale parameter for the prior on alpha_age
  real<lower=0> sigma_alpha_income; // variance/scale parameter for the prior on alpha_income
  real<lower=0> sigma_alpha_sex_race; // variance/scale parameter for the prior on alpha_sex_race
  real<lower=0> sigma_alpha_sex_party; // variance/scale parameter for the prior on alpha_sex_party
  real<lower=0> sigma_alpha_sex_age; // variance/scale parameter for the prior on alpha_sex_age
  real<lower=0> sigma_alpha_sex_income; // variance/scale parameter for the prior on alpha_sex_income
  real<lower=0> sigma_alpha_race_party; // variance/scale parameter for the prior on alpha_race_party
  real<lower=0> sigma_alpha_race_age; // variance/scale parameter for the prior on alpha_race_age
  real<lower=0> sigma_alpha_race_income; // variance/scale parameter for the prior on alpha_race_income
  real<lower=0> sigma_alpha_age_party; // variance/scale parameter for the prior on alpha_age_party
  real<lower=0> sigma_alpha_age_income; // variance/scale parameter for the prior on alpha_age_income
  real<lower=0> sigma_alpha_party_income; // variance/scale parameter for the prior on alpha_party_income
  real<lower=0> sigma_beta_sex; // variance/scale parameter for the prior on beta_sex
  real<lower=0> sigma_beta_race; // variance/scale parameter for the prior on beta_race
  real<lower=0> sigma_beta_party; // variance/scale parameter for the prior on beta_party
  real<lower=0> sigma_beta_age; // variance/scale parameter for the prior on beta_age
  real<lower=0> sigma_beta_income; // variance/scale parameter for the prior on beta_income
}

// TRANSFORMED PARAMETERS BLOCK
transformed parameters {
  real mu_alpha;
  real mu_beta;
  vector[n_sex] alpha_sex;
  vector[n_race] alpha_race;
  vector[n_party] alpha_party;
  vector[n_age] alpha_age;
  vector[n_income] alpha_income;
  vector[n_sex_race] alpha_sex_race;
  vector[n_sex_party] alpha_sex_party;
  vector[n_sex_age] alpha_sex_age;
  vector[n_sex_income] alpha_sex_income;
  vector[n_race_party] alpha_race_party;
  vector[n_race_age] alpha_race_age;
  vector[n_race_income] alpha_race_income;
  vector[n_age_party] alpha_age_party;
  vector[n_age_income] alpha_age_income;
  vector[n_party_income] alpha_party_income;
  vector[n_sex] beta_sex;
  vector[n_race] beta_race;
  vector[n_party] beta_party;
  vector[n_age] beta_age;
  vector[n_income] beta_income;
  
  // REPARAMETERIZATION 
  // centered parameterization makes the model nonidentifiable
  // reparameterized in terms of identifiable combinations of parameters
  mu_alpha = mean(alpha_sex_raw) + mean(alpha_race_raw) + mean(alpha_party_raw) +
    mean(alpha_age_raw) + mean(alpha_income_raw) + mean(alpha_sex_race_raw) +
    mean(alpha_sex_party_raw) + mean(alpha_sex_age_raw) + mean(alpha_sex_income_raw) +
    mean(alpha_race_party_raw) + mean(alpha_race_age_raw) + mean(alpha_race_income_raw) +
    mean(alpha_age_party_raw) + mean(alpha_age_income_raw) + mean(alpha_party_income_raw);
  mu_beta = mean(beta_sex_raw) + mean(beta_race_raw) + mean(beta_party_raw) +
    mean(beta_age_raw) + mean(beta_income_raw);
  alpha_sex = alpha_sex_raw - mean(alpha_sex_raw);
  alpha_race = alpha_race_raw - mean(alpha_race_raw);
  alpha_party = alpha_party_raw - mean(alpha_party_raw);
  alpha_age = alpha_age_raw - mean(alpha_age_raw);
  alpha_income = alpha_income_raw - mean(alpha_income_raw);
  alpha_sex_race = alpha_sex_race_raw - mean(alpha_sex_race_raw);
  alpha_sex_party = alpha_sex_party_raw - mean(alpha_sex_party_raw);
  alpha_sex_age = alpha_sex_age_raw - mean(alpha_sex_age_raw);
  alpha_sex_income = alpha_sex_income_raw - mean(alpha_sex_income_raw);
  alpha_race_party = alpha_race_party_raw - mean(alpha_race_party_raw);
  alpha_race_age = alpha_race_age_raw - mean(alpha_race_age_raw);
  alpha_race_income = alpha_race_income_raw - mean(alpha_race_income_raw);
  alpha_age_party = alpha_age_party_raw - mean(alpha_age_party_raw);
  alpha_age_income = alpha_age_income_raw - mean(alpha_age_income_raw);
  alpha_party_income = alpha_party_income_raw - mean(alpha_party_income_raw);
  beta_sex = beta_sex_raw - mean(beta_sex_raw);
  beta_race = beta_race_raw - mean(beta_race_raw);
  beta_party = beta_party_raw - mean(beta_party_raw);
  beta_age = beta_age_raw - mean(beta_age_raw);
  beta_income = beta_income_raw - mean(beta_income_raw);
}

// MODEL BLOCK
model{
  vector[N] pi;
  
  // PRIORS
  // weakly informative - less regularizing (rc)
  mu_alpha_raw ~ student_t(5, 0, 5);
  mu_beta_raw ~ student_t(5, 0, 2.5);
  sigma_alpha_sex ~ student_t(4, 0, 5);
  sigma_alpha_race ~ student_t(4, 0, 5);
  sigma_alpha_party ~ student_t(4, 0, 5);
  sigma_alpha_age ~ student_t(4, 0, 5);
  sigma_alpha_income ~ student_t(4, 0, 5);
  sigma_alpha_sex_race ~ student_t(4, 0, 5);
  sigma_alpha_sex_party ~ student_t(4, 0, 5);
  sigma_alpha_sex_age ~ student_t(4, 0, 5);
  sigma_alpha_sex_income ~ student_t(4, 0, 5);
  sigma_alpha_race_party ~ student_t(4, 0, 5);
  sigma_alpha_race_age ~ student_t(4, 0, 5);
  sigma_alpha_race_income ~ student_t(4, 0, 5);
  sigma_alpha_age_party ~ student_t(4, 0, 5);
  sigma_alpha_age_income ~ student_t(4, 0, 5);
  sigma_alpha_party_income ~ student_t(4, 0, 5);
  sigma_beta_sex ~ student_t(4, 0, 5);
  sigma_beta_race ~ student_t(4, 0, 5);
  sigma_beta_party ~ student_t(4, 0, 5);
  sigma_beta_age ~ student_t(4, 0, 5);
  sigma_beta_income ~ student_t(4, 0, 5);
    
  // centered parameterization
  // applied to speed computation
  alpha_sex_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex); 
  alpha_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race);
  alpha_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party);
  alpha_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age);
  alpha_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_income);
  alpha_sex_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_race); 
  alpha_sex_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_party);
  alpha_sex_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_age);
  alpha_sex_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_income);
  alpha_race_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_party);
  alpha_race_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_age);
  alpha_race_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_income);
  alpha_age_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age_party);
  alpha_age_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age_income);
  alpha_party_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party_income);
  beta_sex_raw ~ student_t(5, mu_beta_raw, sigma_beta_sex);
  beta_race_raw ~ student_t(5, mu_beta_raw, sigma_beta_race);
  beta_party_raw ~ student_t(5, mu_beta_raw, sigma_beta_party);
  beta_age_raw ~ student_t(5, mu_beta_raw, sigma_beta_age);
  beta_income_raw ~ student_t(5, mu_beta_raw, sigma_beta_income);
  
  // LIKELIHOOD
  for (n in 1:N)
    pi[n] = alpha_sex_raw[sex[n]] + alpha_race_raw[race[n]] +  alpha_party_raw[party[n]] + 
            alpha_age_raw[age[n]] +  alpha_income_raw[income[n]] + 
            alpha_sex_race_raw[sex_race[n]] + alpha_sex_party_raw[sex_party[n]] + 
            alpha_sex_age_raw[sex_age[n]] + alpha_sex_income_raw[sex_income[n]] + 
            alpha_race_party_raw[race_party[n]] + alpha_race_age_raw[race_age[n]] +
            alpha_race_income_raw[race_income[n]] + alpha_age_party_raw[age_party[n]] + 
            alpha_age_income_raw[age_income[n]] + alpha_party_income_raw[party_income[n]] + 
            beta_sex_raw[sex[n]] * theta[n] + beta_race_raw[race[n]] * theta[n] +  
            beta_party_raw[party[n]] * theta[n] + beta_age_raw[age[n]] * theta[n] +  
            beta_income_raw[income[n]] * theta[n];
  y ~ bernoulli_logit(pi);
}"

write(ml_logit_rc_3, "./code/ml_logit_rc_3.stan")
compiled_ml_logit_rc_3 <- stan_model(file = "./code/ml_logit_rc_3.stan")
data_ml_logit_rc_3 <- list(N = nrow(sample_ml),
                           n_sex = length(unique(sample_ml$sex)),
                           n_race = length(unique(sample_ml$race)),
                           n_party = length(unique(sample_ml$party)),
                           n_age = length(unique(sample_ml$age)),
                           n_income = length(unique(sample_ml$income)),
                           n_sex_race = length(unique(sample_ml$sex_race)),
                           n_sex_party = length(unique(sample_ml$sex_party)),
                           n_sex_age = length(unique(sample_ml$sex_age)),
                           n_sex_income = length(unique(sample_ml$sex_income)),
                           n_race_party = length(unique(sample_ml$race_party)),
                           n_race_age = length(unique(sample_ml$race_age)),
                           n_race_income = length(unique(sample_ml$race_income)),
                           n_age_party = length(unique(sample_ml$age_party)),
                           n_age_income = length(unique(sample_ml$age_income)),
                           n_party_income = length(unique(sample_ml$party_income)),
                           sex = sample_ml$sex,
                           race = sample_ml$race,
                           party = sample_ml$party,
                           age = sample_ml$age,
                           income = sample_ml$income,
                           sex_race = sample_ml$sex_race,
                           sex_party = sample_ml$sex_party,
                           sex_age = sample_ml$sex_age,
                           sex_income = sample_ml$sex_income,
                           race_party = sample_ml$race_party,
                           race_age = sample_ml$race_age,
                           race_income = sample_ml$race_income,
                           age_party = sample_ml$age_party,
                           age_income = sample_ml$age_income,
                           party_income = sample_ml$party_income,
                           theta = sample_ml$theta,
                           y = sample_ml$polact)
pars_return_rc_3 <- c("mu_alpha", "mu_beta", 
                      "alpha_sex", "alpha_race", "alpha_party", "alpha_age", 
                      "alpha_income", "alpha_sex_race", "alpha_sex_party", 
                      "alpha_sex_age", "alpha_sex_income", 
                      "alpha_race_party", "alpha_race_age", 
                      "alpha_race_income", "alpha_age_party",
                      "alpha_age_income", "alpha_party_income", 
                      "beta_sex", "beta_race", "beta_party", "beta_age", 
                      "beta_income",
                      "sigma_alpha_sex", "sigma_alpha_race", "sigma_alpha_party", 
                      "sigma_alpha_age", "sigma_alpha_income", "sigma_alpha_sex_race", 
                      "sigma_alpha_sex_party", "sigma_alpha_sex_age", 
                      "sigma_alpha_sex_income", "sigma_alpha_race_party", 
                      "sigma_alpha_race_age", "sigma_alpha_race_income", 
                      "sigma_alpha_age_party","sigma_alpha_age_income", 
                      "sigma_alpha_party_income", 
                      "sigma_beta_sex", "sigma_beta_race", "sigma_beta_party", 
                      "sigma_beta_age", "sigma_beta_income")
fit_ml_logit_rc_3 <- sampling(object = compiled_ml_logit_rc_3,
                              data = data_ml_logit_rc_3,
                              pars = pars_return_rc_3,
                              chains = 4,
                              iter = 2000,
                              warmup = 1000,
                              control = list(max_treedepth = 12),
                              save_warmup = FALSE,
                              init = "random",
                              sample_file = "./data/models/fit_ml_logit_rc_3_chain")
saveRDS(fit_ml_logit_rc_3, "./data/models/fit_ml_logit_rc_3")
fit_ml_logit_posterior_rc_3 <- as.data.frame(fit_ml_logit_rc_3)
fit_ml_logit_posterior_array_rc_3 <- as.array(fit_ml_logit_rc_3)
saveRDS(fit_ml_logit_posterior_rc_3, "./data/models/fit_ml_logit_posterior_rc_3")
saveRDS(fit_ml_logit_posterior_array_rc_3, "./data/models/fit_ml_logit_posterior_array_rc_3")


fit_ml_logit_rc_3 <- readRDS("./data/models/fit_ml_logit_rc")

postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) %>%
  dplyr::select(-c(id, theta, polact))
# party subgroups fixed 
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_npa <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
# racial subgroups fixed 
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_otherrace <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
# age subgroups fixed 
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
# income subgroups fixed 
postest_to_15 <- sample_ml %>% 
  mutate(income = 1L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_15_30 <- sample_ml %>% 
  mutate(income = 2L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_30_50 <- sample_ml %>% 
  mutate(income = 3L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_50_75 <- sample_ml %>% 
  mutate(income = 4L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_75plus <- sample_ml %>% 
  mutate(income = 5L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
parameters <- fit_ml_logit_rc_3@model_pars[43:64]
# collect respective posteriors in a list
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_rc_3, pars = .x)
  }) %>%
  setNames(parameters)

linear_predictor <-  "mu_alpha[n] + mu_beta[n] * x[k] + 
                      alpha_sex[n, sex] + alpha_race[n, race] +  alpha_party[n, party] + 
                      alpha_age[n, age] +  alpha_income[n, income] + 
                      alpha_sex_race[n, sex_race] + alpha_sex_party[n, sex_party] + 
                      alpha_sex_age[n, sex_age] + alpha_sex_income[n, sex_income] + 
                      alpha_race_party[n, race_party] + alpha_race_age[n, race_age] +
                      alpha_race_income[n, race_income] + alpha_age_party[n, age_party] + 
                      alpha_age_income[n, age_income] + alpha_party_income[n, party_income] + 
                      beta_sex[n, sex] * x[k] + beta_race[n, race] * x[k] +  
                      beta_party[n, party] * x[k] + beta_age[n, age] * x[k] +  
                      beta_income[n, income] * x[k]"
levels <- list(x = seq(from = -2.2, to = 2.2, length.out = 50))
registerDoParallel(6) 
# collect names of datasets
postest_datasets <- c("postest_female", "postest_male", "postest_dem", "postest_rep",
                      "postest_npa", "postest_white", "postest_black", "postest_hispanic",
                      "postest_otherrace", "postest_18_29", "postest_30_44", 
                      "postest_45_64", "postest_65plus", "postest_to_15", 
                      "postest_15_30", "postest_30_50", "postest_50_75", 
                      "postest_75plus")
# generate predictions and save to disk
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels, 
                 draws = sum(fit_ml_logit_rc_3@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_rc_3")))
}
rm(pred)

# model with even weaker priors ---------------------------------------------------------
ml_logit_rc_4 <- "// HIERARCHICAL LOGISTIC REGRESSION WITH VARYING INTERCEPTS AND SLOPES 
 // (EVEN WEAKER PRIORS)

// DATA BLOCK
data {
  int<lower=1> N; // number of observations
  int<lower=1> n_sex; // number of sexes
  int<lower=1> n_race; // number of racial groups
  int<lower=1> n_party; // number of party affiliations
  int<lower=1> n_age; // number of age groups
  int<lower=1> n_income; // number of income groups
  int<lower=1> n_sex_race; // number of sexes by racial group
  int<lower=1> n_sex_party; // number of sexes by party affiliation
  int<lower=1> n_sex_age; // number of sexes by age group
  int<lower=1> n_sex_income; // number of sexes by income group
  int<lower=1> n_race_party; // number of racial groups by party affiliation
  int<lower=1> n_race_age; // number of racial groups by age
  int<lower=1> n_race_income; // number of racial groups by income group
  int<lower=1> n_age_party; // number of age groups by party affiliation
  int<lower=1> n_age_income; // number of age groups by income group
  int<lower=1> n_party_income; // number of party affiliations by income group 
  int<lower=1, upper=n_sex> sex[N]; // observed sex for observations
  int<lower=1, upper=n_race> race[N]; // observed racial group for observations
  int<lower=1, upper=n_party> party[N]; // observed party affiliation for observations
  int<lower=1, upper=n_age> age[N]; // observed age group for observations
  int<lower=1, upper=n_income> income[N]; // observed income group for observations
  int<lower=1, upper=n_sex_race> sex_race[N]; // observed racial group with specific sex for observations
  int<lower=1, upper=n_sex_party> sex_party[N]; // observed party affiliation with specific sex for observations
  int<lower=1, upper=n_sex_age> sex_age[N]; // observed age group with specific sex for observations
  int<lower=1, upper=n_sex_income> sex_income[N]; // observed income group with specific sex for obeservations
  int<lower=1, upper=n_race_party> race_party[N]; // observed party affiliation with specific race for observations
  int<lower=1, upper=n_race_age> race_age[N]; // observed age group with specific racial race for observations
  int<lower=1, upper=n_race_income> race_income[N]; // observed income group with specific race for observations
  int<lower=1, upper=n_age_party> age_party[N]; // observed party affiliation with specific age for observations
  int<lower=1, upper=n_age_income> age_income[N]; // observed income group with specific age for observations
  int<lower=1, upper=n_party_income> party_income[N]; // observed income group with specific party affiliation for observations
  real theta[N]; // observed turnout propensity for observations
  int<lower=0,upper=1> y[N]; // observed social media-based political engagement for observations
}

// PARAMETERS BLOCK
parameters {
  real mu_alpha_raw; // global/fixed intercept, location parameter for varying-intercept priors 
  real mu_beta_raw; // global/fixed effect for theta, location parameter for varying-slope priors
  vector[n_sex] alpha_sex_raw; // varying intercept for sexes
  vector[n_race] alpha_race_raw; // varying intercept for racial groups
  vector[n_party] alpha_party_raw; // varying intercept for party groups
  vector[n_age] alpha_age_raw; // varying intercept for age groups
  vector[n_income] alpha_income_raw; // varying intercept for income groups
  vector[n_sex_race] alpha_sex_race_raw; // varying intercept for sex-race interaction
  vector[n_sex_party] alpha_sex_party_raw; // varying intercept for sex-party interaction
  vector[n_sex_age] alpha_sex_age_raw; // varying intercept for sex-age interaction
  vector[n_sex_income] alpha_sex_income_raw; // varying intercept for sex-income interaction
  vector[n_race_party] alpha_race_party_raw; // varying intercept for race-party interaction
  vector[n_race_age] alpha_race_age_raw; // varying intercept for race-age interaction
  vector[n_race_income] alpha_race_income_raw; // varying intercept for race-income interaction
  vector[n_age_party] alpha_age_party_raw; // varying intercept for age-party interaction
  vector[n_age_income] alpha_age_income_raw; // varying intercept for age-income interaction
  vector[n_party_income] alpha_party_income_raw; // varying intercept for party-income interaction
  vector[n_sex] beta_sex_raw; // varying slope for 'theta' among sexes
  vector[n_race] beta_race_raw; // varying slope for 'theta' among racial groups
  vector[n_party] beta_party_raw; // varying slope for 'theta' among party affiliations
  vector[n_age] beta_age_raw; // varying slope for 'theta' among age groups
  vector[n_income] beta_income_raw; // varying slope for 'theta' among income groups
  real<lower=0> sigma_alpha_sex; // variance/scale parameter for the prior on alpha_sex
  real<lower=0> sigma_alpha_race; // variance/scale parameter for the prior on alpha_race
  real<lower=0> sigma_alpha_party; // variance/scale parameter for the prior on alpha_party
  real<lower=0> sigma_alpha_age; // variance/scale parameter for the prior on alpha_age
  real<lower=0> sigma_alpha_income; // variance/scale parameter for the prior on alpha_income
  real<lower=0> sigma_alpha_sex_race; // variance/scale parameter for the prior on alpha_sex_race
  real<lower=0> sigma_alpha_sex_party; // variance/scale parameter for the prior on alpha_sex_party
  real<lower=0> sigma_alpha_sex_age; // variance/scale parameter for the prior on alpha_sex_age
  real<lower=0> sigma_alpha_sex_income; // variance/scale parameter for the prior on alpha_sex_income
  real<lower=0> sigma_alpha_race_party; // variance/scale parameter for the prior on alpha_race_party
  real<lower=0> sigma_alpha_race_age; // variance/scale parameter for the prior on alpha_race_age
  real<lower=0> sigma_alpha_race_income; // variance/scale parameter for the prior on alpha_race_income
  real<lower=0> sigma_alpha_age_party; // variance/scale parameter for the prior on alpha_age_party
  real<lower=0> sigma_alpha_age_income; // variance/scale parameter for the prior on alpha_age_income
  real<lower=0> sigma_alpha_party_income; // variance/scale parameter for the prior on alpha_party_income
  real<lower=0> sigma_beta_sex; // variance/scale parameter for the prior on beta_sex
  real<lower=0> sigma_beta_race; // variance/scale parameter for the prior on beta_race
  real<lower=0> sigma_beta_party; // variance/scale parameter for the prior on beta_party
  real<lower=0> sigma_beta_age; // variance/scale parameter for the prior on beta_age
  real<lower=0> sigma_beta_income; // variance/scale parameter for the prior on beta_income
}

// TRANSFORMED PARAMETERS BLOCK
transformed parameters {
  real mu_alpha;
  real mu_beta;
  vector[n_sex] alpha_sex;
  vector[n_race] alpha_race;
  vector[n_party] alpha_party;
  vector[n_age] alpha_age;
  vector[n_income] alpha_income;
  vector[n_sex_race] alpha_sex_race;
  vector[n_sex_party] alpha_sex_party;
  vector[n_sex_age] alpha_sex_age;
  vector[n_sex_income] alpha_sex_income;
  vector[n_race_party] alpha_race_party;
  vector[n_race_age] alpha_race_age;
  vector[n_race_income] alpha_race_income;
  vector[n_age_party] alpha_age_party;
  vector[n_age_income] alpha_age_income;
  vector[n_party_income] alpha_party_income;
  vector[n_sex] beta_sex;
  vector[n_race] beta_race;
  vector[n_party] beta_party;
  vector[n_age] beta_age;
  vector[n_income] beta_income;
  
  // REPARAMETERIZATION 
  // centered parameterization makes the model nonidentifiable
  // reparameterized in terms of identifiable combinations of parameters
  mu_alpha = mean(alpha_sex_raw) + mean(alpha_race_raw) + mean(alpha_party_raw) +
    mean(alpha_age_raw) + mean(alpha_income_raw) + mean(alpha_sex_race_raw) +
    mean(alpha_sex_party_raw) + mean(alpha_sex_age_raw) + mean(alpha_sex_income_raw) +
    mean(alpha_race_party_raw) + mean(alpha_race_age_raw) + mean(alpha_race_income_raw) +
    mean(alpha_age_party_raw) + mean(alpha_age_income_raw) + mean(alpha_party_income_raw);
  mu_beta = mean(beta_sex_raw) + mean(beta_race_raw) + mean(beta_party_raw) +
    mean(beta_age_raw) + mean(beta_income_raw);
  alpha_sex = alpha_sex_raw - mean(alpha_sex_raw);
  alpha_race = alpha_race_raw - mean(alpha_race_raw);
  alpha_party = alpha_party_raw - mean(alpha_party_raw);
  alpha_age = alpha_age_raw - mean(alpha_age_raw);
  alpha_income = alpha_income_raw - mean(alpha_income_raw);
  alpha_sex_race = alpha_sex_race_raw - mean(alpha_sex_race_raw);
  alpha_sex_party = alpha_sex_party_raw - mean(alpha_sex_party_raw);
  alpha_sex_age = alpha_sex_age_raw - mean(alpha_sex_age_raw);
  alpha_sex_income = alpha_sex_income_raw - mean(alpha_sex_income_raw);
  alpha_race_party = alpha_race_party_raw - mean(alpha_race_party_raw);
  alpha_race_age = alpha_race_age_raw - mean(alpha_race_age_raw);
  alpha_race_income = alpha_race_income_raw - mean(alpha_race_income_raw);
  alpha_age_party = alpha_age_party_raw - mean(alpha_age_party_raw);
  alpha_age_income = alpha_age_income_raw - mean(alpha_age_income_raw);
  alpha_party_income = alpha_party_income_raw - mean(alpha_party_income_raw);
  beta_sex = beta_sex_raw - mean(beta_sex_raw);
  beta_race = beta_race_raw - mean(beta_race_raw);
  beta_party = beta_party_raw - mean(beta_party_raw);
  beta_age = beta_age_raw - mean(beta_age_raw);
  beta_income = beta_income_raw - mean(beta_income_raw);
}

// MODEL BLOCK
model{
  vector[N] pi;
  
  // PRIORS
  // weakly informative (very weak priors)
  mu_alpha_raw ~ student_t(5, 0, 100);
  mu_beta_raw ~ student_t(5, 0, 100);
  sigma_alpha_sex ~ student_t(4, 0, 100);
  sigma_alpha_race ~ student_t(4, 0, 100);
  sigma_alpha_party ~ student_t(4, 0, 100);
  sigma_alpha_age ~ student_t(4, 0, 100);
  sigma_alpha_income ~ student_t(4, 0, 100);
  sigma_alpha_sex_race ~ student_t(4, 0, 100);
  sigma_alpha_sex_party ~ student_t(4, 0, 100);
  sigma_alpha_sex_age ~ student_t(4, 0, 100);
  sigma_alpha_sex_income ~ student_t(4, 0, 100);
  sigma_alpha_race_party ~ student_t(4, 0, 100);
  sigma_alpha_race_age ~ student_t(4, 0, 100);
  sigma_alpha_race_income ~ student_t(4, 0, 100);
  sigma_alpha_age_party ~ student_t(4, 0, 100);
  sigma_alpha_age_income ~ student_t(4, 0, 100);
  sigma_alpha_party_income ~ student_t(4, 0, 100);
  sigma_beta_sex ~ student_t(4, 0, 100);
  sigma_beta_race ~ student_t(4, 0, 100);
  sigma_beta_party ~ student_t(4, 0, 100);
  sigma_beta_age ~ student_t(4, 0, 100);
  sigma_beta_income ~ student_t(4, 0, 100);
    
  // centered parameterization
  // applied to speed computation
  alpha_sex_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex); 
  alpha_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race);
  alpha_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party);
  alpha_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age);
  alpha_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_income);
  alpha_sex_race_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_race); 
  alpha_sex_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_party);
  alpha_sex_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_age);
  alpha_sex_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_sex_income);
  alpha_race_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_party);
  alpha_race_age_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_age);
  alpha_race_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_race_income);
  alpha_age_party_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age_party);
  alpha_age_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_age_income);
  alpha_party_income_raw ~ student_t(5, mu_alpha_raw, sigma_alpha_party_income);
  beta_sex_raw ~ student_t(5, mu_beta_raw, sigma_beta_sex);
  beta_race_raw ~ student_t(5, mu_beta_raw, sigma_beta_race);
  beta_party_raw ~ student_t(5, mu_beta_raw, sigma_beta_party);
  beta_age_raw ~ student_t(5, mu_beta_raw, sigma_beta_age);
  beta_income_raw ~ student_t(5, mu_beta_raw, sigma_beta_income);
  
  // LIKELIHOOD
  for (n in 1:N)
    pi[n] = alpha_sex_raw[sex[n]] + alpha_race_raw[race[n]] +  alpha_party_raw[party[n]] + 
            alpha_age_raw[age[n]] +  alpha_income_raw[income[n]] + 
            alpha_sex_race_raw[sex_race[n]] + alpha_sex_party_raw[sex_party[n]] + 
            alpha_sex_age_raw[sex_age[n]] + alpha_sex_income_raw[sex_income[n]] + 
            alpha_race_party_raw[race_party[n]] + alpha_race_age_raw[race_age[n]] +
            alpha_race_income_raw[race_income[n]] + alpha_age_party_raw[age_party[n]] + 
            alpha_age_income_raw[age_income[n]] + alpha_party_income_raw[party_income[n]] + 
            beta_sex_raw[sex[n]] * theta[n] + beta_race_raw[race[n]] * theta[n] +  
            beta_party_raw[party[n]] * theta[n] + beta_age_raw[age[n]] * theta[n] +  
            beta_income_raw[income[n]] * theta[n];
  y ~ bernoulli_logit(pi);
}"

write(ml_logit_rc_4, "./code/ml_logit_rc_4.stan")
compiled_ml_logit_rc_4 <- stan_model(file = "./code/ml_logit_rc_4.stan")
fit_ml_logit_rc_4 <- sampling(object = compiled_ml_logit_rc_4,
                              data = data_ml_logit_rc_3,
                              pars = pars_return_rc_3,
                              chains = 4,
                              iter = 2000,
                              warmup = 1000,
                              control = list(max_treedepth = 12),
                              save_warmup = FALSE,
                              init = "random",
                              sample_file = "./data/models/fit_ml_logit_rc_4_chain")
saveRDS(fit_ml_logit_rc_4, "./data/models/fit_ml_logit_rc_4")
fit_ml_logit_posterior_rc_4 <- as.data.frame(fit_ml_logit_rc_4)
fit_ml_logit_posterior_array_rc_4 <- as.array(fit_ml_logit_rc_4)
saveRDS(fit_ml_logit_posterior_rc_4, "./data/models/fit_ml_logit_posterior_rc_4")
saveRDS(fit_ml_logit_posterior_array_rc_4, "./data/models/fit_ml_logit_posterior_array_rc_4")



fit_ml_logit_rc_4 <- readRDS("./data/models/fit_ml_logit_rc_4")
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_rc_4, pars = .x)
  }) %>%
  setNames(parameters)
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels, 
                 draws = sum(fit_ml_logit_rc_4@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_rc_4")))
}
rm(pred)

# main model excluding inactive voters
sample_ml <- readRDS("./data/models/sample_ml")
sample_ml <- sample_ml[which(sample_ml$id %in% sample_analysis[sample_analysis$active_all == TRUE,]$voter_id),]
compiled_ml_logit_rc_5 <- stan_model(file = "./code/ml_logit.stan")
data_ml_logit_rc_5 <- list(N = nrow(sample_ml),
                           n_sex = length(unique(sample_ml$sex)),
                           n_race = length(unique(sample_ml$race)),
                           n_party = length(unique(sample_ml$party)),
                           n_age = length(unique(sample_ml$age)),
                           n_income = length(unique(sample_ml$income)),
                           n_sex_race = length(unique(sample_ml$sex_race)),
                           n_sex_party = length(unique(sample_ml$sex_party)),
                           n_sex_age = length(unique(sample_ml$sex_age)),
                           n_sex_income = length(unique(sample_ml$sex_income)),
                           n_race_party = length(unique(sample_ml$race_party)),
                           n_race_age = length(unique(sample_ml$race_age)),
                           n_race_income = length(unique(sample_ml$race_income)),
                           n_age_party = length(unique(sample_ml$age_party)),
                           n_age_income = length(unique(sample_ml$age_income)),
                           n_party_income = length(unique(sample_ml$party_income)),
                           sex = sample_ml$sex,
                           race = sample_ml$race,
                           party = sample_ml$party,
                           age = sample_ml$age,
                           income = sample_ml$income,
                           sex_race = sample_ml$sex_race,
                           sex_party = sample_ml$sex_party,
                           sex_age = sample_ml$sex_age,
                           sex_income = sample_ml$sex_income,
                           race_party = sample_ml$race_party,
                           race_age = sample_ml$race_age,
                           race_income = sample_ml$race_income,
                           age_party = sample_ml$age_party,
                           age_income = sample_ml$age_income,
                           party_income = sample_ml$party_income,
                           theta = sample_ml$theta,
                           y = sample_ml$polact)
pars_return_rc_5 <- c("mu_alpha", "mu_beta", 
                      "alpha_sex", "alpha_race", "alpha_party", "alpha_age", 
                      "alpha_income", "alpha_sex_race", "alpha_sex_party", 
                      "alpha_sex_age", "alpha_sex_income", 
                      "alpha_race_party", "alpha_race_age", 
                      "alpha_race_income", "alpha_age_party",
                      "alpha_age_income", "alpha_party_income", 
                      "beta_sex", "beta_race", "beta_party", "beta_age", 
                      "beta_income",
                      "sigma_alpha_sex", "sigma_alpha_race", "sigma_alpha_party", 
                      "sigma_alpha_age", "sigma_alpha_income", "sigma_alpha_sex_race", 
                      "sigma_alpha_sex_party", "sigma_alpha_sex_age", 
                      "sigma_alpha_sex_income", "sigma_alpha_race_party", 
                      "sigma_alpha_race_age", "sigma_alpha_race_income", 
                      "sigma_alpha_age_party","sigma_alpha_age_income", 
                      "sigma_alpha_party_income", 
                      "sigma_beta_sex", "sigma_beta_race", "sigma_beta_party", 
                      "sigma_beta_age", "sigma_beta_income")
fit_ml_logit_rc_5 <- sampling(object = compiled_ml_logit_rc_5,
                              data = data_ml_logit_rc_5,
                              pars = pars_return_rc_5,
                              chains = 4,
                              iter = 2000,
                              warmup = 1000,
                              control = list(max_treedepth = 12),
                              save_warmup = FALSE,
                              init = "random",
                              sample_file = "./data/models/fit_ml_logit_rc_5_chain")

# save model, posterior, and summary ----------------------------------------------------
# save model
saveRDS(fit_ml_logit_rc_5, "./data/models/fit_ml_logit_rc_5")
# extract posterior as data.frame and array
fit_ml_logit_posterior_rc_5 <- as.data.frame(fit_ml_logit_rc_5)
fit_ml_logit_posterior_array_rc_5 <- as.array(fit_ml_logit_rc_5)
# save posterior
saveRDS(fit_ml_logit_posterior_rc_5, "./data/models/fit_ml_logit_posterior_rc_5")
saveRDS(fit_ml_logit_posterior_array_rc_5, "./data/models/fit_ml_logit_posterior_array_rc_5")

postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income)) %>%
  dplyr::select(-c(id, theta, polact))
# party subgroups fixed 
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_npa <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
# racial subgroups fixed 
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_otherrace <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age),
         race_income = recode(interaction(.$race, .$income, sep = "_"), 
                              !!!key_race_income)) %>%
  dplyr::select(-c(id, theta, polact))
# age subgroups fixed 
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age)) %>%
  dplyr::select(-c(id, theta, polact))
# income subgroups fixed 
postest_to_15 <- sample_ml %>% 
  mutate(income = 1L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_15_30 <- sample_ml %>% 
  mutate(income = 2L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_30_50 <- sample_ml %>% 
  mutate(income = 3L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_50_75 <- sample_ml %>% 
  mutate(income = 4L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))
postest_75plus <- sample_ml %>% 
  mutate(income = 5L) %>%
  mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
         race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income)) %>%
  dplyr::select(-c(id, theta, polact))

fit_ml_logit_rc_5 <- readRDS("./data/models/fit_ml_logit_rc_5")
parameters <- fit_ml_logit_rc_5@model_pars[43:64]
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_rc_5, pars = .x)
  }) %>%
  setNames(parameters)
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels, 
                 draws = sum(fit_ml_logit_rc_5@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_rc_5")))
}
rm(pred)


#### ESTIMATION AND POSTESTIMATION FOR SUBGROUP INTERACTIONS AND VOTER TYPES ============
sample_ml <- readRDS("./data/models/sample_ml")

key_sex <- c("female" = 1L, "male" = 2L)
key_party <- c("dem" = 1L, "rep" = 2L, "npa" = 3L, "other" = 3L) 
key_race <- c("white" = 1L, "black" = 2L, "hispanic" = 3L, "multi" = 4L,
              "asian" = 4L, "native" = 4L, "other" = 4L)
key_sex_race <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, 
                  "2_1" = 5L, "2_2" = 6L, "2_3" = 7L, "2_4" = 8L)
key_sex_party <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, 
                   "2_1" = 4L, "2_2" = 5L, "2_3" = 6L)
key_sex_age <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, 
                 "2_1" = 5L, "2_2" = 6L, "2_3" = 7L, "2_4" = 8L)
key_sex_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L, 
                    "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L)
key_race_party <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, 
                    "2_1" = 4L, "2_2" = 5L, "2_3" = 6L, 
                    "3_1" = 7L, "3_2" = 8L, "3_3" = 9L,
                    "4_1" = 10L, "4_2" = 11L, "4_3" = 12L)
key_race_age <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, 
                  "2_1" = 5L, "2_2" = 6L, "2_3" = 7L, "2_4" = 8L, 
                  "3_1" = 9L, "3_2" = 10L, "3_3" = 11L, "3_4" = 12L,
                  "4_1" = 13L, "4_2" = 14L, "4_3" = 15L, "4_4" = 16L)
key_race_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L,
                     "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L,
                     "3_1" = 11L, "3_2" = 12L, "3_3" = 13L, "3_4" = 14L, "3_5" = 15L,
                     "4_1" = 16L, "4_2" = 17L, "4_3" = 18L, "4_4" = 19L, "4_5" = 20L)
key_age_party <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, 
                   "2_1" = 4L, "2_2" = 5L, "2_3" = 6L, 
                   "3_1" = 7L, "3_2" = 8L, "3_3" = 9L,
                   "4_1" = 10L, "4_2" = 11L, "4_3" = 12L)
key_age_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L,
                    "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L,
                    "3_1" = 11L, "3_2" = 12L, "3_3" = 13L, "3_4" = 14L, "3_5" = 15L,
                    "4_1" = 16L, "4_2" = 17L, "4_3" = 18L, "4_4" = 19L, "4_5" = 20L)
key_party_income <- c("1_1" = 1L, "1_2" = 2L, "1_3" = 3L, "1_4" = 4L, "1_5" = 5L,
                      "2_1" = 6L, "2_2" = 7L, "2_3" = 8L, "2_4" = 9L, "2_5" = 10L,
                      "3_1" = 11L, "3_2" = 12L, "3_3" = 13L, "3_4" = 14L, "3_5" = 15L)

fit_twopl_irt_summary <- readRDS("./data/models/fit_twopl_irt_summary")
item_parameters <- subset(fit_twopl_irt_summary, 
                          subset = str_detect(row.names(fit_twopl_irt_summary), "alpha\\[|beta\\["))
icc_data <- data.frame(alpha = item_parameters[1:14,]$mean,
                       beta = item_parameters[15:28,]$mean,
                       type = c("midterm", "presidential", "midterm", 
                                "presidential", "midterm", "presidential",
                                "midterm", "mprimary", "gprimary", "mprimary", 
                                "gprimary", "mprimary", "gprimary", "mprimary"))
sample_ml$midterm_2006 <- arm::invlogit(icc_data$alpha[1]*(sample_ml$theta - icc_data$beta[1]))
sample_ml$presidential_2008 <- arm::invlogit(icc_data$alpha[2]*(sample_ml$theta - icc_data$beta[2]))
sample_ml$midterm_2010 <- arm::invlogit(icc_data$alpha[3]*(sample_ml$theta - icc_data$beta[3]))
sample_ml$presidential_2012 <- arm::invlogit(icc_data$alpha[4]*(sample_ml$theta - icc_data$beta[4]))
sample_ml$midterm_2014 <- arm::invlogit(icc_data$alpha[5]*(sample_ml$theta - icc_data$beta[5]))
sample_ml$presidential_2016 <- arm::invlogit(icc_data$alpha[6]*(sample_ml$theta - icc_data$beta[6]))
sample_ml$midterm_2018 <- arm::invlogit(icc_data$alpha[7]*(sample_ml$theta - icc_data$beta[7]))
sample_ml$mprimary_2006 <- arm::invlogit(icc_data$alpha[8]*(sample_ml$theta - icc_data$beta[8]))
sample_ml$pprimary_2008 <- arm::invlogit(icc_data$alpha[9]*(sample_ml$theta - icc_data$beta[9]))
sample_ml$mprimary_2010 <- arm::invlogit(icc_data$alpha[10]*(sample_ml$theta - icc_data$beta[10]))
sample_ml$pprimary_2012 <- arm::invlogit(icc_data$alpha[11]*(sample_ml$theta - icc_data$beta[11]))
sample_ml$mprimary_2014 <- arm::invlogit(icc_data$alpha[12]*(sample_ml$theta - icc_data$beta[12]))
sample_ml$pprimary_2016 <- arm::invlogit(icc_data$alpha[13]*(sample_ml$theta - icc_data$beta[13]))
sample_ml$mprimary_2018 <- arm::invlogit(icc_data$alpha[14]*(sample_ml$theta - icc_data$beta[14]))
sample_ml$pr_midterm <- rowMeans(sample_ml[,c("midterm_2006", "midterm_2010", 
                                              "midterm_2014", "midterm_2018")])
sample_ml$pr_presidential <- rowMeans(sample_ml[,c("presidential_2008", "presidential_2012", 
                                                   "presidential_2016")])
sample_ml$pr_primary <- rowMeans(sample_ml[,c("mprimary_2006", "pprimary_2008", 
                                              "mprimary_2010", "pprimary_2012",
                                              "mprimary_2014", "pprimary_2016",
                                              "mprimary_2018")])
sample_ml$type <- ifelse(sample_ml$pr_primary > 0.5 & 
                           sample_ml$pr_midterm > 0.5 & 
                           sample_ml$pr_presidential > 0.5, "highly_engaged",
                         ifelse(sample_ml$pr_primary < 0.5 & 
                                  sample_ml$pr_midterm > 0.5 & 
                                  sample_ml$pr_presidential > 0.5, "regular",
                                ifelse(sample_ml$pr_primary < 0.5 & 
                                         sample_ml$pr_midterm < 0.5 & 
                                         sample_ml$pr_presidential > 0.5, "irregular",
                                       ifelse(sample_ml$pr_primary < 0.5 & 
                                                sample_ml$pr_midterm < 0.5 & 
                                                sample_ml$pr_presidential < 0.5, "low_propensity",NA))))
sample_ml_original <- sample_ml
sample_ml <- sample_ml_original[sample_ml_original$type == "low_propensity",]
sample_ml <- dplyr::select(sample_ml, sex:party_income)

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
fit_ml_logit_lo <- readRDS("./data/models/fit_ml_logit_lo")
parameters <- fit_ml_logit_lo@model_pars[c(32:47)]
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_lo, pars = .x)
  }) %>%
  setNames(parameters)

# specify the linear predictor ----------------------------------------------------------
linear_predictor_2 <- "mu_alpha[n] + alpha_sex[n, sex] + alpha_race[n, race] +  
alpha_party[n, party] + alpha_age[n, age] +  
alpha_income[n, income] + alpha_sex_race[n, sex_race] + 
alpha_sex_party[n, sex_party] + alpha_sex_age[n, sex_age] + 
alpha_sex_income[n, sex_income] + alpha_race_party[n, race_party] + 
alpha_race_age[n, race_age] + alpha_race_income[n, race_income] + 
alpha_age_party[n, age_party] + alpha_age_income[n, age_income] + 
alpha_party_income[n, party_income]"

# specify levels along x ----------------------------------------------------------------
levels_2 <- list(x = 1)

# generate population-averaged predictions ----------------------------------------------
# register parallel backend
registerDoParallel(6) 
# interacted subgroup predictions for electoral subgroups

# create new datasets with specific variables fixed at interacted subgroups for race -----
# all subgroup interactions for race and income
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_white_1 <- as.list(replicate(5, postest_white, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_white_1[[i]] <- postest_white_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_white_to_15 <- postest_white_1[[1]]
postest_white_15_30 <- postest_white_1[[2]]
postest_white_30_50 <- postest_white_1[[3]]
postest_white_50_75 <- postest_white_1[[4]]
postest_white_75_plus <- postest_white_1[[5]]

postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_black_1 <- as.list(replicate(5, postest_black, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_black_1[[i]] <- postest_black_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_black_to_15 <- postest_black_1[[1]]
postest_black_15_30 <- postest_black_1[[2]]
postest_black_30_50 <- postest_black_1[[3]]
postest_black_50_75 <- postest_black_1[[4]]
postest_black_75_plus <- postest_black_1[[5]]

postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_hispanic_1 <- as.list(replicate(5, postest_hispanic, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_hispanic_1[[i]] <- postest_hispanic_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_hispanic_to_15 <- postest_hispanic_1[[1]]
postest_hispanic_15_30 <- postest_hispanic_1[[2]]
postest_hispanic_30_50 <- postest_hispanic_1[[3]]
postest_hispanic_50_75 <- postest_hispanic_1[[4]]
postest_hispanic_75_plus <- postest_hispanic_1[[5]]

postest_other <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_other_1 <- as.list(replicate(5, postest_other, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_other_1[[i]] <- postest_other_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_other_to_15 <- postest_other_1[[1]]
postest_other_15_30 <- postest_other_1[[2]]
postest_other_30_50 <- postest_other_1[[3]]
postest_other_50_75 <- postest_other_1[[4]]
postest_other_75_plus <- postest_other_1[[5]]

postest_datasets <- c("postest_white_to_15", "postest_white_15_30", "postest_white_30_50", 
                      "postest_white_50_75", "postest_white_75_plus",
                      "postest_black_to_15", "postest_black_15_30", "postest_black_30_50", 
                      "postest_black_50_75", "postest_black_75_plus",
                      "postest_hispanic_to_15", "postest_hispanic_15_30", "postest_hispanic_30_50", 
                      "postest_hispanic_50_75", "postest_hispanic_75_plus",
                      "postest_other_to_15", "postest_other_15_30", "postest_other_30_50", 
                      "postest_other_50_75", "postest_other_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_lo")))
}
rm(pred)

# all subgroup interactions for race and age
postest_white_2 <- as.list(replicate(5, postest_white, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_white_2[[i]] <- postest_white_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_white_18_29 <- postest_white_2[[1]]
postest_white_30_44 <- postest_white_2[[2]]
postest_white_45_64 <- postest_white_2[[3]]
postest_white_60plus <- postest_white_2[[4]]

postest_black_2 <- as.list(replicate(5, postest_black, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_black_2[[i]] <- postest_black_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_black_18_29 <- postest_black_2[[1]]
postest_black_30_44 <- postest_black_2[[2]]
postest_black_45_64 <- postest_black_2[[3]]
postest_black_60plus <- postest_black_2[[4]]

postest_hispanic_2 <- as.list(replicate(5, postest_hispanic, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_hispanic_2[[i]] <- postest_hispanic_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_hispanic_18_29 <- postest_hispanic_2[[1]]
postest_hispanic_30_44 <- postest_hispanic_2[[2]]
postest_hispanic_45_64 <- postest_hispanic_2[[3]]
postest_hispanic_60plus <- postest_hispanic_2[[4]]

postest_other_2 <- as.list(replicate(4, postest_other, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_other_2[[i]] <- postest_other_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_other_18_29 <- postest_other_2[[1]]
postest_other_30_44 <- postest_other_2[[2]]
postest_other_45_64 <- postest_other_2[[3]]
postest_other_60plus <- postest_other_2[[4]]

postest_datasets <- c("postest_white_18_29", "postest_white_30_44", "postest_white_45_64", 
                      "postest_white_60plus",
                      "postest_black_18_29", "postest_black_30_44", "postest_black_45_64", 
                      "postest_black_60plus", 
                      "postest_hispanic_18_29", "postest_hispanic_30_44", "postest_hispanic_45_64", 
                      "postest_hispanic_60plus", 
                      "postest_other_18_29", "postest_other_30_44", "postest_other_45_64", 
                      "postest_other_60plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_lo")))
}
rm(pred)

# all subgroup interactions for race and sex
postest_white_3 <- as.list(replicate(2, postest_white, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_white_3[[i]] <- postest_white_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_white_female <- postest_white_3[[1]]
postest_white_male <- postest_white_3[[2]]

postest_black_3 <- as.list(replicate(2, postest_black, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_black_3[[i]] <- postest_black_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_black_female <- postest_black_3[[1]]
postest_black_male <- postest_black_3[[2]]

postest_hispanic_3 <- as.list(replicate(2, postest_hispanic, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_hispanic_3[[i]] <- postest_hispanic_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_hispanic_female <- postest_hispanic_3[[1]]
postest_hispanic_male <- postest_hispanic_3[[2]]

postest_other_3 <- as.list(replicate(2, postest_other, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_other_3[[i]] <- postest_other_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_other_female <- postest_other_3[[1]]
postest_other_male <- postest_other_3[[2]]
postest_datasets <- c("postest_white_female", "postest_white_male", 
                      "postest_black_female", "postest_black_male", 
                      "postest_hispanic_female", "postest_hispanic_male",
                      "postest_other_female", "postest_other_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_lo")))
}
rm(pred)

# all subgroup interactions for race and party
postest_white_4 <- as.list(replicate(3, postest_white, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_white_4[[i]] <- postest_white_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_white_dem <- postest_white_4[[1]]
postest_white_rep <- postest_white_4[[2]]
postest_white_none <- postest_white_4[[3]]

postest_black_4 <- as.list(replicate(3, postest_black, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_black_4[[i]] <- postest_black_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_black_dem <- postest_black_4[[1]]
postest_black_rep <- postest_black_4[[2]]
postest_black_none <- postest_black_4[[3]]

postest_hispanic_4 <- as.list(replicate(3, postest_hispanic, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_hispanic_4[[i]] <- postest_hispanic_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_hispanic_dem <- postest_hispanic_4[[1]]
postest_hispanic_rep <- postest_hispanic_4[[2]]
postest_hispanic_none <- postest_hispanic_4[[3]]

postest_other_4 <- as.list(replicate(3, postest_other, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_other_4[[i]] <- postest_other_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_other_dem <- postest_other_4[[1]]
postest_other_rep <- postest_other_4[[2]]
postest_other_none <- postest_other_4[[3]]

postest_datasets <- c("postest_white_dem", "postest_white_rep", "postest_white_none", 
                      "postest_black_dem", "postest_black_rep", "postest_black_none", 
                      "postest_hispanic_dem", "postest_hispanic_rep", "postest_hispanic_none",
                      "postest_other_dem", "postest_other_rep", "postest_other_none")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_lo")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for age -----
# all subgroup interactions for race and income
# age_race is already included in race subgroup interactions, here only income, party, and sex
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_18_29_1 <- as.list(replicate(5, postest_18_29, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_18_29_1[[i]] <- postest_18_29_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_18_29_to_15 <- postest_18_29_1[[1]]
postest_18_29_15_30 <- postest_18_29_1[[2]]
postest_18_29_30_50 <- postest_18_29_1[[3]]
postest_18_29_50_75 <- postest_18_29_1[[4]]
postest_18_29_75_plus <- postest_18_29_1[[5]]

postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_30_44_1 <- as.list(replicate(5, postest_30_44, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_30_44_1[[i]] <- postest_30_44_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_30_44_to_15 <- postest_30_44_1[[1]]
postest_30_44_15_30 <- postest_30_44_1[[2]]
postest_30_44_30_50 <- postest_30_44_1[[3]]
postest_30_44_50_75 <- postest_30_44_1[[4]]
postest_30_44_75_plus <- postest_30_44_1[[5]]

postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_45_64_1 <- as.list(replicate(5, postest_45_64, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_45_64_1[[i]] <- postest_45_64_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_45_64_to_15 <- postest_45_64_1[[1]]
postest_45_64_15_30 <- postest_45_64_1[[2]]
postest_45_64_30_50 <- postest_45_64_1[[3]]
postest_45_64_50_75 <- postest_45_64_1[[4]]
postest_45_64_75_plus <- postest_45_64_1[[5]]

postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_65plus_1 <- as.list(replicate(5, postest_65plus, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_65plus_1[[i]] <- postest_65plus_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_65plus_to_15 <- postest_65plus_1[[1]]
postest_65plus_15_30 <- postest_65plus_1[[2]]
postest_65plus_30_50 <- postest_65plus_1[[3]]
postest_65plus_50_75 <- postest_65plus_1[[4]]
postest_65plus_75_plus <- postest_65plus_1[[5]]

postest_datasets <- c("postest_18_29_to_15", "postest_18_29_15_30", "postest_18_29_30_50", 
                      "postest_18_29_50_75", "postest_18_29_75_plus",
                      "postest_30_44_to_15", "postest_30_44_15_30", "postest_30_44_30_50", 
                      "postest_30_44_50_75", "postest_30_44_75_plus",
                      "postest_45_64_to_15", "postest_45_64_15_30", "postest_45_64_30_50", 
                      "postest_45_64_50_75", "postest_45_64_75_plus",
                      "postest_65plus_to_15", "postest_65plus_15_30", "postest_65plus_30_50", 
                      "postest_65plus_50_75", "postest_65plus_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_lo")))
}
rm(pred)

postest_18_29_2 <- as.list(replicate(3, postest_18_29, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_18_29_2[[i]] <- postest_18_29_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_18_29_dem <- postest_18_29_2[[1]]
postest_18_29_rep <- postest_18_29_2[[2]]
postest_18_29_none <- postest_18_29_2[[3]]

postest_30_44_2 <- as.list(replicate(3, postest_30_44, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_30_44_2[[i]] <- postest_30_44_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_30_44_dem <- postest_30_44_2[[1]]
postest_30_44_rep <- postest_30_44_2[[2]]
postest_30_44_none <- postest_30_44_2[[3]]

postest_45_64_2 <- as.list(replicate(3, postest_45_64, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_45_64_2[[i]] <- postest_45_64_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_45_64_dem <- postest_45_64_2[[1]]
postest_45_64_rep <- postest_45_64_2[[2]]
postest_45_64_none <- postest_45_64_2[[3]]

postest_65plus_2 <- as.list(replicate(3, postest_65plus, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_65plus_2[[i]] <- postest_65plus_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_65plus_dem <- postest_65plus_2[[1]]
postest_65plus_rep <- postest_65plus_2[[2]]
postest_65plus_none <- postest_65plus_2[[3]]

postest_datasets <- c("postest_18_29_dem", "postest_18_29_rep", "postest_18_29_none",
                      "postest_30_44_dem", "postest_30_44_rep", "postest_30_44_none", 
                      "postest_45_64_dem", "postest_45_64_rep", "postest_45_64_none",
                      "postest_65plus_dem", "postest_65plus_rep", "postest_65plus_none")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_lo")))
}
rm(pred)


postest_18_29_3 <- as.list(replicate(2, postest_18_29, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_18_29_3[[i]] <- postest_18_29_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_18_29_female <- postest_18_29_3[[1]]
postest_18_29_male <- postest_18_29_3[[2]]

postest_30_44_3 <- as.list(replicate(2, postest_30_44, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_30_44_3[[i]] <- postest_30_44_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_30_44_female <- postest_30_44_3[[1]]
postest_30_44_male <- postest_30_44_3[[2]]


postest_45_64_3 <- as.list(replicate(2, postest_45_64, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_45_64_3[[i]] <- postest_45_64_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_45_64_female <- postest_45_64_3[[1]]
postest_45_64_male <- postest_45_64_3[[2]]

postest_65plus_3 <- as.list(replicate(2, postest_65plus, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_65plus_3[[i]] <- postest_65plus_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_65plus_female <- postest_65plus_3[[1]]
postest_65plus_male <- postest_65plus_3[[2]]

postest_datasets <- c("postest_18_29_female", "postest_18_29_male",
                      "postest_30_44_female", "postest_30_44_male", 
                      "postest_45_64_female", "postest_45_64_male",
                      "postest_65plus_female", "postest_65plus_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_lo")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for party -----
# all subgroup interactions for race and income
# party_race, party_age already included, here only income, and sex
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_dem_1 <- as.list(replicate(5, postest_dem, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_dem_1[[i]] <- postest_dem_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_dem_to_15 <- postest_dem_1[[1]]
postest_dem_15_30 <- postest_dem_1[[2]]
postest_dem_30_50 <- postest_dem_1[[3]]
postest_dem_50_75 <- postest_dem_1[[4]]
postest_dem_75_plus <- postest_dem_1[[5]]

postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_rep_1 <- as.list(replicate(5, postest_rep, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_rep_1[[i]] <- postest_rep_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_rep_to_15 <- postest_rep_1[[1]]
postest_rep_15_30 <- postest_rep_1[[2]]
postest_rep_30_50 <- postest_rep_1[[3]]
postest_rep_50_75 <- postest_rep_1[[4]]
postest_rep_75_plus <- postest_rep_1[[5]]

postest_none <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_none_1 <- as.list(replicate(5, postest_none, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_none_1[[i]] <- postest_none_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_none_to_15 <- postest_none_1[[1]]
postest_none_15_30 <- postest_none_1[[2]]
postest_none_30_50 <- postest_none_1[[3]]
postest_none_50_75 <- postest_none_1[[4]]
postest_none_75_plus <- postest_none_1[[5]]

postest_datasets <- c("postest_dem_to_15", "postest_dem_15_30", "postest_dem_30_50", 
                      "postest_dem_50_75", "postest_dem_75_plus",
                      "postest_rep_to_15", "postest_rep_15_30", "postest_rep_30_50", 
                      "postest_rep_50_75", "postest_rep_75_plus",
                      "postest_none_to_15", "postest_none_15_30", "postest_none_30_50", 
                      "postest_none_50_75", "postest_none_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_lo")))
}
rm(pred)

postest_dem_2 <- as.list(replicate(2, postest_dem, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_dem_2[[i]] <- postest_dem_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_dem_female <- postest_dem_2[[1]]
postest_dem_male <- postest_dem_2[[2]]

postest_rep_2 <- as.list(replicate(2, postest_rep, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_rep_2[[i]] <- postest_rep_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_rep_female <- postest_rep_2[[1]]
postest_rep_male <- postest_rep_2[[2]]


postest_none_2 <- as.list(replicate(2, postest_none, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_none_2[[i]] <- postest_none_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_none_female <- postest_none_2[[1]]
postest_none_male <- postest_none_2[[2]]


postest_datasets <- c("postest_dem_female", "postest_dem_male",
                      "postest_rep_female", "postest_rep_male", 
                      "postest_none_female", "postest_none_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_lo")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for sex -----
# all subgroup interactions for race and party, age already included, here only income
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income))
postest_female_1 <- as.list(replicate(5, postest_female, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_female_1[[i]] <- postest_female_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_female_to_15 <- postest_female_1[[1]]
postest_female_15_30 <- postest_female_1[[2]]
postest_female_30_50 <- postest_female_1[[3]]
postest_female_50_75 <- postest_female_1[[4]]
postest_female_75_plus <- postest_female_1[[5]]

postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income))
postest_male_1 <- as.list(replicate(5, postest_male, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_male_1[[i]] <- postest_male_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_male_to_15 <- postest_male_1[[1]]
postest_male_15_30 <- postest_male_1[[2]]
postest_male_30_50 <- postest_male_1[[3]]
postest_male_50_75 <- postest_male_1[[4]]
postest_male_75_plus <- postest_male_1[[5]]

postest_datasets <- c("postest_female_to_15", "postest_female_15_30", "postest_female_30_50", 
                      "postest_female_50_75", "postest_female_75_plus",
                      "postest_male_to_15", "postest_male_15_30", "postest_male_30_50", 
                      "postest_male_50_75", "postest_male_75_plus")

for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_lo@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_lo")))
}
rm(pred)

sample_ml <- sample_ml_original[sample_ml_original$type == "irregular",]
sample_ml <- dplyr::select(sample_ml, sex:party_income)

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
fit_ml_logit_ir <- readRDS("./data/models/fit_ml_logit_ir")
parameters <- fit_ml_logit_ir@model_pars[c(32:47)]
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_ir, pars = .x)
  }) %>%
  setNames(parameters)

# create new datasets with specific variables fixed at interacted subgroups for race -----
# all subgroup interactions for race and income
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_white_1 <- as.list(replicate(5, postest_white, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_white_1[[i]] <- postest_white_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_white_to_15 <- postest_white_1[[1]]
postest_white_15_30 <- postest_white_1[[2]]
postest_white_30_50 <- postest_white_1[[3]]
postest_white_50_75 <- postest_white_1[[4]]
postest_white_75_plus <- postest_white_1[[5]]

postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_black_1 <- as.list(replicate(5, postest_black, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_black_1[[i]] <- postest_black_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_black_to_15 <- postest_black_1[[1]]
postest_black_15_30 <- postest_black_1[[2]]
postest_black_30_50 <- postest_black_1[[3]]
postest_black_50_75 <- postest_black_1[[4]]
postest_black_75_plus <- postest_black_1[[5]]

postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_hispanic_1 <- as.list(replicate(5, postest_hispanic, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_hispanic_1[[i]] <- postest_hispanic_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_hispanic_to_15 <- postest_hispanic_1[[1]]
postest_hispanic_15_30 <- postest_hispanic_1[[2]]
postest_hispanic_30_50 <- postest_hispanic_1[[3]]
postest_hispanic_50_75 <- postest_hispanic_1[[4]]
postest_hispanic_75_plus <- postest_hispanic_1[[5]]

postest_other <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_other_1 <- as.list(replicate(5, postest_other, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_other_1[[i]] <- postest_other_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_other_to_15 <- postest_other_1[[1]]
postest_other_15_30 <- postest_other_1[[2]]
postest_other_30_50 <- postest_other_1[[3]]
postest_other_50_75 <- postest_other_1[[4]]
postest_other_75_plus <- postest_other_1[[5]]

postest_datasets <- c("postest_white_to_15", "postest_white_15_30", "postest_white_30_50", 
                      "postest_white_50_75", "postest_white_75_plus",
                      "postest_black_to_15", "postest_black_15_30", "postest_black_30_50", 
                      "postest_black_50_75", "postest_black_75_plus",
                      "postest_hispanic_to_15", "postest_hispanic_15_30", "postest_hispanic_30_50", 
                      "postest_hispanic_50_75", "postest_hispanic_75_plus",
                      "postest_other_to_15", "postest_other_15_30", "postest_other_30_50", 
                      "postest_other_50_75", "postest_other_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ir")))
}
rm(pred)

# all subgroup interactions for race and age
postest_white_2 <- as.list(replicate(5, postest_white, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_white_2[[i]] <- postest_white_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_white_18_29 <- postest_white_2[[1]]
postest_white_30_44 <- postest_white_2[[2]]
postest_white_45_64 <- postest_white_2[[3]]
postest_white_60plus <- postest_white_2[[4]]

postest_black_2 <- as.list(replicate(5, postest_black, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_black_2[[i]] <- postest_black_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_black_18_29 <- postest_black_2[[1]]
postest_black_30_44 <- postest_black_2[[2]]
postest_black_45_64 <- postest_black_2[[3]]
postest_black_60plus <- postest_black_2[[4]]

postest_hispanic_2 <- as.list(replicate(5, postest_hispanic, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_hispanic_2[[i]] <- postest_hispanic_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_hispanic_18_29 <- postest_hispanic_2[[1]]
postest_hispanic_30_44 <- postest_hispanic_2[[2]]
postest_hispanic_45_64 <- postest_hispanic_2[[3]]
postest_hispanic_60plus <- postest_hispanic_2[[4]]

postest_other_2 <- as.list(replicate(4, postest_other, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_other_2[[i]] <- postest_other_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_other_18_29 <- postest_other_2[[1]]
postest_other_30_44 <- postest_other_2[[2]]
postest_other_45_64 <- postest_other_2[[3]]
postest_other_60plus <- postest_other_2[[4]]

postest_datasets <- c("postest_white_18_29", "postest_white_30_44", "postest_white_45_64", 
                      "postest_white_60plus",
                      "postest_black_18_29", "postest_black_30_44", "postest_black_45_64", 
                      "postest_black_60plus", 
                      "postest_hispanic_18_29", "postest_hispanic_30_44", "postest_hispanic_45_64", 
                      "postest_hispanic_60plus", 
                      "postest_other_18_29", "postest_other_30_44", "postest_other_45_64", 
                      "postest_other_60plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ir")))
}
rm(pred)

# all subgroup interactions for race and sex
postest_white_3 <- as.list(replicate(2, postest_white, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_white_3[[i]] <- postest_white_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_white_female <- postest_white_3[[1]]
postest_white_male <- postest_white_3[[2]]

postest_black_3 <- as.list(replicate(2, postest_black, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_black_3[[i]] <- postest_black_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_black_female <- postest_black_3[[1]]
postest_black_male <- postest_black_3[[2]]

postest_hispanic_3 <- as.list(replicate(2, postest_hispanic, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_hispanic_3[[i]] <- postest_hispanic_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_hispanic_female <- postest_hispanic_3[[1]]
postest_hispanic_male <- postest_hispanic_3[[2]]

postest_other_3 <- as.list(replicate(2, postest_other, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_other_3[[i]] <- postest_other_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_other_female <- postest_other_3[[1]]
postest_other_male <- postest_other_3[[2]]
postest_datasets <- c("postest_white_female", "postest_white_male", 
                      "postest_black_female", "postest_black_male", 
                      "postest_hispanic_female", "postest_hispanic_male",
                      "postest_other_female", "postest_other_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ir")))
}
rm(pred)

# all subgroup interactions for race and party
postest_white_4 <- as.list(replicate(3, postest_white, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_white_4[[i]] <- postest_white_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_white_dem <- postest_white_4[[1]]
postest_white_rep <- postest_white_4[[2]]
postest_white_none <- postest_white_4[[3]]

postest_black_4 <- as.list(replicate(3, postest_black, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_black_4[[i]] <- postest_black_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_black_dem <- postest_black_4[[1]]
postest_black_rep <- postest_black_4[[2]]
postest_black_none <- postest_black_4[[3]]

postest_hispanic_4 <- as.list(replicate(3, postest_hispanic, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_hispanic_4[[i]] <- postest_hispanic_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_hispanic_dem <- postest_hispanic_4[[1]]
postest_hispanic_rep <- postest_hispanic_4[[2]]
postest_hispanic_none <- postest_hispanic_4[[3]]

postest_other_4 <- as.list(replicate(3, postest_other, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_other_4[[i]] <- postest_other_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_other_dem <- postest_other_4[[1]]
postest_other_rep <- postest_other_4[[2]]
postest_other_none <- postest_other_4[[3]]

postest_datasets <- c("postest_white_dem", "postest_white_rep", "postest_white_none", 
                      "postest_black_dem", "postest_black_rep", "postest_black_none", 
                      "postest_hispanic_dem", "postest_hispanic_rep", "postest_hispanic_none",
                      "postest_other_dem", "postest_other_rep", "postest_other_none")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ir")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for age -----
# all subgroup interactions for race and income
# age_race is already included in race subgroup interactions, here only income, party, and sex
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_18_29_1 <- as.list(replicate(5, postest_18_29, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_18_29_1[[i]] <- postest_18_29_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_18_29_to_15 <- postest_18_29_1[[1]]
postest_18_29_15_30 <- postest_18_29_1[[2]]
postest_18_29_30_50 <- postest_18_29_1[[3]]
postest_18_29_50_75 <- postest_18_29_1[[4]]
postest_18_29_75_plus <- postest_18_29_1[[5]]

postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_30_44_1 <- as.list(replicate(5, postest_30_44, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_30_44_1[[i]] <- postest_30_44_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_30_44_to_15 <- postest_30_44_1[[1]]
postest_30_44_15_30 <- postest_30_44_1[[2]]
postest_30_44_30_50 <- postest_30_44_1[[3]]
postest_30_44_50_75 <- postest_30_44_1[[4]]
postest_30_44_75_plus <- postest_30_44_1[[5]]

postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_45_64_1 <- as.list(replicate(5, postest_45_64, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_45_64_1[[i]] <- postest_45_64_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_45_64_to_15 <- postest_45_64_1[[1]]
postest_45_64_15_30 <- postest_45_64_1[[2]]
postest_45_64_30_50 <- postest_45_64_1[[3]]
postest_45_64_50_75 <- postest_45_64_1[[4]]
postest_45_64_75_plus <- postest_45_64_1[[5]]

postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_65plus_1 <- as.list(replicate(5, postest_65plus, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_65plus_1[[i]] <- postest_65plus_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_65plus_to_15 <- postest_65plus_1[[1]]
postest_65plus_15_30 <- postest_65plus_1[[2]]
postest_65plus_30_50 <- postest_65plus_1[[3]]
postest_65plus_50_75 <- postest_65plus_1[[4]]
postest_65plus_75_plus <- postest_65plus_1[[5]]

postest_datasets <- c("postest_18_29_to_15", "postest_18_29_15_30", "postest_18_29_30_50", 
                      "postest_18_29_50_75", "postest_18_29_75_plus",
                      "postest_30_44_to_15", "postest_30_44_15_30", "postest_30_44_30_50", 
                      "postest_30_44_50_75", "postest_30_44_75_plus",
                      "postest_45_64_to_15", "postest_45_64_15_30", "postest_45_64_30_50", 
                      "postest_45_64_50_75", "postest_45_64_75_plus",
                      "postest_65plus_to_15", "postest_65plus_15_30", "postest_65plus_30_50", 
                      "postest_65plus_50_75", "postest_65plus_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ir")))
}
rm(pred)

postest_18_29_2 <- as.list(replicate(3, postest_18_29, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_18_29_2[[i]] <- postest_18_29_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_18_29_dem <- postest_18_29_2[[1]]
postest_18_29_rep <- postest_18_29_2[[2]]
postest_18_29_none <- postest_18_29_2[[3]]

postest_30_44_2 <- as.list(replicate(3, postest_30_44, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_30_44_2[[i]] <- postest_30_44_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_30_44_dem <- postest_30_44_2[[1]]
postest_30_44_rep <- postest_30_44_2[[2]]
postest_30_44_none <- postest_30_44_2[[3]]

postest_45_64_2 <- as.list(replicate(3, postest_45_64, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_45_64_2[[i]] <- postest_45_64_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_45_64_dem <- postest_45_64_2[[1]]
postest_45_64_rep <- postest_45_64_2[[2]]
postest_45_64_none <- postest_45_64_2[[3]]

postest_65plus_2 <- as.list(replicate(3, postest_65plus, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_65plus_2[[i]] <- postest_65plus_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_65plus_dem <- postest_65plus_2[[1]]
postest_65plus_rep <- postest_65plus_2[[2]]
postest_65plus_none <- postest_65plus_2[[3]]

postest_datasets <- c("postest_18_29_dem", "postest_18_29_rep", "postest_18_29_none",
                      "postest_30_44_dem", "postest_30_44_rep", "postest_30_44_none", 
                      "postest_45_64_dem", "postest_45_64_rep", "postest_45_64_none",
                      "postest_65plus_dem", "postest_65plus_rep", "postest_65plus_none")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ir")))
}
rm(pred)


postest_18_29_3 <- as.list(replicate(2, postest_18_29, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_18_29_3[[i]] <- postest_18_29_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_18_29_female <- postest_18_29_3[[1]]
postest_18_29_male <- postest_18_29_3[[2]]

postest_30_44_3 <- as.list(replicate(2, postest_30_44, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_30_44_3[[i]] <- postest_30_44_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_30_44_female <- postest_30_44_3[[1]]
postest_30_44_male <- postest_30_44_3[[2]]


postest_45_64_3 <- as.list(replicate(2, postest_45_64, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_45_64_3[[i]] <- postest_45_64_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_45_64_female <- postest_45_64_3[[1]]
postest_45_64_male <- postest_45_64_3[[2]]

postest_65plus_3 <- as.list(replicate(2, postest_65plus, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_65plus_3[[i]] <- postest_65plus_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_65plus_female <- postest_65plus_3[[1]]
postest_65plus_male <- postest_65plus_3[[2]]

postest_datasets <- c("postest_18_29_female", "postest_18_29_male",
                      "postest_30_44_female", "postest_30_44_male", 
                      "postest_45_64_female", "postest_45_64_male",
                      "postest_65plus_female", "postest_65plus_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ir")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for party -----
# all subgroup interactions for race and income
# party_race, party_age already included, here only income, and sex
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_dem_1 <- as.list(replicate(5, postest_dem, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_dem_1[[i]] <- postest_dem_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_dem_to_15 <- postest_dem_1[[1]]
postest_dem_15_30 <- postest_dem_1[[2]]
postest_dem_30_50 <- postest_dem_1[[3]]
postest_dem_50_75 <- postest_dem_1[[4]]
postest_dem_75_plus <- postest_dem_1[[5]]

postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_rep_1 <- as.list(replicate(5, postest_rep, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_rep_1[[i]] <- postest_rep_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_rep_to_15 <- postest_rep_1[[1]]
postest_rep_15_30 <- postest_rep_1[[2]]
postest_rep_30_50 <- postest_rep_1[[3]]
postest_rep_50_75 <- postest_rep_1[[4]]
postest_rep_75_plus <- postest_rep_1[[5]]

postest_none <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_none_1 <- as.list(replicate(5, postest_none, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_none_1[[i]] <- postest_none_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_none_to_15 <- postest_none_1[[1]]
postest_none_15_30 <- postest_none_1[[2]]
postest_none_30_50 <- postest_none_1[[3]]
postest_none_50_75 <- postest_none_1[[4]]
postest_none_75_plus <- postest_none_1[[5]]

postest_datasets <- c("postest_dem_to_15", "postest_dem_15_30", "postest_dem_30_50", 
                      "postest_dem_50_75", "postest_dem_75_plus",
                      "postest_rep_to_15", "postest_rep_15_30", "postest_rep_30_50", 
                      "postest_rep_50_75", "postest_rep_75_plus",
                      "postest_none_to_15", "postest_none_15_30", "postest_none_30_50", 
                      "postest_none_50_75", "postest_none_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ir")))
}
rm(pred)


postest_dem_2 <- as.list(replicate(2, postest_dem, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_dem_2[[i]] <- postest_dem_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_dem_female <- postest_dem_2[[1]]
postest_dem_male <- postest_dem_2[[2]]

postest_rep_2 <- as.list(replicate(2, postest_rep, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_rep_2[[i]] <- postest_rep_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_rep_female <- postest_rep_2[[1]]
postest_rep_male <- postest_rep_2[[2]]


postest_none_2 <- as.list(replicate(2, postest_none, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_none_2[[i]] <- postest_none_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_none_female <- postest_none_2[[1]]
postest_none_male <- postest_none_2[[2]]


postest_datasets <- c("postest_dem_female", "postest_dem_male",
                      "postest_rep_female", "postest_rep_male", 
                      "postest_none_female", "postest_none_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ir")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for sex -----
# all subgroup interactions for race and party, age already included, here only income
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income))
postest_female_1 <- as.list(replicate(5, postest_female, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_female_1[[i]] <- postest_female_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_female_to_15 <- postest_female_1[[1]]
postest_female_15_30 <- postest_female_1[[2]]
postest_female_30_50 <- postest_female_1[[3]]
postest_female_50_75 <- postest_female_1[[4]]
postest_female_75_plus <- postest_female_1[[5]]

postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income))
postest_male_1 <- as.list(replicate(5, postest_male, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_male_1[[i]] <- postest_male_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_male_to_15 <- postest_male_1[[1]]
postest_male_15_30 <- postest_male_1[[2]]
postest_male_30_50 <- postest_male_1[[3]]
postest_male_50_75 <- postest_male_1[[4]]
postest_male_75_plus <- postest_male_1[[5]]

postest_datasets <- c("postest_female_to_15", "postest_female_15_30", "postest_female_30_50", 
                      "postest_female_50_75", "postest_female_75_plus",
                      "postest_male_to_15", "postest_male_15_30", "postest_male_30_50", 
                      "postest_male_50_75", "postest_male_75_plus")

for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_ir@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_ir")))
}
rm(pred)

sample_ml <- sample_ml_original[sample_ml_original$type == "regular",]
sample_ml <- dplyr::select(sample_ml, sex:party_income)

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
fit_ml_logit_re <- readRDS("./data/models/fit_ml_logit_re")
parameters <- fit_ml_logit_re@model_pars[c(32:47)]
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_re, pars = .x)
  }) %>%
  setNames(parameters)

# create new datasets with specific variables fixed at interacted subgroups for race -----
# all subgroup interactions for race and income
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_white_1 <- as.list(replicate(5, postest_white, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_white_1[[i]] <- postest_white_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_white_to_15 <- postest_white_1[[1]]
postest_white_15_30 <- postest_white_1[[2]]
postest_white_30_50 <- postest_white_1[[3]]
postest_white_50_75 <- postest_white_1[[4]]
postest_white_75_plus <- postest_white_1[[5]]

postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_black_1 <- as.list(replicate(5, postest_black, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_black_1[[i]] <- postest_black_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_black_to_15 <- postest_black_1[[1]]
postest_black_15_30 <- postest_black_1[[2]]
postest_black_30_50 <- postest_black_1[[3]]
postest_black_50_75 <- postest_black_1[[4]]
postest_black_75_plus <- postest_black_1[[5]]

postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_hispanic_1 <- as.list(replicate(5, postest_hispanic, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_hispanic_1[[i]] <- postest_hispanic_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_hispanic_to_15 <- postest_hispanic_1[[1]]
postest_hispanic_15_30 <- postest_hispanic_1[[2]]
postest_hispanic_30_50 <- postest_hispanic_1[[3]]
postest_hispanic_50_75 <- postest_hispanic_1[[4]]
postest_hispanic_75_plus <- postest_hispanic_1[[5]]

postest_other <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_other_1 <- as.list(replicate(5, postest_other, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_other_1[[i]] <- postest_other_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_other_to_15 <- postest_other_1[[1]]
postest_other_15_30 <- postest_other_1[[2]]
postest_other_30_50 <- postest_other_1[[3]]
postest_other_50_75 <- postest_other_1[[4]]
postest_other_75_plus <- postest_other_1[[5]]

postest_datasets <- c("postest_white_to_15", "postest_white_15_30", "postest_white_30_50", 
                      "postest_white_50_75", "postest_white_75_plus",
                      "postest_black_to_15", "postest_black_15_30", "postest_black_30_50", 
                      "postest_black_50_75", "postest_black_75_plus",
                      "postest_hispanic_to_15", "postest_hispanic_15_30", "postest_hispanic_30_50", 
                      "postest_hispanic_50_75", "postest_hispanic_75_plus",
                      "postest_other_to_15", "postest_other_15_30", "postest_other_30_50", 
                      "postest_other_50_75", "postest_other_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)

# all subgroup interactions for race and age
postest_white_2 <- as.list(replicate(5, postest_white, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_white_2[[i]] <- postest_white_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_white_18_29 <- postest_white_2[[1]]
postest_white_30_44 <- postest_white_2[[2]]
postest_white_45_64 <- postest_white_2[[3]]
postest_white_60plus <- postest_white_2[[4]]

postest_black_2 <- as.list(replicate(5, postest_black, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_black_2[[i]] <- postest_black_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_black_18_29 <- postest_black_2[[1]]
postest_black_30_44 <- postest_black_2[[2]]
postest_black_45_64 <- postest_black_2[[3]]
postest_black_60plus <- postest_black_2[[4]]

postest_hispanic_2 <- as.list(replicate(5, postest_hispanic, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_hispanic_2[[i]] <- postest_hispanic_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_hispanic_18_29 <- postest_hispanic_2[[1]]
postest_hispanic_30_44 <- postest_hispanic_2[[2]]
postest_hispanic_45_64 <- postest_hispanic_2[[3]]
postest_hispanic_60plus <- postest_hispanic_2[[4]]

postest_other_2 <- as.list(replicate(4, postest_other, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_other_2[[i]] <- postest_other_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_other_18_29 <- postest_other_2[[1]]
postest_other_30_44 <- postest_other_2[[2]]
postest_other_45_64 <- postest_other_2[[3]]
postest_other_60plus <- postest_other_2[[4]]

postest_datasets <- c("postest_white_18_29", "postest_white_30_44", "postest_white_45_64", 
                      "postest_white_60plus",
                      "postest_black_18_29", "postest_black_30_44", "postest_black_45_64", 
                      "postest_black_60plus", 
                      "postest_hispanic_18_29", "postest_hispanic_30_44", "postest_hispanic_45_64", 
                      "postest_hispanic_60plus", 
                      "postest_other_18_29", "postest_other_30_44", "postest_other_45_64", 
                      "postest_other_60plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)

# all subgroup interactions for race and sex
postest_white_3 <- as.list(replicate(2, postest_white, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_white_3[[i]] <- postest_white_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_white_female <- postest_white_3[[1]]
postest_white_male <- postest_white_3[[2]]

postest_black_3 <- as.list(replicate(2, postest_black, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_black_3[[i]] <- postest_black_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_black_female <- postest_black_3[[1]]
postest_black_male <- postest_black_3[[2]]

postest_hispanic_3 <- as.list(replicate(2, postest_hispanic, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_hispanic_3[[i]] <- postest_hispanic_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_hispanic_female <- postest_hispanic_3[[1]]
postest_hispanic_male <- postest_hispanic_3[[2]]

postest_other_3 <- as.list(replicate(2, postest_other, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_other_3[[i]] <- postest_other_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_other_female <- postest_other_3[[1]]
postest_other_male <- postest_other_3[[2]]
postest_datasets <- c("postest_white_female", "postest_white_male", 
                      "postest_black_female", "postest_black_male", 
                      "postest_hispanic_female", "postest_hispanic_male",
                      "postest_other_female", "postest_other_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)

# all subgroup interactions for race and party
postest_white_4 <- as.list(replicate(3, postest_white, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_white_4[[i]] <- postest_white_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_white_dem <- postest_white_4[[1]]
postest_white_rep <- postest_white_4[[2]]
postest_white_none <- postest_white_4[[3]]

postest_black_4 <- as.list(replicate(3, postest_black, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_black_4[[i]] <- postest_black_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_black_dem <- postest_black_4[[1]]
postest_black_rep <- postest_black_4[[2]]
postest_black_none <- postest_black_4[[3]]

postest_hispanic_4 <- as.list(replicate(3, postest_hispanic, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_hispanic_4[[i]] <- postest_hispanic_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_hispanic_dem <- postest_hispanic_4[[1]]
postest_hispanic_rep <- postest_hispanic_4[[2]]
postest_hispanic_none <- postest_hispanic_4[[3]]

postest_other_4 <- as.list(replicate(3, postest_other, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_other_4[[i]] <- postest_other_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_other_dem <- postest_other_4[[1]]
postest_other_rep <- postest_other_4[[2]]
postest_other_none <- postest_other_4[[3]]

postest_datasets <- c("postest_white_dem", "postest_white_rep", "postest_white_none", 
                      "postest_black_dem", "postest_black_rep", "postest_black_none", 
                      "postest_hispanic_dem", "postest_hispanic_rep", "postest_hispanic_none",
                      "postest_other_dem", "postest_other_rep", "postest_other_none")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)


# create new datasets with specific variables fixed at interacted subgroups for age -----
# all subgroup interactions for race and income
# age_race is already included in race subgroup interactions, here only income, party, and sex
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_18_29_1 <- as.list(replicate(5, postest_18_29, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_18_29_1[[i]] <- postest_18_29_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_18_29_to_15 <- postest_18_29_1[[1]]
postest_18_29_15_30 <- postest_18_29_1[[2]]
postest_18_29_30_50 <- postest_18_29_1[[3]]
postest_18_29_50_75 <- postest_18_29_1[[4]]
postest_18_29_75_plus <- postest_18_29_1[[5]]

postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_30_44_1 <- as.list(replicate(5, postest_30_44, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_30_44_1[[i]] <- postest_30_44_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_30_44_to_15 <- postest_30_44_1[[1]]
postest_30_44_15_30 <- postest_30_44_1[[2]]
postest_30_44_30_50 <- postest_30_44_1[[3]]
postest_30_44_50_75 <- postest_30_44_1[[4]]
postest_30_44_75_plus <- postest_30_44_1[[5]]

postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_45_64_1 <- as.list(replicate(5, postest_45_64, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_45_64_1[[i]] <- postest_45_64_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_45_64_to_15 <- postest_45_64_1[[1]]
postest_45_64_15_30 <- postest_45_64_1[[2]]
postest_45_64_30_50 <- postest_45_64_1[[3]]
postest_45_64_50_75 <- postest_45_64_1[[4]]
postest_45_64_75_plus <- postest_45_64_1[[5]]

postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_65plus_1 <- as.list(replicate(5, postest_65plus, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_65plus_1[[i]] <- postest_65plus_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_65plus_to_15 <- postest_65plus_1[[1]]
postest_65plus_15_30 <- postest_65plus_1[[2]]
postest_65plus_30_50 <- postest_65plus_1[[3]]
postest_65plus_50_75 <- postest_65plus_1[[4]]
postest_65plus_75_plus <- postest_65plus_1[[5]]

postest_datasets <- c("postest_18_29_to_15", "postest_18_29_15_30", "postest_18_29_30_50", 
                      "postest_18_29_50_75", "postest_18_29_75_plus",
                      "postest_30_44_to_15", "postest_30_44_15_30", "postest_30_44_30_50", 
                      "postest_30_44_50_75", "postest_30_44_75_plus",
                      "postest_45_64_to_15", "postest_45_64_15_30", "postest_45_64_30_50", 
                      "postest_45_64_50_75", "postest_45_64_75_plus",
                      "postest_65plus_to_15", "postest_65plus_15_30", "postest_65plus_30_50", 
                      "postest_65plus_50_75", "postest_65plus_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)

postest_18_29_2 <- as.list(replicate(3, postest_18_29, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_18_29_2[[i]] <- postest_18_29_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_18_29_dem <- postest_18_29_2[[1]]
postest_18_29_rep <- postest_18_29_2[[2]]
postest_18_29_none <- postest_18_29_2[[3]]

postest_30_44_2 <- as.list(replicate(3, postest_30_44, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_30_44_2[[i]] <- postest_30_44_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_30_44_dem <- postest_30_44_2[[1]]
postest_30_44_rep <- postest_30_44_2[[2]]
postest_30_44_none <- postest_30_44_2[[3]]

postest_45_64_2 <- as.list(replicate(3, postest_45_64, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_45_64_2[[i]] <- postest_45_64_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_45_64_dem <- postest_45_64_2[[1]]
postest_45_64_rep <- postest_45_64_2[[2]]
postest_45_64_none <- postest_45_64_2[[3]]

postest_65plus_2 <- as.list(replicate(3, postest_65plus, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_65plus_2[[i]] <- postest_65plus_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_65plus_dem <- postest_65plus_2[[1]]
postest_65plus_rep <- postest_65plus_2[[2]]
postest_65plus_none <- postest_65plus_2[[3]]

postest_datasets <- c("postest_18_29_dem", "postest_18_29_rep", "postest_18_29_none",
                      "postest_30_44_dem", "postest_30_44_rep", "postest_30_44_none", 
                      "postest_45_64_dem", "postest_45_64_rep", "postest_45_64_none",
                      "postest_65plus_dem", "postest_65plus_rep", "postest_65plus_none")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)


postest_18_29_3 <- as.list(replicate(2, postest_18_29, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_18_29_3[[i]] <- postest_18_29_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_18_29_female <- postest_18_29_3[[1]]
postest_18_29_male <- postest_18_29_3[[2]]

postest_30_44_3 <- as.list(replicate(2, postest_30_44, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_30_44_3[[i]] <- postest_30_44_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_30_44_female <- postest_30_44_3[[1]]
postest_30_44_male <- postest_30_44_3[[2]]


postest_45_64_3 <- as.list(replicate(2, postest_45_64, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_45_64_3[[i]] <- postest_45_64_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_45_64_female <- postest_45_64_3[[1]]
postest_45_64_male <- postest_45_64_3[[2]]

postest_65plus_3 <- as.list(replicate(2, postest_65plus, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_65plus_3[[i]] <- postest_65plus_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_65plus_female <- postest_65plus_3[[1]]
postest_65plus_male <- postest_65plus_3[[2]]

postest_datasets <- c("postest_18_29_female", "postest_18_29_male",
                      "postest_30_44_female", "postest_30_44_male", 
                      "postest_45_64_female", "postest_45_64_male",
                      "postest_65plus_female", "postest_65plus_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)


# create new datasets with specific variables fixed at interacted subgroups for party -----
# all subgroup interactions for race and income
# party_race, party_age already included, here only income, and sex
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_dem_1 <- as.list(replicate(5, postest_dem, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_dem_1[[i]] <- postest_dem_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_dem_to_15 <- postest_dem_1[[1]]
postest_dem_15_30 <- postest_dem_1[[2]]
postest_dem_30_50 <- postest_dem_1[[3]]
postest_dem_50_75 <- postest_dem_1[[4]]
postest_dem_75_plus <- postest_dem_1[[5]]

postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_rep_1 <- as.list(replicate(5, postest_rep, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_rep_1[[i]] <- postest_rep_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_rep_to_15 <- postest_rep_1[[1]]
postest_rep_15_30 <- postest_rep_1[[2]]
postest_rep_30_50 <- postest_rep_1[[3]]
postest_rep_50_75 <- postest_rep_1[[4]]
postest_rep_75_plus <- postest_rep_1[[5]]

postest_none <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_none_1 <- as.list(replicate(5, postest_none, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_none_1[[i]] <- postest_none_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_none_to_15 <- postest_none_1[[1]]
postest_none_15_30 <- postest_none_1[[2]]
postest_none_30_50 <- postest_none_1[[3]]
postest_none_50_75 <- postest_none_1[[4]]
postest_none_75_plus <- postest_none_1[[5]]

postest_datasets <- c("postest_dem_to_15", "postest_dem_15_30", "postest_dem_30_50", 
                      "postest_dem_50_75", "postest_dem_75_plus",
                      "postest_rep_to_15", "postest_rep_15_30", "postest_rep_30_50", 
                      "postest_rep_50_75", "postest_rep_75_plus",
                      "postest_none_to_15", "postest_none_15_30", "postest_none_30_50", 
                      "postest_none_50_75", "postest_none_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)


postest_dem_2 <- as.list(replicate(2, postest_dem, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_dem_2[[i]] <- postest_dem_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_dem_female <- postest_dem_2[[1]]
postest_dem_male <- postest_dem_2[[2]]

postest_rep_2 <- as.list(replicate(2, postest_rep, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_rep_2[[i]] <- postest_rep_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_rep_female <- postest_rep_2[[1]]
postest_rep_male <- postest_rep_2[[2]]


postest_none_2 <- as.list(replicate(2, postest_none, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_none_2[[i]] <- postest_none_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_none_female <- postest_none_2[[1]]
postest_none_male <- postest_none_2[[2]]


postest_datasets <- c("postest_dem_female", "postest_dem_male",
                      "postest_rep_female", "postest_rep_male", 
                      "postest_none_female", "postest_none_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for sex -----
# all subgroup interactions for race and party, age already included, here only income
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income))
postest_female_1 <- as.list(replicate(5, postest_female, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_female_1[[i]] <- postest_female_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_female_to_15 <- postest_female_1[[1]]
postest_female_15_30 <- postest_female_1[[2]]
postest_female_30_50 <- postest_female_1[[3]]
postest_female_50_75 <- postest_female_1[[4]]
postest_female_75_plus <- postest_female_1[[5]]

postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income))
postest_male_1 <- as.list(replicate(5, postest_male, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_male_1[[i]] <- postest_male_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_male_to_15 <- postest_male_1[[1]]
postest_male_15_30 <- postest_male_1[[2]]
postest_male_30_50 <- postest_male_1[[3]]
postest_male_50_75 <- postest_male_1[[4]]
postest_male_75_plus <- postest_male_1[[5]]

postest_datasets <- c("postest_female_to_15", "postest_female_15_30", "postest_female_30_50", 
                      "postest_female_50_75", "postest_female_75_plus",
                      "postest_male_to_15", "postest_male_15_30", "postest_male_30_50", 
                      "postest_male_50_75", "postest_male_75_plus")

for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_re@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_re")))
}
rm(pred)

sample_ml <- sample_ml_original[sample_ml_original$type == "highly_engaged",]
sample_ml <- dplyr::select(sample_ml, sex:party_income)

# assemble posteriors of parameters in a list -------------------------------------------
# assign parameters
fit_ml_logit_he <- readRDS("./data/models/fit_ml_logit_he")
parameters <- fit_ml_logit_he@model_pars[c(32:47)]
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_he, pars = .x)
  }) %>%
  setNames(parameters)

# create new datasets with specific variables fixed at interacted subgroups for race -----
# all subgroup interactions for race and income
postest_white <- sample_ml %>% 
  mutate(race = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_white_1 <- as.list(replicate(5, postest_white, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_white_1[[i]] <- postest_white_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_white_to_15 <- postest_white_1[[1]]
postest_white_15_30 <- postest_white_1[[2]]
postest_white_30_50 <- postest_white_1[[3]]
postest_white_50_75 <- postest_white_1[[4]]
postest_white_75_plus <- postest_white_1[[5]]

postest_black <- sample_ml %>% 
  mutate(race = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_black_1 <- as.list(replicate(5, postest_black, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_black_1[[i]] <- postest_black_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_black_to_15 <- postest_black_1[[1]]
postest_black_15_30 <- postest_black_1[[2]]
postest_black_30_50 <- postest_black_1[[3]]
postest_black_50_75 <- postest_black_1[[4]]
postest_black_75_plus <- postest_black_1[[5]]

postest_hispanic <- sample_ml %>% 
  mutate(race = 3L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_hispanic_1 <- as.list(replicate(5, postest_hispanic, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_hispanic_1[[i]] <- postest_hispanic_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_hispanic_to_15 <- postest_hispanic_1[[1]]
postest_hispanic_15_30 <- postest_hispanic_1[[2]]
postest_hispanic_30_50 <- postest_hispanic_1[[3]]
postest_hispanic_50_75 <- postest_hispanic_1[[4]]
postest_hispanic_75_plus <- postest_hispanic_1[[5]]

postest_other <- sample_ml %>% 
  mutate(race = 4L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         race_age = recode(interaction(.$race, .$age, sep = "_"), !!!key_race_age))
postest_other_1 <- as.list(replicate(5, postest_other, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_other_1[[i]] <- postest_other_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_other_to_15 <- postest_other_1[[1]]
postest_other_15_30 <- postest_other_1[[2]]
postest_other_30_50 <- postest_other_1[[3]]
postest_other_50_75 <- postest_other_1[[4]]
postest_other_75_plus <- postest_other_1[[5]]

postest_datasets <- c("postest_white_to_15", "postest_white_15_30", "postest_white_30_50", 
                      "postest_white_50_75", "postest_white_75_plus",
                      "postest_black_to_15", "postest_black_15_30", "postest_black_30_50", 
                      "postest_black_50_75", "postest_black_75_plus",
                      "postest_hispanic_to_15", "postest_hispanic_15_30", "postest_hispanic_30_50", 
                      "postest_hispanic_50_75", "postest_hispanic_75_plus",
                      "postest_other_to_15", "postest_other_15_30", "postest_other_30_50", 
                      "postest_other_50_75", "postest_other_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)

# all subgroup interactions for race and age
postest_white_2 <- as.list(replicate(5, postest_white, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_white_2[[i]] <- postest_white_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_white_18_29 <- postest_white_2[[1]]
postest_white_30_44 <- postest_white_2[[2]]
postest_white_45_64 <- postest_white_2[[3]]
postest_white_60plus <- postest_white_2[[4]]

postest_black_2 <- as.list(replicate(5, postest_black, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_black_2[[i]] <- postest_black_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_black_18_29 <- postest_black_2[[1]]
postest_black_30_44 <- postest_black_2[[2]]
postest_black_45_64 <- postest_black_2[[3]]
postest_black_60plus <- postest_black_2[[4]]

postest_hispanic_2 <- as.list(replicate(5, postest_hispanic, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_hispanic_2[[i]] <- postest_hispanic_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_hispanic_18_29 <- postest_hispanic_2[[1]]
postest_hispanic_30_44 <- postest_hispanic_2[[2]]
postest_hispanic_45_64 <- postest_hispanic_2[[3]]
postest_hispanic_60plus <- postest_hispanic_2[[4]]

postest_other_2 <- as.list(replicate(4, postest_other, simplify = FALSE))
age <- c(1L,2L,3L,4L)
for(i in 1:length(age)) { 
  postest_other_2[[i]] <- postest_other_2[[i]] %>% 
    mutate(age = i) %>%
    mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           race_age = recode(interaction(.$race, .$age, sep = "_"), 
                             !!!key_race_age))
}
postest_other_18_29 <- postest_other_2[[1]]
postest_other_30_44 <- postest_other_2[[2]]
postest_other_45_64 <- postest_other_2[[3]]
postest_other_60plus <- postest_other_2[[4]]

postest_datasets <- c("postest_white_18_29", "postest_white_30_44", "postest_white_45_64", 
                      "postest_white_60plus",
                      "postest_black_18_29", "postest_black_30_44", "postest_black_45_64", 
                      "postest_black_60plus", 
                      "postest_hispanic_18_29", "postest_hispanic_30_44", "postest_hispanic_45_64", 
                      "postest_hispanic_60plus", 
                      "postest_other_18_29", "postest_other_30_44", "postest_other_45_64", 
                      "postest_other_60plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)

# all subgroup interactions for race and sex
postest_white_3 <- as.list(replicate(2, postest_white, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_white_3[[i]] <- postest_white_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_white_female <- postest_white_3[[1]]
postest_white_male <- postest_white_3[[2]]

postest_black_3 <- as.list(replicate(2, postest_black, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_black_3[[i]] <- postest_black_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_black_female <- postest_black_3[[1]]
postest_black_male <- postest_black_3[[2]]

postest_hispanic_3 <- as.list(replicate(2, postest_hispanic, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_hispanic_3[[i]] <- postest_hispanic_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_hispanic_female <- postest_hispanic_3[[1]]
postest_hispanic_male <- postest_hispanic_3[[2]]

postest_other_3 <- as.list(replicate(2, postest_other, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_other_3[[i]] <- postest_other_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_other_female <- postest_other_3[[1]]
postest_other_male <- postest_other_3[[2]]
postest_datasets <- c("postest_white_female", "postest_white_male", 
                      "postest_black_female", "postest_black_male", 
                      "postest_hispanic_female", "postest_hispanic_male",
                      "postest_other_female", "postest_other_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)

# all subgroup interactions for race and party
postest_white_4 <- as.list(replicate(3, postest_white, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_white_4[[i]] <- postest_white_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_white_dem <- postest_white_4[[1]]
postest_white_rep <- postest_white_4[[2]]
postest_white_none <- postest_white_4[[3]]

postest_black_4 <- as.list(replicate(3, postest_black, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_black_4[[i]] <- postest_black_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_black_dem <- postest_black_4[[1]]
postest_black_rep <- postest_black_4[[2]]
postest_black_none <- postest_black_4[[3]]

postest_hispanic_4 <- as.list(replicate(3, postest_hispanic, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_hispanic_4[[i]] <- postest_hispanic_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_hispanic_dem <- postest_hispanic_4[[1]]
postest_hispanic_rep <- postest_hispanic_4[[2]]
postest_hispanic_none <- postest_hispanic_4[[3]]

postest_other_4 <- as.list(replicate(3, postest_other, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_other_4[[i]] <- postest_other_4[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_other_dem <- postest_other_4[[1]]
postest_other_rep <- postest_other_4[[2]]
postest_other_none <- postest_other_4[[3]]

postest_datasets <- c("postest_white_dem", "postest_white_rep", "postest_white_none", 
                      "postest_black_dem", "postest_black_rep", "postest_black_none", 
                      "postest_hispanic_dem", "postest_hispanic_rep", "postest_hispanic_none",
                      "postest_other_dem", "postest_other_rep", "postest_other_none")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)


# create new datasets with specific variables fixed at interacted subgroups for age -----
# all subgroup interactions for race and income
# age_race is already included in race subgroup interactions, here only income, party, and sex
postest_18_29 <- sample_ml %>% 
  mutate(age = 1L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_18_29_1 <- as.list(replicate(5, postest_18_29, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_18_29_1[[i]] <- postest_18_29_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_18_29_to_15 <- postest_18_29_1[[1]]
postest_18_29_15_30 <- postest_18_29_1[[2]]
postest_18_29_30_50 <- postest_18_29_1[[3]]
postest_18_29_50_75 <- postest_18_29_1[[4]]
postest_18_29_75_plus <- postest_18_29_1[[5]]

postest_30_44 <- sample_ml %>% 
  mutate(age = 2L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_30_44_1 <- as.list(replicate(5, postest_30_44, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_30_44_1[[i]] <- postest_30_44_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_30_44_to_15 <- postest_30_44_1[[1]]
postest_30_44_15_30 <- postest_30_44_1[[2]]
postest_30_44_30_50 <- postest_30_44_1[[3]]
postest_30_44_50_75 <- postest_30_44_1[[4]]
postest_30_44_75_plus <- postest_30_44_1[[5]]

postest_45_64 <- sample_ml %>% 
  mutate(age = 3L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_45_64_1 <- as.list(replicate(5, postest_45_64, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_45_64_1[[i]] <- postest_45_64_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_45_64_to_15 <- postest_45_64_1[[1]]
postest_45_64_15_30 <- postest_45_64_1[[2]]
postest_45_64_30_50 <- postest_45_64_1[[3]]
postest_45_64_50_75 <- postest_45_64_1[[4]]
postest_45_64_75_plus <- postest_45_64_1[[5]]

postest_65plus <- sample_ml %>% 
  mutate(age = 4L) %>%
  mutate(sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
         race_age = recode(interaction(.$race, .$age, sep = "_"), 
                           !!!key_race_age))
postest_65plus_1 <- as.list(replicate(5, postest_65plus, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_65plus_1[[i]] <- postest_65plus_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_65plus_to_15 <- postest_65plus_1[[1]]
postest_65plus_15_30 <- postest_65plus_1[[2]]
postest_65plus_30_50 <- postest_65plus_1[[3]]
postest_65plus_50_75 <- postest_65plus_1[[4]]
postest_65plus_75_plus <- postest_65plus_1[[5]]

postest_datasets <- c("postest_18_29_to_15", "postest_18_29_15_30", "postest_18_29_30_50", 
                      "postest_18_29_50_75", "postest_18_29_75_plus",
                      "postest_30_44_to_15", "postest_30_44_15_30", "postest_30_44_30_50", 
                      "postest_30_44_50_75", "postest_30_44_75_plus",
                      "postest_45_64_to_15", "postest_45_64_15_30", "postest_45_64_30_50", 
                      "postest_45_64_50_75", "postest_45_64_75_plus",
                      "postest_65plus_to_15", "postest_65plus_15_30", "postest_65plus_30_50", 
                      "postest_65plus_50_75", "postest_65plus_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)

postest_18_29_2 <- as.list(replicate(3, postest_18_29, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_18_29_2[[i]] <- postest_18_29_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_18_29_dem <- postest_18_29_2[[1]]
postest_18_29_rep <- postest_18_29_2[[2]]
postest_18_29_none <- postest_18_29_2[[3]]

postest_30_44_2 <- as.list(replicate(3, postest_30_44, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_30_44_2[[i]] <- postest_30_44_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_30_44_dem <- postest_30_44_2[[1]]
postest_30_44_rep <- postest_30_44_2[[2]]
postest_30_44_none <- postest_30_44_2[[3]]

postest_45_64_2 <- as.list(replicate(3, postest_45_64, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_45_64_2[[i]] <- postest_45_64_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_45_64_dem <- postest_45_64_2[[1]]
postest_45_64_rep <- postest_45_64_2[[2]]
postest_45_64_none <- postest_45_64_2[[3]]

postest_65plus_2 <- as.list(replicate(3, postest_65plus, simplify = FALSE))
party <- c(1L,2L,3L)
for(i in 1:length(party)) { 
  postest_65plus_2[[i]] <- postest_65plus_2[[i]] %>% 
    mutate(party = i) %>%
    mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
           age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_65plus_dem <- postest_65plus_2[[1]]
postest_65plus_rep <- postest_65plus_2[[2]]
postest_65plus_none <- postest_65plus_2[[3]]

postest_datasets <- c("postest_18_29_dem", "postest_18_29_rep", "postest_18_29_none",
                      "postest_30_44_dem", "postest_30_44_rep", "postest_30_44_none", 
                      "postest_45_64_dem", "postest_45_64_rep", "postest_45_64_none",
                      "postest_65plus_dem", "postest_65plus_rep", "postest_65plus_none")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)


postest_18_29_3 <- as.list(replicate(2, postest_18_29, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_18_29_3[[i]] <- postest_18_29_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_18_29_female <- postest_18_29_3[[1]]
postest_18_29_male <- postest_18_29_3[[2]]

postest_30_44_3 <- as.list(replicate(2, postest_30_44, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_30_44_3[[i]] <- postest_30_44_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_30_44_female <- postest_30_44_3[[1]]
postest_30_44_male <- postest_30_44_3[[2]]


postest_45_64_3 <- as.list(replicate(2, postest_45_64, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_45_64_3[[i]] <- postest_45_64_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_45_64_female <- postest_45_64_3[[1]]
postest_45_64_male <- postest_45_64_3[[2]]

postest_65plus_3 <- as.list(replicate(2, postest_65plus, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_65plus_3[[i]] <- postest_65plus_3[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_65plus_female <- postest_65plus_3[[1]]
postest_65plus_male <- postest_65plus_3[[2]]

postest_datasets <- c("postest_18_29_female", "postest_18_29_male",
                      "postest_30_44_female", "postest_30_44_male", 
                      "postest_45_64_female", "postest_45_64_male",
                      "postest_65plus_female", "postest_65plus_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)


# create new datasets with specific variables fixed at interacted subgroups for party -----
# all subgroup interactions for race and income
# party_race, party_age already included, here only income, and sex
postest_dem <- sample_ml %>% 
  mutate(party = 1L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_dem_1 <- as.list(replicate(5, postest_dem, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_dem_1[[i]] <- postest_dem_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_dem_to_15 <- postest_dem_1[[1]]
postest_dem_15_30 <- postest_dem_1[[2]]
postest_dem_30_50 <- postest_dem_1[[3]]
postest_dem_50_75 <- postest_dem_1[[4]]
postest_dem_75_plus <- postest_dem_1[[5]]

postest_rep <- sample_ml %>% 
  mutate(party = 2L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_rep_1 <- as.list(replicate(5, postest_rep, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_rep_1[[i]] <- postest_rep_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_rep_to_15 <- postest_rep_1[[1]]
postest_rep_15_30 <- postest_rep_1[[2]]
postest_rep_30_50 <- postest_rep_1[[3]]
postest_rep_50_75 <- postest_rep_1[[4]]
postest_rep_75_plus <- postest_rep_1[[5]]

postest_none <- sample_ml %>% 
  mutate(party = 3L) %>%
  mutate(sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         race_party = recode(interaction(.$race, .$party, sep = "_"), !!!key_race_party),
         age_party = recode(interaction(.$age, .$party, sep = "_"), !!!key_age_party),
         party_income = recode(interaction(.$party, .$income, sep = "_"), 
                               !!!key_party_income))
postest_none_1 <- as.list(replicate(5, postest_none, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_none_1[[i]] <- postest_none_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_none_to_15 <- postest_none_1[[1]]
postest_none_15_30 <- postest_none_1[[2]]
postest_none_30_50 <- postest_none_1[[3]]
postest_none_50_75 <- postest_none_1[[4]]
postest_none_75_plus <- postest_none_1[[5]]

postest_datasets <- c("postest_dem_to_15", "postest_dem_15_30", "postest_dem_30_50", 
                      "postest_dem_50_75", "postest_dem_75_plus",
                      "postest_rep_to_15", "postest_rep_15_30", "postest_rep_30_50", 
                      "postest_rep_50_75", "postest_rep_75_plus",
                      "postest_none_to_15", "postest_none_15_30", "postest_none_30_50", 
                      "postest_none_50_75", "postest_none_75_plus")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)


postest_dem_2 <- as.list(replicate(2, postest_dem, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_dem_2[[i]] <- postest_dem_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_dem_female <- postest_dem_2[[1]]
postest_dem_male <- postest_dem_2[[2]]

postest_rep_2 <- as.list(replicate(2, postest_rep, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_rep_2[[i]] <- postest_rep_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_rep_female <- postest_rep_2[[1]]
postest_rep_male <- postest_rep_2[[2]]


postest_none_2 <- as.list(replicate(2, postest_none, simplify = FALSE))
sex <- c(1L,2L)
for(i in 1:length(sex)) { 
  postest_none_2[[i]] <- postest_none_2[[i]] %>% 
    mutate(sex = i) %>%
    mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
           sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
           sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
           sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                               !!!key_sex_income))
}
postest_none_female <- postest_none_2[[1]]
postest_none_male <- postest_none_2[[2]]


postest_datasets <- c("postest_dem_female", "postest_dem_male",
                      "postest_rep_female", "postest_rep_male", 
                      "postest_none_female", "postest_none_male")
for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)

# create new datasets with specific variables fixed at interacted subgroups for sex -----
# all subgroup interactions for race and party, age already included, here only income
postest_female <- sample_ml %>% 
  mutate(sex = 1L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income))
postest_female_1 <- as.list(replicate(5, postest_female, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_female_1[[i]] <- postest_female_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_female_to_15 <- postest_female_1[[1]]
postest_female_15_30 <- postest_female_1[[2]]
postest_female_30_50 <- postest_female_1[[3]]
postest_female_50_75 <- postest_female_1[[4]]
postest_female_75_plus <- postest_female_1[[5]]

postest_male <- sample_ml %>% 
  mutate(sex = 2L) %>%
  mutate(sex_race = recode(interaction(.$sex, .$race, sep = "_"), !!!key_sex_race),
         sex_party = recode(interaction(.$sex, .$party, sep = "_"), !!!key_sex_party),
         sex_age = recode(interaction(.$sex, .$age, sep = "_"), !!!key_sex_age),
         sex_income = recode(interaction(.$sex, .$income, sep = "_"), 
                             !!!key_sex_income))
postest_male_1 <- as.list(replicate(5, postest_male, simplify = FALSE))
income <- c(1L,2L,3L,4L,5L)
for(i in 1:length(income)) { 
  postest_male_1[[i]] <- postest_male_1[[i]] %>% 
    mutate(income = i) %>%
    mutate(sex_income = recode(interaction(.$sex, .$income, sep = "_"), !!!key_sex_income),
           race_income = recode(interaction(.$race, .$income, sep = "_"), !!!key_race_income),
           age_income = recode(interaction(.$age, .$income, sep = "_"), !!!key_age_income),
           party_income = recode(interaction(.$party, .$income, sep = "_"), 
                                 !!!key_party_income))
}
postest_male_to_15 <- postest_male_1[[1]]
postest_male_15_30 <- postest_male_1[[2]]
postest_male_30_50 <- postest_male_1[[3]]
postest_male_50_75 <- postest_male_1[[4]]
postest_male_75_plus <- postest_male_1[[5]]

postest_datasets <- c("postest_female_to_15", "postest_female_15_30", "postest_female_30_50", 
                      "postest_female_50_75", "postest_female_75_plus",
                      "postest_male_to_15", "postest_male_15_30", "postest_male_30_50", 
                      "postest_male_50_75", "postest_male_75_plus")

for (i in 1:length(postest_datasets)) {
  pred <- mlAMEs(data = eval(parse(text = postest_datasets[i])), levels = levels_2, 
                 draws = sum(fit_ml_logit_he@sim$n_save), posterior = posteriors, 
                 linear_predictor = linear_predictor_2, type = "logit")
  saveRDS(pred, str_c("./data/models/", 
                      str_replace(postest_datasets[i], "postest", "pred_he")))
}
rm(pred)