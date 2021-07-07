# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Additional analyses script
# July 2021
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./data/models/fit_ml_logit_2'
'./data/models/sample_ml'
'./data/analysis/vep_prop'
'./data/analysis/cvap_prop2'
'./data/analysis/rvp_prop'
'./data/analysis/pci_cvap'
'./data/social-media-activity/april2019/social-media-act.sqlite'
'./data/analysis/analysis_processed'
'./data/analysis/sample_analysis'
'./data/voter-files-updated/'
'./data/voter-histories-updated/'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/models/poststrat_*'
'./data/voter_record_sample_hist'
'./ddata/voter_record_sample'
'./ddata/voter_record_sample_processed'
'./ddata/voter_record_sample_processed_geo'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 42 - PREPARATIONS
Line 55 - POST-STRATIFY GROUP ESTIMATES TO ESTIMATED TARGET POPULATION PROPORTIONS
Line 296 - SOCIAL MEDIA-BASED PARTICIPATION AND VOTING OVERLAP FOR VARIOUS SUBSETS
Line 791 - 2018 TURNOUT AMONG SUBGROUPS OF THE REGISTERED VOTER POPULATION
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("jqd-voting-and-sm-pol-participation")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")


#### POST-STRATIFY GROUP ESTIMATES TO ESTIMATED TARGET POPULATION PROPORTIONS ===========

# import model, target population data, and sample data ---------------------------------
fit_ml_logit_2 <- readRDS("./data/models/fit_ml_logit_2")
sample_ml <- readRDS("./data/models/sample_ml")
vep <- readRDS("./data/analysis/vep_prop")[c(2:7),]
cvap_prop2 <- readRDS("./data/analysis/cvap_prop2")[c(1:4),]
rvp <- readRDS("./data/analysis/rvp_prop")[c(13:16),]
rvp$estimate[4] <- rvp$estimate[3]+rvp$estimate[4]  
rvp <- rvp[-3,]
pci_cvap <- readRDS("./data/analysis/pci_cvap")
pci_cvap <- rbind(pci_cvap[5,], pci_cvap[1:4,])
pci_cvap$Freq <- pci_cvap$Freq*100
colnames(pci_cvap) <- c("variable", "estimate")
pci_cvap$moe <- NA
target <- rbind(vep, cvap_prop2, rvp, pci_cvap)
target$estimate <- target$estimate/100

# construct data for each ideal type ----------------------------------------------------
ideal <- expand.grid(sex = unique(sample_ml$sex), party = unique(sample_ml$party),
                     race = unique(sample_ml$race), age = unique(sample_ml$age),
                     income = unique(sample_ml$income))
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
ideal$sex_race <- interaction(ideal$sex, ideal$race, sep = "_")
ideal$sex_race <- recode(ideal$sex_race, !!!key_sex_race)
ideal$sex_party <- interaction(ideal$sex, ideal$party, sep = "_")
ideal$sex_party <- recode(ideal$sex_party, !!!key_sex_party)
ideal$sex_age <- interaction(ideal$sex, ideal$age, sep = "_")
ideal$sex_age <- recode(ideal$sex_age, !!!key_sex_age)
ideal$sex_income <- interaction(ideal$sex, ideal$income, sep = "_")
ideal$sex_income <- recode(ideal$sex_income, !!!key_sex_income)
ideal$race_party <- interaction(ideal$race, ideal$party, sep = "_")
ideal$race_party <- recode(ideal$race_party, !!!key_race_party)
ideal$race_age <- interaction(ideal$race, ideal$age, sep = "_")
ideal$race_age <- recode(ideal$race_age, !!!key_race_age)
ideal$race_income <- interaction(ideal$race, ideal$income, sep = "_")
ideal$race_income <- recode(ideal$race_income, !!!key_race_income)
ideal$age_party <- interaction(ideal$age, ideal$party, sep = "_")
ideal$age_party <- recode(ideal$age_party, !!!key_age_party)
ideal$age_income <- interaction(ideal$age, ideal$income, sep = "_")
ideal$age_income <- recode(ideal$age_income, !!!key_age_income)
ideal$party_income <- interaction(ideal$party, ideal$income, sep = "_")
ideal$party_income <- recode(ideal$party_income, !!!key_party_income)

# prepare predictions for each ideal type ----------------------------------------------
parameters <- fit_ml_logit_2@model_pars[c(32:47)]
# collect respective posteriors in a list
posteriors <- parameters %>% 
  purrr::map(~{
    a <- as.matrix(fit_ml_logit_2, pars = .x)
  }) %>%
  setNames(parameters)
linear_predictor_2 <- "mu_alpha[n] + alpha_sex[n, sex] + alpha_race[n, race] +  
                       alpha_party[n, party] + alpha_age[n, age] +  
                       alpha_income[n, income] + alpha_sex_race[n, sex_race] + 
                       alpha_sex_party[n, sex_party] + alpha_sex_age[n, sex_age] + 
                       alpha_sex_income[n, sex_income] + alpha_race_party[n, race_party] + 
                       alpha_race_age[n, race_age] + alpha_race_income[n, race_income] + 
                       alpha_age_party[n, age_party] + alpha_age_income[n, age_income] + 
                       alpha_party_income[n, party_income]"
linear_predictor_2 <- linear_predictor_2 %>%
  str_replace_all("\\[n", str_c("\\[1\\:", n_draws))
linear_predictor_2 <- parse(text = linear_predictor_2)

# construct predictions arranged separately for each subgroup ---------------------------
# arranged by sex
ideal_sex <- arrange(ideal, sex, party, race, age, income)
ideal_list <- c(list(n_draws = 4000),
                as.list(ideal_sex),
                posteriors)
for (i in 1:length(ideal_list)) {
  assign(names(ideal_list)[i], ideal_list[[i]])
}
prediction_sex <- arm::invlogit(eval(linear_predictor_2))
# arranged by party
ideal_party <- arrange(ideal, party, sex, race, age, income)
ideal_list <- c(list(n_draws = 4000),
                as.list(ideal_party),
                posteriors)
for (i in 1:length(ideal_list)) {
  assign(names(ideal_list)[i], ideal_list[[i]])
}
prediction_party <- arm::invlogit(eval(linear_predictor_2))
# arranged by race
ideal_race <- arrange(ideal, race, sex, party, age, income)
ideal_list <- c(list(n_draws = 4000),
                as.list(ideal_race),
                posteriors)
for (i in 1:length(ideal_list)) {
  assign(names(ideal_list)[i], ideal_list[[i]])
}
prediction_race <- arm::invlogit(eval(linear_predictor_2))
# arranged by age
ideal_age <- arrange(ideal, age, sex, party, race, income)
ideal_list <- c(list(n_draws = 4000),
                as.list(ideal_age),
                posteriors)
for (i in 1:length(ideal_list)) {
  assign(names(ideal_list)[i], ideal_list[[i]])
}
prediction_age <- arm::invlogit(eval(linear_predictor_2))
# arranged by income
ideal_income <- arrange(ideal, income, sex, party, race, age)
ideal_list <- c(list(n_draws = 4000),
                as.list(ideal_income),
                posteriors)
for (i in 1:length(ideal_list)) {
  assign(names(ideal_list)[i], ideal_list[[i]])
}
prediction_income <- arm::invlogit(eval(linear_predictor_2))

# # construct synthetic joint distributions arranged separately for each subgroup -------
# marginal distribution of groups
target_sex <- target[c(2,1),2]
target_race <- target[c(5,3,4,6),2]
target_age <- target[c(7:10),2]
target_party <- target[c(11:13),2] 
target_income <- target[c(14:18),2]
synthetic_joint_sex <- rep(NA, dim(ideal)[1])
for (i in 1:dim(ideal)[1]) {
  synthetic_joint_sex[i] <- target_sex[ideal_sex[i,1]]*target_party[ideal_sex[i,2]]*
    target_race[ideal_sex[i,3]]*target_age[ideal_sex[i,4]]*target_income[ideal_sex[i,5]]
}
synthetic_joint_party <- rep(NA, dim(ideal)[1])
for (i in 1:dim(ideal)[1]) {
  synthetic_joint_party[i] <- target_sex[ideal_party[i,1]]*target_party[ideal_party[i,2]]*
    target_race[ideal_party[i,3]]*target_age[ideal_party[i,4]]*target_income[ideal_party[i,5]]
}
synthetic_joint_race <- rep(NA, dim(ideal)[1])
for (i in 1:dim(ideal)[1]) {
  synthetic_joint_race[i] <- target_sex[ideal_race[i,1]]*target_party[ideal_race[i,2]]*
    target_race[ideal_race[i,3]]*target_age[ideal_race[i,4]]*target_income[ideal_race[i,5]]
}
synthetic_joint_age <- rep(NA, dim(ideal)[1])
for (i in 1:dim(ideal)[1]) {
  synthetic_joint_age[i] <- target_sex[ideal_age[i,1]]*target_party[ideal_age[i,2]]*
    target_race[ideal_age[i,3]]*target_age[ideal_age[i,4]]*target_income[ideal_age[i,5]]
}
synthetic_joint_income<- rep(NA, dim(ideal)[1])
for (i in 1:dim(ideal)[1]) {
  synthetic_joint_income[i] <- target_sex[ideal_income[i,1]]*target_party[ideal_income[i,2]]*
    target_race[ideal_income[i,3]]*target_age[ideal_income[i,4]]*target_income[ideal_income[i,5]]
}

# poststratify posterior group estimates ------------------------------------------------
# sex
poststrat_female <- rowSums(prediction_sex[,1:240] %*% diag(synthetic_joint_sex[1:240]))/
  sum(synthetic_joint_sex[1:240])
poststrat_male <- rowSums(prediction_sex[,241:480] %*% diag(synthetic_joint_sex[241:480]))/
  sum(synthetic_joint_sex[241:480])
# age
poststrat_18_29 <- rowSums(prediction_age[,1:120] %*% diag(synthetic_joint_age[1:120]))/
  sum(synthetic_joint_age[1:120])
poststrat_30_44 <- rowSums(prediction_age[,121:240] %*% diag(synthetic_joint_age[121:240]))/
  sum(synthetic_joint_age[121:240])
poststrat_45_64 <- rowSums(prediction_age[,241:360] %*% diag(synthetic_joint_age[241:360]))/
  sum(synthetic_joint_age[241:360])
poststrat_65plus <- rowSums(prediction_age[,361:480] %*% diag(synthetic_joint_age[361:480]))/
  sum(synthetic_joint_age[361:480])
# race
poststrat_white <- rowSums(prediction_race[,1:120] %*% diag(synthetic_joint_race[1:120]))/
  sum(synthetic_joint_race[1:120])
poststrat_black <- rowSums(prediction_race[,121:240] %*% diag(synthetic_joint_race[121:240]))/
  sum(synthetic_joint_race[121:240])
poststrat_hispanic <- rowSums(prediction_race[,241:360] %*% diag(synthetic_joint_race[241:360]))/
  sum(synthetic_joint_race[241:360])
poststrat_other <- rowSums(prediction_race[,361:480] %*% diag(synthetic_joint_race[361:480]))/
  sum(synthetic_joint_race[361:480])
# party
poststrat_dem <- rowSums(prediction_party[,1:160] %*% diag(synthetic_joint_party[1:160]))/
  sum(synthetic_joint_party[1:160])
poststrat_rep <- rowSums(prediction_party[,161:320] %*% diag(synthetic_joint_party[161:320]))/
  sum(synthetic_joint_party[161:320])
poststrat_none <- rowSums(prediction_party[,321:480] %*% diag(synthetic_joint_party[321:480]))/
  sum(synthetic_joint_party[321:480])
# income
poststrat_to_15 <- rowSums(prediction_income[,1:96] %*% diag(synthetic_joint_income[1:96]))/
  sum(synthetic_joint_income[1:96])
poststrat_15_30 <- rowSums(prediction_income[,97:192] %*% diag(synthetic_joint_income[97:192]))/
  sum(synthetic_joint_income[97:192])
poststrat_30_50 <- rowSums(prediction_income[,193:288] %*% diag(synthetic_joint_income[193:288]))/
  sum(synthetic_joint_income[193:288])
poststrat_50_75 <- rowSums(prediction_income[,289:384] %*% diag(synthetic_joint_income[289:384]))/
  sum(synthetic_joint_income[289:384])
poststrat_75_plus <- rowSums(prediction_income[,385:480] %*% diag(synthetic_joint_income[385:480]))/
  sum(synthetic_joint_income[385:480])

saveRDS(poststrat_male, "./data/models/poststrat_male")
saveRDS(poststrat_female, "./data/models/poststrat_female")
saveRDS(poststrat_dem, "./data/models/poststrat_dem")
saveRDS(poststrat_rep, "./data/models/poststrat_rep")
saveRDS(poststrat_none, "./data/models/poststrat_none")
saveRDS(poststrat_18_29, "./data/models/poststrat_18_29")
saveRDS(poststrat_30_44, "./data/models/poststrat_30_44")
saveRDS(poststrat_45_64, "./data/models/poststrat_45_64")
saveRDS(poststrat_65plus, "./data/models/poststrat_65plus")
saveRDS(poststrat_white, "./data/models/poststrat_white")
saveRDS(poststrat_black, "./data/models/poststrat_black")
saveRDS(poststrat_hispanic, "./data/models/poststrat_hispanic")
saveRDS(poststrat_other, "./data/models/poststrat_other")
saveRDS(poststrat_to_15, "./data/models/poststrat_to_15")
saveRDS(poststrat_15_30, "./data/models/poststrat_15_30")
saveRDS(poststrat_30_50, "./data/models/poststrat_30_50")
saveRDS(poststrat_50_75, "./data/models/poststrat_50_75")
saveRDS(poststrat_75_plus, "./data/models/poststrat_75_plus")


#### SOCIAL MEDIA-BASED PARTICIPATION AND VOTING OVERLAP FOR VARIOUS SUBSETS ============

# create subsample indicators -----------------------------------------------------------
# initiate connection to sql database
con_1 <- dbConnect(SQLite(), 
                   dbname = "./data/social-media-activity/april2019/social-media-act.sqlite")
# gather twitter ids of those active in any way
active_all <- unique(c(unique(collect(tbl(con_1, "change_friends"))$t_ids),
                       unique(collect(tbl(con_1, "change_likes"))$t_ids),
                       unique(collect(tbl(con_1, "change_statuses"))$t_ids)))
# gather twitter ids of those active as regards statuses
active_statuses <- unique(collect(tbl(con_1, "change_statuses"))$t_ids)
# import processed texts
analysis_processed <- readRDS("./data/analysis/analysis_processed")
# group by twitter id and count statuses activities
activity <- analysis_processed %>%
  group_by(t_ids) %>%
  summarize(count = n())
active_over90 <- filter(activity, count > quantile(activity$count, prob = 0.9))
active_over95 <- filter(activity, count > quantile(activity$count, prob = 0.95))
active_over99 <- filter(activity, count > quantile(activity$count, prob = 0.99))
active_under20 <- filter(activity, count < quantile(activity$count, prob = 0.20))
dbDisconnect(con_1)
# active_all - subset active statuses, friends, likes during period investigated
# active_statuses - subset active statuses during period investigated
# active_over90 - subset activity volume upper 10 percent
# active_over95 - subset activity volume upper 5 percent
# active_over99 - subset activity volume upper 1 percent

# merge indicators with analysis sample -------------------------------------------------
sample_analysis <- readRDS("./data/analysis/sample_analysis")
sample_analysis$active_all <- FALSE # doesn't really make sense since being active is a prerequisite of participation
sample_analysis$active_statuses <- FALSE # as above
sample_analysis$active_over90 <- FALSE
sample_analysis$active_over95 <- FALSE
sample_analysis$active_over99 <- FALSE
sample_analysis$active_under20 <- FALSE
sample_analysis$active_all[which(sample_analysis$twitter_id %in% active_all)] <- TRUE
sample_analysis$active_statuses[which(sample_analysis$twitter_id %in% active_statuses)] <- TRUE
sample_analysis$active_over90[which(sample_analysis$twitter_id %in% active_over90$t_ids)] <- TRUE
sample_analysis$active_over95[which(sample_analysis$twitter_id %in% active_over95$t_ids)] <- TRUE
sample_analysis$active_over99[which(sample_analysis$twitter_id %in% active_over99$t_ids)] <- TRUE
sample_analysis$active_under20[which(sample_analysis$twitter_id %in% active_under20$t_ids)] <- TRUE

# check voting and online participation overlap for multiple subsets --------------------
subsample_euler_1_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & active_over99 == TRUE) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_1_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & active_over99 == TRUE) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_1_1$original.values[6]/(subsample_euler_1_1$original.values[6] +
                                        subsample_euler_1_1$original.values[7])
subsample_euler_1_2$original.values[6]/(subsample_euler_1_2$original.values[6] +
                                          subsample_euler_1_2$original.values[7])
subsample_euler_2_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & active_over95 == TRUE) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_2_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & active_over95 == TRUE) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_2_1$original.values[6]/(subsample_euler_2_1$original.values[6] +
                                          subsample_euler_2_1$original.values[7])
subsample_euler_2_2$original.values[6]/(subsample_euler_2_2$original.values[6] +
                                          subsample_euler_2_2$original.values[7])
subsample_euler_3_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & active_over90 == TRUE) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_3_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & active_over90 == TRUE) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_3_1$original.values[6]/(subsample_euler_3_1$original.values[6] +
                                          subsample_euler_3_1$original.values[7])
subsample_euler_3_2$original.values[6]/(subsample_euler_3_2$original.values[6] +
                                          subsample_euler_3_2$original.values[7])
subsample_euler_4_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & active_under20 == TRUE) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_4_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & active_under20 == TRUE) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_4_1$original.values[6]/(subsample_euler_4_1$original.values[6] +
                                        subsample_euler_4_1$original.values[7])
subsample_euler_4_2$original.values[6]/(subsample_euler_4_2$original.values[6] +
                                          subsample_euler_4_2$original.values[7])
subsample_euler_5_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE) %>%
  dplyr::select(voted_gen_2018, polact_full_a2) %>%
  mutate(polact_full_a2 = ifelse(polact_full_a2 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_5_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE) %>%
  dplyr::select(voted_pri_2018, polact_full_a2) %>%
  mutate(polact_full_a2 = ifelse(polact_full_a2 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_5_1$original.values[6]/(subsample_euler_5_1$original.values[6] +
                                          subsample_euler_5_1$original.values[7])
subsample_euler_5_2$original.values[6]/(subsample_euler_5_2$original.values[6] +
                                        subsample_euler_5_2$original.values[7])
subsample_euler_6_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE) %>%
  dplyr::select(voted_gen_2018, polact_full_a3) %>%
  mutate(polact_full_a3 = ifelse(polact_full_a3 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_6_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE) %>%
  dplyr::select(voted_pri_2018, polact_full_a3) %>%
  mutate(polact_full_a3 = ifelse(polact_full_a3 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_6_1$original.values[6]/(subsample_euler_6_1$original.values[6] +
                                          subsample_euler_6_1$original.values[7])
subsample_euler_6_2$original.values[6]/(subsample_euler_6_2$original.values[6] +
                                          subsample_euler_6_2$original.values[7])
subsample_euler_7_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE) %>%
  dplyr::select(voted_gen_2018, polact_full_a4) %>%
  mutate(polact_full_a4 = ifelse(polact_full_a4 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_7_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE) %>%
  dplyr::select(voted_pri_2018, polact_full_a4) %>%
  mutate(polact_full_a4 = ifelse(polact_full_a4 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_7_1$original.values[6]/(subsample_euler_7_1$original.values[6] +
                                          subsample_euler_7_1$original.values[7])
subsample_euler_7_2$original.values[6]/(subsample_euler_7_2$original.values[6] +
                                          subsample_euler_7_2$original.values[7])
subsample_euler_8_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_elw_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_8_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_elw_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_8_1$original.values[6]/(subsample_euler_8_1$original.values[6] +
                                          subsample_euler_8_1$original.values[7])
subsample_euler_8_2$original.values[6]/(subsample_euler_8_2$original.values[6] +
                                        subsample_euler_8_2$original.values[7])
subsample_euler_9_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_elwm5_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_9_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_elwm5_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_9_1$original.values[6]/(subsample_euler_9_1$original.values[6] +
                                        subsample_euler_9_1$original.values[7])
subsample_euler_9_2$original.values[6]/(subsample_euler_9_2$original.values[6] +
                                          subsample_euler_9_2$original.values[7])
subsample_euler_10_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_elwp5_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_10_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_elwp5_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_10_1$original.values[6]/(subsample_euler_10_1$original.values[6] +
                                        subsample_euler_10_1$original.values[7])
subsample_euler_10_2$original.values[6]/(subsample_euler_10_2$original.values[6] +
                                           subsample_euler_10_2$original.values[7])
subsample_euler_11_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_082018_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_11_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_082018_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_11_1$original.values[6]/(subsample_euler_11_1$original.values[6] +
                                           subsample_euler_11_1$original.values[7])
subsample_euler_11_2$original.values[6]/(subsample_euler_11_2$original.values[6] +
                                           subsample_euler_11_2$original.values[7])
subsample_euler_12_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_092018_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_12_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_092018_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_12_1$original.values[6]/(subsample_euler_12_1$original.values[6] +
                                        subsample_euler_12_1$original.values[7])
subsample_euler_12_2$original.values[6]/(subsample_euler_12_2$original.values[6] +
                                           subsample_euler_12_2$original.values[7])
subsample_euler_13_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_102018_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_13_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_102018_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_13_1$original.values[6]/(subsample_euler_13_1$original.values[6] +
                                        subsample_euler_13_1$original.values[7])
subsample_euler_13_2$original.values[6]/(subsample_euler_13_2$original.values[6] +
                                           subsample_euler_13_2$original.values[7])
subsample_euler_14_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_112018_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_14_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_112018_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_14_1$original.values[6]/(subsample_euler_14_1$original.values[6] +
                                        subsample_euler_14_1$original.values[7])
subsample_euler_14_2$original.values[6]/(subsample_euler_14_2$original.values[6] +
                                           subsample_euler_14_2$original.values[7])
subsample_euler_15_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_122018_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_15_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_122018_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_15_1$original.values[6]/(subsample_euler_15_1$original.values[6] +
                                        subsample_euler_15_1$original.values[7])
subsample_euler_15_2$original.values[6]/(subsample_euler_15_2$original.values[6] +
                                           subsample_euler_15_2$original.values[7])
subsample_euler_16_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_012019_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_16_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_012019_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_16_1$original.values[6]/(subsample_euler_16_1$original.values[6] +
                                        subsample_euler_16_1$original.values[7])
subsample_euler_16_2$original.values[6]/(subsample_euler_16_2$original.values[6] +
                                           subsample_euler_16_2$original.values[7])
subsample_euler_17_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_022019_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_17_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_022019_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_17_1$original.values[6]/(subsample_euler_17_1$original.values[6] +
                                        subsample_euler_17_1$original.values[7])
subsample_euler_17_2$original.values[6]/(subsample_euler_17_2$original.values[6] +
                                           subsample_euler_17_2$original.values[7])
subsample_euler_18_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_032019_a1 == 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_18_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_032019_a1 == 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_18_1$original.values[6]/(subsample_euler_18_1$original.values[6] +
                                        subsample_euler_18_1$original.values[7])
subsample_euler_18_2$original.values[6]/(subsample_euler_18_2$original.values[6] +
                                           subsample_euler_18_2$original.values[7])
subsample_euler_19_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_full_a1 == 1 & party != "npa") %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_19_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_full_a1 == 1 & party != "npa") %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_19_1$original.values[6]/(subsample_euler_19_1$original.values[6] +
                                           subsample_euler_19_1$original.values[7])
subsample_euler_19_2$original.values[6]/(subsample_euler_19_2$original.values[6] +
                                           subsample_euler_19_2$original.values[7])
subsample_euler_19_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_full_a1 == 1 & party == "npa") %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_19_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_full_a1 == 1 & party == "npa") %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_19_1$original.values[6]/(subsample_euler_19_1$original.values[6] +
                                           subsample_euler_19_1$original.values[7])
subsample_euler_19_2$original.values[6]/(subsample_euler_19_2$original.values[6] +
                                           subsample_euler_19_2$original.values[7])
subsample_euler_20_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & polact_full_a1 == 1 & (registration_status != "INA" | registration_status_2018 != "INA")) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_20_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & polact_full_a1 == 1 & (registration_status != "INA" | registration_status_2018 != "INA")) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_20_1$original.values[6]/(subsample_euler_20_1$original.values[6] +
                                           subsample_euler_20_1$original.values[7])
subsample_euler_20_2$original.values[6]/(subsample_euler_20_2$original.values[6] +
                                           subsample_euler_20_2$original.values[7])
subsample_euler_21_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & theta_median >= -1.5) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_21_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & theta_median >= -1.5) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_21_1$original.values[6]/(subsample_euler_21_1$original.values[6] +
                                           subsample_euler_21_1$original.values[7])
subsample_euler_21_2$original.values[6]/(subsample_euler_21_2$original.values[6] +
                                           subsample_euler_21_2$original.values[7])
subsample_euler_22_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & theta_median >= -1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_22_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & theta_median >= -1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_22_1$original.values[6]/(subsample_euler_22_1$original.values[6] +
                                           subsample_euler_22_1$original.values[7])
subsample_euler_22_2$original.values[6]/(subsample_euler_22_2$original.values[6] +
                                           subsample_euler_22_2$original.values[7])
subsample_euler_23_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & theta_median >= -0.5) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_23_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & theta_median >= -0.5) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_23_1$original.values[6]/(subsample_euler_23_1$original.values[6] +
                                           subsample_euler_23_1$original.values[7])
subsample_euler_23_2$original.values[6]/(subsample_euler_23_2$original.values[6] +
                                           subsample_euler_23_2$original.values[7])
subsample_euler_24_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & theta_median >= 0) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_24_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & theta_median >= 0) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_24_1$original.values[6]/(subsample_euler_24_1$original.values[6] +
                                           subsample_euler_24_1$original.values[7])
subsample_euler_24_2$original.values[6]/(subsample_euler_24_2$original.values[6] +
                                           subsample_euler_24_2$original.values[7])
subsample_euler_25_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & theta_median >= 0.5) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_25_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & theta_median >= 0.5) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_25_1$original.values[6]/(subsample_euler_25_1$original.values[6] +
                                           subsample_euler_25_1$original.values[7])
subsample_euler_25_2$original.values[6]/(subsample_euler_25_2$original.values[6] +
                                           subsample_euler_25_2$original.values[7])
subsample_euler_26_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & theta_median >= 1) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_26_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & theta_median >= 1) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_26_1$original.values[6]/(subsample_euler_26_1$original.values[6] +
                                           subsample_euler_26_1$original.values[7])
subsample_euler_26_2$original.values[6]/(subsample_euler_26_2$original.values[6] +
                                           subsample_euler_26_2$original.values[7])
subsample_euler_27_1 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_gen_2018 == TRUE & theta_median >= 1.5) %>%
  dplyr::select(voted_gen_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_27_2 <- sample_analysis %>%
  filter(terminated == FALSE & eligible_pri_2018 == TRUE & theta_median >= 1.5) %>%
  dplyr::select(voted_pri_2018, polact_full_a1) %>%
  mutate(polact_full_a1 = ifelse(polact_full_a1 == 1, TRUE, FALSE),
         pop = TRUE) %>%
  euler()
subsample_euler_27_1$original.values[6]/(subsample_euler_27_1$original.values[6] +
                                           subsample_euler_25_1$original.values[7])
subsample_euler_27_2$original.values[6]/(subsample_euler_27_2$original.values[6] +
                                           subsample_euler_27_2$original.values[7])


#### 2018 TURNOUT AMONG SUBGROUPS OF THE REGISTERED VOTER POPULATION ====================

set.seed(823016)

# import voter records 2018 ------------------------
varlabs <- c("county", "id", "surname", "sufname", "forename", "midname", "exempt",
             "street", "addresssup", "place", "state", "zip", "mail1", "mail2", "mail3",
             "mailplace", "mailstate", "mailzip", "mailcountry", "gender", "race",
             "birth", "regist", "party", "precinct", "precinctgroup", "precinctsplit",
             "precinctsuff", "status", "congdist", "housdist", "sendist", "comdist",
             "schooldist", "areacode", "phonenum", "phoneext", "email")
varlabs <- varlabs[-c(7,11,13:19,26:28)]
folder_b <- "./data/voter-files-updated/"
files_b <- list.files(folder_b)
sample <- data.frame(matrix(nrow = 0, ncol = 26,
                            dimnames = list(c(), varlabs)))
for (i in 1:length(files_b)) {
  b <- fread(input = paste0(folder_b, files_b[i]), header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(7,11,13:19,26:28), na.strings = c("", "*"),
             quote = "")
  sample  <- rbind(sample, b)
  cat(".")
}

sample <- sample %>%
  sample_n(size = 1000000)

# remove duplicate entries of registered voters -----------------------------------------
# transform register date to class Date variable
sample$regist <- sample$regist %>% str_replace_all("\\/", "-") %>% mdy()
# sort data by register date in ascending order
sample <- arrange(sample, regist)
# remove duplicate entries (record ID) keeping the most recent date of registry
sample <- sample[!duplicated(sample$id, fromLast = TRUE),]

# add voter histories -----------------------
sample <- sample %>% mutate(gen2006 = NA, gen2008 = NA, gen2010 = NA,
                            gen2012 = NA, gen2014 = NA, gen2016 = NA,
                            gen2018 = NA, pri2006 = NA, pri2008 = NA,
                            pri2010 = NA, pri2012 = NA, pri2014 = NA,
                            pri2016 = NA, pri2018 = NA)
sample <- as.data.frame(sample)

sample_hist <- voteHist2(histpath = "./data/voter-histories-updated/",
                         sample = sample)
sample_hist <- sample_hist %>%
  dplyr::distinct(id, .keep_all = TRUE)

saveRDS(sample_hist, "data/voter_record_sample_hist")
saveRDS(sample, "data/voter_record_sample")
sample_processed <- sample_hist
sample_processed <- sample_processed %>%
  dplyr::rename(last_name = surname,
                middle_name = midname,
                first_name = forename,
                voter_id = id,
                sex = gender,
                birth_date = birth,
                registration_date = regist,
                registration_status = status,
                congressional_district = congdist,
                voted_gen_2006 = gen2006,
                voted_gen_2008 = gen2008,
                voted_gen_2010 = gen2010,
                voted_gen_2012 = gen2012,
                voted_gen_2014 = gen2014,
                voted_gen_2016 = gen2016,
                voted_gen_2018 = gen2018,
                voted_pri_2006 = pri2006,
                voted_pri_2008 = pri2008,
                voted_pri_2010 = pri2010,
                voted_pri_2012 = pri2012,
                voted_pri_2014 = pri2014,
                voted_pri_2016 = pri2016,
                voted_pri_2018 = pri2018) %>%
  dplyr::select(voter_id, first_name, middle_name, last_name, county,
                place, zip, street, congressional_district, precinct, sex, birth_date, race, 
                party, registration_date, registration_status, voted_gen_2006:voted_pri_2018)

# format name variables -----------------------------------------------------------------
sample_processed$first_name <- tolower(sample_processed$first_name)
sample_processed$middle_name <- tolower(sample_processed$middle_name)
sample_processed$last_name <- tolower(sample_processed$last_name)

# format county variable ----------------------------------------------------------------
sample_processed$county <- tolower(sample_processed$county)

# format geographic variables -----------------------------------------------------------
sample_processed$place <- tolower(sample_processed$place)
sample_processed$zip <- as.integer(str_extract(sample_processed$zip, "^[[:digit:]]{5}"))
sample_processed$street <- tolower(sample_processed$street) %>% str_squish()

# format sex variable and fill missing values -------------------------------------------
sample_processed$sex <- ifelse(sample_processed$sex == "F", "female",
                               ifelse(sample_processed$sex == "M", "male",
                                      NA))
# remove missings
sample_processed <- sample_processed %>%
  filter(!is.na(sex))

# format birth_date variable and compute age --------------------------------------------
sample_processed$birth_date <- sample_processed$birth_date %>%
  str_replace_all("/", "-") %>%
  mdy
sample_processed <- sample_processed %>%
  filter(!is.na(birth_date))
# age until February 2019
sample_processed$age <- age_calc(dob = sample_processed$birth_date,
                                 enddate = Sys.Date(),
                                 units = "years",
                                 precise = TRUE)
sample_processed <- dplyr::select(sample_processed, voter_id:birth_date, age, 
                                  race:voted_pri_2018)

# format ethnicity variable -------------------------------------
sample_processed <- sample_processed %>%
  filter(race != 9)

# format race
sample_processed$race <- ifelse(sample_processed$race == 1, "native",
                                ifelse(sample_processed$race == 2, "asian",
                                       ifelse(sample_processed$race == 3, "black",
                                              ifelse(sample_processed$race == 4, "hispanic",
                                                     ifelse(sample_processed$race == 5, "white",
                                                            ifelse(sample_processed$race == 6, "other",
                                                                   ifelse(sample_processed$race == 7, "multi", NA)))))))

# format party variable -----------------------------------------------------------------
sample_processed$party <- ifelse(sample_processed$party == "DEM", "dem",
                                 ifelse(sample_processed$party == "REP", "rep",
                                        ifelse(sample_processed$party == "NPA", "npa",
                                               "other")))

# assign election-specific voting eligibility variables ---------------------------------
eligibility_age <- data.frame(gen_2006 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("11/07/2006") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2008 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("11/04/2008") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2010 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("11/02/2010") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2012 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("11/06/2012") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2014 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("11/04/2014") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2016 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("11/08/2016") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2018 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("11/06/2018") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2006 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("09/05/2006") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2008 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("08/26/2008") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2010 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("08/24/2010") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2012 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("08/14/2012") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2014 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("08/26/2014") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2016 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("08/30/2016") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2018 = age_calc(dob = sample_processed$birth_date, 
                                                  enddate = (mdy("08/28/2018") - 29),
                                                  units = "years", precise = TRUE))
eligibility_reg <- data.frame(gen_2006 = (sample_processed$registration_date - 
                                            mdy("11/07/2006")) <= 0,
                              gen_2008 = (sample_processed$registration_date - 
                                            mdy("11/04/2008")) <= 0,
                              gen_2010 = (sample_processed$registration_date - 
                                            mdy("11/02/2010")) <= 0,
                              gen_2012 = (sample_processed$registration_date - 
                                            mdy("11/06/2012")) <= 0,
                              gen_2014 = (sample_processed$registration_date - 
                                            mdy("11/04/2014")) <= 0,
                              gen_2016 = (sample_processed$registration_date - 
                                            mdy("11/08/2016")) <= 0,
                              gen_2018 = (sample_processed$registration_date - 
                                            mdy("11/06/2018")) <= 0,
                              pri_2006 = (sample_processed$registration_date - 
                                            mdy("09/05/2006")) <= 0,
                              pri_2008 = (sample_processed$registration_date - 
                                            mdy("08/26/2008")) <= 0,
                              pri_2010 = (sample_processed$registration_date - 
                                            mdy("08/24/2010")) <= 0,
                              pri_2012 = (sample_processed$registration_date - 
                                            mdy("08/14/2012")) <= 0,
                              pri_2014 = (sample_processed$registration_date - 
                                            mdy("08/26/2014")) <= 0,
                              pri_2016 = (sample_processed$registration_date - 
                                            mdy("08/30/2016")) <= 0,
                              pri_2018 = (sample_processed$registration_date - 
                                            mdy("08/28/2018")) <= 0)
sample_processed <- sample_processed %>%
  mutate(eligible_gen_2006 = eligibility_age$gen_2006 >= 18 & eligibility_reg$gen_2006 == TRUE,
         eligible_gen_2008 = eligibility_age$gen_2008 >= 18 & eligibility_reg$gen_2008 == TRUE,
         eligible_gen_2010 = eligibility_age$gen_2010 >= 18 & eligibility_reg$gen_2010 == TRUE,
         eligible_gen_2012 = eligibility_age$gen_2012 >= 18 & eligibility_reg$gen_2012 == TRUE,
         eligible_gen_2014 = eligibility_age$gen_2014 >= 18 & eligibility_reg$gen_2014 == TRUE,
         eligible_gen_2016 = eligibility_age$gen_2016 >= 18 & eligibility_reg$gen_2016 == TRUE,
         eligible_gen_2018 = eligibility_age$gen_2018 >= 18 & eligibility_reg$gen_2018 == TRUE,
         eligible_pri_2006 = eligibility_age$pri_2006 >= 18 & eligibility_reg$pri_2006 == TRUE,
         eligible_pri_2008 = eligibility_age$pri_2008 >= 18 & eligibility_reg$pri_2008 == TRUE,
         eligible_pri_2010 = eligibility_age$pri_2010 >= 18 & eligibility_reg$pri_2010 == TRUE,
         eligible_pri_2012 = eligibility_age$pri_2012 >= 18 & eligibility_reg$pri_2012 == TRUE,
         eligible_pri_2014 = eligibility_age$pri_2014 >= 18 & eligibility_reg$pri_2014 == TRUE,
         eligible_pri_2016 = eligibility_age$pri_2016 >= 18 & eligibility_reg$pri_2016 == TRUE,
         eligible_pri_2018 = eligibility_age$pri_2018 >= 18 & eligibility_reg$pri_2018 == TRUE)
sample_processed <- sample_processed %>% 
  replace_na(list(voted_gen_2006 = FALSE, voted_gen_2008 = FALSE, voted_gen_2010 = FALSE,
                  voted_gen_2012 = FALSE, voted_gen_2014 = FALSE, voted_gen_2016 = FALSE,
                  voted_gen_2018 = FALSE, voted_pri_2006 = FALSE, voted_pri_2008 = FALSE,
                  voted_pri_2010 = FALSE, voted_pri_2012 = FALSE, voted_pri_2014 = FALSE,
                  voted_pri_2016 = FALSE, voted_pri_2018 = FALSE))
saveRDS(sample_processed, "data/voter_record_sample_processed")
sample_processed <- readRDS("data/voter_record_sample_processed")
sample_a <- sample_processed %>%
  filter(registration_status == "ACT" & eligible_gen_2018 == TRUE)
sample_a <- sample_a %>%
  dplyr::sample_n(100000)
sample_a <- sample_a %>%
  mutate(lon = NA, lat = NA, address = NA, confidence = NA)
# geocode via Bing Maps API -------------------------------------------------------------
# max 50,000 queries per day, increase and continue on subsequent days
sample_geo <- geoLocate2(data = sample_geo, range = 1:nrow(sample_geo), lon_column = "lon", 
                         lat_column = "lat", city_column = "place", 
                         street_column = "street", zip_column = "zip", 
                         accuracy_column = "confidence", address_column = "address", 
                         bing_maps_key = "API KEY")
saveRDS(sample_geo, "data/voter_record_sample_processed_geo")

# get censusblock
# sample_geo$censusblock <- NA
for (i in 1:nrow(sample_geo)) {
  if (is.na(sample_geo$lat[i])) {
    next
  }
  if (is.na(sample_geo$censusblock[i])) {
    sample_geo$censusblock[i] <- tigris::call_geolocator_latlon(lat = sample_geo$lat[i], 
                                                                lon = sample_geo$lon[i])
  }
  cat(".")
  if (i%%1000 == 0)
    cat("\n", i, "\n")
}

census_api_key <- "API KEY"
sample_geo <- readRDS("data/voter_record_sample_processed_geo")
sample_geo$state <- str_extract(sample_geo$censusblock,
                                "^[[:digit:]]{2}")
sample_geo <- sample_geo %>%
  mutate(censusblock = ifelse(state == "12", censusblock, NA))

# extract fips county codes from census block -------------------------------------------
sample_geo$county <- str_extract(sample_geo$censusblock, 
                                 "(?<=^[[:digit:]]{2})[[:digit:]]{3}")

# shorten census block to census block-group code ---------------------------------------
sample_geo$censusblock_group <- str_replace(sample_geo$censusblock, "[[:digit:]]{3}$", 
                                            "")
counties <- unique(sample_geo$county) %>%
  na.omit()

# collect census block group data for PER CAPITA INCOME IN THE PAST 12 MONTHS -----------
iter <- 1
for (i in 1:length(counties)) {
  cat(i,"\n")
  censusdata <- getCensus(name = "acs/acs5", vintage = 2017, key = census_api_key, 
                          region = "block group:*",
                          regionin = str_c("state:12+county:", 
                                           counties[i]),
                          vars = c("B19301_001E"))
  if (iter == 1) {
    income <- censusdata
  } else {
    income <- rbind(income,censusdata)
  }
  iter <- iter + 1
  rm(censusdata)
}
income$censusblock_group <- str_c(income$state, income$county, income$tract, 
                                  income$block_group)
income <- dplyr::select(income, censusblock_group, B19301_001E)
sample_geo <- left_join(x = sample_geo, y = income, by = "censusblock_group")
sample_geo[which(sample_geo$B19301_001E == -666666666),]$B19301_001E <- NA
colnames(sample_geo)[53] <- "income_pci"
sample_geo2 <- sample_geo %>%
  filter(!is.na(income_pci))

# process
sample_processed <- sample_geo %>%
  dplyr::select(voter_id, county, sex, birth_date, age, race, party, voted_gen_2018,
                voted_pri_2018, eligible_pri_2018, income_pci)
a <- mice(data = sample_processed, m = 5,  meth = c("", "", "", "", "", "", "", "", "", "", "cart"))
b <- mice::complete(a, "broad", include = FALSE) %>%
  dplyr::select(starts_with("income_pci")) %>%
  rowMeans()
sample_processed$income_pci <- ifelse(is.na(sample_processed$income_pci), b, sample_processed$income_pci)

# construct age and race groups ---------------------------------------------------------
sample_processed$age_group <- ifelse(sample_processed$age <= 29, "age_18_29",
                                     ifelse(sample_processed$age > 29 & sample_processed$age <= 44, "age_30_44", 
                                            ifelse(sample_processed$age > 44 & sample_processed$age <= 64, "age_45_64", 
                                                   ifelse(sample_processed$age > 64, "age_65_plus", 
                                                          NA))))

sample_processed$race_group <- ifelse(sample_processed$race == "multi", "other",
                                      ifelse(sample_processed$race == "asian", "other",
                                             ifelse(sample_processed$race == "native", "other",
                                                    sample_processed$race)))

sample_processed$income <- ifelse(sample_processed$income_pci <= 15000, 1L,
                                  ifelse(sample_processed$income_pci > 15000 & 
                                           sample_processed$income_pci <= 30000, 2L,
                                         ifelse(sample_processed$income_pci > 30000 & 
                                                  sample_processed$income_pci <= 50000, 3L,
                                                ifelse(sample_processed$income_pci > 50000 & 
                                                         sample_processed$income_pci <= 75000, 4L,
                                                       ifelse(sample_processed$income_pci > 75000, 5L, 
                                                              NA))))) %>%
  factor()

# table 
table(sample_processed$voted_gen_2018, sample_processed$sex)[2,]/table(sample_processed$sex)
table(sample_processed$voted_gen_2018, sample_processed$age_group)[2,]/table(sample_processed$age_group)
table(sample_processed$voted_gen_2018, sample_processed$race_group)[2,]/table(sample_processed$race_group)
table(sample_processed$voted_gen_2018, sample_processed$party)[2,]/table(sample_processed$party)
table(sample_processed$voted_gen_2018, sample_processed$income)[2,]/table(sample_processed$income)

sample_processed_pri <- sample_processed %>%
  filter(eligible_pri_2018 == TRUE)

table(sample_processed_pri$voted_pri_2018, sample_processed_pri$sex)[2,]/table(sample_processed_pri$sex)
table(sample_processed_pri$voted_pri_2018, sample_processed_pri$age_group)[2,]/table(sample_processed_pri$age_group)
table(sample_processed_pri$voted_pri_2018, sample_processed_pri$race_group)[2,]/table(sample_processed_pri$race_group)
table(sample_processed_pri$voted_pri_2018, sample_processed_pri$party)[2,]/table(sample_processed_pri$party)
table(sample_processed_pri$voted_pri_2018, sample_processed_pri$income)[2,]/table(sample_processed_pri$income)
