# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Target population estimation script
# April 2019
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./data/auxiliary/acs2017-oneyear-pums-pl.csv'
'./data/sample/*'
'./data/voter-files/*'
'./data/voter-histories/'
'./data/voter-histories-updated/'
'./data/voter-files-updated/'
'./data/auxiliary/cvap-blockgr-acs5-2017.csv'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/analysis/cvap'
'./data/analysis/cvap_prop'
'./data/analysis/cvap2'
'./data/analysis/cvap2_prop'
'./data/analysis/vep'
'./data/analysis/vep_prop'
'./data/analysis/vep2'
'./data/analysis/vep2_prop'
'./data/analysis/record_sample'
'./data/analysis/rvp_prop'
'./data/analysis/pci_cvap'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 46 - PREPARATIONS
Line 59 - ESTIMATE CITIZEN VOTING AGE POPULAION (CVAP) WITH ACS AND PUMS DATA
Line 233 - ESTIMATE CITIZEN VOTING AGE POPULATION (CVAP) WITH PUMS DATA
Line 296 - ESTIMATE VOTING-ELIGIBLE POPULATION (VEP) BY CORRECTING FOR CORRECTIONAL POPULATION
Line 388 - ESTIMATE REGISTERED-VOTER POPULATION (RVP) WITH VOTER FILE DATA
Line 706 - ESTIMATE PER CAPITA INCOME DISTRIBUTION OF CITIZEN-VOTING AGE POPULATION
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")


#### ESTIMATE CITIZEN VOTING AGE POPULAION (CVAP) WITH ACS AND PUMS DATA ================

# assign API key for US census bureau API -----------------------------------------------
census_api_key <- "API KEY"

# collect acs census data ---------------------------------------------------------------
# population total, sex, and race from acs 2017 1-year subject table citizen voting age 
# population characteristics (estimate, percentage)
# total = S2901_C01_001E
# male = S2901_C01_006E
# female = S2901_C01_007E
# white = S2901_C01_016E
# black = S2901_C01_009E
# hispanic = S2901_C01_015E
# asian = S2901_C01_010E
# native = S2901_C01_011E
# islander = S2901_C01_012E
# other race = S2901_C01_013E
# multiracial = S2901_C01_014E
# retrieve estimates with margings of error
cvap <- get_acs(geography = "state", state = "FL", year = 2017, survey = "acs1",
                key = census_api_key, 
                variables = c("S2901_C01_001E","S2901_C01_006E","S2901_C01_007E",
                              "S2901_C01_016E", "S2901_C01_009E", "S2901_C01_015E",
                              "S2901_C01_010E", "S2901_C01_011E", "S2901_C01_012E",
                              "S2901_C01_013E", "S2901_C01_014E"),
                moe_level = 95)
# adjust variable names
cvap$variable <- ifelse(cvap$variable == "S2901_C01_001", "total",
                  ifelse(cvap$variable == "S2901_C01_006", "male",
                   ifelse(cvap$variable == "S2901_C01_007", "female",
                    ifelse(cvap$variable == "S2901_C01_016", "white",
                     ifelse(cvap$variable == "S2901_C01_009", "black",
                      ifelse(cvap$variable == "S2901_C01_015", "hispanic",
                       ifelse(cvap$variable == "S2901_C01_010", "asian",
                        ifelse(cvap$variable == "S2901_C01_011", "native",
                         ifelse(cvap$variable == "S2901_C01_012", "islander",
                          ifelse(cvap$variable == "S2901_C01_013", "other_race",
                           ifelse(cvap$variable == "S2901_C01_014", "multiracial",
                                  cvap$variable)))))))))))
cvap <- select(cvap, variable, estimate, moe)
# sum estimate of race asian, native, islander, other, and multiracial together
race_other_est <- sum(c(cvap$estimate[5],cvap$estimate[6],
                        cvap$estimate[7],cvap$estimate[8],
                        cvap$estimate[9]))
# calculate appropriate margin of error for summed racial groups
race_other_moe <- moe_sum(estimate = c(cvap$estimate[5],cvap$estimate[6],
                                       cvap$estimate[7],cvap$estimate[8],
                                       cvap$estimate[9]), 
                          moe = c(cvap$moe[5],cvap$moe[6],cvap$moe[7],cvap$moe[8],
                                  cvap$moe[9]))
# add summed estimate to data and remove redundant subgroups
cvap <- rbind(cvap, data.frame(variable = "other", estimate = race_other_est, 
                               moe = race_other_moe))
cvap <- cvap[-c(5:9),]

# compute share of black-hispanic and other-hispanic from PUMS data ---------------------
# in the acs white is non-hispanic but all other races incorporate hispanics, this is why
# the summed total of all races exceeds the total cvap. To compare race groups with the
# sample, hispanics must be included as hispanic voters are coded as such in the
# voter file and not as black-hispanic etc. Also, the correctional population is coded
# for white, black, hispanic, and other. For this reason PUMS (Public Use Microdata 
# Sample) data is used to check shares of hispanics among other races and subtract 
# accordingly.compute amount of hispanics by which summed race groups exceeds the 
# population total
exc_hisp <- sum(cvap$estimate[c(4,5,6,7)])-cvap$estimate[1]
# import PUMS data
fl_pums <- read.csv("./data/auxiliary/acs2017-oneyear-pums-pl.csv")
# add group variable for hispanics
fl_pums$hispanic <- as.factor(ifelse(fl_pums$HISP == 1, "no", "yes"))
# add group variable for race groups
fl_pums$race <- as.factor(ifelse(fl_pums$RAC1P == 1, "white",
                                 ifelse(fl_pums$RAC1P == 2, "black", "other")))
# remove white and non-hispanics (interest is in share of hispanics among black and
# other race groups)
fl_pums <- fl_pums[-which(fl_pums$race == "white"|fl_pums$hispanic == "no"),]
# group data by race
fl_pums_grouped <- fl_pums %>% group_by(race)
# compute proportions
hisp_shares <- group.proportion(x = fl_pums_grouped, gp.var = NULL)
exc_hisp_black <- exc_hisp*hisp_shares$Pct.[1] # 90887.92
exc_hisp_other <- exc_hisp*hisp_shares$Pct.[2] # 378135.1

# remove excessive black and other hispanics from respective subgroups ------------------
cvap$estimate[4] <-  cvap$estimate[4]-exc_hisp_black
cvap$estimate[7] <-  cvap$estimate[7]-exc_hisp_other
rm(fl_pums,fl_pums_grouped,hisp_shares,exc_hisp,exc_hisp_black,exc_hisp_other,
   race_other_est,race_other_moe)

# collect PUMS data for age groups ------------------------------------------------------
# PUMS data is used for age group estimates as available acs tables from
# the census bureau yield age groups that are not in line with the correctional
# population. This needs to align however, as the cvap is corrected by the
# correctional population to estimate the vep
# import PUMS data
fl_pums <- read.csv("./data/auxiliary/acs2017-oneyear-pums-pl.csv")
# add group variable for age groups
fl_pums$age_group <- as.factor(ifelse(fl_pums$AGEP <= 17, "age_17_under",
                                ifelse(fl_pums$AGEP >= 18 & fl_pums$AGEP <= 24, "age_18_24",
                                 ifelse(fl_pums$AGEP >= 25 & fl_pums$AGEP <= 34, "age_25_34",
                                  ifelse(fl_pums$AGEP >= 35 & fl_pums$AGEP <= 49, "age_35_49",
                                   ifelse(fl_pums$AGEP >= 50 & fl_pums$AGEP <= 59, "age_50_59",
                                    ifelse(fl_pums$AGEP >= 60, "age_60plus", NA)))))))
# add group variable for citizenship
fl_pums$citizen <- as.factor(ifelse(fl_pums$CIT == 5, "no", "yes"))
# remove under 18 (interest is in citizen voting-age population)
fl_pums <- fl_pums[-which(fl_pums$age_group == "age_17_under"|fl_pums$citizen == "no"),]
# group data by citizenship and age_group
fl_pums_grouped <- fl_pums %>% group_by(citizen, age_group)
# compute population totals with standard error
cvap_age <- group.estimate(x = fl_pums_grouped, estimate, gp.var = NULL)
# PUMS an acs data come to very similar estimates, however, the point estimate for the
# total population is 15026324-15014950 = 11374 higher than the acs data. This is 
# expected as PUMS is only a sample of the acs. To align the total population estimate
# parts of the excessive population in the PUMS estimate are subtracted from age group 
# age group estimates proportional to subgroup size. This does not change the 
# proportions of the age_group among the total population.
cvap_age$estimate <- cvap_age$estimate - 
  c((cvap_age$estimate[1] * (cvap_age$estimate[6]-cvap$estimate[1]) / cvap_age$estimate[6]),
    (cvap_age$estimate[2] * (cvap_age$estimate[6]-cvap$estimate[1]) / cvap_age$estimate[6]),
    (cvap_age$estimate[3] * (cvap_age$estimate[6]-cvap$estimate[1]) / cvap_age$estimate[6]),
    (cvap_age$estimate[4] * (cvap_age$estimate[6]-cvap$estimate[1]) / cvap_age$estimate[6]),
    (cvap_age$estimate[5] * (cvap_age$estimate[6]-cvap$estimate[1]) / cvap_age$estimate[6]),
    cvap_age$estimate[6]-cvap$estimate[1])
# compute 95 percent margin of error
cvap_age$MoE <- cvap_age$SE*1.96

# once more for age groups so that they align with the model for poststratification -----
fl_pums2 <- read.csv("./data/auxiliary/acs2017-oneyear-pums-pl.csv")
# add group variable for age groups
fl_pums2$age_group <- as.factor(ifelse(fl_pums2$AGEP <= 17, "age_17_under",
                                 ifelse(fl_pums2$AGEP >= 18 & fl_pums2$AGEP <= 29, "age_18_29",
                                  ifelse(fl_pums2$AGEP >= 30 & fl_pums2$AGEP <= 44, "age_30_44",
                                   ifelse(fl_pums2$AGEP >= 45 & fl_pums2$AGEP <= 64, "age_45_64",
                                    ifelse(fl_pums2$AGEP >= 65, "age_60plus", NA))))))
# add group variable for citizenship
fl_pums2$citizen <- as.factor(ifelse(fl_pums2$CIT == 5, "no", "yes"))
# remove under 18 (interest is in citizen voting-age population)
fl_pums2 <- fl_pums2[-which(fl_pums2$age_group == "age_17_under"|fl_pums2$citizen == "no"),]
# group data by citizenship and age_group
fl_pums2_grouped <- fl_pums2 %>% group_by(citizen, age_group)
# compute population totals with standard error
cvap_age2 <- group.estimate(x = fl_pums2_grouped, estimate, gp.var = NULL)
cvap_age2$estimate <- cvap_age2$estimate - 
  c((cvap_age2$estimate[1] * (cvap_age2$estimate[5]-cvap$estimate[1]) / cvap_age2$estimate[5]),
    (cvap_age2$estimate[2] * (cvap_age2$estimate[5]-cvap$estimate[1]) / cvap_age2$estimate[5]),
    (cvap_age2$estimate[3] * (cvap_age2$estimate[5]-cvap$estimate[1]) / cvap_age2$estimate[5]),
    (cvap_age2$estimate[4] * (cvap_age2$estimate[5]-cvap$estimate[1]) / cvap_age2$estimate[5]),
    cvap_age2$estimate[5]-cvap$estimate[1])
# compute 95 percent margin of error
cvap_age2$MoE <- cvap_age2$SE*1.96

# calculate proportions -----------------------------------------------------------------
cvap_prop2 <- data.frame(variable = cvap_age2$age_group,
                         estimate = cvap_age2$estimate/cvap$estimate[1]*100,
                         moe = cvap_age2$MoE/cvap$estimate[1]*100)
saveRDS(cvap_prop2, "./data/analysis/cvap_prop2")


# join cvap acs and pums data -----------------------------------------------------------
colnames(cvap_age)[c(2,5)] <- c("variable", "moe")
cvap_age <- select(cvap_age, variable, estimate, moe)
cvap <- rbind(cvap, cvap_age[1:5,])

# calculate proportions -----------------------------------------------------------------
cvap_prop <- data.frame(variable = cvap$variable,
                        estimate = cvap$estimate/cvap$estimate[1]*100,
                        moe = cvap$moe/cvap$estimate[1]*100)

# save cvap data ------------------------------------------------------------------------
saveRDS(cvap, "./data/analysis/cvap")
saveRDS(cvap_prop, "./data/analysis/cvap_prop")


#### ESTIMATE CITIZEN VOTING AGE POPULATION (CVAP) WITH PUMS DATA =======================

# yields almost exactly the same as acs combined with pums, meaning way excessive
# hispanic population is subtracted is correct

# import PUMS data
fl_pums <- read.csv("./data/auxiliary/acs2017-oneyear-pums-pl.csv")
# add group variable for age groups
fl_pums$age_group <- as.factor(ifelse(fl_pums$AGEP <= 17, "age_17_under",
                                ifelse(fl_pums$AGEP >= 18 & fl_pums$AGEP <= 24, "age_18_24",
                                 ifelse(fl_pums$AGEP >= 25 & fl_pums$AGEP <= 34, "age_25_34",
                                  ifelse(fl_pums$AGEP >= 35 & fl_pums$AGEP <= 49, "age_35_49",
                                   ifelse(fl_pums$AGEP >= 50 & fl_pums$AGEP <= 59, "age_50_59",
                                    ifelse(fl_pums$AGEP >= 60, "age_60plus", NA)))))))
# add group variable for sex groups
fl_pums$sex_group <- as.factor(ifelse(fl_pums$SEX == 1, "male",
                                ifelse(fl_pums$SEX == 2, "female", NA)))
# add group variable for citizenship
fl_pums$citizen_group <- as.factor(ifelse(fl_pums$CIT == 5, "no", "yes"))
# add group variable for hispanics
fl_pums$hispanic <- as.factor(ifelse(fl_pums$HISP == 1, "no", "yes"))
# add group variable for race groups
fl_pums$race <- as.factor(ifelse(fl_pums$RAC1P == 1, "white",
                                 ifelse(fl_pums$RAC1P == 2, "black", "other")))
# remove under 18 and non-citizens (interest is in citizen voting-age population)
fl_pums <- fl_pums[-which(fl_pums$age_group == "age_17_under"|fl_pums$citizen_group == "no"),]
# remove hispanics for correct calculation of black white and other races
fl_pums2 <- fl_pums[-which(fl_pums$hispanic == "yes"),]
# group data by sex, age, and race
fl_pums_grouped_sex <- fl_pums %>% group_by(sex_group)
fl_pums_grouped_age <- fl_pums %>% group_by(age_group)
fl_pums_grouped_hispanic <- fl_pums %>% group_by(hispanic)
fl_pums_grouped_race <- fl_pums2 %>% group_by(race)
# compute population totals with standard error
cvap2_sex <- group.estimate(x = fl_pums_grouped_sex, estimate, gp.var = NULL)
colnames(cvap2_sex)[1] <- "variable"
cvap2_age <- group.estimate(x = fl_pums_grouped_age, estimate, gp.var = NULL)
colnames(cvap2_age)[1] <- "variable"
cvap2_hispanic <- group.estimate(x = fl_pums_grouped_hispanic, estimate, gp.var = NULL)
colnames(cvap2_hispanic)[1] <- "variable"
cvap2_race <- group.estimate(x = fl_pums_grouped_race, estimate, gp.var = NULL)
colnames(cvap2_race)[1] <- "variable"
# bind estimates together
cvap2 <- rbind(cvap2_sex[3,],cvap2_sex[c(2,1),],cvap2_race[1,],cvap2_hispanic[2,],
               cvap2_race[c(3,2),],cvap2_age[c(1:5),])
cvap2$variable <- as.character(cvap2$variable)
cvap2[c(1,5),1] <- c("total", "hispanic")
rownames(cvap2) <- NULL
# compute 95 percent margin of error
cvap2$MoE <- cvap2$SE*1.96

# calculate proportions -----------------------------------------------------------------
cvap2_prop <- data.frame(variable = cvap2$variable,
                    estimate = cvap2$estimate/cvap2$estimate[1]*100,
                    moe = cvap2$MoE/cvap2$estimate[1]*100)

# save cvap data ------------------------------------------------------------------------
saveRDS(cvap2, "./data/analysis/cvap2")
saveRDS(cvap2_prop, "./data/analysis/cvap2_prop")
rm(fl_pums, fl_pums_grouped, fl_pums_grouped_age, fl_pums_grouped_hispanic,
   fl_pums_grouped_race, fl_pums_grouped_sex, fl_pums2)


#### ESTIMATE VOTING-ELIGIBLE POPULATION (VEP) BY CORRECTING FOR CORRECTIONAL POPULATION=

# collect correctional population data --------------------------------------------------
# from most recent 2017-2018 Annual Report of the Florida Department of Corrections
# these are no estimates but come straight from the Florida prison system
# non-citizen inmate population =  4642
# http://www.dc.state.fl.us/pub/annual/1718/FDC_AR2017-18.pdf
# inmate_pop = prisoners, supervision_pop = parole + probation
correctional_pop <- data.frame(type = c(rep("inmate", 13), rep("supervision", 13)),
                               variable = rep(c("total", "male", "female", "black", "hispanic",
                                            "white", "other", "age_17_under", "age_18_24", 
                                            "age_25_34", "age_35_49", "age_50_59", 
                                            "age_60plus"),2),
                               total = c(96253,89595,6658,45299,11980,38604,370,102,8596,
                                         29445,34772,15674,7664,166157,125524,40633,50957,
                                         28705,85574,921,86,23953,49229,53870,25109,
                                         13910))

# restrict correctional population to 18 years and over ---------------------------------
# official data includes the correctional population of age 17 and under, not included
# is how those 17 and under distribute among males and females and race groups. Those
# 17 under are however no more than 0.07 percent (188) of the total correctional 
# population. They are hence treated as excessive population and removed from sex and
# race groups proportional to group size
correctional_pop$total<- correctional_pop$total - 
  c(correctional_pop$total[8],
    (correctional_pop$total[2] * correctional_pop$total[8] / correctional_pop$total[1]),
    (correctional_pop$total[3] * correctional_pop$total[8] / correctional_pop$total[1]),
    (correctional_pop$total[4] * correctional_pop$total[8] / correctional_pop$total[1]),
    (correctional_pop$total[5] * correctional_pop$total[8] / correctional_pop$total[1]),
    (correctional_pop$total[6] * correctional_pop$total[8] / correctional_pop$total[1]),
    (correctional_pop$total[7] * correctional_pop$total[8] / correctional_pop$total[1]),
    correctional_pop$total[8], 0, 0, 0, 0, 0,
    correctional_pop$total[21],
    (correctional_pop$total[15] * correctional_pop$total[21] / correctional_pop$total[14]),
    (correctional_pop$total[16] * correctional_pop$total[21] / correctional_pop$total[14]),
    (correctional_pop$total[17] * correctional_pop$total[21] / correctional_pop$total[14]),
    (correctional_pop$total[18] * correctional_pop$total[21] / correctional_pop$total[14]),
    (correctional_pop$total[19] * correctional_pop$total[21] / correctional_pop$total[14]),
    (correctional_pop$total[20] * correctional_pop$total[21] / correctional_pop$total[14]),
    correctional_pop$total[21], 0, 0, 0, 0, 0)
correctional_pop <- correctional_pop[-c(8,21),]
rownames(correctional_pop) <- NULL

# adjust cvap by correction for correctional population ---------------------------------
vep <- data.frame(variable = cvap$variable,
                  estimate = cvap$estimate - 
  c((correctional_pop$total[1]+correctional_pop$total[13]),
    (correctional_pop$total[2]+correctional_pop$total[14]),
    (correctional_pop$total[3]+correctional_pop$total[15]),
    (correctional_pop$total[4]+correctional_pop$total[16]),
    (correctional_pop$total[5]+correctional_pop$total[17]),
    (correctional_pop$total[6]+correctional_pop$total[18]),
    (correctional_pop$total[7]+correctional_pop$total[19]),
    (correctional_pop$total[8]+correctional_pop$total[20]),
    (correctional_pop$total[9]+correctional_pop$total[21]),
    (correctional_pop$total[10]+correctional_pop$total[22]),
    (correctional_pop$total[11]+correctional_pop$total[23]),
    (correctional_pop$total[12]+correctional_pop$total[24])),
                  moe  = cvap$moe)
vep2 <- data.frame(variable = cvap2$variable,
                   estimate = cvap2$estimate - 
                    c((correctional_pop$total[1]+correctional_pop$total[13]),
                      (correctional_pop$total[2]+correctional_pop$total[14]),
                      (correctional_pop$total[3]+correctional_pop$total[15]),
                      (correctional_pop$total[4]+correctional_pop$total[16]),
                      (correctional_pop$total[5]+correctional_pop$total[17]),
                      (correctional_pop$total[6]+correctional_pop$total[18]),
                      (correctional_pop$total[7]+correctional_pop$total[19]),
                      (correctional_pop$total[8]+correctional_pop$total[20]),
                      (correctional_pop$total[9]+correctional_pop$total[21]),
                      (correctional_pop$total[10]+correctional_pop$total[22]),
                      (correctional_pop$total[11]+correctional_pop$total[23]),
                      (correctional_pop$total[12]+correctional_pop$total[24])),
                  moe  = cvap2$MoE)

# calculate proportions -----------------------------------------------------------------
vep_prop <- data.frame(variable = vep$variable,
                        estimate = vep$estimate/vep$estimate[1]*100,
                        moe = vep$moe/vep$estimate[1]*100)
vep2_prop <- data.frame(variable = vep2$variable,
                        estimate = vep2$estimate/vep2$estimate[1]*100,
                        moe = vep2$moe/vep2$estimate[1]*100)

# save vep data ------------------------------------------------------------------------
saveRDS(vep, "./data/analysis/vep")
saveRDS(vep_prop, "./data/analysis/vep_prop")
saveRDS(vep2, "./data/analysis/vep2")
saveRDS(vep2_prop, "./data/analysis/vep2_prop")
rm(vep,vep_prop,vep2,vep2_prop)


#### ESTIMATE REGISTERED-VOTER POPULATION (RVP) WITH VOTER FILE DATA ====================

# import voter record and draw large random sample --------------------------------------
# import original voter record (October 2017)
impMatchBind(folder_a = "./data/sample/",
             folder_b = "./data/voter-files/",
             bind_record = TRUE)
rm(sample)
# draw sample from original voter record
record_original_sample <- sample_n(tbl = records, size = 100000, replace = FALSE)
rm(records)

# format sample of voter record ---------------------------------------------------------
# format sex variable
record_original_sample$gender <- ifelse(record_original_sample$gender == "F", "female",
                                  ifelse(record_original_sample$gender == "M", "male",
                                         NA))
# remove missings
record_original_sample <- record_original_sample[-which(is.na(
                                                        record_original_sample$gender)),]
# format age variable
record_original_sample$birth <- record_original_sample$birth %>%
  str_replace_all("/", "-") %>%
  mdy
# remove missings
record_original_sample <- record_original_sample[-which(is.na(
  record_original_sample$birth)),]
# age until mid 2017
record_original_sample$age <- age_calc(dob = record_original_sample$birth,
                                       enddate = mdy("06-01-2017"),
                                       units = "years",
                                       precise = TRUE)
# add age groups
record_original_sample$age_group <- ifelse(record_original_sample$age < 18, "age_17_under",
                                     ifelse(record_original_sample$age >= 18 & record_original_sample$age < 25, "age_18_24",
                                      ifelse(record_original_sample$age >= 25 & record_original_sample$age < 35, "age_25_34",
                                       ifelse(record_original_sample$age >= 35 & record_original_sample$age < 50, "age_35_49",
                                        ifelse(record_original_sample$age >= 50 & record_original_sample$age < 60, "age_50_59",
                                         ifelse(record_original_sample$age >= 60, "age_60plus", NA))))))
# format race variable
record_original_sample$race <- ifelse(record_original_sample$race == 1, "other",
                                ifelse(record_original_sample$race == 2, "other",
                                 ifelse(record_original_sample$race == 3, "black",
                                  ifelse(record_original_sample$race == 4, "hispanic",
                                   ifelse(record_original_sample$race == 5, "white",
                                    ifelse(record_original_sample$race == 6, "other",
                                     ifelse(record_original_sample$race == 7, "other", NA)))))))
# remove missings
record_original_sample <- record_original_sample[-which(is.na(
  record_original_sample$race)),]
# format party variable
record_original_sample$party <- ifelse(record_original_sample$party == "DEM", "dem",
                                 ifelse(record_original_sample$party == "REP", "rep",
                                  ifelse(record_original_sample$party == "NPA", "npa",
                                         "other")))
# format email variable
record_original_sample$email <- ifelse(is.na(record_original_sample$email), FALSE, TRUE)
# format registration date
record_original_sample$regist <- record_original_sample$regist %>% 
  str_replace_all("\\/", "-") %>% mdy()
# add voting eligibility variable
eligibility_age <- data.frame(gen_2006 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("11/07/2006") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2008 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("11/04/2008") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2010 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("11/02/2010") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2012 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("11/06/2012") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2014 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("11/04/2014") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2016 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("11/08/2016") - 29),
                                                  units = "years", precise = TRUE),
                              gen_2018 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("11/06/2018") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2006 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("09/05/2006") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2008 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("08/26/2008") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2010 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("08/24/2010") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2012 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("08/14/2012") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2014 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("08/26/2014") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2016 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("08/30/2016") - 29),
                                                  units = "years", precise = TRUE),
                              pri_2018 = age_calc(dob = record_original_sample$birth, 
                                                  enddate = (mdy("08/28/2018") - 29),
                                                  units = "years", precise = TRUE))
eligibility_reg <- data.frame(gen_2006 = (record_original_sample$regist - 
                                            mdy("11/07/2006")) <= 0,
                              gen_2008 = (record_original_sample$regist - 
                                            mdy("11/04/2008")) <= 0,
                              gen_2010 = (record_original_sample$regist - 
                                            mdy("11/02/2010")) <= 0,
                              gen_2012 = (record_original_sample$regist - 
                                            mdy("11/06/2012")) <= 0,
                              gen_2014 = (record_original_sample$regist - 
                                            mdy("11/04/2014")) <= 0,
                              gen_2016 = (record_original_sample$regist - 
                                            mdy("11/08/2016")) <= 0,
                              gen_2018 = (record_original_sample$regist - 
                                            mdy("11/06/2018")) <= 0,
                              pri_2006 = (record_original_sample$regist - 
                                            mdy("09/05/2006")) <= 0,
                              pri_2008 = (record_original_sample$regist - 
                                            mdy("08/26/2008")) <= 0,
                              pri_2010 = (record_original_sample$regist - 
                                            mdy("08/24/2010")) <= 0,
                              pri_2012 = (record_original_sample$regist - 
                                            mdy("08/14/2012")) <= 0,
                              pri_2014 = (record_original_sample$regist - 
                                            mdy("08/26/2014")) <= 0,
                              pri_2016 = (record_original_sample$regist - 
                                            mdy("08/30/2016")) <= 0,
                              pri_2018 = (record_original_sample$regist - 
                                            mdy("08/28/2018")) <= 0)
record_original_sample <- record_original_sample %>%
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
rm(eligibility_age,eligibility_reg)

# subset sample of voter record ---------------------------------------------------------
record_original_sample <- select(record_original_sample, id, county, congdist, gender,
                                 race, party, age_group, status, email, 
                                 eligible_gen_2006:eligible_pri_2018)

# collect turnout histories for sample of voter record ----------------------------------
record_original_sample <- record_original_sample %>% 
  mutate(gen2006 = NA, gen2008 = NA, gen2010 = NA, gen2012 = NA, gen2014 = NA, 
         gen2016 = NA, gen2018 = NA, pri2006 = NA, pri2008 = NA, pri2010 = NA, 
         pri2012 = NA, pri2014 = NA, pri2016 = NA, pri2018 = NA)
turnout_1 <- voteHist(histpath = "./data/voter-histories/", 
                             sample = record_original_sample)
turnout_2 <- voteHist(histpath = "./data/voter-histories-updated//", 
                             sample = record_original_sample)
record_original_sample <- cbind(record_original_sample,
                                select(turnout_1, gen2006:gen2016),
                                select(turnout_2, gen2018), 
                                select(turnout_1, pri2006:pri2016),
                                select(turnout_2, pri2018))
record_original_sample <- record_original_sample[,-c(24:37)]
rm(turnout_1,turnout_2)
saveRDS(record_original_sample, "./data/analysis/record_sample")

# add updated 2018 registry data --------------------------------------------------------
impMatchBind(folder_a = "./data/sample/", 
             folder_b = "./data/voter-files-updated/",
             bind_record = TRUE)
record_original_sample$reg2018 <- ifelse(record_original_sample$id %in% records$id, TRUE, 
                                         FALSE)
record_original_sample$status2018 <- records$status[match(record_original_sample$id, 
                                                          records$id)]
# if someone is not registered 2018 make ineligible
record_original_sample$eligible_gen_2018 <- ifelse(record_original_sample$reg2018 == FALSE,
                                             FALSE, record_original_sample$eligible_gen_2018)
record_original_sample$eligible_pri_2018 <- ifelse(record_original_sample$reg2018 == FALSE,
                                             FALSE, record_original_sample$eligible_pri_2018)
rm(records,sample)

# calculate turnout rate ----------------------------------------------------------------
# melt turnout
turnout <- melt(data = dplyr::select(record_original_sample, id, gen2006:pri2018),
                id.vars = "id", variable.name = "election", value.name = "turnout")
# melt eligibility
eligibility <- melt(data = dplyr::select(record_original_sample, id, 
                                  eligible_gen_2006:eligible_pri_2018), 
                    id.vars = "id", variable.name = "election", 
                    value.name = "eligible")
# format election variable
eligibility$election <- str_remove_all(eligibility$election, "^eligible_|_")
# join long format turnout and eligibility
record_sample_turnout <- left_join(x = turnout, y = eligibility, by = c("id", "election"))
record_sample_turnout$turnout <- ifelse(is.na(record_sample_turnout$turnout), 0, 1)
record_sample_turnout$eligible <- ifelse(record_sample_turnout$eligible == TRUE, 1, 0)
record_sample_turnout <- left_join(record_sample_turnout, 
                                   record_original_sample[,c("id","status","reg2018",
                                                             "status2018")], 
                                   by = "id")
# summarize individual turnout
turnout_rate <- record_sample_turnout[-which(record_sample_turnout$eligible == 0),] %>% 
  group_by(id) %>% 
  summarize(voted = sum(turnout), observed = n())
turnout_rate$rate <- turnout_rate$voted/turnout_rate$observed
turnout_rate$group <- ifelse(turnout_rate$rate <= 0.25, "0.25",
                       ifelse(turnout_rate$rate > 0.25 & turnout_rate$rate <= 0.5, "0.5",
                        ifelse(turnout_rate$rate > 0.5 & turnout_rate$rate <= 0.75, "0.75",
                         ifelse(turnout_rate$rate > 0.75 & turnout_rate$rate <= 1, "1", NA))))

# calculate turnout at 2016 and 2018 elections ------------------------------------------
# a = all, b = INA voters excluded (spot on when excluding INA voters, otherwise still close)
turnout_gen_2016a <- record_sample_turnout[which(record_sample_turnout$election == "gen2016" & 
                              record_sample_turnout$eligible == 1),]
turnout_pri_2016a <- record_sample_turnout[which(record_sample_turnout$election == "pri2016" & 
                                              record_sample_turnout$eligible == 1),]
turnout_gen_2018a <- record_sample_turnout[which(record_sample_turnout$election == "gen2018" & 
                                                  record_sample_turnout$eligible == 1),]
turnout_pri_2018a <- record_sample_turnout[which(record_sample_turnout$election == "pri2018" & 
                                                  record_sample_turnout$eligible == 1),]
turnout_gen_2016b <- record_sample_turnout[which(record_sample_turnout$election == "gen2016" & 
                                                  record_sample_turnout$eligible == 1 &
                                                   record_sample_turnout$status == "ACT"),]
turnout_pri_2016b <- record_sample_turnout[which(record_sample_turnout$election == "pri2016" & 
                                                  record_sample_turnout$eligible == 1 &
                                                   record_sample_turnout$status == "ACT"),]
turnout_gen_2018b <- record_sample_turnout[which(record_sample_turnout$election == "gen2018" & 
                                                  record_sample_turnout$eligible == 1 &
                                                   record_sample_turnout$reg2018 == TRUE &
                                                   record_sample_turnout$status2018 == "ACT"),]
turnout_pri_2018b <- record_sample_turnout[which(record_sample_turnout$election == "pri2018" & 
                                                  record_sample_turnout$eligible == 1 &
                                                   record_sample_turnout$reg2018 == TRUE &
                                                   record_sample_turnout$status2018 == "ACT"),]

# compile rvp data ----------------------------------------------------------------------
rvp_prop <- data.frame(variable = c("male", "female", "black", "hispanic", "white", 
                               "other", "age_17_under", "age_18_24", "age_25_34", 
                               "age_35_49", "age_50_59", "age_60plus","democrat",
                               "republican", "other", "npa", "to_0_25", "to_26_49",
                               "to_50_74", "to_75_100", "to_pri_2016", 
                               "to_gen_2016", "to_pri_2018", "to_gen_2018"),
                  estimate = c(length(which(record_original_sample$gender == "male"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$gender == "female"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$race == "black"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$race == "hispanic"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$race == "white"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$race == "other"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$age_group == "age_17_under"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$age_group == "age_18_24"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$age_group == "age_25_34"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$age_group == "age_35_49"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$age_group == "age_50_59"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$age_group == "age_60plus"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$party == "dem"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$party == "rep"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$party == "other"))/
                                 dim(record_original_sample)[1],
                               length(which(record_original_sample$party == "npa"))/
                                 dim(record_original_sample)[1],
                               (prop.table(table(turnout_rate$group)))[1],
                               (prop.table(table(turnout_rate$group)))[2],
                               (prop.table(table(turnout_rate$group)))[3],
                               (prop.table(table(turnout_rate$group)))[4],
                               mean(turnout_pri_2016a$turnout),
                               mean(turnout_gen_2016a$turnout),
                               mean(turnout_pri_2018a$turnout),
                               mean(turnout_gen_2018a$turnout)))
# calculate margins of error
rvp_prop$moe <- c(sqrt(rvp_prop$estimate[1]*(1-rvp_prop$estimate[1])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[2]*(1-rvp_prop$estimate[2])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[3]*(1-rvp_prop$estimate[3])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[4]*(1-rvp_prop$estimate[4])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[5]*(1-rvp_prop$estimate[5])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[6]*(1-rvp_prop$estimate[6])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[7]*(1-rvp_prop$estimate[7])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[8]*(1-rvp_prop$estimate[8])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[9]*(1-rvp_prop$estimate[9])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[10]*(1-rvp_prop$estimate[10])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[11]*(1-rvp_prop$estimate[11])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[12]*(1-rvp_prop$estimate[12])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[13]*(1-rvp_prop$estimate[13])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[14]*(1-rvp_prop$estimate[14])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[15]*(1-rvp_prop$estimate[15])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[16]*(1-rvp_prop$estimate[16])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[17]*(1-rvp_prop$estimate[17])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[18]*(1-rvp_prop$estimate[18])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[19]*(1-rvp_prop$estimate[19])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[20]*(1-rvp_prop$estimate[20])/dim(record_original_sample)[1])*1.96,
                  sqrt(rvp_prop$estimate[21]*(1-rvp_prop$estimate[1])/dim(turnout_pri_2016a)[1])*1.96,
                  sqrt(rvp_prop$estimate[22]*(1-rvp_prop$estimate[1])/dim(turnout_gen_2016a)[1])*1.96,
                  sqrt(rvp_prop$estimate[23]*(1-rvp_prop$estimate[1])/dim(turnout_pri_2018a)[1])*1.96,
                  sqrt(rvp_prop$estimate[24]*(1-rvp_prop$estimate[1])/dim(turnout_gen_2018a)[1])*1.96)
rvp_prop[,c(2,3)] <- rvp_prop[,c(2,3)]*100

# save rvp data -------------------------------------------------------------------------
saveRDS(rvp_prop,"./data/analysis/rvp_prop")


#### ESTIMATE PER CAPITA INCOME DISTRIBUTION OF CITIZEN-VOTING AGE POPULATION ===========

# collect per-capita income for all block groups ----------------------------------------
county_fips <- c("001","003","005","007","009","011","013","015","017","019","021","023",
                 "027","029","031","033","035","037","039","041","043","045","047","049",
                 "051","053","055","057","059","061","063","065","067","069","071","073",
                 "075","077","079","081","083","085","086","087","089","091","093","095",
                 "097","099","101","103","105","107","109","111","113","115","117","119",
                 "121","123","125","127","129","131","133")
iter <- 1
for (i in 1:length(county_fips)) {
  cat(i,"\n")
  censusdata <- getCensus(name = "acs/acs5", vintage = 2017, key = census_api_key, 
                          region = "block group:*",
                          regionin = str_c("state:12+county:", 
                                           county_fips[i]),
                          vars = c("B19301_001E"))
  if (iter == 1) {
    income <- censusdata
  } else {
    income <- rbind(income,censusdata)
  }
  iter <- iter + 1
  rm(censusdata)
}
# generate county-tract-blockgroup id
income$id <- str_c(income$county, income$tract, income$block)

# collect citizen-voting age population totals for all block groups ---------------------
cvap_blockgr <- read.csv("./data/auxiliary/cvap-blockgr-acs5-2017.csv")
cvap_blockgr <- cvap_blockgr[str_detect(cvap_blockgr$GEONAME, "Florida"),]
cvap_blockgr <- cvap_blockgr[cvap_blockgr$lntitle == "Total",]
# generate county-tract-blockgroup id
cvap_blockgr$id <- str_extract(cvap_blockgr$geoid, "[[:digit:]]{10}$")

# map per capita income to block groups of cvap and expand to population ----------------
pci_cvap <- left_join(cvap_blockgr[,c("id","CVAP_EST")], income[,c("B19301_001E", "id")],
                      by = "id")
pci_cvap <- filter(pci_cvap, B19301_001E >= 0 & !is.na(B19301_001E) &
                             CVAP_EST > 0)
expanded_pci_cvap <-  data.frame(id = rep(pci_cvap$id, pci_cvap$CVAP_EST),
                                B19301_001E = rep(pci_cvap$B19301_001E, pci_cvap$CVAP_EST))
expanded_pci_cvap$pci_groups <- ifelse(expanded_pci_cvap$B19301_001E <= 15000, "to_15",
                           ifelse(expanded_pci_cvap$B19301_001E > 15000 & 
                                    expanded_pci_cvap$B19301_001E <= 30000, "15_30",
                                  ifelse(expanded_pci_cvap$B19301_001E > 30000 & 
                                           expanded_pci_cvap$B19301_001E <= 50000, "30_50",
                                         ifelse(expanded_pci_cvap$B19301_001E > 50000 & 
                                                  expanded_pci_cvap$B19301_001E <= 75000, "50_75",
                                                ifelse(expanded_pci_cvap$B19301_001E > 75000, "75_plus", 
                                                       NA)))))
pci_cvap <- as.data.frame(prop.table(table(expanded_pci_cvap$pci_groups)))
saveRDS(pci_cvap, "./data/analysis/pci_cvap")
