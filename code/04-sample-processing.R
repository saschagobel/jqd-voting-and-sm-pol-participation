# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Göbel
# Sampling script
# April 2019
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./data/sample/*'
'./data/voter-files/*'
'./data/voter-histories/*'
'./data/voter-files-updated/*'
'./data/voter-histories-updated/*'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/sample-original'
'./data/sample-processed'
'./data/analysis/sp_prop'
'./data/analysis/sp_prop2'
'./data/analysis/sp_prop3'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 38 - PREPARATIONS
Line 58 - CLEAN UP SAMPLE
Line 167 - UPDATE AS OF 2018 MIDTERMS
Line 189 - ADD VOTING HISTORY INFORMATION
Line 229 - CLEAN FURTHER (FORMATING ETC.)
Line 654 - COMPUTE SAMPLE POPULATION (SP) CHARACTERISTICS
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
# import twitter and record data, match, and bind together
impMatchBind(folder_a = "./data/sample/", 
             folder_b = "./data/voter-files/",
             bind_record = FALSE)


#### CLEAN UP SAMPLE ====================================================================

# remove NA ids -------------------------------------------------------------------------
sample <- sample[!is.na(sample$id),]

# remove twitter ids that are systematic mismatches to the voter file -------------------
which(table(sample$t_handle) > 20)

# users auburn72md, David_Krause22, jenpayne76 are systematic mismatches
sample <- sample[-which(sample$t_handle %in% c("auburn72md", "David_Krause22", 
                                               "jenpayne76")),]

# manually sort out and remove voter ids, for which handles occur more frequently
# than 4 times usually family members that passed the same e-mail address ---------------
sample <- sample[-which(sample$id %in% c(108333899, 105441948, 122020808, 115480261, 
  122879372, 115770313, 121208467, 115577849, 104883489, 121680958, 115628402,
  110537519, 106643891, 122745775, 106566831, 124746708, 115801595, 121984931, 
  104981927, 105129951, 105600794, 108326523, 122969185, 113056208, 121873075, 
  110854033, 123967171, 113748818, 122157254, 124286282, 124287145, 115725722,
  114592033, 119348897, 119353379, 120004028, 107861893, 120392761, 115137258,
  100366009, 115699151, 113942622, 105420037, 114893101, 106106794, 110706237, 
  111077743, 123357699, 115162332, 108217796, 122258089, 124303397, 108319709,
  108313472, 117611922, 124138580, 112403229, 121616009, 119120529, 115435004,
  123801179, 113454115, 113560256, 124131772, 106650382, 118769611, 117561782,
  124081160, 108307835, 114720494, 106613341, 119268528, 123021518, 105184111,
  118360650, 121035658, 119017527, 123822223, 104972841, 108305505, 123473231, 
  113697312, 122424870, 113556804, 123348892, 114644203, 109804500, 109537439, 
  121454420, 105617480, 116173322, 121274761, 104851160, 122646508, 113046255,
  121442256, 113714875, 122324299, 114060590, 110197347, 124394420, 121368502,
  108305929, 121531158, 122394520, 124152326, 121453357, 102009151, 124553245,
  119845360, 123254979, 124346977, 104065523, 122987540, 104088359, 123078308,
  106414248, 123019444, 115362503, 110538346, 108334768, 123453772, 115751217,
  106195305, 123974263, 123861065, 103035837, 104827130, 122894084, 115945331, 
  118357888, 117060935, 103973514, 103927531, 119166770, 107851992, 125063922, 
  104086606, 103965798, 117482323, 124279709, 113802988, 122583865, 124034212, 
  101849458, 104697245, 120093695, 122567528, 124506738, 106165149, 124997630, 
  106306077, 111874452, 123205514, 121029490, 123189475, 122573141, 106216458, 
  106216456, 124943993, 116899740, 123893532, 113557831, 121213592, 113529377,
  121204057, 109044439, 115709574, 111544977, 113621702, 119440287, 123640385, 
  116281828, 123600715, 110006994, 109334777, 109158901, 109729718, 114711393, 
  104155386, 122474625, 109668525, 109410316, 115850683, 120780476, 104076383, 
  104089542, 121377925, 124303260, 122160636, 119360784, 116485560, 120026995, 
  120053330, 110872389, 118843332, 124107172, 118719818, 113627601, 113582142,
  111822923, 124611236, 111757624, 111414539, 124081668, 111425141, 111552432, 
  111628689, 123498835, 112872632, 105052470, 101041146, 105009345, 103906791, 
  110269067, 102076836, 102235286, 110431271, 113771764, 114754634, 124250613,
  123026669, 118369214, 106568434, 110776291, 113596372, 117503223, 113770187, 
  113821885, 122071598, 123208174, 113817713, 113796656, 113681106, 113696785,
  119486002, 123465357, 113638423, 123474281, 118714158, 123600247, 109283428, 
  103300059, 103401215, 124246528, 108943557, 123844442, 124113738, 106978453, 
  120399746, 106935539, 106326985, 106548578, 115779366, 123222822, 117774025,
  101952731, 101593725, 115501823, 111119462, 115983620, 102423235, 108332596,
  113223119, 122645794, 123662494, 113749128, 123786506, 118619297, 122708417, 
  107772073, 108312101, 118877109, 116394835, 109918171, 112947550, 124048806, 
  122636919, 117871044, 112799426, 119533835, 109128846, 109128845, 102342717, 
  114243792, 102319762, 119073999, 121979376, 122236706, 106542485, 110692141, 
  107929975, 106627918, 106087529, 119274656, 119274847, 121463603, 108304523,
  119300740, 119156520, 114975147, 123586099, 119755938, 118870478, 117099591,
  118880062, 117237205, 108299014, 122139539, 122139464, 113428470, 123867056, 
  123867049, 113415537, 123491854, 108334087, 113293928, 113473946, 113427513, 
  112251399, 111848267, 117837779, 123457659, 119282974, 104868801, 122047561, 
  106397410, 106482498, 106412289, 106417702, 123006561, 120040803, 108906401, 
  121094900, 108321416, 113649790, 115274507, 114781739, 106034744, 107714664,
  113092786, 120206982, 124319917, 124559344, 114778018, 111830361, 124582272, 
  116540949, 102635265, 117719597, 116427403, 118430147, 106597937, 106329345, 
  106469353, 106369972, 108340865, 116324634, 108294767, 116324634, 108294767, 
  116324634, 108294767, 123854138, 103459524, 122284945, 123413251, 118975116,
  121204787, 124767021, 124326848, 106230755, 115767903, 113153024, 121433079, 
  121808211, 122561337, 106535796, 120313850, 106553617, 106553619, 121637354,
  123196319, 103914898, 104131111, 107582036, 106566706, 105691831, 105691097,
  110704762, 122268920, 105624798, 414433910, 123055816, 109877667, 102150142,
  122999136, 124555791, 117788972, 124084043, 124676692, 123590445)),]
# n = 108927

# remove duplicate entries of registered voters -----------------------------------------
# transform register date to class Date variable
sample$regist <- sample$regist %>% str_replace_all("\\/", "-") %>% mdy()
# sort data by register date in ascending order
sample <- arrange(sample, regist)
# remove duplicate entries (record ID) keeping the most recent date of registry
sample <- sample[!duplicated(sample$id, fromLast = TRUE),]

# manage remaining duplicate twitter handles --------------------------------------------
# identify subset of the sample which consists of duplicate twitter handles
sub_1 <- sample[which(duplicated(sample$t_ids, fromLast = FALSE) | 
                        duplicated(sample$t_ids, fromLast = TRUE)),]
# identify matches in full twitter and registry name
sub_1$cmatch <- ifelse(str_trim(tolower(sub_1$t_names)) == 
                         str_trim(tolower(str_c(sub_1$forename, " ", sub_1$surname))), 
                       TRUE, FALSE)
# remove matches from sub_1, they must remain within sample
sub_1 <- sub_1[!sub_1$cmatch == TRUE,]
# identify those which are still duplicated and those which are not duplicated anymore 
# (mismatched)
sub_2 <- sub_1[which(duplicated(sub_1$t_ids, fromLast = FALSE) | 
                       duplicated(sub_1$t_ids, fromLast = TRUE)),]
# remove mismatches from sample
sample <- sample[-which(sample$id %in% sub_1[which(sub_1$id %ni% sub_2$id),"id"]),]

# remove remaining duplicates from sample
sample <- sample[-which(sample$id %in% sub_2$id),]


#### UPDATE AS OF 2018 MIDTERMS =========================================================

# import updated registry data ----------------------------------------------------------
sample_original <- sample
impMatchBind(folder_a = "./data/sample/", 
             folder_b = "./data/voter-files-updated//",
             bind_record = TRUE)
rm(sample)

# assign registration in 2018 -----------------------------------------------------------
sample_original$reg2018 <- ifelse(sample_original$id %in% records$id, TRUE, FALSE)
# how many in sample not registered 2018
which(!(sample_original$reg2018)) %>% length

# assign registration status in 2018 ----------------------------------------------------
sample_original$status2018 <- records$status[match(sample_original$id, records$id)]
# how many in sample not registered 2018
which(sample_original$status2018 == "INA") %>% length
rm(records)


#### ADD VOTING HISTORY INFORMATION =====================================================

# add election columns to sample data frame ---------------------------------------------
sample_original <- sample_original %>% mutate(gen2006 = NA, gen2008 = NA, gen2010 = NA, 
                                              gen2012 = NA, gen2014 = NA, gen2016 = NA, 
                                              gen2018 = NA, pri2006 = NA, pri2008 = NA, 
                                              pri2010 = NA, pri2012 = NA, pri2014 = NA, 
                                              pri2016 = NA, pri2018 = NA)

# collect turnout history in general and primary elections from 2006 to 2016 ------------
sample_original2 <- voteHist(histpath = "./data/voter-histories/", 
                             sample = sample_original)

# collect turnout history in general and primary elections in 2018 ----------------------
sample_original3 <- voteHist(histpath = "./data/voter-histories-updated//", 
                             sample = sample_original)

# bind turnout histories to sample ------------------------------------------------------
sample_original <- cbind(select(sample_original, Name:statuscomb),
                         select(sample_original2, gen2006:gen2016),
                         select(sample_original3, gen2018), 
                         select(sample_original2, pri2006:pri2016),
                         select(sample_original3, pri2018))
rm(sample_original2,sample_original3)
saveRDS(sample_original, "./data/analysis/sample_original")


#### CLEAN FURTHER (FORMATING ETC.) =====================================================

# import sample_original ----------------------------------------------------------------
sample_processed <- readRDS("./data/analysis/sample_original")

# drop unneeded variables and rename ----------------------------------------------------
colnames(sample_processed)[c(1,2,4,5,6,8,12,17,19,20,23,24,32,33,35:48)] <- c("last_name", 
                            "first_name", "twitter_handle", "twitter_name", "twitter_id", 
                            "voter_id", "middle_name", "sex", "birth_date", 
                            "registration_date", "registration_status", 
                            "congressional_district", "registered_2018", 
                            "registration_status_2018", "voted_gen_2006", 
                            "voted_gen_2008", "voted_gen_2010", "voted_gen_2012", 
                            "voted_gen_2014", "voted_gen_2016", "voted_gen_2018", 
                            "voted_pri_2006", "voted_pri_2008", "voted_pri_2010", 
                            "voted_pri_2012", "voted_pri_2014", "voted_pri_2016", 
                            "voted_pri_2018")
sample_processed <- select(sample_original, voter_id, twitter_id, twitter_handle, 
                           twitter_name, first_name, middle_name, last_name, county, 
                           congressional_district, precinct, sex, birth_date, race, 
                           party, email, areacode, phonenum, registration_date, 
                           registration_status, registered_2018, 
                           registration_status_2018, voted_gen_2006:voted_pri_2018)

# join geolocation and census data (income) ---------------------------------------------
sample_geo <- readRDS("./data/auxiliary/sample_geo")
sample_geo <- select(sample_geo, id, place, street, zip, lon, lat, censusblock, county, 
                     income_pci, income_household)
colnames(sample_geo)[c(1,8)] <- c("voter_id", "county_fips")
sample_processed <- left_join(x = sample_processed, y = sample_geo, by = "voter_id")
sample_processed <- select(sample_processed, voter_id:county, county_fips,
                           congressional_district, precinct, place, zip, street, 
                           censusblock, lon, lat, sex:party, income_pci, 
                           income_household, email:voted_pri_2018)

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
# extract missings
missing_sex <- sample_processed[which(is.na(sample_processed$sex)), 
                                c("voter_id","first_name","birth_date")]
# extract year from birth date
missing_sex$birth_date <- missing_sex$birth_date %>% 
  str_extract("[[:digit:]]{4}")
# build key
missing_sex$key <- str_c(missing_sex$first_name, missing_sex$birth_date)

# predict gender using first names and year of birth 
predicted_sex <- gender_df(data = missing_sex, name_col = "first_name", 
                           year_col = "birth_date", method = "ssa")
# build key
predicted_sex$key <- str_c(predicted_sex$name, predicted_sex$year_min)
# join prediction with missings
missing_sex <- left_join(x = missing_sex, y = predicted_sex[,c("gender", "key")], 
                         by = "key")
# predict gender for remaining missings using first names 
predicted_sex <- gender(missing_sex[which(is.na(missing_sex$gender)),"first_name"], 
                     method = "genderize")
# add key
predicted_sex$voter_id <- missing_sex[which(is.na(missing_sex$gender)),]$voter_id
# remove missings
predicted_sex <- na.omit(predicted_sex)
# join prediction with missings
missing_sex <- left_join(x = missing_sex, y = predicted_sex[,c("voter_id", "gender")], 
                         by = "voter_id")
missing_sex$sex <- ifelse(is.na(missing_sex$gender.x), missing_sex$gender.y,
                          missing_sex$gender.x)
# fill remaining missings manually by looking them up in the data and researching online
missing_sex$sex[which(is.na(missing_sex$sex))] <- c("f","m","m","m","m","f","f","f","f",          
    "f","f","f","f","f","m","m","m","f", "f","m","m","f","f","f","f","f","f",
    "m","f","m","f","f","f","m","m","m","m","m","f","m","m","m","m","f","m","m","m","f","m",
    "f","m","f","m","f","m","f","m","m","m","f","m","f","f","f","f","f","m","m","m","f","m",
    "f","m","f","m","m","m","f","f","f","m","f","m","m","f","f","f","m","m","f","f","f","f",
    "m","m","f","f","m","f","f","m","m","f","f","m","m","f","f","m","m","m","f","f","f","m",
    "m","m","f","m","m","f","f","f","f","f","f","f","f","f","m","f","m","m","f","m","m","m",
    "m","f","f","f","m","f","f","f","m","m","f")
missing_sex$sex <- ifelse(missing_sex$sex == "f", "female",
                          ifelse(missing_sex$sex == "m", "male", missing_sex$sex))
colnames(missing_sex)[7] <- "sex_fill"
sample_processed <- left_join(x = sample_processed, 
                              y = missing_sex[,c("voter_id","sex_fill")], 
                              by = "voter_id")
sample_processed$sex <- ifelse(is.na(sample_processed$sex), sample_processed$sex_fill,
                               sample_processed$sex)

# format birth_date variable and compute age --------------------------------------------
sample_processed$birth_date <- sample_processed$birth_date %>%
  str_replace_all("/", "-") %>%
  mdy
# age until February 2019
sample_processed$age <- age_calc(dob = sample_processed$birth_date,
                                 enddate = Sys.Date(),
                                 units = "years",
                                 precise = TRUE)
sample_processed <- select(sample_processed, voter_id:birth_date, age, 
                           race:voted_pri_2018)

# format ethnicity variable and fill missing values -------------------------------------
# assign API key for US census bureau API
census_api_key <- "API KEY"
# extract missings
missing_race <- sample_processed[which(sample_processed$race == 9), 
                                 c("voter_id","last_name","county_fips","censusblock",
                                   "sex","age","party")]
# format
missing_race$state <- "FL"
missing_race$tract <- str_extract(missing_race$censusblock, 
                                  "(?<=^[[:digit:]]{5})[[:digit:]]{6}")
missing_race$sex <- as.numeric(ifelse(missing_race$sex == "female", 1, 0))
missing_race$party <- as.numeric(ifelse(missing_race$party == "DEM", 1,
                                        ifelse(missing_race$party == "REP", 2,
                                               0)))
missing_race$age <- round(missing_race$age)
missing_race <- select(missing_race, voter_id, last_name, state, county_fips, tract,
                       age, sex, party)
colnames(missing_race)[c(2,4)] <- c("surname", "county")
# predict race for missings using Bayesian race prediction in Imai and Khanna (2015)
predicted_race <- predict_race(voter.file = missing_race, census.geo = "tract", 
                               census.key = census_api_key, age = TRUE, sex = TRUE, 
                               party = "party")
rm(missing_race)
# remove failed predictions
predicted_race <- predicted_race[-which(is.na(predicted_race$pred.whi)),]
# extract race predictions
predicted_race$race <- predicted_race$race <- colnames(predicted_race[, 10:14])[unlist(
  apply(predicted_race[, 10:14], 1, which.max))]
predicted_race$race <- ifelse(predicted_race$race == "pred.whi", 5,
                          ifelse(predicted_race$race == "pred.his", 4,
                               ifelse(predicted_race$race == "pred.asi", 2,
                                      ifelse(predicted_race$race == "pred.bla", 3,
                                  ifelse(predicted_race$race == "pred.oth", 6, NA)))))
predicted_race <- select(predicted_race, voter_id, race)
colnames(predicted_race)[2] <- "race_fill"
sample_processed <- left_join(x = sample_processed, 
                              y = predicted_race[,c("voter_id","race_fill")], 
                              by = "voter_id")
sample_processed$race <- ifelse(sample_processed$race == 9, sample_processed$race_fill,
                               sample_processed$race)
sample_processed$race[95860] <- 5
# format race
sample_processed$race <- ifelse(sample_processed$race == 1, "native",
                            ifelse(sample_processed$race == 2, "asian",
                               ifelse(sample_processed$race == 3, "black",
                                  ifelse(sample_processed$race == 4, "hispanic",
                                     ifelse(sample_processed$race == 5, "white",
                                       ifelse(sample_processed$race == 6, "other",
                                ifelse(sample_processed$race == 7, "multi", NA)))))))
sample_processed <- select(sample_processed, -race_fill)

# format party variable -----------------------------------------------------------------
sample_processed$party <- ifelse(sample_processed$party == "DEM", "dem",
                                 ifelse(sample_processed$party == "REP", "rep",
                                        ifelse(sample_processed$party == "NPA", "npa",
                                               "other")))

# format phone variable -----------------------------------------------------------------
sample_processed$phone <- str_c("1", sample_processed$areacode, 
                                sample_processed$phonenum)
sample_processed <- select(sample_processed, voter_id:email, phone, 
                           registration_date:voted_pri_2018)

# update registration status variable ---------------------------------------------------
sample_processed$registration_status <- ifelse(sample_processed$registration_status == "INA" &
                                                 sample_processed$registration_status_2018 == "ACT",
                                               "ACT", sample_processed$registration_status)

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
# assign eligibility at specific election
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
sample_processed$eligible_gen_2018 <- ifelse(sample_processed$registered_2018 == FALSE,
                                             FALSE, sample_processed$eligible_gen_2018)
sample_processed$eligible_pri_2018 <- ifelse(sample_processed$registered_2018 == FALSE,
                                             FALSE, sample_processed$eligible_pri_2018)

# format voted variables ----------------------------------------------------------------
# replace NA with FALSE
sample_processed <- sample_processed %>% 
  replace_na(list(voted_gen_2006 = FALSE, voted_gen_2008 = FALSE, voted_gen_2010 = FALSE,
                  voted_gen_2012 = FALSE, voted_gen_2014 = FALSE, voted_gen_2016 = FALSE,
                  voted_gen_2018 = FALSE, voted_pri_2006 = FALSE, voted_pri_2008 = FALSE,
                  voted_pri_2010 = FALSE, voted_pri_2012 = FALSE, voted_pri_2014 = FALSE,
                  voted_pri_2016 = FALSE, voted_pri_2018 = FALSE))

# correct registration status variable ---------------------------------------------------
sample_processed[which(sample_processed$voted_gen_2016 == TRUE & 
                         sample_processed$registration_status == "INA"),
                 "registration_status"] <- "ACT"
sample_processed[which(sample_processed$voted_pri_2016 == TRUE & 
                         sample_processed$registration_status == "INA"),
                 "registration_status"] <- "ACT"
sample_processed[which(sample_processed$voted_gen_2018 == TRUE & 
                         sample_processed$registration_status_2018 == "INA"),
                 "registration_status_2018"] <- "ACT"
sample_processed[which(sample_processed$voted_pri_2018 == TRUE & 
                         sample_processed$registration_status_2018 == "INA"),
                 "registration_status_2018"] <- "ACT"
ina_voters <- sample_processed[which(sample_processed$registration_status == "INA"|
                                       sample_processed$registration_status_2018 == "INA"),]
# import 2019 record
impMatchBind(folder_a = "./data/sample/", 
             folder_b = "./data/voter-files-historical/2019/",
             bind_record = TRUE)
rm(sample)
# extract INA voters registered in 2019 and with ACT status
reg2019 <- records[which(records$id %in% ina_voters$voter_id),]
reg2019 <- reg2019[which(reg2019$status == "ACT"),]
# update registration status of INA voters
ina_voters[which(ina_voters$voter_id %in% reg2019$id), "registration_status_2018"] <- "ACT"
ina_voters[which(ina_voters$voter_id %in% reg2019$id), "registration_status"] <- "ACT"
ina_voters <- ina_voters[which(ina_voters$registration_status == "ACT"),]
sample_processed[which(sample_processed$voter_id %in% 
                         ina_voters$voter_id), "registration_status_2018"] <- "ACT"
sample_processed[which(sample_processed$voter_id %in% 
                         ina_voters$voter_id), "registration_status"] <- "ACT"
rm(records,reg2019)

# check INA voters eligibility from 2016 register ---------------------------------------
ina_voters <- sample_processed[which(sample_processed$registration_status == "INA"|
                                     sample_processed$registration_status_2018 == "INA"),]
# import 2016 record
impMatchBind(folder_a = "./data/sample/", 
             folder_b = "./data/voter-files-historical/2016/",
             bind_record = TRUE)
rm(sample)
# extract INA voters registered in 2016 and with ACT status
reg2016 <- records[which(records$id %in% ina_voters$voter_id),]
reg2016 <- reg2016[which(reg2016$status == "ACT"),] # 2225

# compute turnout rates -----------------------------------------------------------------
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
turnout_rate <- left_join(x = turnout, y = eligibility, by = c("voter_id", "election"))
# format turnout rate
turnout_rate$turnout <- ifelse(turnout_rate$turnout == TRUE, 1, 0)
turnout_rate$eligible <- ifelse(turnout_rate$eligible == TRUE, 1, 0)
# summarize individual turnout
turnout_rate <- turnout_rate[-which(turnout_rate$eligible == 0),] %>% 
  group_by(voter_id) %>% 
  summarize(voted = sum(turnout), observed = n())
turnout_rate$rate <- turnout_rate$voted/turnout_rate$observed
turnout_rate$group <- ifelse(turnout_rate$rate <= 0.25, "0.25",
                             ifelse(turnout_rate$rate > 0.25 & turnout_rate$rate <= 0.5, "0.5",
                                    ifelse(turnout_rate$rate > 0.5 & turnout_rate$rate <= 0.75, "0.75",
                                           ifelse(turnout_rate$rate > 0.75 & turnout_rate$rate <= 1, "1", NA))))
sample_processed <- left_join(sample_processed, turnout_rate, by = "voter_id")
colnames(sample_processed)[c(61,62)] <- c("turnout_rate", "turnout_group")

# flag protected or terminated twitter accounts -----------------------------------------
# this step requires some data collection from the data-collection script
# establish connection to sql databases
con_1 <- dbConnect(SQLite(), 
                   dbname = "./data/social-media-activity/safety5/social-media-init.sqlite")
con_2 <- dbConnect(SQLite(), 
                   dbname = "./data/social-media-activity/safety5/social-media-bas.sqlite")
# extract protected accounts
terminated <- rbind(collect(tbl(con_1, "terminated")), collect(tbl(con_2, "terminated")))
# disconnect from databases
dbDisconnect(conn = con_1)
dbDisconnect(conn = con_2)
rm(con_1,con_2)
# format protected flag
terminated$protected <- ifelse(is.na(terminated$protected), 1, terminated$protected)
colnames(terminated)[c(1,3)] <- c("twitter_id", "terminated")
terminated <- terminated[!duplicated(terminated$twitter_id),]
# bind protected flag to sample and format
sample_processed <- left_join(sample_processed, select(terminated, twitter_id, 
                                                       terminated), 
                              by = "twitter_id")
sample_processed$terminated <- ifelse(is.na(sample_processed$terminated), FALSE, TRUE)

# flag inactive twitter accounts --------------------------------------------------------
con_1 <- dbConnect(SQLite(), 
                   dbname = "./data/social-media-activity/safety5/social-media-act.sqlite")
active <- unique(c(unique(collect(tbl(con_1, "change_friends"))$t_ids),
                   unique(collect(tbl(con_1, "change_likes"))$t_ids),
                   unique(collect(tbl(con_1, "change_statuses"))$t_ids)))
sample_processed$active <- FALSE
sample_processed$active[which(sample_processed$twitter_id %in% active)] <- TRUE
dbDisconnect(con_1)

# save data to disk ---------------------------------------------------------------------
saveRDS(sample_processed, "./data/analysis/sample_processed")


#### COMPUTE SAMPLE POPULATION (SP) CHARACTERISTICS =====================================

# construct age and race groups ---------------------------------------------------------
sample_processed$age_group <-  ifelse(sample_processed$age >= 18 & sample_processed$age < 25, "age_18_24",
                                ifelse(sample_processed$age >= 25 & sample_processed$age < 35, "age_25_34",
                                 ifelse(sample_processed$age >= 35 & sample_processed$age < 50, "age_35_49",
                                  ifelse(sample_processed$age >= 50 & sample_processed$age < 60, "age_50_59",
                                   ifelse(sample_processed$age >= 60, "age_60plus", NA)))))
sample_processed$race_group <- ifelse(sample_processed$race == "multi", "other",
                                ifelse(sample_processed$race == "asian", "other",
                                 ifelse(sample_processed$race == "native", "other",
                                        sample_processed$race)))

# adjust sample to different subsets ----------------------------------------------------
sample <- sample_processed[-which(sample_processed$terminated == TRUE |
                                  sample_processed$registered_2018 == FALSE),]
sample2 <- sample_processed[-which(sample_processed$terminated == TRUE |
                                   sample_processed$registered_2018 == FALSE |
                                   sample_processed$active == FALSE),]
# assign proportions
sp_prop <- data.frame(variable = c("male", "female", "black", "hispanic", "white", 
                                    "other", "age_18_24", "age_25_34", 
                                    "age_35_49", "age_50_59", "age_60plus","democrat",
                                    "republican", "other", "npa", "to_0_25", "to_26_49",
                                    "to_50_74", "to_75_100", "to_pri_2016", 
                                    "to_gen_2016", "to_pri_2018", "to_gen_2018"),
                      estimate = c(prop.table(table(sample_processed$sex))[2],
                                   prop.table(table(sample_processed$sex))[1],
                                   prop.table(table(sample_processed$race_group))[1],
                                   prop.table(table(sample_processed$race_group))[2],
                                   prop.table(table(sample_processed$race_group))[4],
                                   prop.table(table(sample_processed$race_group))[3],
                                   prop.table(table(sample_processed$age_group))[1],
                                   prop.table(table(sample_processed$age_group))[2],
                                   prop.table(table(sample_processed$age_group))[3],
                                   prop.table(table(sample_processed$age_group))[4],
                                   prop.table(table(sample_processed$age_group))[5],
                                   prop.table(table(sample_processed$party))[1],
                                   prop.table(table(sample_processed$party))[4],
                                   prop.table(table(sample_processed$party))[3],
                                   prop.table(table(sample_processed$party))[2],
                                   prop.table(table(sample_processed$turnout_group))[1],
                                   prop.table(table(sample_processed$turnout_group))[2],
                                   prop.table(table(sample_processed$turnout_group))[3],
                                   prop.table(table(sample_processed$turnout_group))[4],
                                   length(which(sample_processed$eligible_pri_2016 == TRUE &
                                                  sample_processed$voted_pri_2016 == TRUE))/
                                     length(which(sample_processed$eligible_pri_2016 == TRUE)),
                                   length(which(sample_processed$eligible_gen_2016 == TRUE &
                                                sample_processed$voted_gen_2016 == TRUE))/
                                     length(which(sample_processed$eligible_gen_2016 == TRUE)),
                                   length(which(sample_processed$eligible_pri_2018 == TRUE &
                                                  sample_processed$voted_pri_2018 == TRUE))/
                                     length(which(sample_processed$eligible_pri_2018 == TRUE)),
                                   length(which(sample_processed$eligible_gen_2018 == TRUE &
                                                  sample_processed$voted_gen_2018 == TRUE))/
                                     length(which(sample_processed$eligible_gen_2018 == TRUE))))
sp_prop2 <- data.frame(variable = c("male", "female", "black", "hispanic", "white", 
                                    "other", "age_18_24", "age_25_34", 
                                    "age_35_49", "age_50_59", "age_60plus","democrat",
                                    "republican", "other", "npa", "to_0_25", "to_26_49",
                                    "to_50_74", "to_75_100", "to_pri_2016", 
                                    "to_gen_2016", "to_pri_2018", "to_gen_2018"),
                       estimate = c(prop.table(table(sample$sex))[2],
                                    prop.table(table(sample$sex))[1],
                                    prop.table(table(sample$race_group))[1],
                                    prop.table(table(sample$race_group))[2],
                                    prop.table(table(sample$race_group))[4],
                                    prop.table(table(sample$race_group))[3],
                                    prop.table(table(sample$age_group))[1],
                                    prop.table(table(sample$age_group))[2],
                                    prop.table(table(sample$age_group))[3],
                                    prop.table(table(sample$age_group))[4],
                                    prop.table(table(sample$age_group))[5],
                                    prop.table(table(sample$party))[1],
                                    prop.table(table(sample$party))[4],
                                    prop.table(table(sample$party))[3],
                                    prop.table(table(sample$party))[2],
                                    prop.table(table(sample$turnout_group))[1],
                                    prop.table(table(sample$turnout_group))[2],
                                    prop.table(table(sample$turnout_group))[3],
                                    prop.table(table(sample$turnout_group))[4],
                                    length(which(sample$eligible_pri_2016 == TRUE &
                                                   sample$voted_pri_2016 == TRUE))/
                                      length(which(sample$eligible_pri_2016 == TRUE)),
                                    length(which(sample$eligible_gen_2016 == TRUE &
                                                   sample$voted_gen_2016 == TRUE))/
                                      length(which(sample$eligible_gen_2016 == TRUE)),
                                    length(which(sample$eligible_pri_2018 == TRUE &
                                                   sample$voted_pri_2018 == TRUE))/
                                      length(which(sample$eligible_pri_2018 == TRUE)),
                                    length(which(sample$eligible_gen_2018 == TRUE &
                                                   sample$voted_gen_2018 == TRUE))/
                                      length(which(sample$eligible_gen_2018 == TRUE))))
sp_prop3 <- data.frame(variable = c("male", "female", "black", "hispanic", "white", 
                                    "other", "age_18_24", "age_25_34", 
                                    "age_35_49", "age_50_59", "age_60plus","democrat",
                                    "republican", "other", "npa", "to_0_25", "to_26_49",
                                    "to_50_74", "to_75_100", "to_pri_2016", 
                                    "to_gen_2016", "to_pri_2018", "to_gen_2018"),
                       estimate = c(prop.table(table(sample2$sex))[2],
                                    prop.table(table(sample2$sex))[1],
                                    prop.table(table(sample2$race_group))[1],
                                    prop.table(table(sample2$race_group))[2],
                                    prop.table(table(sample2$race_group))[4],
                                    prop.table(table(sample2$race_group))[3],
                                    prop.table(table(sample2$age_group))[1],
                                    prop.table(table(sample2$age_group))[2],
                                    prop.table(table(sample2$age_group))[3],
                                    prop.table(table(sample2$age_group))[4],
                                    prop.table(table(sample2$age_group))[5],
                                    prop.table(table(sample2$party))[1],
                                    prop.table(table(sample2$party))[4],
                                    prop.table(table(sample2$party))[3],
                                    prop.table(table(sample2$party))[2],
                                    prop.table(table(sample2$turnout_group))[1],
                                    prop.table(table(sample2$turnout_group))[2],
                                    prop.table(table(sample2$turnout_group))[3],
                                    prop.table(table(sample2$turnout_group))[4],
                                    length(which(sample2$eligible_pri_2016 == TRUE &
                                                   sample2$voted_pri_2016 == TRUE))/
                                      length(which(sample2$eligible_pri_2016 == TRUE)),
                                    length(which(sample2$eligible_gen_2016 == TRUE &
                                                   sample2$voted_gen_2016 == TRUE))/
                                      length(which(sample2$eligible_gen_2016 == TRUE)),
                                    length(which(sample2$eligible_pri_2018 == TRUE &
                                                   sample2$voted_pri_2018 == TRUE))/
                                      length(which(sample2$eligible_pri_2018 == TRUE)),
                                    length(which(sample2$eligible_gen_2018 == TRUE &
                                                   sample2$voted_gen_2018 == TRUE))/
                                      length(which(sample2$eligible_gen_2018 == TRUE))))

# save population characteristics to disk -----------------------------------------------
saveRDS(sp_prop, "./data/analysis/sp_prop")
saveRDS(sp_prop2, "./data/analysis/sp_prop2")
saveRDS(sp_prop3, "./data/analysis/sp_prop3")
