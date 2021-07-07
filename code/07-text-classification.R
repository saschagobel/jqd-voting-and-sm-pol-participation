# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Text classification script
# April 2019
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./code/keyword_algorithm.py'
'./data/classification/reference_set/*'
'./data/classification/search_set/*'
'./data/classification/reference_set_updated/*'
'./data/classification/training_data_processed'
'./data/classification/test_data_processed'
'./data/classification/validation_data_processed'
'./data/classification/training_data'
'./data/classification/training_data_rater2'
'./data/analysis/analysis_processed'
'./data/analysis/sample_processed'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/classification/dict_init'
'./data/classification/dict_updated'
'./data/analysis/analysis_categorized'
'./data/analysis/sample_analysis'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 44 - PREPARATIONS
Line 62 - GENERATE DICTIONARY-BASED CLASSIFIER VIA COMPUTER-ASSISTED KEYWORD DISCOVERY
Line 292 - EVALUATE DICTIONARY-BASED CLASSIFIER PERFORMANCE
Line 480 - GENERATE MACHINE LEARNING-BASED CLASSIFIER VIA DEEP LEARNING
Line 548 - EVALUATE MACHINE LEARNING-BASED CLASSIFIER PERFORMANCE
Line 564 - ASSESS INTERRATER RELIABILITY FOR TRAINING SET
Line 581 - CLASSIFY DATA FOR ANALYSIS BASED ON SELECTED CLASSIFIER
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")

# source python script ------------------------------------------------------------------
source_python("./code/keyword_algorithm.py")
# keyword_algorithm.py can be downloaded from 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FMJDCD


#### GENERATE DICTIONARY-BASED CLASSIFIER VIA COMPUTER-ASSISTED KEYWORD DISCOVERY =======

# The below code is applied iteratively in tandem with part of "06-text-processing.R".

# run computer-assisted keyword detection algorithm on initial documents ----------------
# initialize keyword object
pol_par <- Keywords()
# import reference set documents from directory of .txt files
pol_par$ReferenceSet(data='./data/classification/reference_set')
# import search set documents from directory of .txt files
pol_par$SearchSet(data='./data/classification/search_set')
# process the data (most processing was already done in "06-text-processing.R")
pol_par$ProcessData(stem=FALSE, remove_numbers=FALSE, remove_punct=FALSE, 
                    remove_stopwords=FALSE, remove_wordlist=NULL, 
                    keep_twitter_symbols=TRUE, keep_urls=TRUE, min_wordlength=3)
# produce list of reference set uni-, bi-, and trigrams by appearance
pol_par$ReferenceKeywords(ngrams=as.integer(3))
# classify search set documents using different classifiers
pol_par$ClassifyDocs(algorithms=c('nbayes', 'logit', 'SVM'))
# identify estimated target set within search set documents
pol_par$FindTargetSet()
# identify and rank uni-, bi-, and trigram keywords within target and non-target sets
pol_par$FindKeywords(ngrams=as.integer(3))
# store target set keywords and stats
keywords_target <- pol_par$target_stats
# store reference set keywords and stats
keywords_reference <- pol_par$reference_stats
# store non-target set keywords and stats
keywords_nontarget <- pol_par$nontarget_stats

# assemble dictionary from initial run --------------------------------------------------
dict_init <- c("@realdonaldtrump", "trump", "president", "vote", "democrats", "election", 
               "government", "republican", "kavanaugh", "obama", "congress", "democrat", 
               "political", "@potus", "democratic", "dems", "@andrewgillum", "donald", 
               "gop", "mueller", "white house", "voted", "republicans", "party", 
               "voting", "votes", "trumps", "voters", "donald trump", "senator", 
               "federal", "administration", "@marcorubio", "bill", "president trump", 
               "senate", "@krassenstein", "leader", "kavanaughs", "voter", "polls", 
               "immigration", "supreme court", "senators", "law", "brett", 
               "border security", "district", "candidate", "elections", "cohen", 
               "committee", "unconstitutional", "pelosi", "mccain", "@repadamschiff", 
               "policy", "politicians", "#bringithome", "bush", "marco rubio", "rubio", 
               "majority", "@hillaryclinton", "leaders", "sanders", "@gop", "districts",
               "lindsey graham", "clinton", "brett kavanaughs", "speaker", "agenda",
               "@berniesanders", "@betoorourke", "presidency", "impeachment", "shutdown", 
               "matt gaetz", "immigrants", "manafort", "@lindseygrahamsc", "governor", 
               "rep", "parties", "national security", "gov", "republicanos", "@dhsgov", 
               "liberals", "blasey", "midterms", "#maga", "@peterdaou", "democracy", 
               "tax", "gaetz", "potus", "jeff flake", "pompeo", "polling", "politics", 
               "@kamalaharris", "representantes",  "blasey ford", "@repmattgaetz", 
               "@repswalwell", "@secpompeo", "wages", "taxpayer", "washington", 
               "@nancypelosi", "@senatemajldr", "@senbillnelson", "ted cruz", 
               "@whitehouse", "@senkamalaharris", "@sensanders", "conservatives", 
               "@tedlieu", "feinstein", "obamacare", "beto", "gun lobby", "politician", 
               "beto orourke", "chief staff", "susan collins", "governments", "gobierno", 
               "candidates", "neil gorsuch", "alexandria ocasiocortez", "ocasiocortez", 
               "unemployment", "partisan", "progressives", "progressive", "presidential", 
               "turnout", "george bush", "amy klobuchar", "brian kemp", "presidente", 
               "hillary", "brett kavanaugh", "@jeffmerkley", "@jeffflake", "#gop", 
               "#flapol", "maga", "@jimjordan", "#sotu", "leftist", "elected", "elect", 
               "@flotus","mcconnell", "@chuckgrassley", "kyrsten sinema", "#walkaway", 
               "scotus", "#voteblue", "#buildthewall", "klobuchar", "#trumpresign",
               "#voteredtosaveamerica", "midterm", "#electionday", "liberal", "senado", 
               "republicano", "polarization", "@senwarren", "@repmarkmeadows",
               "votar", "@speakerpelosi", "@thedemocrats")
saveRDS(dict_init, "./data/classification/dict_init")
# switch to "06-text-processing.R" and run section "update reference set text data in
# tandem with keyword detection algorithm"

# run computer-assisted keyword detection algorithm on updated documents ----------------
# start here after having updated the reference set in "06-text-processing.R"
# initialize keyword object
pol_par <- Keywords()
# import reference set documents from directory of .txt files
pol_par$ReferenceSet(data='./data/classification/reference_set_updated/')
# import search set documents from directory of .txt files
pol_par$SearchSet(data='./data/classification/search_set')
# process the data (most processing was already done in "06-text-processing.R")
pol_par$ProcessData(stem=FALSE, remove_numbers=FALSE, remove_punct=FALSE, 
                    remove_stopwords=FALSE, remove_wordlist=NULL, 
                    keep_twitter_symbols=TRUE, keep_urls=TRUE, min_wordlength=3)
# produce list of reference set uni-, bi-, and trigrams by appearance
pol_par$ReferenceKeywords(ngrams=as.integer(2))
# classify search set documents using different classifiers
pol_par$ClassifyDocs(algorithms=c('nbayes', 'logit', 'SVM'))
# identify estimated target set within search set documents
pol_par$FindTargetSet()
# identify and rank uni-, bi-, and trigram keywords within target and non-target sets
pol_par$FindKeywords(ngrams=as.integer(2))
# store target set keywords and stats
keywords_target <- pol_par$target_stats
# store reference set keywords and stats
keywords_reference <- pol_par$reference_stats
# store non-target set keywords and stats
keywords_nontarget <- pol_par$nontarget_stats

# assemble dictionary from updated runs -------------------------------------------------
dict_updated <- c("wall", "laws", "economy", "collusion", "fraud", "@tedcruz", "schumer", 
                  "amendment", "socialism", "رئيس", "state", "corruption", "govt", 
                  "regulation", "@barackobama", "ballots", "ballot", "electoral", 
                  "taxes", "border wall", "@presssec", "electorate", "@senatorcollins", 
                  "campaign", "nominee", "@donaldjtrumpjr", "@sarahpalinusa", 
                  "@cnnpolitics", "@senategop", "@senatedems", "desantis", 
                  "#stopkavanaugh", "primaries", "@rondesantisfl", "@housegop", 
                  "@thedemcoalition", "@senschumer", "congressional", "pence", 
                  "senatemajldr", "@senblumenthal","#scotus", "medicare", "#potus", 
                  "gillum", "gubernatorial", "@senfeinstein", "campaigning", "élections", 
                  "#floridaprimaries", "#buildthatwall", "#istandwithbrett", "impeached", 
                  "@nelsonforsenate", "@flgovscott", "represented", "@corybooker", "@vp", 
                  "demagogue", "clintons", "precincts", "congressman", "#votegillum", 
                  "@senwhitehouse", "#whatsatstake", "@scottforflorida", "השמאלנות", 
                  "#vote", "#redwaverising", "#shawforflorida", "#theresistance", 
                  "@forourfuturefl", "@momsdemand", "@senjoemanchin", "#bluewave", 
                  "#draintheswamp", "#democrats", "nominate", "nominated", "judiciary", 
                  "#backfiretrump", "constitution", "murkowski", "#kavanaugh", 
                  "@senjohnmccain", "congressmen", "congresswoman", "trumprussia", 
                  "impeach", "@stabenow", "@sengillibrand", "@sentedcruz", 
                  "administrations", "politique", "politisyen", "elegir", "candidatos", 
                  "prezidan", "senador", "selfgovernment", "america first", "partyline",
                  "#confirmkavanaugh", "#votered", "@housedemocrats", "bipartisanship",      
                  "leftwing", "rightwing", "#govote", "#thisisnotdemocracy", 
                  "senator collins", "paul manafort", "attorney general", "gun law",
                  "gun laws", "senator john", "judiciary committee", "joe manchin", 
                  "electoral college", "mitch mcconnell", "house judiciary", 
                  "granted immunity", "justice system", "outlaw islam", "lgbtq rights", 
                  "making america great", "make america great", "#trumpshutdown", 
                  "indicted", "socialist", "accountable", "republic", "policies", 
                  "rights", "constitutional", "reform", "crooked", "@mattgaetz", 
                  "@speakerryan", "@ocasio", "@staceyabrams", "@fladems", "@brianschatz",
                  "@lisamurkowski", "@gopchairwoman", "@govmikehuckabee", 
                  "@govrondesantis", "@amyklobuchar", "@statedept", "#kavanaughhearings", 
                  "#resist", "#ibelievechristineblaseyford", "#fakepresident", 
                  "#brettkavanaugh", "#bluetsunami", "#republicans", "#impeachtrump", 
                  "#votedem", "#votethemout", "#flgovdebate", "#trumprussia", 
                  "#paintourcountryred", "#muellertime", "#nevertrump", "#kavanaughvote", 
                  "#midterms", "#traitortrump", "#americafirst", "#betoforsenate", 
                  "#gopdebate", "#gunsense", "#fairtax", "#votingrights", 
                  "#nationalvoterregistrationday", "#walkawayfromdemocrats", 
                  "#votebeto", "#bordersecurity", "#thewall", "#floridaelection", 
                  "#endtheshutdown", "sen", "dem", "maxine waters", "jahana hayes", 
                  "@bensasse", "public office", "#speakerpelosi", "mayor", "muellers", 
                  "sessions", "#trumpaddress", "@secnielsen", "sec nielsen", "grassley", 
                  "#vpdebate", "arming teachers", "taxpayers", "vote blue", 
                  "national emergency", "@repthomasmassie", "bernie", "@ocasio2018", 
                  "electoral system", "#guncontrol", "legislature", "legislative", 
                  "disenfranchised", "enfranchised", "enfranchisement", 
                  "disenfranchisement", "representatives", "housegop", "senategop",
                  "term limit", "term limits", "congresswomen", "legislator",
                  "senrickscott", "senorrinhatch", "trumpexpress", "mick mulvaney", 
                  "sen judiciary", "@govhowarddean", "#bluewave2018", "#ivoted", 
                  "massgovernor", "buildthedamnwall", "statedept", "blue wave", 
                  "#michaelcohen", "pences", "trumpshutdown", "lawmaker", "legislation",
                  "grand old party", "build the wall", "policymaker", "policymakers", 
                  "@orlandomayor", "union address", "#protrump", "#antitrump",
                  "@votersincharge", "makeamericagreatagain", "govmt", "constituents",
                  "obamas", "gun control", "#trump", "@senwarrens")
dict_updated <- c(dict_init, dict_updated)
saveRDS(dict_updated, "./data/classification/dict_updated")
# switch to "06-text-processing.R" and run section "update reference set text data in
# tandem with keyword detection algorithm" if more iterations are desired

# filter raw dictionary to produce different dictionary versions ------------------------
# dict_raw includes the complete dictionary (428 keywords)
# dict_filtered_1 excludes very ambiguous terms (359)
# dict_filtered_2 excludes additional potentially ambiguous terms (340)
# dict_filtered_3 excludes additional potentially ambiguous terms (331)
# dict_filtered_5 includes only few high profile politicians (3)
dict_updated <- readRDS("./data/classification/dict_updated")
dict_raw <- sort(unique(tolower(dict_updated))) 
dict_filtered_1 <- dict_raw[!(dict_raw %in% 
                                c("השמאלנות", "accountable", "agenda", "bill", "رئيس", 
                                  "campaign", "campaigning", "candidate", "candidates", 
                                  "candidatos", "collusion", "conservatives", "constitution", 
                                  "constitutional", "corruption", "crooked", "demagogue", 
                                  "democracy", "economy", "election", "fraud", "government", 
                                  "governments", "immigrants", "immigration", "indicted", 
                                  "law", "laws", "leader", "leaders", "leftist", "liberal", 
                                  "liberals", "majority", "mccain", "nominate", "nominated", 
                                  "nominee", "party", "polarization", "policies", "policy", 
                                  "political", "precincts", "presidency", "president", 
                                  "presidente", "prezidan", "progressive", "progressives", 
                                  "reform", "representantes", "republic", "rights", 
                                  "socialism", "socialist", "speaker", "state", 
                                  "supreme court", "tax", "taxes","unconstitutional", 
                                  "unemployment", "votar", "vote", "voted", "votes", 
                                  "wages", "wall"))]
dict_filtered_2 <- dict_raw[!(dict_raw %in% 
                      c("@cnnpolitics", "@donaldjtrumpjr", "@krassenstein", "@peterdaou",
                        "@thedemcoalition", "השמאלנות", "accountable", "agenda", 
                        "attorney general", "beto", "bill", "brett", "رئيس",
                        "@forourfuturefl", "bush", "campaign", "campaigning", "candidate",
                        "candidates", "candidatos", "cohen", "collusion", "conservatives",
                        "constitution", "constitutional", "corruption", "crooked",
                        "demagogue", "democracy", "donald", "economy", "election", 
                        "fraud", "government", "governments", "immigrants", "immigration",     
                        "indicted", "law", "laws", "leader", "leaders", "leftist", 
                        "lgbtq rights", "liberal", "liberals", "majority", "mccain",
                        "medicare", "national security", "neil gorsuch", "nominate", 
                        "nominated", "nominee", "outlaw islam", "party", "polarization",
                        "policies", "policy", "political", "precincts", "presidency", 
                        "president", "presidente", "presidential","prezidan", 
                        "progressive", "progressives", "reform", "representantes", 
                        "republic", "rights", "selfgovernment","socialism", "socialist", 
                        "speaker", "state", "supreme court", "tax", "taxes","unconstitutional", 
                        "unemployment", "votar", "vote", "voted", "votes", "wages", "wall"))]
dict_filtered_3 <-  dict_raw[!(dict_raw %in% 
                                 c("@cnnpolitics", "@donaldjtrumpjr", "@krassenstein", "@peterdaou",
                                   "@thedemcoalition", "השמאלנות", "accountable", "agenda", 
                                   "attorney general", "beto", "bill", "brett", "رئيس",
                                   "@forourfuturefl", "bush", "campaign", "campaigning", "candidate",
                                   "candidates", "candidatos", "cohen", "collusion", "conservatives",
                                   "constitution", "constitutional", "corruption", "crooked",
                                   "demagogue", "democracy", "donald", "economy", "election", 
                                   "fraud", "government", "governments", "immigrants", "immigration",     
                                   "indicted", "law", "laws", "leader", "leaders", "leftist", 
                                   "lgbtq rights", "liberal", "liberals", "majority", "mccain",
                                   "medicare", "national security", "neil gorsuch", "nominate", 
                                   "nominated", "nominee", "outlaw islam", "party", "polarization",
                                   "policies", "policy", "political", "precincts", "presidency", 
                                   "president", "presidente", "presidential","prezidan", 
                                   "progressive", "progressives", "reform", "representantes", 
                                   "republic", "rights", "selfgovernment","socialism", "socialist", 
                                   "speaker", "state", "supreme court", "tax", "taxes","unconstitutional", 
                                   "unemployment", "votar", "vote", "voted", "votes", "wages", "wall",
                                   "#resist", "#theresistance", "amendment", "schumer", "district",               
                                   "districts", "washington", "regulation", "committee", 
                                   "national security"))]
dict_filtered_4 <- c("trump", "clinton", "obama", "romney")


#### EVALUATE DICTIONARY-BASED CLASSIFIER PERFORMANCE ===================================

# import processed test and validation data and prepare as corpus -----------------------
test_data_pro <- readRDS("./data/classification/test_data_processed")
test_data_corpus <- corpus(test_data_pro,
                           docid_field = "doc_id",
                           text_field = "text_pro")
validation_data_pro <- readRDS("./data/classification/validation_data_processed")
validation_data_corpus <- corpus(validation_data_pro,
                           docid_field = "doc_id",
                           text_field = "text_pro")

# prepare uni-, bi-, and- tri-gram dictionaries -----------------------------------------
dict_raw_uni <- dictionary(list(polact = dict_raw[which(str_count(
                                           dict_raw, " ") == 0)]))
dict_raw_bi <- dictionary(list(polact = dict_raw[which(str_count(
                                          dict_raw, " ") == 1)]))
dict_raw_tri <- dictionary(list(polact = dict_raw[which(str_count(
                                           dict_raw, " ") == 2)]))
dict_filtered_1_uni <- dictionary(list(polact = dict_filtered_1[which(str_count(
                                                  dict_filtered_1, " ") == 0)]))
dict_filtered_1_bi <- dictionary(list(polact = dict_filtered_1[which(str_count(
                                                 dict_filtered_1, " ") == 1)]))
dict_filtered_1_tri <- dictionary(list(polact = dict_filtered_1[which(str_count(
                                                  dict_filtered_1, " ") == 2)]))
dict_filtered_2_uni <- dictionary(list(polact = dict_filtered_2[which(str_count(
                                                  dict_filtered_2, " ") == 0)]))
dict_filtered_2_bi <- dictionary(list(polact = dict_filtered_2[which(str_count(
                                                 dict_filtered_2, " ") == 1)]))
dict_filtered_2_tri <- dictionary(list(polact = dict_filtered_2[which(str_count(
                                                 dict_filtered_2, " ") == 2)]))
dict_filtered_3_uni <- dictionary(list(polact = dict_filtered_3[which(str_count(
                                                  dict_filtered_3, " ") == 0)]))
dict_filtered_3_bi <- dictionary(list(polact = dict_filtered_3[which(str_count(
                                                 dict_filtered_3, " ") == 1)]))
dict_filtered_3_tri <- dictionary(list(polact = dict_filtered_3[which(str_count(
                                                  dict_filtered_3, " ") == 2)]))
dict_filtered_4_uni <- dictionary(list(polact = dict_filtered_4[which(str_count(
                                                  dict_filtered_4, " ") == 0)]))

# apply dictionaries to test data to generate predictions -------------------------------
# apply dictionaries
test_dfm_uni_raw <- dfm(test_data_corpus, ngrams = 1L, dictionary = dict_raw_uni)
test_dfm_bi_raw <- dfm(test_data_corpus, ngrams = 2L, dictionary = dict_raw_bi)
test_dfm_tri_raw <- dfm(test_data_corpus, ngrams = 3L, dictionary = dict_raw_tri)
test_dfm_uni_1 <- dfm(test_data_corpus, ngrams = 1L, dictionary = dict_filtered_1_uni)
test_dfm_bi_1 <- dfm(test_data_corpus, ngrams = 2L, dictionary = dict_filtered_1_bi)
test_dfm_tri_1 <- dfm(test_data_corpus, ngrams = 3L, dictionary = dict_filtered_1_tri)
test_dfm_uni_2 <- dfm(test_data_corpus, ngrams = 1L, dictionary = dict_filtered_2_uni)
test_dfm_bi_2 <- dfm(test_data_corpus, ngrams = 2L, dictionary = dict_filtered_2_bi)
test_dfm_tri_2 <- dfm(test_data_corpus, ngrams = 3L, dictionary = dict_filtered_2_tri)
test_dfm_uni_3 <- dfm(test_data_corpus, ngrams = 1L, dictionary = dict_filtered_3_uni)
test_dfm_bi_3 <- dfm(test_data_corpus, ngrams = 2L, dictionary = dict_filtered_3_bi)
test_dfm_tri_3 <- dfm(test_data_corpus, ngrams = 3L, dictionary = dict_filtered_3_tri)
test_dfm_uni_4 <- dfm(test_data_corpus, ngrams = 1L, dictionary = dict_filtered_4_uni)
# convert to data frames
test_dfm_uni_raw <- convert(test_dfm_uni_raw, to = "data.frame")
test_dfm_bi_raw <- convert(test_dfm_bi_raw, to = "data.frame")
test_dfm_tri_raw <- convert(test_dfm_tri_raw, to = "data.frame")
test_dfm_uni_1 <- convert(test_dfm_uni_1, to = "data.frame")
test_dfm_bi_1 <- convert(test_dfm_bi_1, to = "data.frame")
test_dfm_tri_1 <- convert(test_dfm_tri_1, to = "data.frame")
test_dfm_uni_2 <- convert(test_dfm_uni_2, to = "data.frame")
test_dfm_bi_2 <- convert(test_dfm_bi_2, to = "data.frame")
test_dfm_tri_2 <- convert(test_dfm_tri_2, to = "data.frame")
test_dfm_uni_3 <- convert(test_dfm_uni_3, to = "data.frame")
test_dfm_bi_3 <- convert(test_dfm_bi_3, to = "data.frame")
test_dfm_tri_3 <- convert(test_dfm_tri_3, to = "data.frame")
test_dfm_uni_4 <- convert(test_dfm_uni_4, to = "data.frame")
# adjust colnames
colnames(test_dfm_uni_raw) <- c("doc_id", "polact_uni")
colnames(test_dfm_bi_raw) <- c("doc_id", "polact_bi")
colnames(test_dfm_tri_raw) <- c("doc_id", "polact_tri")
colnames(test_dfm_uni_1) <- c("doc_id", "polact_uni")
colnames(test_dfm_bi_1) <- c("doc_id", "polact_bi")
colnames(test_dfm_tri_1) <- c("doc_id", "polact_tri")
colnames(test_dfm_uni_2) <- c("doc_id", "polact_uni")
colnames(test_dfm_bi_2) <- c("doc_id", "polact_bi")
colnames(test_dfm_tri_2) <- c("doc_id", "polact_tri")
colnames(test_dfm_uni_3) <- c("doc_id", "polact_uni")
colnames(test_dfm_bi_3) <- c("doc_id", "polact_bi")
colnames(test_dfm_tri_3) <- c("doc_id", "polact_tri")
colnames(test_dfm_uni_4) <- c("doc_id", "polact_uni")
# join back with test data for evaluation
dict_raw_eval <- left_join(x = test_data_pro, y = test_dfm_uni_raw, by = "doc_id")
dict_raw_eval <- left_join(x = dict_raw_eval, y = test_dfm_bi_raw, by = "doc_id")
dict_raw_eval <- left_join(x = dict_raw_eval, y = test_dfm_tri_raw, by = "doc_id")
dict_filtered_1_eval <- left_join(x = test_data_pro, y = test_dfm_uni_1, by = "doc_id")
dict_filtered_1_eval <- left_join(x = dict_filtered_1_eval, y = test_dfm_bi_1, by = "doc_id")
dict_filtered_1_eval <- left_join(x = dict_filtered_1_eval, y = test_dfm_tri_1, by = "doc_id")
dict_filtered_2_eval <- left_join(x = test_data_pro, y = test_dfm_uni_2, by = "doc_id")
dict_filtered_2_eval <- left_join(x = dict_filtered_2_eval, y = test_dfm_bi_2, by = "doc_id")
dict_filtered_2_eval <- left_join(x = dict_filtered_2_eval, y = test_dfm_tri_2, by = "doc_id")
dict_filtered_3_eval <- left_join(x = test_data_pro, y = test_dfm_uni_3, by = "doc_id")
dict_filtered_3_eval <- left_join(x = dict_filtered_3_eval, y = test_dfm_bi_3, by = "doc_id")
dict_filtered_3_eval <- left_join(x = dict_filtered_3_eval, y = test_dfm_tri_3, by = "doc_id")
dict_filtered_4_eval <- left_join(x = test_data_pro, y = test_dfm_uni_4, by = "doc_id")
# generate final prediction
dict_raw_eval$pred <- ifelse((dict_raw_eval$polact_uni +
                                       dict_raw_eval$polact_bi +
                                       dict_raw_eval$polact_tri) > 0, 1, 0)
dict_filtered_1_eval$pred <- ifelse((dict_filtered_1_eval$polact_uni +
                                       dict_filtered_1_eval$polact_bi +
                                       dict_filtered_1_eval$polact_tri) > 0, 1, 0)
dict_filtered_2_eval$pred <- ifelse((dict_filtered_2_eval$polact_uni +
                                       dict_filtered_2_eval$polact_bi +
                                       dict_filtered_2_eval$polact_tri) > 0, 1, 0)
dict_filtered_3_eval$pred <- ifelse((dict_filtered_3_eval$polact_uni +
                                       dict_filtered_3_eval$polact_bi +
                                       dict_filtered_3_eval$polact_tri) > 0, 1, 0)
dict_filtered_4_eval$pred <- ifelse(dict_filtered_4_eval$polact_uni > 0, 1, 0)

# evaluate dictionaries against test set ------------------------------------------------
dict_raw_cm  <- as.matrix(table(actual = dict_raw_eval$political, 
                                       predicted = dict_raw_eval$pred))
dict_raw_tpr <- dict_raw_cm[2,2]/sum(dict_raw_cm[2,])
dict_raw_fpr <- dict_raw_cm[1,2]/sum(dict_raw_cm[1,])
dict_raw_tnr <- 1 - dict_raw_fpr
dict_raw_fnr <- 1 - dict_raw_tpr
dict_raw_acc <- sum(diag(dict_raw_cm)) / sum(dict_raw_cm) 
dict_filtered_1_cm  <- as.matrix(table(actual = dict_filtered_1_eval$political, 
                                      predicted = dict_filtered_1_eval$pred))
dict_filtered_1_tpr <- dict_filtered_1_cm[2,2]/sum(dict_filtered_1_cm[2,])
dict_filtered_1_fpr <- dict_filtered_1_cm[1,2]/sum(dict_filtered_1_cm[1,])
dict_filtered_1_tnr <- 1 - dict_filtered_1_fpr
dict_filtered_1_fnr <- 1 - dict_filtered_1_tpr
dict_filtered_1_acc <- sum(diag(dict_filtered_1_cm)) / sum(dict_filtered_1_cm) 
dict_filtered_2_cm  <- as.matrix(table(actual = dict_filtered_2_eval$political, 
                                       predicted = dict_filtered_2_eval$pred))
dict_filtered_2_tpr <- dict_filtered_2_cm[2,2]/sum(dict_filtered_2_cm[2,])
dict_filtered_2_fpr <- dict_filtered_2_cm[1,2]/sum(dict_filtered_2_cm[1,])
dict_filtered_2_tnr <- 1 - dict_filtered_2_fpr
dict_filtered_2_fnr <- 1 - dict_filtered_2_tpr
dict_filtered_2_acc <- sum(diag(dict_filtered_2_cm)) / sum(dict_filtered_2_cm) 
dict_filtered_3_cm  <- as.matrix(table(actual = dict_filtered_3_eval$political, 
                                       predicted = dict_filtered_3_eval$pred))
dict_filtered_3_tpr <- dict_filtered_3_cm[2,2]/sum(dict_filtered_3_cm[2,])
dict_filtered_3_fpr <- dict_filtered_3_cm[1,2]/sum(dict_filtered_3_cm[1,])
dict_filtered_3_tnr <- 1 - dict_filtered_3_fpr
dict_filtered_3_fnr <- 1 - dict_filtered_3_tpr
dict_filtered_3_acc <- sum(diag(dict_filtered_3_cm)) / sum(dict_filtered_3_cm) 
dict_filtered_4_cm  <- as.matrix(table(actual = dict_filtered_4_eval$political, 
                                       predicted = dict_filtered_4_eval$pred))
dict_filtered_4_tpr <- dict_filtered_4_cm[2,2]/sum(dict_filtered_4_cm[2,])
dict_filtered_4_fpr <- dict_filtered_4_cm[1,2]/sum(dict_filtered_4_cm[1,])
dict_filtered_4_tnr <- 1 - dict_filtered_4_fpr
dict_filtered_4_fnr <- 1 - dict_filtered_4_tpr
dict_filtered_4_acc <- sum(diag(dict_filtered_4_cm)) / sum(dict_filtered_4_cm) 

# apply selected dictionary to validation data to generate predictions ------------------
# apply dictionary
validation_dfm_uni_3 <- dfm(validation_data_corpus, ngrams = 1L, 
                            dictionary = dict_filtered_3_uni)
validation_dfm_bi_3 <- dfm(validation_data_corpus, ngrams = 2L, 
                           dictionary = dict_filtered_3_bi)
validation_dfm_tri_3 <- dfm(validation_data_corpus, ngrams = 3L, 
                            dictionary = dict_filtered_3_tri)
# convert to data frame
validation_dfm_uni_3 <- convert(validation_dfm_uni_3, to = "data.frame")
validation_dfm_bi_3 <- convert(validation_dfm_bi_3, to = "data.frame")
validation_dfm_tri_3 <- convert(validation_dfm_tri_3, to = "data.frame")
# adjust colnames
colnames(validation_dfm_uni_3) <- c("doc_id", "polact_uni")
colnames(validation_dfm_bi_3) <- c("doc_id", "polact_bi")
colnames(validation_dfm_tri_3) <- c("doc_id", "polact_tri")
# join back with validation data for evaluation
dict_filtered_3_eval2 <- left_join(x = validation_data_pro, 
                                   y = validation_dfm_uni_3, by = "doc_id")
dict_filtered_3_eval2 <- left_join(x = dict_filtered_3_eval2, 
                                   y = validation_dfm_bi_3, by = "doc_id")
dict_filtered_3_eval2 <- left_join(x = dict_filtered_3_eval2, 
                                   y = validation_dfm_tri_3, by = "doc_id")
# generate final prediction
dict_filtered_3_eval2$pred <- ifelse((dict_filtered_3_eval2$polact_uni +
                                       dict_filtered_3_eval2$polact_bi +
                                       dict_filtered_3_eval2$polact_tri) > 0, 1, 0)

# evaluate selected dictionary against validation set -----------------------------------
# generate confusion matrix and accuracy scores
dict_filtered_3_cm2  <- as.matrix(table(actual = dict_filtered_3_eval2$political, 
                                       predicted = dict_filtered_3_eval2$pred))
dict_filtered_3_tpr2 <- dict_filtered_3_cm2[2,2]/sum(dict_filtered_3_cm2[2,])
dict_filtered_3_fpr2 <- dict_filtered_3_cm2[1,2]/sum(dict_filtered_3_cm2[1,])
dict_filtered_3_tnr2 <- 1 - dict_filtered_3_fpr2
dict_filtered_3_fnr2 <- 1 - dict_filtered_3_tpr2
dict_filtered_3_acc2 <- sum(diag(dict_filtered_3_cm2)) / sum(dict_filtered_3_cm2) 


#### GENERATE MACHINE LEARNING-BASED CLASSIFIER VIA DEEP LEARNING =======================

# import training, test, and validation data and tokenize text --------------------------
training_data_processed <- readRDS("./data/classification/training_data_processed")
test_data_processed <- readRDS("./data/classification/test_data_processed")
validation_data_processed <- readRDS("./data/classification/validation_data_processed")

# second run with larger training:
second_run <- rbind(training_data_processed,test_data_processed,validation_data_processed)
second_run <- second_run[sample(nrow(second_run)),] # shuffle rowise, apply multiple times
training_data_processed <- second_run[1:8000,]
test_data_processed <- second_run[8001:10000,]
validation_data_processed <- second_run[10001:12000,]

tokenizer_training <- keras::text_tokenizer(num_words = 10000)
tokenizer_test <- keras::text_tokenizer(num_words = 10000)
tokenizer_validation <- keras::text_tokenizer(num_words = 10000)
tokenizer_training %>% 
  fit_text_tokenizer(training_data_processed$text_pro)
tokenizer_test %>% 
  fit_text_tokenizer(test_data_processed$text_pro)
tokenizer_validation %>% 
  fit_text_tokenizer(validation_data_processed$text_pro)

# transform texts into sequences of integers --------------------------------------------
training_sequences <- texts_to_sequences(tokenizer_training, training_data_processed$text_pro)
test_sequences <- texts_to_sequences(tokenizer_test, test_data_processed$text_pro)
validation_sequences <- texts_to_sequences(tokenizer_validation, validation_data_processed$text_pro)

# pad texts to same length to fit into word index matrix --------------------------------
input_length <- 150
text_training <- training_sequences %>%
  pad_sequences(maxlen = input_length)
text_test <- test_sequences %>%
  pad_sequences(maxlen = input_length)
text_validation <- validation_sequences %>%
  pad_sequences(maxlen = input_length)

# store response vectors ----------------------------------------------------------------
response_training <- training_data_processed$political
response_test <- test_data_processed$political
response_validation <- validation_data_processed$political

# build model ---------------------------------------------------------------------------
dl_model <- keras_model_sequential() %>% # set up keras model with linear stack of layers
  layer_embedding(input_dim = 10000, output_dim = 128, input_length = input_length) %>% # embedding layer
  layer_flatten() %>% # flatten 3D tensor of embeddings into 2D tensor
  layer_dense(units = 64, activation = "relu") %>% # add classifier
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 32, activation = "relu") %>% # add classifier
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 16, activation = "relu") %>% # add classifier
  layer_dense(units = 1, activation = "sigmoid")
dl_model %>% keras::compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

# train model and evaluate against test set --------------------------------------------
trained_1 <- dl_model %>% 
  fit(x = text_training,
      y = response_training,
      epochs = 20,
      validation_data = list(text_test, response_test)
  )


#### EVALUATE MACHINE LEARNING-BASED CLASSIFIER PERFORMANCE =============================

# generate accuracy score ---------------------------------------------------------------
dl_model %>% keras::evaluate(text_validation, response_validation)

# generate confusion matrix -------------------------------------------------------------
dl_model_prediction <- as.matrix(table(actual = validation_data_processed$political,
                                       predicted = predict_classes(dl_model, 
                                                                   text_validation)))
dl_model_prediction_tpr <- dl_model_prediction[2,2]/sum(dl_model_prediction[2,])
dl_model_prediction_fpr <- dl_model_prediction[1,2]/sum(dl_model_prediction[1,])
dl_model_prediction_tnr <- 1 - dl_model_prediction_fpr
dl_model_prediction_fnr <- 1 - dl_model_prediction_tpr
dl_model_prediction_acc <- sum(diag(dl_model_prediction)) / sum(dl_model_prediction)


#### ASSESS INTERRATER RELIABILITY FOR TRAINING SET =====================================

# import training data coded by both raters and bind together ---------------------------
training_data <- readRDS("./data/classification/training_data")
training_data_rater2 <- readRDS("./data/classification/training_data_rater2")
training_data_both <- cbind(training_data, training_data_rater2)
colnames(training_data_both)[5:8] <- c("ids_2", "created_2", "text_2", "political_2")

# assess interrater reliability ---------------------------------------------------------
# cohen's kappa
kappa_score <- cohen.kappa(x = training_data_both[, c("political","political_2")])$kappa
# 95 % confidence bands
kappa__cis <- cohen.kappa(x = training_data_both[, c("political","political_2")])$confid
# confusion matrix
rater_cm <- cohen.kappa(x = training_data_both[, c("political","political_2")])$agree


#### CLASSIFY DATA FOR ANALYSIS BASED ON SELECTED CLASSIFIER ============================

# import processed data for analysis and apply selected dictionary ----------------------
analysis_categorized <- applyDict(data = readRDS("./data/analysis/analysis_processed"), 
                                  varname = "polact", slicesize = 100000,
                                  dictionary_uni = dict_filtered_3_uni,
                                  dictionary_bi = dict_filtered_3_bi, 
                                  dictionary_tri = dict_filtered_3_tri)

# combine ngram-based classification into binary measure of political participation -----
analysis_categorized$polact <- ifelse(analysis_categorized$polact_uni > 0 |
                                      analysis_categorized$polact_bi > 0 |
                                      analysis_categorized$polact_tri > 0, 1, 0)
analysis_categorized <- filter(analysis_categorized, !created < date("2018-08-01"))
saveRDS(analysis_categorized, "./data/analysis/analysis_categorized")

# summarize political participation on person-level for various time periods and
# under different assumptions defining political participation --------------------------
# a1 - political participation = 1 occurence of polact in the respective period
# a2 - political participation = 5 occurences of polact in the respective period
# a3 - political participation = 10 occurences of polact in the respective period
# a4 - political participation = 25 occurences of polact in the respective period
analysis_categorized_full_a1 <- analysis_categorized %>% # covering all periods, a1
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_full_a1"))
analysis_categorized_full_a2 <- analysis_categorized %>% # covering all periods, a2
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_full_a2"))
analysis_categorized_full_a3 <- analysis_categorized %>% # covering all periods, a3
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_full_a3"))
analysis_categorized_full_a4 <- analysis_categorized %>% # covering all periods, a4
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_full_a4"))
analysis_categorized_082018_a1 <- analysis_categorized %>% # covering august 2018, a1
  filter(created >= date("2018-08-01") & created <= date("2018-08-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_082018_a1"))
analysis_categorized_082018_a2 <- analysis_categorized %>% # covering august 2018, a2
  filter(created >= date("2018-08-01") & created <= date("2018-08-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_082018_a2"))
analysis_categorized_082018_a3 <- analysis_categorized %>% # covering august 2018, a3
  filter(created >= date("2018-08-01") & created <= date("2018-08-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_082018_a3"))
analysis_categorized_082018_a4 <- analysis_categorized %>% # covering august 2018, a4
  filter(created >= date("2018-08-01") & created <= date("2018-08-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_082018_a4"))
analysis_categorized_092018_a1 <- analysis_categorized %>% # covering september 2018, a1
  filter(created >= date("2018-09-01") & created <= date("2018-09-30")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_092018_a1"))
analysis_categorized_092018_a2 <- analysis_categorized %>% # covering september 2018, a2
  filter(created >= date("2018-09-01") & created <= date("2018-09-30")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_092018_a2"))
analysis_categorized_092018_a3 <- analysis_categorized %>% # covering september 2018, a3
  filter(created >= date("2018-09-01") & created <= date("2018-09-30")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_092018_a3"))
analysis_categorized_092018_a4 <- analysis_categorized %>% # covering september 2018, a4
  filter(created >= date("2018-09-01") & created <= date("2018-09-30")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_092018_a4"))
analysis_categorized_102018_a1 <- analysis_categorized %>% # covering october 2018, a1
  filter(created >= date("2018-10-01") & created <= date("2018-10-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_102018_a1"))
analysis_categorized_102018_a2 <- analysis_categorized %>% # covering october 2018, a2
  filter(created >= date("2018-10-01") & created <= date("2018-10-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_102018_a2"))
analysis_categorized_102018_a3 <- analysis_categorized %>% # covering october 2018, a3
  filter(created >= date("2018-10-01") & created <= date("2018-10-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_102018_a3"))
analysis_categorized_102018_a4 <- analysis_categorized %>% # covering october 2018, a4
  filter(created >= date("2018-10-01") & created <= date("2018-10-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_102018_a4"))
analysis_categorized_112018_a1 <- analysis_categorized %>% # covering november 2018, a1
  filter(created >= date("2018-11-01") & created <= date("2018-11-30")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_112018_a1"))
analysis_categorized_112018_a2 <- analysis_categorized %>% # covering november 2018, a2
  filter(created >= date("2018-11-01") & created <= date("2018-11-30")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_112018_a2"))
analysis_categorized_112018_a3 <- analysis_categorized %>% # covering november 2018, a3
  filter(created >= date("2018-11-01") & created <= date("2018-11-30")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_112018_a3"))
analysis_categorized_112018_a4 <- analysis_categorized %>% # covering november 2018, a4
  filter(created >= date("2018-11-01") & created <= date("2018-11-30")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_112018_a4"))
analysis_categorized_122018_a1 <- analysis_categorized %>% # covering december 2018, a1
  filter(created >= date("2018-12-01") & created <= date("2018-12-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_122018_a1"))
analysis_categorized_122018_a2 <- analysis_categorized %>% # covering december 2018, a2
  filter(created >= date("2018-12-01") & created <= date("2018-12-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_122018_a2"))
analysis_categorized_122018_a3 <- analysis_categorized %>% # covering december 2018, a3
  filter(created >= date("2018-12-01") & created <= date("2018-12-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_122018_a3"))
analysis_categorized_122018_a4 <- analysis_categorized %>% # covering december 2018, a4
  filter(created >= date("2018-12-01") & created <= date("2018-12-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_122018_a4"))
analysis_categorized_012019_a1 <- analysis_categorized %>% # covering january 2019, a1
  filter(created >= date("2019-01-01") & created <= date("2019-01-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_012019_a1"))
analysis_categorized_012019_a2 <- analysis_categorized %>% # covering january 2019, a2
  filter(created >= date("2019-01-01") & created <= date("2019-01-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact)  %>%
  set_colnames(c("twitter_id", "polact_012019_a2"))
analysis_categorized_012019_a3 <- analysis_categorized %>% # covering january 2019, a3
  filter(created >= date("2019-01-01") & created <= date("2019-01-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact)  %>%
  set_colnames(c("twitter_id", "polact_012019_a3"))
analysis_categorized_012019_a4 <- analysis_categorized %>% # covering january 2019, a4
  filter(created >= date("2019-01-01") & created <=date("2019-01-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_012019_a4"))
analysis_categorized_022019_a1 <- analysis_categorized %>% # covering february 2019, a1
  filter(created >= date("2019-02-01") & created <= date("2019-02-28")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_022019_a1"))
analysis_categorized_022019_a2 <- analysis_categorized %>% # covering february 2019, a2
  filter(created >= date("2019-02-01") & created <= date("2019-02-28")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_022019_a2"))
analysis_categorized_022019_a3 <- analysis_categorized %>% # covering february 2019, a3
  filter(created >= date("2019-02-01") & created <= date("2019-02-28")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_022019_a3"))
analysis_categorized_022019_a4 <- analysis_categorized %>% # covering february 2019, a4
  filter(created >= date("2019-02-01") & created <= date("2019-02-28")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_022019_a4"))
analysis_categorized_032019_a1 <- analysis_categorized %>% # covering march 2019, a1
  filter(created >= date("2019-03-01") & created <= date("2019-03-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_032019_a1"))
analysis_categorized_032019_a2 <- analysis_categorized %>% # covering march 2019, a2
  filter(created >= date("2019-03-01") & created <= date("2019-03-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_032019_a2"))
analysis_categorized_032019_a3 <- analysis_categorized %>% # covering march 2019, a3
  filter(created >= date("2019-03-01") & created <= date("2019-03-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_032019_a3"))
analysis_categorized_032019_a4 <- analysis_categorized %>% # covering march 2019, a4
  filter(created >= date("2019-03-01") & created <= date("2019-03-31")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_032019_a4"))
analysis_categorized_elw_a1 <- analysis_categorized %>% # covering midterm election week 2019, a1
  filter(created >= date("2018-11-05") & created <= date("2018-11-11")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elw_a1"))
analysis_categorized_elw_a2 <- analysis_categorized %>% # covering midterm election week 2019, a2
  filter(created >= date("2018-11-05") & created <= date("2018-11-11")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elw_a2"))
analysis_categorized_elw_a3 <- analysis_categorized %>% # covering midterm election week 2019, a3
  filter(created >= date("2018-11-05") & created <= date("2018-11-11")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elw_a3"))
analysis_categorized_elw_a4 <- analysis_categorized %>% # covering midterm election week 2019, a4
  filter(created >= date("2018-11-05") & created <= date("2018-11-11")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elw_a4"))
analysis_categorized_elwm1_a1 <- analysis_categorized %>% # covering one week prior to midterm election week 2018, a1
  filter(created >= date("2018-10-29") & created <= date("2018-11-04")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm1_a1"))
analysis_categorized_elwm1_a2 <- analysis_categorized %>% # covering one week prior to midterm election week 2019, a2
  filter(created >= date("2018-10-29") & created <= date("2018-11-04")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm1_a2"))
analysis_categorized_elwm1_a3 <- analysis_categorized %>% # covering one week prior to midterm election week 2019, a3
  filter(created >= date("2018-10-29") & created <= date("2018-11-04")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm1_a3"))
analysis_categorized_elwm1_a4 <- analysis_categorized %>% # covering one week prior to midterm election week 2019, a4
  filter(created >= date("2018-10-29") & created <= date("2018-11-04")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm1_a4"))
analysis_categorized_elwm2_a1 <- analysis_categorized %>% # covering two weeks prior to midterm election week 2018, a1
  filter(created >= date("2018-10-22") & created <= date("2018-10-28")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm2_a1"))
analysis_categorized_elwm2_a2 <- analysis_categorized %>% # covering two weeks prior to midterm election week 2019, a2
  filter(created >= date("2018-10-22") & created <= date("2018-10-28")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm2_a2"))
analysis_categorized_elwm2_a3 <- analysis_categorized %>% # covering two weeks prior to midterm election week 2019, a3
  filter(created >= date("2018-10-22") & created <= date("2018-10-28")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm2_a3"))
analysis_categorized_elwm2_a4 <- analysis_categorized %>% # covering two weeks prior to midterm election week 2019, a4
  filter(created >= date("2018-10-22") & created <= date("2018-10-28")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm2_a4"))
analysis_categorized_elwm3_a1 <- analysis_categorized %>% # covering three weeks prior to midterm election week 2018, a1
  filter(created >= date("2018-10-15") & created <= date("2018-10-21")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm3_a1"))
analysis_categorized_elwm3_a2 <- analysis_categorized %>% # covering three weeks prior to midterm election week 2019, a2
  filter(created >= date("2018-10-15") & created <= date("2018-10-21")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm3_a2"))
analysis_categorized_elwm3_a3 <- analysis_categorized %>% # covering three weeks prior to midterm election week 2019, a3
  filter(created >= date("2018-10-15") & created <= date("2018-10-21")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm3_a3"))
analysis_categorized_elwm3_a4 <- analysis_categorized %>% # covering three weeks prior to midterm election week 2019, a4
  filter(created >= date("2018-10-15") & created <= date("2018-10-21")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm3_a4"))
analysis_categorized_elwm4_a1 <- analysis_categorized %>% # covering four weeks prior to midterm election week 2018, a1
  filter(created >= date("2018-10-08") & created <= date("2018-10-14")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm4_a1"))
analysis_categorized_elwm4_a2 <- analysis_categorized %>% # covering four weeks prior to midterm election week 2019, a2
  filter(created >= date("2018-10-08") & created <= date("2018-10-14")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm4_a2"))
analysis_categorized_elwm4_a3 <- analysis_categorized %>% # covering four weeks prior to midterm election week 2019, a3
  filter(created >= date("2018-10-08") & created <= date("2018-10-14")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm4_a3"))
analysis_categorized_elwm4_a4 <- analysis_categorized %>% # covering four weeks prior to midterm election week 2019, a4
  filter(created >= date("2018-10-08") & created <= date("2018-10-14")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm4_a4"))
analysis_categorized_elwm5_a1 <- analysis_categorized %>% # covering five weeks prior to midterm election week 2018, a1
  filter(created >= date("2018-10-01") & created <= date("2018-10-07")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm5_a1"))
analysis_categorized_elwm5_a2 <- analysis_categorized %>% # covering five weeks prior to midterm election week 2019, a2
  filter(created >= date("2018-10-01") & created <= date("2018-10-07")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm5_a2"))
analysis_categorized_elwm5_a3 <- analysis_categorized %>% # covering five weeks prior to midterm election week 2019, a3
  filter(created >= date("2018-10-01") & created <= date("2018-10-07")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm5_a3"))
analysis_categorized_elwm5_a4 <- analysis_categorized %>% # covering five weeks prior to midterm election week 2019, a4
  filter(created >= date("2018-10-01") & created <= date("2018-10-07")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwm5_a4"))
analysis_categorized_elwp1_a1 <- analysis_categorized %>% # covering one week after midterm election week 2018, a1
  filter(created >= date("2018-11-12") & created <= date("2018-11-18")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp1_a1"))
analysis_categorized_elwp1_a2 <- analysis_categorized %>% # covering one week after midterm election week 2019, a2
  filter(created >= date("2018-11-12") & created <= date("2018-11-18")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp1_a2"))
analysis_categorized_elwp1_a3 <- analysis_categorized %>% # covering one week after midterm election week 2019, a3
  filter(created >= date("2018-11-12") & created <= date("2018-11-18")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp1_a3"))
analysis_categorized_elwp1_a4 <- analysis_categorized %>% # covering one week after midterm election week 2019, a4
  filter(created >= date("2018-11-12") & created <= date("2018-11-18")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp1_a4"))
analysis_categorized_elwp2_a1 <- analysis_categorized %>% # covering two weeks after midterm election week 2018, a1
  filter(created >= date("2018-11-19") & created <= date("2018-11-25")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp2_a1"))
analysis_categorized_elwp2_a2 <- analysis_categorized %>% # covering two weeks after midterm election week 2019, a2
  filter(created >= date("2018-11-19") & created <= date("2018-11-25")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp2_a2"))
analysis_categorized_elwp2_a3 <- analysis_categorized %>% # covering two weeks after midterm election week 2019, a3
  filter(created >= date("2018-11-19") & created <= date("2018-11-25")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp2_a3"))
analysis_categorized_elwp2_a4 <- analysis_categorized %>% # covering two weeks after midterm election week 2019, a4
  filter(created >= date("2018-11-19") & created <= date("2018-11-25")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp2_a4"))
analysis_categorized_elwp3_a1 <- analysis_categorized %>% # covering three weeks after midterm election week 2018, a1
  filter(created >= date("2018-11-26") & created <= date("2018-12-02")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp3_a1"))
analysis_categorized_elwp3_a2 <- analysis_categorized %>% # covering three weeks after midterm election week 2019, a2
  filter(created >= date("2018-11-26") & created <= date("2018-12-02")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp3_a2"))
analysis_categorized_elwp3_a3 <- analysis_categorized %>% # covering three weeks after midterm election week 2019, a3
  filter(created >= date("2018-11-26") & created <= date("2018-12-02")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp3_a3"))
analysis_categorized_elwp3_a4 <- analysis_categorized %>% # covering three weeks after midterm election week 2019, a4
  filter(created >= date("2018-11-26") & created <= date("2018-12-02")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp3_a4"))
analysis_categorized_elwp4_a1 <- analysis_categorized %>% # covering four weeks after midterm election week 2018, a1
  filter(created >= date("2018-12-03") & created <= date("2018-12-09")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp4_a1"))
analysis_categorized_elwp4_a2 <- analysis_categorized %>% # covering four weeks after midterm election week 2019, a2
  filter(created >= date("2018-12-03") & created <= date("2018-12-09")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp4_a2"))
analysis_categorized_elwp4_a3 <- analysis_categorized %>% # covering four weeks after midterm election week 2019, a3
  filter(created >= date("2018-12-03") & created <= date("2018-12-09")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp4_a3"))
analysis_categorized_elwp4_a4 <- analysis_categorized %>% # covering four weeks after midterm election week 2019, a4
  filter(created >= date("2018-12-03") & created <= date("2018-12-09")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp4_a4"))
analysis_categorized_elwp5_a1 <- analysis_categorized %>% # covering four weeks after midterm election week 2018, a1
  filter(created >= date("2018-12-10") & created <= date("2018-12-16")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq > 0, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp5_a1"))
analysis_categorized_elwp5_a2 <- analysis_categorized %>% # covering four weeks after midterm election week 2019, a2
  filter(created >= date("2018-12-10") & created <= date("2018-12-16")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 5, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp5_a2"))
analysis_categorized_elwp5_a3 <- analysis_categorized %>% # covering four weeks after midterm election week 2019, a3
  filter(created >= date("2018-12-10") & created <= date("2018-12-16")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 10, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp5_a3"))
analysis_categorized_elwp5_a4 <- analysis_categorized %>% # covering four weeks after midterm election week 2019, a4
  filter(created >= date("2018-12-10") & created <= date("2018-12-16")) %>%
  group_by(t_ids) %>% 
  summarize(polact_freq = sum(polact)) %>%
  mutate(polact = ifelse(polact_freq >= 25, 1, 0)) %>%
  select(t_ids, polact) %>%
  set_colnames(c("twitter_id", "polact_elwp5_a4"))

# bind person-level data to the processed sample ----------------------------------------
sample_analysis <- readRDS("./data/analysis/sample_processed")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_full_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_082018_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_092018_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_102018_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_112018_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_122018_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_012019_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_022019_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_032019_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm5_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm4_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm3_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm2_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm1_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elw_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp1_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp2_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp3_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp4_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp5_a1,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_full_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_082018_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_092018_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_102018_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_112018_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_122018_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_012019_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_022019_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_032019_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm5_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm4_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm3_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm2_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm1_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elw_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp1_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp2_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp3_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp4_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp5_a2,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_full_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_082018_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_092018_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_102018_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_112018_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_122018_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_012019_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_022019_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_032019_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm5_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm4_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm3_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm2_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm1_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elw_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp1_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp2_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp3_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp4_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp5_a3,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_full_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_082018_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_092018_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_102018_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_112018_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_122018_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_012019_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_022019_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_032019_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm5_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm4_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm3_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm2_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwm1_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elw_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp1_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp2_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp3_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp4_a4,
                             by = "twitter_id")
sample_analysis <- left_join(x = sample_analysis, y = analysis_categorized_elwp5_a4,
                             by = "twitter_id")
sample_analysis[, 66:145][is.na(sample_analysis[, 66:145])] <- 0
saveRDS(sample_analysis, "./data/analysis/sample_analysis")
