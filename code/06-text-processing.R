# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Text processing script
# April 2019
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./data/social-media-activity/april2019/social-media-act.sqlite'
'./data/social-media-activity/april2019/social-media-search.sqlite'
'./data/classification/training_data'
'./data/classification/dict_init'
'./data/classification/dict_updated'
'./data/classification/test_data'
'./data/classification/validation_data'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/classification/reference_set/*.txt'
'./data/classification/search_set/*.txt''
'./data/classification/reference_set_updated/*.txt'
'./data/classification/training_data_processed'
'./data/classification/test_data_processed'
'./data/classification/validation_data_processed'
'./data/analysis//analysis_processed'
'./figures/preText_plot.pdf'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 46 - PREPARATIONS
Line 65 - PROCESS REFERENCE SET TEXT DATA FOR DICTIONARY APPROACH
Line 336 - PROCESS SEARCH SET TEXT DATA FOR DICTIONARY APPROACH
Line 684 - UPDATE REFERENCE SET TEXT DATA IN TANDEM WITH KEYWORD DETECTION
Line 936 - PROCESS TRAINING DATA FOR CLASSIFIER GENERATION
Line 1227 - PROCESS TEST DATA FOR CLASSIFIER TUNING
Line 1526 - PROCESS VALIDATION DATA FOR CLASSIFIER EVALUATION
Line 1820 - PROCESS DATA FOR ANALYSES
Line 2305 - ASSESS SENSITIVITY OF PREPROCESSING STEPS
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("D:/Sascha/projects/online-participation")

# install and load packages and import functions ----------------------------------------
source("./code/packages.R")
source("./code/functions.R")

# open connections to SQL databases -----------------------------------------------------
con_1 <- dbConnect(SQLite(), 
           dbname = "./data/social-media-activity/april2019/social-media-act.sqlite")
con_2 <- dbConnect(SQLite(), 
           dbname = "./data/social-media-activity/april2019/social-media-search.sqlite")


#### PROCESS REFERENCE SET TEXT DATA FOR DICTIONARY APPROACH ============================

# compile historical statuses data ------------------------------------------------------
# this is data from users twitter timeline before realtime data collection commenced
# first without @mentions to accurately predict the language of statuses
sql_dat_beginning <- sqlToR(connection = con_1, table = "beginning_statuses", 
                            columns = c(1,4,5,6,8), language = TRUE, rm_meta = TRUE, 
                            rm_emoji = TRUE, rm_urls = TRUE, rm_rt = TRUE, 
                            rm_mention = TRUE)
# store the language vector and remove statuses
sql_dat_language <- sql_dat_beginning$lang
rm(sql_dat_beginning)
# now compile with mentions and add language vector afterwards
sql_dat_beginning <- sqlToR(connection = con_1, table = "beginning_statuses", 
                            columns = c(1,4,5,6,8), language = FALSE, rm_meta = TRUE, 
                            rm_emoji = TRUE, rm_urls = TRUE, rm_rt = TRUE, 
                            rm_mention = FALSE)
sql_dat_beginning$language <- sql_dat_language
rm(sql_dat_language)

# compile realtime statuses data --------------------------------------------------------
# this is data from users twitter timeline collected in realtime since August 2018
# first without @mentions to accurately predict the language of statuses
sql_dat_change <- sqlToR(connection = con_1, table = "change_statuses", 
                         columns = c(1,4,5,6,8), language = TRUE, rm_meta = TRUE, 
                         rm_emoji = TRUE, rm_urls = TRUE, rm_rt = TRUE, 
                         rm_mention = TRUE)
# store the language vector and remove statuses
sql_dat_language <- sql_dat_change$lang
rm(sql_dat_change)
# now compile with mentions and add language vector afterwards
sql_dat_change <- sqlToR(connection = con_1, table = "change_statuses", 
                         columns = c(1,4,5,6,8), language = FALSE, rm_meta = TRUE, 
                         rm_emoji = TRUE, rm_urls = TRUE, rm_rt = TRUE, 
                         rm_mention = FALSE)
sql_dat_change$language <- sql_dat_language
rm(sql_dat_language)

# row bind historical and realtime statuses data ----------------------------------------
sql_dat_statuses <- rbind(sql_dat_beginning, sql_dat_change)
rm(sql_dat_beginning)
rm(sql_dat_change)
# March 2018 -  10,773,862 statuses

# import training data and select reference set (entries coded as political) ------------
reference_set_ids <- readRDS("./data/classification/training_data")
reference_set_ids <- reference_set_ids[which(reference_set_ids$political == 1),]$ids
reference_set <- sql_dat_statuses[sql_dat_statuses$ids %in% reference_set_ids,]

# replace '#' and '@' with unique string to preserve during text pre-processing ---------
hash_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                     pattern = "[A-Za-z]"), " ")
at_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                   pattern = "[A-Za-z]"), " ")
reference_set$text <- str_replace_all(reference_set$text, "#", hash_string)
reference_set$text <- str_replace_all(reference_set$text, "@", at_string)

# build language-specific text corpora from the reference set ---------------------------
# prepare for tranformation into text corpora
colnames(reference_set)[3] <- c("doc_id")
reference_set <- select(reference_set, doc_id, text, everything())
# prepare language specific subsets for which stopwords are available
r_arab <- reference_set[reference_set$language == "ARABIC",]
r_beng <- reference_set[reference_set$language == "BENGALI",]
r_chin <- reference_set[reference_set$language == "CHINESE",]
r_czech <- reference_set[reference_set$language == "CZECH",]
r_dut <- reference_set[reference_set$language == "DUTCH",]
r_eng <- reference_set[reference_set$language == "ENGLISH",]
r_fre <- reference_set[reference_set$language == "FRENCH",]
r_ger <- reference_set[reference_set$language == "GERMAN",]
r_gre <- reference_set[reference_set$language == "GREEK",]
r_heb <- reference_set[reference_set$language == "HEBREW",]
r_hun <- reference_set[reference_set$language == "HUNGARIAN",]
r_ind <- reference_set[reference_set$language == "INDONESIAN",]
r_ita <- reference_set[reference_set$language == "ITALIAN",]
r_jap <- reference_set[reference_set$language == "JAPANESE",]
r_pers <- reference_set[reference_set$language == "PERSIAN",]
r_pol <- reference_set[reference_set$language == "POLISH",]
r_por <- reference_set[reference_set$language == "PORTUGUESE",]
r_rus <- reference_set[reference_set$language == "RUSSIAN",]
r_spa <- reference_set[reference_set$language == "SPANISH",]
r_tur <- reference_set[reference_set$language == "TURKISH",]
# tranform into corpora
cr_arab <- DataframeSource(r_arab[,1:2]) %>% VCorpus()
cr_beng <- DataframeSource(r_beng[,1:2]) %>% VCorpus()
cr_chin <- DataframeSource(r_chin[,1:2]) %>% VCorpus()
cr_czech <- DataframeSource(r_czech[,1:2]) %>% VCorpus()
cr_dut <- DataframeSource(r_dut[,1:2]) %>% VCorpus()
cr_eng <- DataframeSource(r_eng[,1:2]) %>% VCorpus()
cr_fre <- DataframeSource(r_fre[,1:2]) %>% VCorpus()
cr_ger <- DataframeSource(r_ger[,1:2]) %>% VCorpus()
cr_gre <- DataframeSource(r_gre[,1:2]) %>% VCorpus()
cr_heb <- DataframeSource(r_heb[,1:2]) %>% VCorpus()
cr_hun <- DataframeSource(r_hun[,1:2]) %>% VCorpus()
cr_ind <- DataframeSource(r_ind[,1:2]) %>% VCorpus()
cr_ita <- DataframeSource(r_ita[,1:2]) %>% VCorpus()
cr_jap <- DataframeSource(r_jap[,1:2]) %>% VCorpus()
cr_pers <- DataframeSource(r_pers[,1:2]) %>% VCorpus()
cr_pol <- DataframeSource(r_pol[,1:2]) %>% VCorpus()
cr_por <- DataframeSource(r_por[,1:2]) %>% VCorpus()
cr_rus <- DataframeSource(r_rus[,1:2]) %>% VCorpus()
cr_spa <- DataframeSource(r_spa[,1:2]) %>% VCorpus()
cr_tur <- DataframeSource(r_tur[,1:2]) %>% VCorpus()

# process reference set texts -----------------------------------------------------------
cr_arab <- cr_arab %>%
  tm_map(removeWords, stopwords(language = "ar", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_arab$text_pro <- cr_arab
cr_beng <- cr_beng %>%
  tm_map(removeWords, stopwords(language = "bn", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_beng$text_pro <- cr_beng
cr_chin <- cr_chin %>%
  tm_map(removeWords, stopwords(language = "zh", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_chin$text_pro <- cr_chin
cr_czech <- cr_czech %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "cs", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_czech$text_pro <- cr_czech
cr_dut <- cr_dut %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "nl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_dut$text_pro <- cr_dut
cr_eng <- cr_eng %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "en")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_eng$text_pro <- str_replace_all(cr_eng, "[:punct:]", "")
cr_fre <- cr_fre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "fr", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_fre$text_pro <- str_replace_all(cr_fre, "[:punct:]", "")
cr_ger <- cr_ger %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "de", source = "snowball")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_ger$text_pro <- str_replace_all(cr_ger, "[:punct:]", "")
cr_gre <- cr_gre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "el", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_gre$text_pro <- str_replace_all(cr_gre, "[:punct:]", "")
cr_heb <- cr_heb %>%
  tm_map(removeWords, stopwords(language = "he", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_heb$text_pro <- str_replace_all(cr_heb, "[:punct:]", "")
cr_hun <- cr_hun %>%
  tm_map(removeWords, stopwords(language = "hu", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_hun$text_pro <- str_replace_all(cr_hun, "[:punct:]", "")
cr_ind <- cr_ind %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "id", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_ind$text_pro <- str_replace_all(cr_ind, "[:punct:]", "")
cr_ita <- cr_ita %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "it", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_ita$text_pro <- str_replace_all(cr_ita, "[:punct:]", "")
cr_jap <- cr_jap %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ja", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_jap$text_pro <- str_replace_all(cr_jap, "[:punct:]", "")
cr_pers <- cr_pers %>%
  tm_map(removeWords, stopwords(language = "fa", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_pers$text_pro <- cr_pers
cr_pol <- cr_pol %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_pol$text_pro <- str_replace_all(cr_pol, "[:punct:]", "")
cr_por <- cr_por %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pt", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_por$text_pro <- str_replace_all(cr_por, "[:punct:]", "")
cr_rus <- cr_rus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ru", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_rus$text_pro <- str_replace_all(cr_rus, "[:punct:]", "")
cr_spa <- cr_spa %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "es", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_spa$text_pro <- str_replace_all(cr_spa, "[:punct:]", "")
cr_tur <- cr_tur %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "tr", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_tur$text_pro <- str_replace_all(cr_tur, "[:punct:]", "")

# rbind processed text of the reference set and restore '#' and '@' ---------------------
reference_set_pro <- rbind(r_arab, r_beng, r_chin, r_czech, r_dut, r_eng, r_fre, r_ger,
                           r_gre, r_heb, r_hun, r_ind, r_ita, r_jap, r_pers, r_pol, 
                           r_por, r_rus, r_spa, r_tur)
reference_set_pro$text_pro <- str_replace_all(reference_set_pro$text_pro, 
                                              paste0(tolower(hash_string),"|", 
                                                     hash_string), " #")
reference_set_pro$text_pro <- str_replace_all(reference_set_pro$text_pro, 
                                              paste0(tolower(at_string),"|", 
                                                     at_string), " @")
reference_set_pro$text_pro <- str_trim(reference_set_pro$text_pro)
reference_set_pro$text_pro <- str_squish(reference_set_pro$text_pro)
reference_set_pro <- select(reference_set_pro, GUID = doc_id, contents = text_pro)

# write reference set to .txt in directory ----------------------------------------------
for(i in 1:length(reference_set_pro$contents)) {
  writeLines(reference_set_pro$contents[i], 
             str_c("./data/classification/reference_set/",i, ".txt"), useBytes=T)
}


#### PROCESS SEARCH SET TEXT DATA FOR DICTIONARY APPROACH ===============================

# compile search set statuses data ------------------------------------------------------
# the search set was compiled by querying the Twitter Streaming API daily for status 
# updates occuring in Florida 
sql_dat_search <- sqlToR(connection = con_2, table = "search_stream", 
                         columns = c(1,2,3,4), language = TRUE, rm_meta = TRUE, 
                         rm_emoji = TRUE, rm_urls = TRUE, rm_rt = TRUE, 
                         rm_mention = TRUE)
# store the language vector and remove statuses
sql_dat_language <- sql_dat_search$lang
rm(sql_dat_search)
# now compile with mentions and add language vector afterwards
sql_dat_search <- sqlToR(connection = con_2, table = "search_stream", 
                         columns = c(1,2,3,4), language = FALSE, rm_meta = TRUE, 
                         rm_emoji = TRUE, rm_urls = TRUE, rm_rt = TRUE, 
                         rm_mention = FALSE)
sql_dat_search$language <- sql_dat_language
rm(sql_dat_language)
# March 2018 -  728,089 statuses

# replace '#' and '@' with unique string to preserve during text pre-processing ---------
hash_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                     pattern = "[A-Za-z]"), " ")
at_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                   pattern = "[A-Za-z]"), " ")
sql_dat_search$text <- str_replace_all(sql_dat_search$text, "#", hash_string)
sql_dat_search$text <- str_replace_all(sql_dat_search$text, "@", at_string)

# build language-specific text corpora from the reference set ---------------------------
# prepare for tranformation into text corpora
colnames(sql_dat_search)[3] <- c("doc_id")
sql_dat_search <- select(sql_dat_search, doc_id, text, everything())
# drop statuses in extremely rarely occuring languages or languages that are wrongly 
# assigned
sql_dat_search <- sql_dat_search[which(sql_dat_search$language %ni% 
                                 c("INUKTITUT", "KINYARWANDA", "LAOTHIAN", "MALTESE", 
                                   "MARATHI", "SINHALESE", "BELARUSIAN","GEORGIAN",
                                   "TELUGU", "BULGARIAN")),]
# take large random sample of size ~100,000 heeding each language
search_set <- sql_dat_search %>% group_by(language) %>% 
  sample_n(54000, replace = TRUE) %>% ungroup
search_set <- search_set[-which(duplicated(search_set$doc_id)),]
# prepare language specific subsets for which stopwords are available
s_arab <- search_set[search_set$language == "ARABIC",]
s_beng <- search_set[search_set$language == "BENGALI",]
s_chin <- search_set[search_set$language == "CHINESE",]
s_czech <- search_set[search_set$language == "CZECH",]
s_dut <- search_set[search_set$language == "DUTCH",]
s_eng <- search_set[search_set$language == "ENGLISH",]
s_eng$text <- iconv(s_eng$text, "latin1", "ASCII", sub = " ")
s_fre <- search_set[search_set$language == "FRENCH",]
s_ger <- search_set[search_set$language == "GERMAN",]
s_gre <- search_set[search_set$language == "GREEK",]
s_hac <- search_set[search_set$language == "HAITIAN_CREOLE",]
s_heb <- search_set[search_set$language == "HEBREW",]
s_hin <- search_set[search_set$language == "HINDI",]
s_hun <- search_set[search_set$language == "HUNGARIAN",]
s_ind <- search_set[search_set$language == "INDONESIAN",]
s_ita <- search_set[search_set$language == "ITALIAN",]
s_jap <- search_set[search_set$language == "JAPANESE",]
s_kan <- search_set[search_set$language == "KANNADA",]
s_kor <- search_set[search_set$language == "KOREAN",]
s_mac <- search_set[search_set$language == "MACEDONIAN",]
s_may <- search_set[search_set$language == "MALAYALAM",]
s_nep <- search_set[search_set$language == "NEPALI",]
s_pers <- search_set[search_set$language == "PERSIAN",]
s_pol <- search_set[search_set$language == "POLISH",]
s_por <- search_set[search_set$language == "PORTUGUESE",]
s_por$text <- iconv(s_por$text, "latin1", "ASCII", sub = " ")
s_rus <- search_set[search_set$language == "RUSSIAN",]
s_ser <- search_set[search_set$language == "SERBIAN",]
s_spa <- search_set[search_set$language == "SPANISH",]
s_spa$text <- iconv(s_spa$text, "latin1", "ASCII", sub = " ")
s_tam <- search_set[search_set$language == "TAMIL",]
s_tha <- search_set[search_set$language == "THAI",]
s_tur <- search_set[search_set$language == "TURKISH",]
s_urd <- search_set[search_set$language == "URDU",]
# tranform into corpora
cs_arab <- DataframeSource(s_arab[,1:2]) %>% VCorpus()
cs_beng <- DataframeSource(s_beng[,1:2]) %>% VCorpus()
cs_chin <- DataframeSource(s_chin[,1:2]) %>% VCorpus()
cs_czech <- DataframeSource(s_czech[,1:2]) %>% VCorpus()
cs_dut <- DataframeSource(s_dut[,1:2]) %>% VCorpus()
cs_eng <- DataframeSource(s_eng[,1:2]) %>% VCorpus()
cs_fre <- DataframeSource(s_fre[,1:2]) %>% VCorpus()
cs_ger <- DataframeSource(s_ger[,1:2]) %>% VCorpus()
cs_gre <- DataframeSource(s_gre[,1:2]) %>% VCorpus()
cs_hac <- DataframeSource(s_hac[,1:2]) %>% VCorpus()
cs_heb <- DataframeSource(s_heb[,1:2]) %>% VCorpus()
cs_hin <- DataframeSource(s_hin[,1:2]) %>% VCorpus()
cs_hun <- DataframeSource(s_hun[,1:2]) %>% VCorpus()
cs_ind <- DataframeSource(s_ind[,1:2]) %>% VCorpus()
cs_ita <- DataframeSource(s_ita[,1:2]) %>% VCorpus()
cs_jap <- DataframeSource(s_jap[,1:2]) %>% VCorpus()
cs_kan <- DataframeSource(s_kan[,1:2]) %>% VCorpus()
cs_kor <- DataframeSource(s_kor[,1:2]) %>% VCorpus()
cs_mac <- DataframeSource(s_mac[,1:2]) %>% VCorpus()
cs_may <- DataframeSource(s_may[,1:2]) %>% VCorpus()
cs_nep <- DataframeSource(s_nep[,1:2]) %>% VCorpus()
cs_pers <- DataframeSource(s_pers[,1:2]) %>% VCorpus()
cs_pol <- DataframeSource(s_pol[,1:2]) %>% VCorpus()
cs_por <- DataframeSource(s_por[,1:2]) %>% VCorpus()
cs_rus <- DataframeSource(s_rus[,1:2]) %>% VCorpus()
cs_ser <- DataframeSource(s_ser[,1:2]) %>% VCorpus()
cs_spa <- DataframeSource(s_spa[,1:2]) %>% VCorpus()
cs_tam <- DataframeSource(s_tam[,1:2]) %>% VCorpus()
cs_tha <- DataframeSource(s_tha[,1:2]) %>% VCorpus()
cs_tur <- DataframeSource(s_tur[,1:2]) %>% VCorpus()
cs_urd <- DataframeSource(s_urd[,1:2]) %>% VCorpus()

# process search set texts --------------------------------------------------------------
cs_arab <- cs_arab %>%
  tm_map(removeWords, stopwords(language = "ar", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_arab$text_pro <- cs_arab
cs_beng <- cs_beng %>%
  tm_map(removeWords, stopwords(language = "bn", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_beng$text_pro <- cs_beng
cs_chin <- cs_chin %>%
  tm_map(removeWords, stopwords(language = "zh", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_chin$text_pro <- cs_chin
cs_czech <- cs_czech %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "cs", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_czech$text_pro <- cs_czech
cs_dut <- cs_dut %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "nl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_dut$text_pro <- cs_dut
cs_eng <- cs_eng %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "en")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_eng$text_pro <- str_replace_all(cs_eng, "[:punct:]", "")
cs_fre <- cs_fre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "fr", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_fre$text_pro <- str_replace_all(cs_fre, "[:punct:]", "")
cs_ger <- cs_ger %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "de", source = "snowball")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_ger$text_pro <- str_replace_all(cs_ger, "[:punct:]", "")
cs_gre <- cs_gre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "el", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_gre$text_pro <- str_replace_all(cs_gre, "[:punct:]", "")
cs_hac <- cs_hac %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_hac$text_pro <- str_replace_all(cs_hac, "[:punct:]", "")
cs_heb <- cs_heb %>%
  tm_map(removeWords, stopwords(language = "he", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_heb$text_pro <- str_replace_all(cs_heb, "[:punct:]", "")
cs_hin <- cs_hin %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "hi", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_hin$text_pro <- str_replace_all(cs_hin, "[:punct:]", "")
cs_hun <- cs_hun %>%
  tm_map(removeWords, stopwords(language = "hu", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_hun$text_pro <- str_replace_all(cs_hun, "[:punct:]", "")
cs_ind <- cs_ind %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "id", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_ind$text_pro <- str_replace_all(cs_ind, "[:punct:]", "")
cs_ita <- cs_ita %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "it", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_ita$text_pro <- str_replace_all(cs_ita, "[:punct:]", "")
cs_jap <- cs_jap %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ja", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_jap$text_pro <- str_replace_all(cs_jap, "[:punct:]", "")
cs_kan <- cs_kan %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_kan$text_pro <- str_replace_all(cs_kan, "[:punct:]", "")
cs_kor <- cs_kor %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_kor$text_pro <- str_replace_all(cs_kor, "[:punct:]", "")
cs_mac <- cs_mac %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_mac$text_pro <- str_replace_all(cs_mac, "[:punct:]", "")
cs_may <- cs_may %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_may$text_pro <- str_replace_all(cs_may, "[:punct:]", "")
cs_nep <- cs_nep %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_nep$text_pro <- str_replace_all(cs_nep, "[:punct:]", "")
cs_pers <- cs_pers %>%
  tm_map(removeWords, stopwords(language = "fa", source = "stopwords-iso")) %>% 
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_pers$text_pro <- cs_pers
cs_pol <- cs_pol %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_pol$text_pro <- str_replace_all(cs_pol, "[:punct:]", "")
cs_por <- cs_por %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pt", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_por$text_pro <- str_replace_all(cs_por, "[:punct:]", "")
cs_rus <- cs_rus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ru", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_rus$text_pro <- str_replace_all(cs_rus, "[:punct:]", "")
cs_ser <- cs_ser %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_ser$text_pro <- str_replace_all(cs_ser, "[:punct:]", "")
cs_spa <- cs_spa %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "es", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_spa$text_pro <- str_replace_all(cs_spa, "[:punct:]", "")
cs_tam <- cs_tam %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_tam$text_pro <- str_replace_all(cs_tam, "[:punct:]", "")
cs_tha <- cs_tha %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "th", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_tha$text_pro <- str_replace_all(cs_tha, "[:punct:]", "")
cs_tur <- cs_tur %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "tr", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_tur$text_pro <- str_replace_all(cs_tur, "[:punct:]", "")
cs_urd <- cs_urd %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ur", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
s_urd$text_pro <- str_replace_all(cs_urd, "[:punct:]", "")

# rbind processed text of the search set and restore '#' and '@' ------------------------
search_set_pro <- rbind(s_arab, s_beng, s_chin, s_czech, s_dut, s_eng, s_fre, s_ger,
                        s_gre, s_hac, s_heb, s_hin, s_hun, s_ind, s_ita, s_jap, s_kan,
                        s_kor, s_mac, s_may, s_nep, s_pers, s_pol, s_por, s_rus, 
                        s_ser, s_spa, s_tam, s_tha, s_tur, s_urd)
search_set_pro$text_pro <- str_replace_all(search_set_pro$text_pro, 
                                              paste0(tolower(hash_string),"|", 
                                                     hash_string), " #")
search_set_pro$text_pro <- str_replace_all(search_set_pro$text_pro, 
                                              paste0(tolower(at_string),"|", 
                                                     at_string), " @")
search_set_pro$text_pro <- str_trim(search_set_pro$text_pro)
search_set_pro$text_pro <- str_squish(search_set_pro$text_pro)
search_set_pro <- select(search_set_pro, GUID = doc_id, contents = text_pro)

# write reference set to .txt in directory ----------------------------------------------
for(i in 1:length(search_set_pro$contents)) {
  writeLines(search_set_pro$contents[i], 
             str_c("./data/classification/search_set/",i, ".txt"), useBytes=T)
}


#### UPDATE REFERENCE SET TEXT DATA IN TANDEM WITH KEYWORD DETECTION ====================
# Below code works in tandem with "07.text-classification.R". The above reference 
# and search sets where used to arrive at keywords that are here used to filter 
# additional statuses, which are then added to the reference set to update the keyword 
# detection algorithm in "07.text-classification.R". This is an iterative process, that
# is illustrated in the below code and in part of "07.text-classification.R".

# import dictionary built from initial reference and search sets ------------------------
# start here after having run "07-text-classification.R" for the initial set of documents
# or the updated set of documents if another iteration is desired
#dict <- readRDS("./data/classification/dict_init") # choose after initial run
dict <- readRDS("./data/classification/dict_updated") # choose after updated run

# take a large sample of statuses and retrive statuses that match the dictionary --------
statuses_sample <- sample_n(sql_dat_statuses, 100000)
statuses_sample <- statuses_sample[str_detect(statuses_sample$text, 
                                              regex(paste0(dict, collapse = "|"),
                                              ignore_case = TRUE)),]

# collect sample of statuses ------------------------------------------------------------
reference_set <- sample_n(statuses_sample, 2500)

# replace '#' and '@' with unique string to preserve during text pre-processing ---------
hash_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                     pattern = "[A-Za-z]"), " ")
at_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                   pattern = "[A-Za-z]"), " ")
reference_set$text <- str_replace_all(reference_set$text, "#", hash_string)
reference_set$text <- str_replace_all(reference_set$text, "@", at_string)

# build language-specific text corpora from the reference set ---------------------------
# adjust below as necessary specific to the updated reference set
# prepare for tranformation into text corpora
colnames(reference_set)[3] <- c("doc_id")
reference_set <- select(reference_set, doc_id, text, everything())
# drop statuses in extremely rarely occuring languages or languages that are wrongly 
# assigned
reference_set <- reference_set[which(reference_set$language %ni% 
                                         c("KINYARWANDA","TELUGU", "KOREAN")),]
# prepare language specific subsets for which stopwords are available
#r_arab <- reference_set[reference_set$language == "ARABIC",]
#r_beng <- reference_set[reference_set$language == "BENGALI",]
#r_chin <- reference_set[reference_set$language == "CHINESE",]
#r_czech <- reference_set[reference_set$language == "CZECH",]
#r_dut <- reference_set[reference_set$language == "DUTCH",]
r_eng <- reference_set[reference_set$language == "ENGLISH",]
r_eng$text <- iconv(r_eng$text, "latin1", "ASCII", sub = " ")
r_fre <- reference_set[reference_set$language == "FRENCH",]
r_ger <- reference_set[reference_set$language == "GERMAN",]
#r_gre <- reference_set[reference_set$language == "GREEK",]
#r_heb <- reference_set[reference_set$language == "HEBREW",]
#r_hun <- reference_set[reference_set$language == "HUNGARIAN",]
#r_ind <- reference_set[reference_set$language == "INDONESIAN",]
r_ita <- reference_set[reference_set$language == "ITALIAN",]
#r_jap <- reference_set[reference_set$language == "JAPANESE",]
#r_pers <- reference_set[reference_set$language == "PERSIAN",]
r_pol <- reference_set[reference_set$language == "POLISH",]
r_por <- reference_set[reference_set$language == "PORTUGUESE",]
#r_rus <- reference_set[reference_set$language == "RUSSIAN",]
r_spa <- reference_set[reference_set$language == "SPANISH",]
r_tur <- reference_set[reference_set$language == "TURKISH",]
# tranform into corpora
#cr_arab <- DataframeSource(r_arab[,1:2]) %>% VCorpus()
#cr_beng <- DataframeSource(r_beng[,1:2]) %>% VCorpus()
#cr_chin <- DataframeSource(r_chin[,1:2]) %>% VCorpus()
#cr_czech <- DataframeSource(r_czech[,1:2]) %>% VCorpus()
#cr_dut <- DataframeSource(r_dut[,1:2]) %>% VCorpus()
cr_eng <- DataframeSource(r_eng[,1:2]) %>% VCorpus()
cr_fre <- DataframeSource(r_fre[,1:2]) %>% VCorpus()
cr_ger <- DataframeSource(r_ger[,1:2]) %>% VCorpus()
#cr_gre <- DataframeSource(r_gre[,1:2]) %>% VCorpus()
#cr_heb <- DataframeSource(r_heb[,1:2]) %>% VCorpus()
#cr_hun <- DataframeSource(r_hun[,1:2]) %>% VCorpus()
#cr_ind <- DataframeSource(r_ind[,1:2]) %>% VCorpus()
cr_ita <- DataframeSource(r_ita[,1:2]) %>% VCorpus()
#cr_jap <- DataframeSource(r_jap[,1:2]) %>% VCorpus()
#cr_pers <- DataframeSource(r_pers[,1:2]) %>% VCorpus()
cr_pol <- DataframeSource(r_pol[,1:2]) %>% VCorpus()
cr_por <- DataframeSource(r_por[,1:2]) %>% VCorpus()
#cr_rus <- DataframeSource(r_rus[,1:2]) %>% VCorpus()
cr_spa <- DataframeSource(r_spa[,1:2]) %>% VCorpus()
cr_tur <- DataframeSource(r_tur[,1:2]) %>% VCorpus()

# process reference set texts -----------------------------------------------------------
#cr_arab <- cr_arab %>%
#  tm_map(removeWords, stopwords(language = "ar", source = "misc")) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_arab$text_pro <- cr_arab
#cr_beng <- cr_beng %>%
#  tm_map(removeWords, stopwords(language = "bn", source = "stopwords-iso")) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_beng$text_pro <- cr_beng
#cr_chin <- cr_chin %>%
#  tm_map(removeWords, stopwords(language = "zh", source = "misc")) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_chin$text_pro <- cr_chin
#cr_czech <- cr_czech %>%
#  tm_map(content_transformer(tolower)) %>%
#  tm_map(removeWords, stopwords(language = "cs", source = "stopwords-iso")) %>%
#  tm_map(removeNumbers) %>%
#  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_czech$text_pro <- cr_czech
#cr_dut <- cr_dut %>%
#  tm_map(content_transformer(tolower)) %>%
#  tm_map(removeWords, stopwords(language = "nl", source = "stopwords-iso")) %>%
#  tm_map(removeNumbers) %>%
#  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_dut$text_pro <- cr_dut
cr_eng <- cr_eng %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "en")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_eng$text_pro <- str_replace_all(cr_eng, "[:punct:]", "")
cr_fre <- cr_fre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "fr", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_fre$text_pro <- str_replace_all(cr_fre, "[:punct:]", "")
cr_ger <- cr_ger %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "de", source = "snowball")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_ger$text_pro <- str_replace_all(cr_ger, "[:punct:]", "")
#cr_gre <- cr_gre %>%
#  tm_map(content_transformer(tolower)) %>%
#  tm_map(removeWords, stopwords(language = "el", source = "stopwords-iso")) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_gre$text_pro <- str_replace_all(cr_gre, "[:punct:]", "")
#cr_heb <- cr_heb %>%
#  tm_map(removeWords, stopwords(language = "he", source = "stopwords-iso")) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_heb$text_pro <- str_replace_all(cr_heb, "[:punct:]", "")
#cr_hun <- cr_hun %>%
#  tm_map(removeWords, stopwords(language = "hu", source = "stopwords-iso")) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_hun$text_pro <- str_replace_all(cr_hun, "[:punct:]", "")
#cr_ind <- cr_ind %>%
#  tm_map(content_transformer(tolower)) %>%
#  tm_map(removeWords, stopwords(language = "id", source = "stopwords-iso")) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_ind$text_pro <- str_replace_all(cr_ind, "[:punct:]", "")
cr_ita <- cr_ita %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "it", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_ita$text_pro <- str_replace_all(cr_ita, "[:punct:]", "")
#cr_jap <- cr_jap %>%
#  tm_map(content_transformer(tolower)) %>%
#  tm_map(removeWords, stopwords(language = "ja", source = "stopwords-iso")) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_jap$text_pro <- str_replace_all(cr_jap, "[:punct:]", "")
#cr_pers <- cr_pers %>%
#  tm_map(removeWords, stopwords(language = "fa", source = "stopwords-iso")) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_pers$text_pro <- cr_pers
cr_pol <- cr_pol %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_pol$text_pro <- str_replace_all(cr_pol, "[:punct:]", "")
cr_por <- cr_por %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pt", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_por$text_pro <- str_replace_all(cr_por, "[:punct:]", "")
#cr_rus <- cr_rus %>%
#  tm_map(content_transformer(tolower)) %>%
#  tm_map(removeWords, stopwords(language = "ru", source = "stopwords-iso")) %>%
#  tm_map(stripWhitespace) %>%
#  sapply(`$.data.frame`, "content") %>%
#  unname()
#r_rus$text_pro <- str_replace_all(cr_rus, "[:punct:]", "")
cr_spa <- cr_spa %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "es", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_spa$text_pro <- str_replace_all(cr_spa, "[:punct:]", "")
cr_tur <- cr_tur %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "tr", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
r_tur$text_pro <- str_replace_all(cr_tur, "[:punct:]", "")

# rbind processed text of the reference set and restore '#' and '@' ---------------------
reference_set_pro <- rbind(r_eng, r_por, r_spa, r_tur)
reference_set_pro$text_pro <- str_replace_all(reference_set_pro$text_pro, 
                                              paste0(tolower(hash_string),"|", 
                                                     hash_string), " #")
reference_set_pro$text_pro <- str_replace_all(reference_set_pro$text_pro, 
                                              paste0(tolower(at_string),"|", 
                                                     at_string), " @")
reference_set_pro$text_pro <- str_trim(reference_set_pro$text_pro)
reference_set_pro$text_pro <- str_squish(reference_set_pro$text_pro)
reference_set_pro <- select(reference_set_pro, GUID = doc_id, contents = text_pro)

# update reference set documents with selected statuses ---------------------------------
# adjust NUMBER+i  after each iteration, whereby NUMBER is the current number of documents
# in the reference set
for(i in 1:length(reference_set_pro$contents)) {
  writeLines(reference_set_pro$contents[i], 
             str_c("./data/classification/reference_set_updated/",3021+i, ".txt"), useBytes=T)
}
# switch to "07-text-classification.R" and run computer-assisted keyword detection 
# algorithm on updated documents


#### PROCESS TRAINING DATA FOR CLASSIFIER GENERATION ====================================

# import test data ----------------------------------------------------------------------
training_data <- readRDS("./data/classification/training_data")
training_data <- select(training_data, ids, created, political)
training_data <- left_join(training_data, sql_dat_statuses[,c("text", "ids", "language")], 
                           by = "ids")

# replace '#' and '@' with unique string to preserve during text pre-processing ---------
hash_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                     pattern = "[A-Za-z]"), " ")
at_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                   pattern = "[A-Za-z]"), " ")
training_data$text <- str_replace_all(training_data$text, "#", hash_string)
training_data$text <- str_replace_all(training_data$text, "@", at_string)

# build language-specific text corpora from the test data -------------------------------
# prepare for tranformation into text corpora
colnames(training_data)[1] <- c("doc_id")
training_data <- select(training_data, doc_id, text, everything())
# prepare language specific subsets for which stopwords are available
tr_arab <- training_data[training_data$language == "ARABIC",]
tr_beng <- training_data[training_data$language == "BENGALI",]
tr_bulg <- training_data[training_data$language == "BULGARIAN",]
tr_chin <- training_data[training_data$language == "CHINESE",]
tr_czech <- training_data[training_data$language == "CZECH",]
tr_dut <- training_data[training_data$language == "DUTCH",]
tr_eng <- training_data[training_data$language == "ENGLISH",]
tr_fre <- training_data[training_data$language == "FRENCH",]
tr_ger <- training_data[training_data$language == "GERMAN",]
tr_gre <- training_data[training_data$language == "GREEK",]
tr_hac <- training_data[training_data$language == "HAITIAN_CREOLE",]
tr_heb <- training_data[training_data$language == "HEBREW",]
tr_hin <- training_data[training_data$language == "HINDI",]
tr_hun <- training_data[training_data$language == "HUNGARIAN",]
tr_ind <- training_data[training_data$language == "INDONESIAN",]
tr_ita <- training_data[training_data$language == "ITALIAN",]
tr_jap <- training_data[training_data$language == "JAPANESE",]
tr_kor <- training_data[training_data$language == "KOREAN",]
tr_pers <- training_data[training_data$language == "PERSIAN",]
tr_pol <- training_data[training_data$language == "POLISH",]
tr_por <- training_data[training_data$language == "PORTUGUESE",]
tr_rus <- training_data[training_data$language == "RUSSIAN",]
tr_ser <- training_data[training_data$language == "SERBIAN",]
tr_spa <- training_data[training_data$language == "SPANISH",]
tr_telu <- training_data[training_data$language == "TELUGU",]
tr_tha <- training_data[training_data$language == "THAI",]
tr_tur <- training_data[training_data$language == "TURKISH",]
tr_urd <- training_data[training_data$language == "URDU",]
# tranform into corpora
ctr_arab <- DataframeSource(tr_arab[,1:2]) %>% VCorpus()
ctr_beng <- DataframeSource(tr_beng[,1:2]) %>% VCorpus()
ctr_bulg <- DataframeSource(tr_bulg[,1:2]) %>% VCorpus()
ctr_chin <- DataframeSource(tr_chin[,1:2]) %>% VCorpus()
ctr_czech <- DataframeSource(tr_czech[,1:2]) %>% VCorpus()
ctr_dut <- DataframeSource(tr_dut[,1:2]) %>% VCorpus()
ctr_eng <- DataframeSource(tr_eng[,1:2]) %>% VCorpus()
ctr_fre <- DataframeSource(tr_fre[,1:2]) %>% VCorpus()
ctr_ger <- DataframeSource(tr_ger[,1:2]) %>% VCorpus()
ctr_gre <- DataframeSource(tr_gre[,1:2]) %>% VCorpus()
ctr_hac <- DataframeSource(tr_hac[,1:2]) %>% VCorpus()
ctr_heb <- DataframeSource(tr_heb[,1:2]) %>% VCorpus()
ctr_hin <- DataframeSource(tr_hin[,1:2]) %>% VCorpus()
ctr_hun <- DataframeSource(tr_hun[,1:2]) %>% VCorpus()
ctr_ind <- DataframeSource(tr_ind[,1:2]) %>% VCorpus()
ctr_ita <- DataframeSource(tr_ita[,1:2]) %>% VCorpus()
ctr_jap <- DataframeSource(tr_jap[,1:2]) %>% VCorpus()
ctr_kor <- DataframeSource(tr_kor[,1:2]) %>% VCorpus()
ctr_pers <- DataframeSource(tr_pers[,1:2]) %>% VCorpus()
ctr_pol <- DataframeSource(tr_pol[,1:2]) %>% VCorpus()
ctr_por <- DataframeSource(tr_por[,1:2]) %>% VCorpus()
ctr_rus <- DataframeSource(tr_rus[,1:2]) %>% VCorpus()
ctr_ser <- DataframeSource(tr_ser[,1:2]) %>% VCorpus()
ctr_spa <- DataframeSource(tr_spa[,1:2]) %>% VCorpus()
ctr_telu <- DataframeSource(tr_telu[,1:2]) %>% VCorpus()
ctr_tha <- DataframeSource(tr_tha[,1:2]) %>% VCorpus()
ctr_tur <- DataframeSource(tr_tur[,1:2]) %>% VCorpus()
ctr_urd <- DataframeSource(tr_urd[,1:2]) %>% VCorpus()

# process reference set texts -----------------------------------------------------------
ctr_arab <- ctr_arab %>%
  tm_map(removeWords, stopwords(language = "ar", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_arab$text_pro <- ctr_arab
ctr_beng <- ctr_beng %>%
  tm_map(removeWords, stopwords(language = "bn", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_beng$text_pro <- ctr_beng
ctr_bulg <- ctr_bulg %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_bulg$text_pro <- ctr_bulg
ctr_chin <- ctr_chin %>%
  tm_map(removeWords, stopwords(language = "zh", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_chin$text_pro <- ctr_chin
ctr_czech <- ctr_czech %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "cs", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_czech$text_pro <- ctr_czech
ctr_dut <- ctr_dut %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "nl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_dut$text_pro <- ctr_dut
ctr_eng <- ctr_eng %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "en")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_eng$text_pro <- str_replace_all(ctr_eng, "[:punct:]", "")
ctr_fre <- ctr_fre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "fr", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_fre$text_pro <- str_replace_all(ctr_fre, "[:punct:]", "")
ctr_ger <- ctr_ger %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "de", source = "snowball")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_ger$text_pro <- str_replace_all(ctr_ger, "[:punct:]", "")
ctr_gre <- ctr_gre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "el", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_gre$text_pro <- str_replace_all(ctr_gre, "[:punct:]", "")
ctr_hac <- ctr_hac %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_hac$text_pro <- ctr_hac
ctr_heb <- ctr_heb %>%
  tm_map(removeWords, stopwords(language = "he", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_heb$text_pro <- str_replace_all(ctr_heb, "[:punct:]", "")
ctr_hin <- ctr_hin %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "hi", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_hin$text_pro <- str_replace_all(ctr_hin, "[:punct:]", "")
ctr_hun <- ctr_hun %>%
  tm_map(removeWords, stopwords(language = "hu", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_hun$text_pro <- str_replace_all(ctr_hun, "[:punct:]", "")
ctr_ind <- ctr_ind %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "id", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_ind$text_pro <- str_replace_all(ctr_ind, "[:punct:]", "")
ctr_ita <- ctr_ita %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "it", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_ita$text_pro <- str_replace_all(ctr_ita, "[:punct:]", "")
ctr_jap <- ctr_jap %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ja", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_jap$text_pro <- str_replace_all(ctr_jap, "[:punct:]", "")
ctr_kor <- ctr_kor %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_kor$text_pro <- str_replace_all(ctr_kor, "[:punct:]", "")
ctr_pers <- ctr_pers %>%
  tm_map(removeWords, stopwords(language = "fa", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_pers$text_pro <- ctr_pers
ctr_pol <- ctr_pol %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_pol$text_pro <- str_replace_all(ctr_pol, "[:punct:]", "")
ctr_por <- ctr_por %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pt", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_por$text_pro <- str_replace_all(ctr_por, "[:punct:]", "")
ctr_ser <- ctr_ser %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_ser$text_pro <- str_replace_all(ctr_ser, "[:punct:]", "")
ctr_rus <- ctr_rus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ru", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_rus$text_pro <- str_replace_all(ctr_rus, "[:punct:]", "")
ctr_spa <- ctr_spa %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "es", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_spa$text_pro <- str_replace_all(ctr_spa, "[:punct:]", "")
ctr_telu <- ctr_telu %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_telu$text_pro <- ctr_telu
ctr_tha <- ctr_tha %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "th", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_tha$text_pro <- str_replace_all(ctr_tha, "[:punct:]", "")
ctr_tur <- ctr_tur %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "tr", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_tur$text_pro <- str_replace_all(ctr_tur, "[:punct:]", "")
ctr_urd <- ctr_urd %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ur", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tr_urd$text_pro <- str_replace_all(ctr_urd, "[:punct:]", "")

# rbind processed text of the reference set and restore '#' and '@' ---------------------
training_data_pro <- rbind(tr_arab, tr_beng, tr_bulg, tr_chin, tr_czech, tr_dut, tr_eng, 
                           tr_fre, tr_ger, tr_gre, tr_hac, tr_heb, tr_hin, tr_hun, 
                           tr_ind, tr_ita, tr_jap, tr_kor, tr_pers, tr_pol, tr_por, 
                           tr_rus, tr_ser, tr_spa, tr_telu, tr_tha, tr_tur, tr_urd)
training_data_pro$text_pro <- str_replace_all(training_data_pro$text_pro, 
                                          paste0(tolower(hash_string),"|", 
                                                 hash_string), " #")
training_data_pro$text_pro <- str_replace_all(training_data_pro$text_pro, 
                                          paste0(tolower(at_string),"|", 
                                                 at_string), " @")
training_data_pro$text_pro <- str_trim(training_data_pro$text_pro)
training_data_pro$text_pro <- str_squish(training_data_pro$text_pro)

# save processed test data to disk ------------------------------------------------------
saveRDS(training_data_pro, "./data/classification/training_data_processed")


#### PROCESS TEST DATA FOR CLASSIFIER TUNING ============================================

# import test data ----------------------------------------------------------------------
test_data <- readRDS("./data/classification/test_data")
test_data <- select(test_data, ids, created, political)
test_data <- left_join(test_data, sql_dat_statuses[,c("text", "ids", "language")], 
                       by = "ids")

# replace '#' and '@' with unique string to preserve during text pre-processing ---------
hash_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                     pattern = "[A-Za-z]"), " ")
at_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                   pattern = "[A-Za-z]"), " ")
test_data$text <- str_replace_all(test_data$text, "#", hash_string)
test_data$text <- str_replace_all(test_data$text, "@", at_string)

# build language-specific text corpora from the test data -------------------------------
# prepare for tranformation into text corpora
colnames(test_data)[1] <- c("doc_id")
test_data <- select(test_data, doc_id, text, everything())
# prepare language specific subsets for which stopwords are available
t_arab <- test_data[test_data$language == "ARABIC",]
t_beng <- test_data[test_data$language == "BENGALI",]
t_bulg <- test_data[test_data$language == "BULGARIAN",]
t_chin <- test_data[test_data$language == "CHINESE",]
t_czech <- test_data[test_data$language == "CZECH",]
t_czech$text <- iconv(t_czech$text, "latin1", "ASCII", sub = " ")
t_dut <- test_data[test_data$language == "DUTCH",]
t_eng <- test_data[test_data$language == "ENGLISH",]
t_fre <- test_data[test_data$language == "FRENCH",]
t_ger <- test_data[test_data$language == "GERMAN",]
t_gre <- test_data[test_data$language == "GREEK",]
t_hac <- test_data[test_data$language == "HAITIAN_CREOLE",]
t_heb <- test_data[test_data$language == "HEBREW",]
t_hin <- test_data[test_data$language == "HINDI",]
t_hun <- test_data[test_data$language == "HUNGARIAN",]
t_ind <- test_data[test_data$language == "INDONESIAN",]
t_ita <- test_data[test_data$language == "ITALIAN",]
t_jap <- test_data[test_data$language == "JAPANESE",]
t_kin <- test_data[test_data$language == "KINYARWANDA",]
t_kor <- test_data[test_data$language == "KOREAN",]
t_pers <- test_data[test_data$language == "PERSIAN",]
t_pol <- test_data[test_data$language == "POLISH",]
t_por <- test_data[test_data$language == "PORTUGUESE",]
t_rus <- test_data[test_data$language == "RUSSIAN",]
t_ser <- test_data[test_data$language == "SERBIAN",]
t_spa <- test_data[test_data$language == "SPANISH",]
t_telu <- test_data[test_data$language == "TELUGU",]
t_tha <- test_data[test_data$language == "THAI",]
t_tur <- test_data[test_data$language == "TURKISH",]
t_urd <- test_data[test_data$language == "URDU",]
# tranform into corpora
ct_arab <- DataframeSource(t_arab[,1:2]) %>% VCorpus()
ct_beng <- DataframeSource(t_beng[,1:2]) %>% VCorpus()
ct_bulg <- DataframeSource(t_bulg[,1:2]) %>% VCorpus()
ct_chin <- DataframeSource(t_chin[,1:2]) %>% VCorpus()
ct_czech <- DataframeSource(t_czech[,1:2]) %>% VCorpus()
ct_dut <- DataframeSource(t_dut[,1:2]) %>% VCorpus()
ct_eng <- DataframeSource(t_eng[,1:2]) %>% VCorpus()
ct_fre <- DataframeSource(t_fre[,1:2]) %>% VCorpus()
ct_ger <- DataframeSource(t_ger[,1:2]) %>% VCorpus()
ct_gre <- DataframeSource(t_gre[,1:2]) %>% VCorpus()
ct_hac <- DataframeSource(t_hac[,1:2]) %>% VCorpus()
ct_heb <- DataframeSource(t_heb[,1:2]) %>% VCorpus()
ct_hin <- DataframeSource(t_hin[,1:2]) %>% VCorpus()
ct_hun <- DataframeSource(t_hun[,1:2]) %>% VCorpus()
ct_ind <- DataframeSource(t_ind[,1:2]) %>% VCorpus()
ct_ita <- DataframeSource(t_ita[,1:2]) %>% VCorpus()
ct_jap <- DataframeSource(t_jap[,1:2]) %>% VCorpus()
ct_kin <- DataframeSource(t_kin[,1:2]) %>% VCorpus()
ct_kor <- DataframeSource(t_kor[,1:2]) %>% VCorpus()
ct_pers <- DataframeSource(t_pers[,1:2]) %>% VCorpus()
ct_pol <- DataframeSource(t_pol[,1:2]) %>% VCorpus()
ct_por <- DataframeSource(t_por[,1:2]) %>% VCorpus()
ct_rus <- DataframeSource(t_rus[,1:2]) %>% VCorpus()
ct_ser <- DataframeSource(t_ser[,1:2]) %>% VCorpus()
ct_spa <- DataframeSource(t_spa[,1:2]) %>% VCorpus()
ct_telu <- DataframeSource(t_telu[,1:2]) %>% VCorpus()
ct_tha <- DataframeSource(t_tha[,1:2]) %>% VCorpus()
ct_tur <- DataframeSource(t_tur[,1:2]) %>% VCorpus()
ct_urd <- DataframeSource(t_urd[,1:2]) %>% VCorpus()

# process reference set texts -----------------------------------------------------------
ct_arab <- ct_arab %>%
  tm_map(removeWords, stopwords(language = "ar", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_arab$text_pro <- ct_arab
ct_beng <- ct_beng %>%
  tm_map(removeWords, stopwords(language = "bn", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_beng$text_pro <- ct_beng
ct_bulg <- ct_bulg %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_bulg$text_pro <- ct_bulg
ct_chin <- ct_chin %>%
  tm_map(removeWords, stopwords(language = "zh", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_chin$text_pro <- ct_chin
ct_czech <- ct_czech %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "cs", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_czech$text_pro <- ct_czech
ct_dut <- ct_dut %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "nl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_dut$text_pro <- ct_dut
ct_eng <- ct_eng %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "en")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_eng$text_pro <- str_replace_all(ct_eng, "[:punct:]", "")
ct_fre <- ct_fre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "fr", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_fre$text_pro <- str_replace_all(ct_fre, "[:punct:]", "")
ct_ger <- ct_ger %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "de", source = "snowball")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_ger$text_pro <- str_replace_all(ct_ger, "[:punct:]", "")
ct_gre <- ct_gre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "el", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_gre$text_pro <- str_replace_all(ct_gre, "[:punct:]", "")
ct_hac <- ct_hac %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_hac$text_pro <- ct_hac
ct_heb <- ct_heb %>%
  tm_map(removeWords, stopwords(language = "he", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_heb$text_pro <- str_replace_all(ct_heb, "[:punct:]", "")
ct_hin <- ct_hin %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "hi", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_hin$text_pro <- str_replace_all(ct_hin, "[:punct:]", "")
ct_hun <- ct_hun %>%
  tm_map(removeWords, stopwords(language = "hu", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_hun$text_pro <- str_replace_all(ct_hun, "[:punct:]", "")
ct_ind <- ct_ind %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "id", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_ind$text_pro <- str_replace_all(ct_ind, "[:punct:]", "")
ct_ita <- ct_ita %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "it", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_ita$text_pro <- str_replace_all(ct_ita, "[:punct:]", "")
ct_jap <- ct_jap %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ja", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_jap$text_pro <- str_replace_all(ct_jap, "[:punct:]", "")
ct_kin <- ct_kin %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_kin$text_pro <- ct_kin
ct_kor <- ct_kor %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_kor$text_pro <- str_replace_all(ct_kor, "[:punct:]", "")
ct_pers <- ct_pers %>%
  tm_map(removeWords, stopwords(language = "fa", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_pers$text_pro <- ct_pers
ct_pol <- ct_pol %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_pol$text_pro <- str_replace_all(ct_pol, "[:punct:]", "")
ct_por <- ct_por %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pt", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_por$text_pro <- str_replace_all(ct_por, "[:punct:]", "")
ct_rus <- ct_rus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ru", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_rus$text_pro <- str_replace_all(ct_rus, "[:punct:]", "")
ct_ser <- ct_ser %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_ser$text_pro <- str_replace_all(ct_ser, "[:punct:]", "")
ct_spa <- ct_spa %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "es", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_spa$text_pro <- str_replace_all(ct_spa, "[:punct:]", "")
ct_telu <- ct_telu %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_telu$text_pro <- ct_telu
ct_tha <- ct_tha %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "th", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_tha$text_pro <- str_replace_all(ct_tha, "[:punct:]", "")
ct_tur <- ct_tur %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "tr", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_tur$text_pro <- str_replace_all(ct_tur, "[:punct:]", "")
ct_urd <- ct_urd %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ur", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
t_urd$text_pro <- str_replace_all(ct_urd, "[:punct:]", "")

# rbind processed text of the reference set and restore '#' and '@' ---------------------
test_data_pro <- rbind(t_arab, t_beng, t_bulg, t_chin, t_czech, t_dut, t_eng, t_fre, 
                       t_ger, t_gre, t_hac, t_heb, t_hin, t_hun, t_ind, t_ita, t_jap, 
                       t_kin, t_kor, t_pers, t_pol, t_por, t_rus, t_ser, t_spa, t_telu,
                       t_tha, t_tur, t_urd)
test_data_pro$text_pro <- str_replace_all(test_data_pro$text_pro, 
                                              paste0(tolower(hash_string),"|", 
                                                     hash_string), " #")
test_data_pro$text_pro <- str_replace_all(test_data_pro$text_pro, 
                                              paste0(tolower(at_string),"|", 
                                                     at_string), " @")
test_data_pro$text_pro <- str_trim(test_data_pro$text_pro)
test_data_pro$text_pro <- str_squish(test_data_pro$text_pro)

# save processed test data to disk ------------------------------------------------------
saveRDS(test_data_pro, "./data/classification/test_data_processed")


#### PROCESS VALIDATION DATA FOR CLASSIFIER EVALUATION ==================================

# import test data ----------------------------------------------------------------------
validation_data <- readRDS("./data/classification/validation_data")
validation_data <- select(validation_data, ids, created, political)
validation_data <- left_join(validation_data, sql_dat_statuses[,c("text", "ids", 
                                                                  "language")], 
                             by = "ids")

# replace '#' and '@' with unique string to preserve during text pre-processing ---------
hash_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                     pattern = "[A-Za-z]"), " ")
at_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                   pattern = "[A-Za-z]"), " ")
validation_data$text <- str_replace_all(validation_data$text, "#", hash_string)
validation_data$text <- str_replace_all(validation_data$text, "@", at_string)

# build language-specific text corpora from the test data -------------------------------
# prepare for tranformation into text corpora
colnames(validation_data)[1] <- c("doc_id")
validation_data <- select(validation_data, doc_id, text, everything())
# prepare language specific subsets for which stopwords are available
v_arab <- validation_data[validation_data$language == "ARABIC",]
v_beng <- validation_data[validation_data$language == "BENGALI",]
v_bulg <- validation_data[validation_data$language == "BULGARIAN",]
v_chin <- validation_data[validation_data$language == "CHINESE",]
v_czech <- validation_data[validation_data$language == "CZECH",]
v_dut <- validation_data[validation_data$language == "DUTCH",]
v_eng <- validation_data[validation_data$language == "ENGLISH",]
v_fre <- validation_data[validation_data$language == "FRENCH",]
v_ger <- validation_data[validation_data$language == "GERMAN",]
v_gre <- validation_data[validation_data$language == "GREEK",]
v_hac <- validation_data[validation_data$language == "HAITIAN_CREOLE",]
v_heb <- validation_data[validation_data$language == "HEBREW",]
v_hin <- validation_data[validation_data$language == "HINDI",]
v_hun <- validation_data[validation_data$language == "HUNGARIAN",]
v_ind <- validation_data[validation_data$language == "INDONESIAN",]
v_ita <- validation_data[validation_data$language == "ITALIAN",]
v_jap <- validation_data[validation_data$language == "JAPANESE",]
v_kor <- validation_data[validation_data$language == "KOREAN",]
v_pers <- validation_data[validation_data$language == "PERSIAN",]
v_pol <- validation_data[validation_data$language == "POLISH",]
v_por <- validation_data[validation_data$language == "PORTUGUESE",]
v_rus <- validation_data[validation_data$language == "RUSSIAN",]
v_ser <- validation_data[validation_data$language == "SERBIAN",]
v_spa <- validation_data[validation_data$language == "SPANISH",]
v_spa$text <- iconv(v_spa$text, "latin1", "ASCII", sub = " ")
v_telu <- validation_data[validation_data$language == "TELUGU",]
v_tha <- validation_data[validation_data$language == "THAI",]
v_tur <- validation_data[validation_data$language == "TURKISH",]
v_urd <- validation_data[validation_data$language == "URDU",]
# tranform into corpora
cv_arab <- DataframeSource(v_arab[,1:2]) %>% VCorpus()
cv_beng <- DataframeSource(v_beng[,1:2]) %>% VCorpus()
cv_bulg <- DataframeSource(v_bulg[,1:2]) %>% VCorpus()
cv_chin <- DataframeSource(v_chin[,1:2]) %>% VCorpus()
cv_czech <- DataframeSource(v_czech[,1:2]) %>% VCorpus()
cv_dut <- DataframeSource(v_dut[,1:2]) %>% VCorpus()
cv_eng <- DataframeSource(v_eng[,1:2]) %>% VCorpus()
cv_fre <- DataframeSource(v_fre[,1:2]) %>% VCorpus()
cv_ger <- DataframeSource(v_ger[,1:2]) %>% VCorpus()
cv_gre <- DataframeSource(v_gre[,1:2]) %>% VCorpus()
cv_hac <- DataframeSource(v_hac[,1:2]) %>% VCorpus()
cv_heb <- DataframeSource(v_heb[,1:2]) %>% VCorpus()
cv_hin <- DataframeSource(v_hin[,1:2]) %>% VCorpus()
cv_hun <- DataframeSource(v_hun[,1:2]) %>% VCorpus()
cv_ind <- DataframeSource(v_ind[,1:2]) %>% VCorpus()
cv_ita <- DataframeSource(v_ita[,1:2]) %>% VCorpus()
cv_jap <- DataframeSource(v_jap[,1:2]) %>% VCorpus()
cv_kor <- DataframeSource(v_kor[,1:2]) %>% VCorpus()
cv_pers <- DataframeSource(v_pers[,1:2]) %>% VCorpus()
cv_pol <- DataframeSource(v_pol[,1:2]) %>% VCorpus()
cv_por <- DataframeSource(v_por[,1:2]) %>% VCorpus()
cv_rus <- DataframeSource(v_rus[,1:2]) %>% VCorpus()
cv_ser <- DataframeSource(v_ser[,1:2]) %>% VCorpus()
cv_spa <- DataframeSource(v_spa[,1:2]) %>% VCorpus()
cv_telu <- DataframeSource(v_telu[,1:2]) %>% VCorpus()
cv_tha <- DataframeSource(v_tha[,1:2]) %>% VCorpus()
cv_tur <- DataframeSource(v_tur[,1:2]) %>% VCorpus()
cv_urd <- DataframeSource(v_urd[,1:2]) %>% VCorpus()

# process reference set texts -----------------------------------------------------------
cv_arab <- cv_arab %>%
  tm_map(removeWords, stopwords(language = "ar", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_arab$text_pro <- cv_arab
cv_beng <- cv_beng %>%
  tm_map(removeWords, stopwords(language = "bn", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_beng$text_pro <- cv_beng
cv_bulg <- cv_bulg %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_bulg$text_pro <- cv_bulg
cv_chin <- cv_chin %>%
  tm_map(removeWords, stopwords(language = "zh", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_chin$text_pro <- cv_chin
cv_czech <- cv_czech %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "cs", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_czech$text_pro <- cv_czech
cv_dut <- cv_dut %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "nl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_dut$text_pro <- cv_dut
cv_eng <- cv_eng %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "en")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_eng$text_pro <- str_replace_all(cv_eng, "[:punct:]", "")
cv_fre <- cv_fre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "fr", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_fre$text_pro <- str_replace_all(cv_fre, "[:punct:]", "")
cv_ger <- cv_ger %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "de", source = "snowball")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_ger$text_pro <- str_replace_all(cv_ger, "[:punct:]", "")
cv_gre <- cv_gre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "el", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_gre$text_pro <- str_replace_all(cv_gre, "[:punct:]", "")
cv_hac <- cv_hac %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_hac$text_pro <- cv_hac
cv_heb <- cv_heb %>%
  tm_map(removeWords, stopwords(language = "he", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_heb$text_pro <- str_replace_all(cv_heb, "[:punct:]", "")
cv_hin <- cv_hin %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "hi", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_hin$text_pro <- str_replace_all(cv_hin, "[:punct:]", "")
cv_hun <- cv_hun %>%
  tm_map(removeWords, stopwords(language = "hu", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_hun$text_pro <- str_replace_all(cv_hun, "[:punct:]", "")
cv_ind <- cv_ind %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "id", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_ind$text_pro <- str_replace_all(cv_ind, "[:punct:]", "")
cv_ita <- cv_ita %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "it", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_ita$text_pro <- str_replace_all(cv_ita, "[:punct:]", "")
cv_jap <- cv_jap %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ja", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_jap$text_pro <- str_replace_all(cv_jap, "[:punct:]", "")
cv_kor <- cv_kor %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_kor$text_pro <- str_replace_all(cv_kor, "[:punct:]", "")
cv_pers <- cv_pers %>%
  tm_map(removeWords, stopwords(language = "fa", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_pers$text_pro <- cv_pers
cv_pol <- cv_pol %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_pol$text_pro <- str_replace_all(cv_pol, "[:punct:]", "")
cv_por <- cv_por %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pt", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_por$text_pro <- str_replace_all(cv_por, "[:punct:]", "")
cv_rus <- cv_rus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ru", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_rus$text_pro <- str_replace_all(cv_rus, "[:punct:]", "")
cv_ser <- cv_ser %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_ser$text_pro <- str_replace_all(cv_ser, "[:punct:]", "")
cv_spa <- cv_spa %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "es", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_spa$text_pro <- str_replace_all(cv_spa, "[:punct:]", "")
cv_telu <- cv_telu %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_telu$text_pro <- cv_telu
cv_tha <- cv_tha %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "th", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_tha$text_pro <- str_replace_all(cv_tha, "[:punct:]", "")
cv_tur <- cv_tur %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "tr", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_tur$text_pro <- str_replace_all(cv_tur, "[:punct:]", "")
cv_urd <- cv_urd %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ur", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
v_urd$text_pro <- str_replace_all(cv_urd, "[:punct:]", "")


# rbind processed text of the reference set and restore '#' and '@' ---------------------
validation_data_pro <- rbind(v_arab, v_beng, v_bulg, v_chin, v_czech, v_dut, v_eng, 
                             v_fre, v_ger, v_gre, v_hac, v_heb, v_hin, v_hun, v_ind,
                             v_ita, v_jap, v_kor, v_pers, v_pol, v_por, v_rus, v_ser,
                             v_spa, v_telu, v_tha, v_tur, v_urd)
validation_data_pro$text_pro <- str_replace_all(validation_data_pro$text_pro, 
                                          paste0(tolower(hash_string),"|", 
                                                 hash_string), " #")
validation_data_pro$text_pro <- str_replace_all(validation_data_pro$text_pro, 
                                          paste0(tolower(at_string),"|", 
                                                 at_string), " @")
validation_data_pro$text_pro <- str_trim(validation_data_pro$text_pro)
validation_data_pro$text_pro <- str_squish(validation_data_pro$text_pro)

# save processed test data to disk ------------------------------------------------------
saveRDS(validation_data_pro, "./data/classification/validation_data_processed")


#### PROCESS DATA FOR ANALYSES ==========================================================

# compile realtime statuses data --------------------------------------------------------
# this is data from users twitter timeline collected daily since August 
# 2018 first without @mentions to accurately predict the language of statuses
sql_dat_change <- sqlToR(connection = con_1, table = "change_statuses", 
                         columns = c(1,4,5,6,8), language = TRUE, rm_meta = TRUE, 
                         rm_emoji = TRUE, rm_urls = TRUE, rm_rt = TRUE, 
                         rm_mention = TRUE)
# store the language vector and remove statuses
sql_dat_language <- sql_dat_change$lang
rm(sql_dat_change)
# now compile with mentions and add language vector afterwards
sql_dat_change <- sqlToR(connection = con_1, table = "change_statuses", 
                         columns = c(1,4,5,6,8), language = FALSE, rm_meta = TRUE, 
                         rm_emoji = TRUE, rm_urls = TRUE, rm_rt = TRUE, 
                         rm_mention = FALSE)
sql_dat_change$language <- sql_dat_language
rm(sql_dat_language)
# April 2018 -  6539737 statuses

# replace '#' and '@' with unique string to preserve during text pre-processing ---------
hash_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                     pattern = "[A-Za-z]"), " ")
at_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, 
                                                   pattern = "[A-Za-z]"), " ")
sql_dat_change$text <- str_replace_all(sql_dat_change$text, "#", hash_string)
sql_dat_change$text <- str_replace_all(sql_dat_change$text, "@", at_string)

# build language-specific text corpora and process --------------------------------------
# prepare for tranformation into text corpora
colnames(sql_dat_change)[3] <- c("doc_id")
sql_dat_change <- select(sql_dat_change, doc_id, text, everything())
# prepare language specific subsets for which stopwords are available, transform into 
# corpora and process
arab <- sql_dat_change[sql_dat_change$language == "ARABIC",]
c_arab <- DataframeSource(arab[,1:2]) %>% VCorpus()
c_arab <- c_arab %>%
  tm_map(removeWords, stopwords(language = "ar", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
arab$text_pro <- c_arab
arme <- sql_dat_change[sql_dat_change$language == "ARMENIAN",]
c_arme <- DataframeSource(arme[,1:2]) %>% VCorpus()
c_arme <- c_arme %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
arme$text_pro <- c_arme
bela <- sql_dat_change[sql_dat_change$language == "BELARUSIAN",]
c_bela <- DataframeSource(bela[,1:2]) %>% VCorpus()
c_bela <- c_bela %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
bela$text_pro <- c_bela
beng <- sql_dat_change[sql_dat_change$language == "BENGALI",]
c_beng <- DataframeSource(beng[,1:2]) %>% VCorpus()
c_beng <- c_beng %>%
  tm_map(removeWords, stopwords(language = "bn", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
beng$text_pro <- c_beng
biha <- sql_dat_change[sql_dat_change$language == "BIHARI",]
c_biha <- DataframeSource(biha[,1:2]) %>% VCorpus()
c_biha <- c_biha %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
biha$text_pro <- c_biha
bulg <- sql_dat_change[sql_dat_change$language == "BULGARIAN",]
c_bulg <- DataframeSource(bulg[,1:2]) %>% VCorpus()
c_bulg <- c_bulg %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
bulg$text_pro <- c_bulg
burm <- sql_dat_change[sql_dat_change$language == "BURMESE",]
c_burm <- DataframeSource(burm[,1:2]) %>% VCorpus()
c_burm <- c_burm %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
burm$text_pro <- c_burm
chin <- sql_dat_change[sql_dat_change$language == "CHINESE",]
c_chin <- DataframeSource(chin[,1:2]) %>% VCorpus()
c_chin <- c_chin %>%
  tm_map(removeWords, stopwords(language = "zh", source = "misc")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
chin$text_pro <- c_chin
czech <- sql_dat_change[sql_dat_change$language == "CZECH",]
czech$text <- iconv(czech$text, "latin1", "ASCII", sub = " ")
c_czech <- DataframeSource(czech[,1:2]) %>% VCorpus()
c_czech <- c_czech %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "cs", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
czech$text_pro <- c_czech
dut <- sql_dat_change[sql_dat_change$language == "DUTCH",]
c_dut <- DataframeSource(dut[,1:2]) %>% VCorpus()
c_dut <- c_dut %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "nl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = FALSE) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
dut$text_pro <- c_dut
eng <- sql_dat_change[sql_dat_change$language == "ENGLISH",]
eng$text <- iconv(eng$text, "latin1", "ASCII", sub = " ")
# split english and process iteratively
from <- sapply(split(1:nrow(eng), ceiling(seq_along(1:nrow(eng))/100000)), `[[`, 1)
to <- sapply(split(1:nrow(eng), ceiling(seq_along(1:nrow(eng))/100000)), tail, n = 1)
for (i in 1:length(from)) {
  if (i == 1) {
    cat(yellow("PROCESSING TEXT DATA\n"))
  }
  c_eng <- DataframeSource(eng[from[i]:to[i],1:2]) %>% VCorpus()
  cat("eng", from[i], "to", to[i], "tranformed to VCorpus\n")
  c_eng <- c_eng %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords(language = "en")) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace) %>%
    sapply(`$.data.frame`, "content") %>%
    unname()
  c_eng <- str_replace_all(c_eng, "[:punct:]", "")
  cat("eng", from[i], "to", to[i], "processed\n")
  if (i == 1) {
    eng_pro <- c_eng
    cat(green(length(eng_pro), "done\n"))
  } else {
    eng_pro <- c(eng_pro, c_eng)
    cat(green(length(eng_pro), "done\n"))
  }
}
eng$text_pro <- eng_pro
rm(eng_pro)
fre <- sql_dat_change[sql_dat_change$language == "FRENCH",]
c_fre <- DataframeSource(fre[,1:2]) %>% VCorpus()
c_fre <- c_fre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "fr", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
fre$text_pro <- str_replace_all(c_fre, "[:punct:]", "")
geor <- sql_dat_change[sql_dat_change$language == "GEORGIAN",]
c_geor <- DataframeSource(geor[,1:2]) %>% VCorpus()
c_geor <- c_geor %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
geor$text_pro <- c_geor
ger <- sql_dat_change[sql_dat_change$language == "GERMAN",]
ger <- ger[-c(936),]
c_ger <- DataframeSource(ger[,1:2]) %>% VCorpus()
c_ger <- c_ger %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "de", source = "snowball")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
ger$text_pro <- str_replace_all(c_ger, "[:punct:]", "")
gre <- sql_dat_change[sql_dat_change$language == "GREEK",]
c_gre <- DataframeSource(gre[,1:2]) %>% VCorpus()
c_gre <- c_gre %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "el", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
gre$text_pro <- str_replace_all(c_gre, "[:punct:]", "")
hac <- sql_dat_change[sql_dat_change$language == "HAITIAN_CREOLE",]
c_hac <- DataframeSource(hac[,1:2]) %>% VCorpus()
c_hac <- c_hac %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
hac$text_pro <- str_replace_all(c_hac, "[:punct:]", "")
heb <- sql_dat_change[sql_dat_change$language == "HEBREW",]
c_heb <- DataframeSource(heb[,1:2]) %>% VCorpus()
c_heb <- c_heb %>%
  tm_map(removeWords, stopwords(language = "he", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
heb$text_pro <- str_replace_all(c_heb, "[:punct:]", "")
hin <- sql_dat_change[sql_dat_change$language == "HINDI",]
c_hin <- DataframeSource(hin[,1:2]) %>% VCorpus()
c_hin <- c_hin %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "hi", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
hin$text_pro <- str_replace_all(c_hin, "[:punct:]", "")
hun <- sql_dat_change[sql_dat_change$language == "HUNGARIAN",]
c_hun <- DataframeSource(hun[,1:2]) %>% VCorpus()
c_hun <- c_hun %>%
  tm_map(removeWords, stopwords(language = "hu", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
hun$text_pro <- str_replace_all(c_hun, "[:punct:]", "")
ind <- sql_dat_change[sql_dat_change$language == "INDONESIAN",]
c_ind <- DataframeSource(ind[,1:2]) %>% VCorpus()
c_ind <- c_ind %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "id", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
ind$text_pro <- str_replace_all(c_ind, "[:punct:]", "")
inuk <- sql_dat_change[sql_dat_change$language == "INUKTITUT",]
c_inuk <- DataframeSource(inuk[,1:2]) %>% VCorpus()
c_inuk <- c_inuk %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
inuk$text_pro <- c_inuk
ita <- sql_dat_change[sql_dat_change$language == "ITALIAN",]
c_ita <- DataframeSource(ita[,1:2]) %>% VCorpus()
c_ita <- c_ita %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "it", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
ita$text_pro <- str_replace_all(c_ita, "[:punct:]", "")
jap <- sql_dat_change[sql_dat_change$language == "JAPANESE",]
jap <- jap[-c(1256,5108,5346),] # weird encoding
c_jap <- DataframeSource(jap[,1:2]) %>% VCorpus()
c_jap <- c_jap %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ja", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
jap$text_pro <- str_replace_all(c_jap, "[:punct:]", "")
kan <- sql_dat_change[sql_dat_change$language == "KANNADA",]
c_kan <- DataframeSource(kan[,1:2]) %>% VCorpus()
c_kan <- c_kan %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
kan$text_pro <- str_replace_all(c_kan, "[:punct:]", "")
khme <- sql_dat_change[sql_dat_change$language == "KHMER",]
c_khme <- DataframeSource(khme[,1:2]) %>% VCorpus()
c_khme <- c_khme %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
khme$text_pro <- c_khme
kiny <- sql_dat_change[sql_dat_change$language == "KINYARWANDA",]
c_kiny <- DataframeSource(kiny[,1:2]) %>% VCorpus()
c_kiny <- c_kiny %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
kiny$text_pro <- c_kiny
kor <- sql_dat_change[sql_dat_change$language == "KOREAN",]
kor <- kor[-c(234,2705,3268),] # weird encoding
c_kor <- DataframeSource(kor[,1:2]) %>% VCorpus()
c_kor <- c_kor %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
kor$text_pro <- str_replace_all(c_kor, "[:punct:]", "")
laot <- sql_dat_change[sql_dat_change$language == "LAOTHIAN",]
c_laot <- DataframeSource(laot[,1:2]) %>% VCorpus()
c_laot <- c_laot %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
laot$text_pro <- c_laot
mac <- sql_dat_change[sql_dat_change$language == "MACEDONIAN",]
c_mac <- DataframeSource(mac[,1:2]) %>% VCorpus()
c_mac <- c_mac %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
mac$text_pro <- str_replace_all(c_mac, "[:punct:]", "")
malt <- sql_dat_change[sql_dat_change$language == "MALTESE",]
c_malt <- DataframeSource(malt[,1:2]) %>% VCorpus()
c_malt <- c_malt %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
malt$text_pro <- c_malt
nepa <- sql_dat_change[sql_dat_change$language == "NEPALI",]
c_nepa <- DataframeSource(nepa[,1:2]) %>% VCorpus()
c_nepa <- c_nepa %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
nepa$text_pro <- str_replace_all(c_nepa, "[:punct:]", "")
pers <- sql_dat_change[sql_dat_change$language == "PERSIAN",]
c_pers <- DataframeSource(pers[,1:2]) %>% VCorpus()
c_pers <- c_pers %>%
  tm_map(removeWords, stopwords(language = "fa", source = "stopwords-iso")) %>% 
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
pers$text_pro <- c_pers
pol <- sql_dat_change[sql_dat_change$language == "POLISH",]
c_pol <- DataframeSource(pol[,1:2]) %>% VCorpus()
c_pol <- c_pol %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pl", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
pol$text_pro <- str_replace_all(c_pol, "[:punct:]", "")
por <- sql_dat_change[sql_dat_change$language == "PORTUGUESE",]
por <- por[-c(759,3682),]
c_por <- DataframeSource(por[,1:2]) %>% VCorpus()
c_por <- c_por %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords(language = "pt", source = "stopwords-iso")) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
por$text_pro <- str_replace_all(c_por, "[:punct:]", "")
rus <- sql_dat_change[sql_dat_change$language == "RUSSIAN",]
c_rus <- DataframeSource(rus[,1:2]) %>% VCorpus()
c_rus <- c_rus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ru", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
rus$text_pro <- str_replace_all(c_rus, "[:punct:]", "")
ser <- sql_dat_change[sql_dat_change$language == "SERBIAN",]
c_ser <- DataframeSource(ser[,1:2]) %>% VCorpus()
c_ser <- c_ser %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
ser$text_pro <- str_replace_all(c_ser, "[:punct:]", "")
spa <- sql_dat_change[sql_dat_change$language == "SPANISH",]
spa$text <- iconv(spa$text, "latin1", "ASCII", sub = " ")
# split spanish and process iteratively
from <- sapply(split(1:nrow(spa), ceiling(seq_along(1:nrow(spa))/100000)), `[[`, 1)
to <- sapply(split(1:nrow(spa), ceiling(seq_along(1:nrow(spa))/100000)), tail, n = 1)
for (i in 1:length(from)) {
  if (i == 1) {
    cat(yellow("PROCESSING TEXT DATA\n"))
  }
  c_spa <- DataframeSource(spa[from[i]:to[i],1:2]) %>% VCorpus()
  cat("spa", from[i], "to", to[i], "tranformed to VCorpus\n")
  c_spa <- c_spa %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords(language = "es", source = "stopwords-iso")) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace) %>%
    sapply(`$.data.frame`, "content") %>%
    unname()
  c_spa <- str_replace_all(c_spa, "[:punct:]", "")
  cat("spa", from[i], "to", to[i], "processed\n")
  if (i == 1) {
    spa_pro <- c_spa
    cat(green(length(spa_pro), "done\n"))
  } else {
    spa_pro <- c(spa_pro, c_spa)
    cat(green(length(spa_pro), "done\n"))
  }
}
spa$text_pro <- spa_pro
rm(spa_pro)
syri <- sql_dat_change[sql_dat_change$language == "SYRIAC",]
c_syri <- DataframeSource(syri[,1:2]) %>% VCorpus()
c_syri <- c_syri %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
syri$text_pro <- c_syri
tam <- sql_dat_change[sql_dat_change$language == "TAMIL",]
c_tam <- DataframeSource(tam[,1:2]) %>% VCorpus()
c_tam <- c_tam %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tam$text_pro <- str_replace_all(c_tam, "[:punct:]", "")
telu <- sql_dat_change[sql_dat_change$language == "TELUGU",]
c_telu <- DataframeSource(telu[,1:2]) %>% VCorpus()
c_telu <- c_telu %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
telu$text_pro <- c_telu
tha <- sql_dat_change[sql_dat_change$language == "THAI",]
c_tha <- DataframeSource(tha[,1:2]) %>% VCorpus()
c_tha <- c_tha %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "th", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tha$text_pro <- str_replace_all(c_tha, "[:punct:]", "")
tur <- sql_dat_change[sql_dat_change$language == "TURKISH",]
c_tur <- DataframeSource(tur[,1:2]) %>% VCorpus()
c_tur <- c_tur %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "tr", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
tur$text_pro <- str_replace_all(c_tur, "[:punct:]", "")
urd <- sql_dat_change[sql_dat_change$language == "URDU",]
c_urd <- DataframeSource(urd[,1:2]) %>% VCorpus()
c_urd <- c_urd %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(language = "ur", source = "stopwords-iso")) %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
urd$text_pro <- str_replace_all(c_urd, "[:punct:]", "")
egyp <- sql_dat_change[sql_dat_change$language == "X_EGYPTIAN_HIEROGLYPHS",]
c_egyp <- DataframeSource(egyp[,1:2]) %>% VCorpus()
c_egyp <- c_egyp %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
egyp$text_pro <- c_egyp
yi <- sql_dat_change[sql_dat_change$language == "X_YI",]
c_yi <- DataframeSource(yi[,1:2]) %>% VCorpus()
c_yi <- c_yi %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
yi$text_pro <- c_yi
yidd <- sql_dat_change[sql_dat_change$language == "YIDDISH",]
c_yidd <- DataframeSource(yidd[,1:2]) %>% VCorpus()
c_yidd <- c_yidd %>%
  tm_map(stripWhitespace) %>%
  sapply(`$.data.frame`, "content") %>%
  unname()
yidd$text_pro <- c_yidd
rm(c_arab, c_arme, c_bela, c_beng, c_biha, c_bulg, c_burm, c_chin, c_czech, c_dut, c_eng,
   c_fre, c_geor, c_ger, c_gre, c_hac, c_heb, c_hin, c_hun, c_ind, c_inuk, c_ita, c_jap, 
   c_kan, c_khme, c_kiny, c_kor, c_laot, c_mac, c_malt, c_nepa, c_pers, c_pol, c_por,  
   c_rus, c_ser, c_spa, c_syri, c_tam, c_telu, c_tha, c_tur, c_urd, c_egyp, c_yi, c_yidd)

# rbind processed text of search and reference sets -------------------------------------
sql_dat_change <- rbind(arab, arme, bela, beng, biha, bulg, burm, chin, czech, dut, eng, 
                        fre, geor, ger, gre, hac, heb, hin, hun, ind, inuk, ita, jap, 
                        kan, khme, kiny, kor, laot, mac, malt, nepa, pers, pol, por, rus, 
                        ser, spa, syri, tam, telu, tha, tur, urd, egyp, yi, yidd)
rm(arab, arme, bela, beng, biha, bulg, burm, chin, czech, dut, eng, 
   fre, geor, ger, gre, hac, heb, hin, hun, ind, inuk, ita, jap, 
   kan, khme, kiny, kor, laot, mac, malt, nepa, pers, pol, por, rus, 
   ser, spa, syri, tam, telu, tha, tur, urd, egyp, yi, yidd)
sql_dat_change$text_pro <- str_replace_all(sql_dat_change$text_pro, 
                                           paste0(tolower(hash_string),"|", 
                                                  hash_string), " #")
sql_dat_change$text_pro <- str_replace_all(sql_dat_change$text_pro, 
                                           paste0(tolower(at_string),"|", 
                                                  at_string), " @")
sql_dat_change$text_pro <- str_trim(sql_dat_change$text_pro)
sql_dat_change$text_pro <- str_squish(sql_dat_change$text_pro)
saveRDS(sql_dat_change, "./data/analysis/analysis_processed")


#### ASSESS SENSITIVITY OF PREPROCESSING STEPS ==========================================

# draw random sample of statuses --------------------------------------------------------
sql_dat_change <- sql_dat_change[sql_dat_change$language == "ENGLISH",]
sample_dat <- sample_n(sql_dat_change, 1000)

# preprocess statuses in 128 different ways ---------------------------------------------
preprocessed_text <- factorial_preprocessing(text = sample_dat$text)

# calculate preText scores for each preprocessing specification -------------------------
pretext_results <- preText(preprocessed_text)

# display results from preText call -----------------------------------------------------
preText_plot <- regression_coefficient_plot(pretext_results,
                            remove_intercept = TRUE)

# save plot as .pdf ---------------------------------------------------------------------
ggsave("./figures/preText_plot.pdf", preText_plot, width = 8.5, height = 6.5, 
       dpi = 1200, device = cairo_pdf)

# close connections to SQL databases ----------------------------------------------------
dbDisconnect(con_1)
dbDisconnect(con_2)
