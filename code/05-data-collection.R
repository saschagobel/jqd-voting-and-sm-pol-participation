# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Twitter data collection script
# July 2018
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./data/sample_processed'
'./data/app_token'
'./data/social-media-activity/collection'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/social-media-activity/collection'
'./data/social-media-activity/social-media-bas.sqlite'
'./data/social-media-activity/social-media-act.sqlite'
'./data/social-media-activity/social-media-search.sqlite'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 32 - PREPARATIONS
Line 64 - SAMPLE DATA COLLECTION
Line 96 - DATA COLLECTION FOR SEARCH SET
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("jqd-voting-and-sm-pol-participation")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")

# read data from disk -------------------------------------------------------------------
# import previous collection if exists
if (file.exists("./data/social-media-activity/collection")) {
  collection <- readRDS("./data/social-media-activity/collection")
} else {
# import processed sample and app_token if previous collection does not exist
  sample <- readRDS("./data/sample_processed")
  # import app-token credentials for Twitter API
  app_token <- readRDS("./data/app_token")
}
  
# open connections to SQL databases -----------------------------------------------------
con_1 <- dbConnect(SQLite(), 
                   dbname = "./data/social-media-activity/social-media-bas.sqlite")
con_2 <- dbConnect(SQLite(), 
                   dbname = "./data/social-media-activity/social-media-act.sqlite")
con_3 <- dbConnect(SQLite(), 
                   dbname = "./data/social-media-activity/social-media-search.sqlite")


#### SAMPLE DATA COLLECTION =============================================================

# run lookup ----------------------------------------------------------------------------
if (!exists("collection")) {
  collection <- lookup(record = sample[,c("t_ids", "t_handle")], credentials = app_token,
                       connection = con_1)
} else {
  collection <- lookup(record = collection,
                       connection = con_1)
}

# drop high profile celebs and activists, lots of followers, lots of friends ------------
if (!file.exists("./data/social-media-activity/collection")) {
  drop <- which(collection[[3]]$count > 25000)
  drop <- c(drop, which(collection[[4]]$count > 25000))
  sample <- sample[-which(sample$t_ids %in% collection[[1]]$t_ids[drop]),]
  rm(collection)
  collection <- lookup(record = sample[,c("t_ids", "t_handle")], credentials = app_token,
                         connection = con_1)
}

# run collector -------------------------------------------------------------------------
collection <- collector(record = collection, connection = con_2, connection_2 = con_1)

# save collection to disk ---------------------------------------------------------------
saveRDS(collection, file = "./data/social-media-activity/collection")

# terminate connection to SQL database --------------------------------------------------
dbDisconnect(conn = con_1)
dbDisconnect(conn = con_2)


#### DATA COLLECTION FOR SEARCH SET =====================================================

# wait some time after main collection --------------------------------------------------
wait <- sample(950:1200,1)
cat("\n Waiting", wait/60, "minutes before commencing with compiling search set \n")
wait <- rep(wait/50, 50)
progress <- txtProgressBar(min = 0, max = 50, style = 3)
for (i in 1:50) {
  Sys.sleep(wait[i])
  setTxtProgressBar(progress, i)
}

# set OAuth signature for search API (only account level) -----------------
search_signature <- create_token(app = "APP NAME", 
                                 consumer_key = "CONSUMER KEY",
                                 consumer_secret = "CONSUMER SECRET",
                                 access_token = "ACCESS TOKEN",
                                 access_secret = "ACCESS SECRET",
                                 set_renv = FALSE)

# set coordinate bounding boxes for USA and FLORIDA -------------------------------------
coords_florida <- list(place = "Florida, USA", 
                        box = c(sw.lng = -87.63490, sq.lat = 24.39631, 
                                ne.lng = -79.97431, ne.lat = 31.00097),
                        point = c(lat = 27.66483, lon = -81.51575))
class(coords_florida) <- c("coords", "list")


# run search API - retrieves tweets from up to 9 days ago and uses all available -------
# geographic infos
search_data <- search_tweets(n = 5000, include_rts = FALSE, 
                            geocode = coords_florida, token = search_signature)

# format data ---------------------------------------------------------------------------
search_data <- select(search_data, t_ids = user_id, ids = status_id, 
                      created = created_at, text = text, lang = lang,
                      name = name, screen_name = screen_name)
search_data$created <- as_date(search_data$created)

# pass to sql database ------------------------------------------------------------------
dbWriteTable(conn = con_3, name = "search_stream",
             value = search_data, append = TRUE)
dbDisconnect(conn = con_3)