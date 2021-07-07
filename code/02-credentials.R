# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Credentials script
# April 2019
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
nothing
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/credentials'
'./data/app_token'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 29 - PREPARATIONS
Line 38 - BUILD CREDENTIALS DATA
Line 62 - AUTHORIZE APPS
Line 122 - BUILD APP-TOKEN DATA
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("")


#### BUILD CREDENTIALS DATA =============================================================

# collect components --------------------------------------------------------------------
emails <- c("EMAIL ACCOUNTS CONNECTED TO RESEARCHER'S TWITTER ACCOUNTS")
passes <- c("PASSWORDS FOR EMAIL ACCOUNTS")
t_handle <- c("TWITTER HANDLES OF RESEARCHER'S TWITTER ACCOUNTS")
mobile <- c("MOBILE NUMBERS CONNECTED TO RESEARCHER'S TWITTER ACCOUNTS")
app_holder <- c("LOGICAL WHETHER ACCOUNT IS CONNECTED WITH A TWITTER APPLICATION")
app <- c("NAMES OF THE TWITTER APPLICATIONS")
consumer_keys <- c("CONSUMER KEYS OF THE TWITTER APPLICATIONS")
consumer_secret <- c("CONSUMER SECRETS OF THE TWITTER APPLICATIONS")

# assemble as data.frame ----------------------------------------------------------------
credentials <- data.frame(user = emails,
                          pass = passes,
                          t_handle = t_handle,
                          mobile = mobile,
                          app_holder = app_holder,
                          app = app,
                          consumer_key = consumer_keys,
                          consumer_secret = consumer_secret,
                          stringsAsFactors = FALSE)


#### AUTHORIZE APPS =====================================================================

# preparations --------------------------------------------------------------------------
# download SSL certificates
download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")

# set SSL as global option
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", 
                                                 package = "RCurl")))

# assign consumer key and secret of apps ------------------------------------------------
# 'app 1'
consumer_key_1 <- "CONSUMER KEY OF THE TWITTER APPLICATION"
consumer_secret_1 <- "CONSUMER SECRET OF THE TWITTER APPLICATION"

# 'app 2'
consumer_key_2 <-  "CONSUMER KEY OF THE TWITTER APPLICATION"
consumer_secret_2 <- "CONSUMER SECRET OF THE TWITTER APPLICATION"

# create class OAuth object to manage OAuth authentification ----------------------------
# app 1
OAuth_1 <- OAuthFactory$new(
  consumerKey = consumer_key_1,
  consumerSecret = consumer_secret_1,
  requestURL = "https://api.twitter.com/oauth/request_token",
  accessURL = "https://api.twitter.com/oauth/access_token",
  authURL = "https://api.twitter.com/oauth/authorize"
)

# app 2
OAuth_2 <- OAuthFactory$new(
  consumerKey = consumer_key_2,
  consumerSecret = consumer_secret_2,
  requestURL = "https://api.twitter.com/oauth/request_token",
  accessURL = "https://api.twitter.com/oauth/access_token",
  authURL = "https://api.twitter.com/oauth/authorize"
)

# authorize apps using above credentials ------------------------------------------------
# apply authorization iteratively for a set of credentials with respective user always 
# logged in at Twitter.com. The handshake opens a browser window, accept authorization
# and copy the pin into the console and hit Enter. Then retrieve the access token and
# secret. Start over with next user credentials.

# app 1
OAuth_1$handshake(cainfo = "cacert.pem")

# app 2
OAuth_2$handshake(cainfo = "cacert.pem")

# retrieve access token and secret for authorization conducted by logged in user
# app 1
OAuth_1$oauthKey
OAuth_1$oauthSecret

# app 2
OAuth_2$oauthKey
OAuth_2$oauthSecret


#### BUILD APP-TOKEN DATA ===============================================================

# collect components --------------------------------------------------------------------
app <- c("AUTHORZED APPS")
app_holder <-  c("TWITTER HANDLES CONNECTED TO AUTHORIZED APPS")
auth_user <- c("TWITTER HANDLES OF ACCOUNTS THAT HAVE AUTHORIZED APPS")
consumer_key <- c("CONSUMER KEYS OF AUTHORIZED APPS")
access_token <- c("COLLECTED ACCESS TOKENS FOR ACCOUNTS THAT HAVE AUTHORIZED APPS")
access_secret <- c("COLLECTED ACCESS SECRETS FOR ACCOUNTS THAT HAVE AUTHORIZED APPS")

# assemble as data.frame ----------------------------------------------------------------
app_token <- data.frame(app = app,
                        app_holder = app_holder,
                        auth_user = auth_user,
                        consumer_key = consumer_key,
                        consumer_secret = consumer_secret,
                        access_token = access_token,
                        access_secret = access_secret,
                        status_lookup = "open",
                        reset_lookup = as.POSIXct(NA),
                        stringsAsFactors = FALSE)

# save credentials and app-token combinations to disk -----------------------------------
saveRDS(credentials, file = "./data/credentials")
saveRDS(app_token, file = "./data/app_token")
