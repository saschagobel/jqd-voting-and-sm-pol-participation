# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Sampling script
# April 2019
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./data/voter-files/*'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/sample-raw/*', 
'./data/sample/*'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 29 - PREPARATIONS
Line 42 - SETUP SELENIUM WEBDRIVER
Line 55 - PREPARE E-MAILS FROM VOTER FILES TO SAMPLE FROM TWITTER
Line 334 - SAMPLE BY IDENTIFYING TWITTER HANDLES FOR VOTER FILE ENTRIES
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")


#### SETUP SELENIUM WEBDRIVER ===========================================================

# browser window for gmail --------------------------------------------------------------
rD1 <- rsDriver(verbose = TRUE, browser = "firefox", port = 4445L, version = "3.5.3", 
                phantomver = NULL)
remDr1 <- rD1$client

# browser window for twitter ------------------------------------------------------------
rD2 <- rsDriver(verbose = TRUE, browser = "firefox", port = 4446L, version = "3.5.3", 
                phantomver = NULL)
remDr2 <- rD2$client


#### PREPARE E-MAILS FROM VOTER FILES TO SAMPLE FROM TWITTER ============================

# specify column names ------------------------------------------------------------------
varlabs <- c("county", "id", "surname", "sufname", "forename", "midname", "exempt",
             "street", "addresssup", "place", "state", "zip", "mail1", "mail2", "mail3",
             "mailplace", "mailstate", "mailzip", "mailcountry", "gender", "race",
             "birth", "regist", "party", "precinct", "precinctgroup", "precinctsplit",
             "precinctsuff", "status", "congdist", "housdist", "sendist", "comdist",
             "schooldist", "areacode", "phonenum", "phoneext", "email")

# drop column names of columns that are not required ------------------------------------
varlabs <- varlabs[-c(4,6,7,9,11,13:19,26:28)]

# import voter file data ----------------------------------------------------------------
ora <- fread(input = "./data/voter-files/ORA_20170711.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
ala <- fread(input = "./data/voter-files/ALA_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
bak <- fread(input = "./data/voter-files/BAK_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
bay <- fread(input = "./data/voter-files/BAY_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
bra <- fread(input = "./data/voter-files/BRA_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
bro <- fread(input = "./data/voter-files/BRO_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
bre <- fread(input = "./data/voter-files/BRE_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
cal <- fread(input = "./data/voter-files/CAL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
cll <- fread(input = "./data/voter-files/CLL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
esc <- fread(input = "./data/voter-files/ESC_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
lee <- fread(input = "./data/voter-files/LEE_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
sar <- fread(input = "./data/voter-files/SAR_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
stl <- fread(input = "./data/voter-files/STL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
san <- fread(input = "./data/voter-files/SAN_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
stj <- fread(input = "./data/voter-files/STJ_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
sum <- fread(input = "./data/voter-files/SUM_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
put <- fread(input = "./data/voter-files/PUT_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
pol <- fread(input = "./data/voter-files/POL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
osc <- fread(input = "./data/voter-files/OSC_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
pas <- fread(input = "./data/voter-files/PAS_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
duv <- fread(input = "./data/voter-files/DUV_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
cha <- fread(input = "./data/voter-files/CHA_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
cit <- fread(input = "./data/voter-files/CIT_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
cla <- fread(input = "./data/voter-files/CLA_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
clm <- fread(input = "./data/voter-files/CLM_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
dad <- fread(input = "./data/voter-files/DAD_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
des <- fread(input = "./data/voter-files/DES_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
dix <- fread(input = "./data/voter-files/DIX_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
fla <- fread(input = "./data/voter-files/FLA_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
fra <- fread(input = "./data/voter-files/FRA_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
gad <- fread(input = "./data/voter-files/GAD_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
hil <- fread(input = "./data/voter-files/HIL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
pal <- fread(input = "./data/voter-files/PAL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
pin <- fread(input = "./data/voter-files/PIN_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
gil <- fread(input = "./data/voter-files/GIL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
gla <- fread(input = "./data/voter-files/GLA_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
gul <- fread(input = "./data/voter-files/GUL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
ham <- fread(input = "./data/voter-files/HAM_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
har <- fread(input = "./data/voter-files/HAR_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
hen <- fread(input = "./data/voter-files/HEN_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
her <- fread(input = "./data/voter-files/HER_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
hig <- fread(input = "./data/voter-files/HIG_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
hol <- fread(input = "./data/voter-files/HOL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
ind <- fread(input = "./data/voter-files/IND_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
jac <- fread(input = "./data/voter-files/JAC_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
jef <- fread(input = "./data/voter-files/JEF_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
laf <- fread(input = "./data/voter-files/LAF_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
lak <- fread(input = "./data/voter-files/LAK_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
leo <- fread(input = "./data/voter-files/LEO_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
lev <- fread(input = "./data/voter-files/LEV_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
lib <- fread(input = "./data/voter-files/LIB_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
mad <- fread(input = "./data/voter-files/MAD_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
man <- fread(input = "./data/voter-files/MAN_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
mon <- fread(input = "./data/voter-files/MON_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
mrn <- fread(input = "./data/voter-files/MRN_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
mrt <- fread(input = "./data/voter-files/MRT_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
nas <- fread(input = "./data/voter-files/NAS_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
oka <- fread(input = "./data/voter-files/OKA_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
oke <- fread(input = "./data/voter-files/OKE_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
sem <- fread(input = "./data/voter-files/SEM_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
suw <- fread(input = "./data/voter-files/SUW_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
tay <- fread(input = "./data/voter-files/TAY_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
uni <- fread(input = "./data/voter-files/UNI_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
wak <- fread(input = "./data/voter-files/WAK_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
wal <- fread(input = "./data/voter-files/WAL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
was <- fread(input = "./data/voter-files/WAS_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))
vol <- fread(input = "./data/voter-files/VOL_20171010.txt", header = FALSE,
             stringsAsFactors = FALSE, col.names = varlabs,
             drop = c(4,6,7,9,11,13:19,26:28), na.strings = c("", "*"))

# drop entries in voter files that do not report an email -------------------------------
ora_mails <- na.omit(object = ora, cols = "email")
ala_mails <- na.omit(object = ala, cols = "email")
bay_mails <- na.omit(object = bay, cols = "email")
bro_mails <- na.omit(object = bro, cols = "email")
bre_mails <- na.omit(object = bre, cols = "email")
cll_mails <- na.omit(object = cll, cols = "email")
san_mails <- na.omit(object = san, cols = "email")
stj_mails <- na.omit(object = stj, cols = "email")
sum_mails <- na.omit(object = sum, cols = "email")
put_mails <- na.omit(object = put, cols = "email")
pol_mails <- na.omit(object = pol, cols = "email")
osc_mails <- na.omit(object = osc, cols = "email")
pas_mails <- na.omit(object = pas, cols = "email")
duv_mails <- na.omit(object = duv, cols = "email")
cal_mails <- na.omit(object = cal, cols = "email")
cha_mails <- na.omit(object = cha, cols = "email")
cit_mails <- na.omit(object = cit, cols = "email")
cla_mails <- na.omit(object = cla, cols = "email")
clm_mails <- na.omit(object = clm, cols = "email")
dad_mails <- na.omit(object = dad, cols = "email")
des_mails <- na.omit(object = des, cols = "email")
dix_mails <- na.omit(object = dix, cols = "email")
fla_mails <- na.omit(object = fla, cols = "email")
hil_mails <- na.omit(object = hil, cols = "email")
fra_mails <- na.omit(object = fra, cols = "email")
gad_mails <- na.omit(object = gad, cols = "email")
pal_mails <- na.omit(object = pal, cols = "email")
pin_mails <- na.omit(object = pin, cols = "email")
gil_mails <- na.omit(object = gil, cols = "email")
gla_mails <- na.omit(object = gla, cols = "email")
gul_mails <- na.omit(object = gul, cols = "email")
ham_mails <- na.omit(object = ham, cols = "email")
har_mails <- na.omit(object = har, cols = "email")
hen_mails <- na.omit(object = hen, cols = "email")
her_mails <- na.omit(object = her, cols = "email")
hig_mails <- na.omit(object = hig, cols = "email")
hol_mails <- na.omit(object = hol, cols = "email")
ind_mails <- na.omit(object = ind, cols = "email")
jac_mails <- na.omit(object = jac, cols = "email")
jef_mails <- na.omit(object = jef, cols = "email")
laf_mails <- na.omit(object = laf, cols = "email")
lak_mails <- na.omit(object = lak, cols = "email")
leo_mails <- na.omit(object = leo, cols = "email")
lev_mails <- na.omit(object = lev, cols = "email")
lib_mails <- na.omit(object = lib, cols = "email")
mad_mails <- na.omit(object = mad, cols = "email")
man_mails <- na.omit(object = man, cols = "email")
mon_mails <- na.omit(object = mon, cols = "email")
mrn_mails <- na.omit(object = mrn, cols = "email")
mrt_mails <- na.omit(object = mrt, cols = "email")
nas_mails <- na.omit(object = nas, cols = "email")
oka_mails <- na.omit(object = oka, cols = "email")
oke_mails <- na.omit(object = oke, cols = "email")
suw_mails <- na.omit(object = suw, cols = "email")
tay_mails <- na.omit(object = tay, cols = "email")
uni_mails <- na.omit(object = uni, cols = "email")
wak_mails <- na.omit(object = wak, cols = "email")
wal_mails <- na.omit(object = wal, cols = "email")
was_mails <- na.omit(object = was, cols = "email")
vol_mails <- na.omit(object = vol, cols = "email")


#### SAMPLE BY IDENTIFYING TWITTER HANDLES FOR VOTER FILE ENTRIES =======================

# collect twitter handles for registered users with e-mails -----------------------------
# apply the following code iteratively for each of the above county specific e-mail 
# datasets. The code wraps the 'mailToTwitter' function ensuring that the data is 
# properly collected, keeping track of the process, and granting additional exception 
# handling. Provide credentials for g-mail and twitter
# in the 'mailToTwitter' block. Expect this to take many days.
mails <- county_mails
remDr1$open()
remDr2$open()
handles <- NULL
counter <- 0
while(dim(mails)[[1]] > 0) {
  if (dim(mails)[[1]] >= 2000) {
    mails_input <-  mails[1:2000,]
    mails <- mails[-c(1:2000),]
  } else {
    mails_input <- mails
    mails <- mails[-c(1:dim(mails)[[1]]),]
  }
  result <- NULL
  counter <- counter + 1
  cat("iteration", counter, ": running\n")
  while(is.null(result)) {
    try(
      withTimeout(expr = {
        result <- mailToTwitter(email = "EMAIL ADDRESS",
                          pass = "PASSWORD",
                          email2 = "EMAIL ADDRESS",
                          pass2 = "PASSWORD",
                          mails = mails_input,
                          folder = getwd())
        }, timeout = 4000, onTimeout = "warning"),
      silent = FALSE
    )
    if (is.null(result)) {
      cat("An error occured, now restaring current iteration\n")
      remDr1$close()
      remDr2$close()
      capture.output(remDr1$open(), file = "NUL")
      capture.output(remDr2$open(), file = "NUL")
      Sys.sleep(30)
    }
  }
  mails <- rbind(result[[4]], mails)
  mails <- mails[sample(nrow(mails)),]
  if (is.null(handles)) {
    handles <- list(result)
  } else {
    handles <- c(handles, list(result))
  }
  if (dim(mails)[[1]] > 0) {
    cat("iteration", counter, ": completed\n    setting up session for next iteration\n")
    remDr1$close()
    remDr2$close()
    capture.output(remDr1$open(), file = "NUL")
    capture.output(remDr2$open(), file = "NUL")
    cat("    information:\n      - previous iteration retrieved", dim(result[[1]])[[1]],
        "Twitter handles from", sum(unlist(lapply(lapply(result[[2]], dim), `[[`, 1))),
        "e-mails\n", "     -", dim(mails)[[1]], "e-mails remain in the pool\n")
    Sys.sleep(30)
  } else {
    cat("all iterations completed\n")
  }
}

# save sampled data to disk -------------------------------------------------------------
# apply for each county
county_raw <- handles
county_matched <- bind_rows(lapply(county_raw, `[[`, 1))
saveRDS(county_raw, "./data/sample-raw/county")
saveRDS(county_matched, "./data/sample/county")
