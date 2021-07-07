# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Packages script
# April 2019
# ---------------------------------------------------------------------------------------


#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(RSelenium, stringr, data.table, dplyr, XML, R.utils, lubridate, ggmap,
               httr, magrittr, crayon, rlist, ROAuth, dbplyr, DBI, RSQLite, pryr, ggplot2,
               rgeos, maptools, extrafont, gender, wru, USAboundaries, stringi, rlang,
               tidyr, cld2, remoji, rvest, tm, stopwords, quanteda, eeptools, censusr,
               censusapi,rstan,bayesplot,abind,PUMSutils,tidycensus,reticulate,psych,
               preText,keras,foreach,doParallel,purrr,arm, grid, gridExtra, eulerr,
               ggpubr,streamR,rtweet,
               install = TRUE, 
               update = FALSE)

# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())
