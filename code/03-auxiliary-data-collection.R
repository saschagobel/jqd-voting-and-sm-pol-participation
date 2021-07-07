# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Auxiliary data script
# April 2019
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
'./data/sample/*'
'./data/voter-files/*'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
'./data/auxiliary/sample_geo'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 28 - PREPARATIONS
Line 47 - GEOCODE SAMPLED INDIVIDUALS
Line 112 - COLLECT CENSUS DATA
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
# import twitter and record data, match, and bind together
impMatchBind(folder_a = "./data/sample/", 
             folder_b = "./data/voter-files/",
             bind_record = FALSE)


#### GEOCODE SAMPLED INDIVIDUALS ========================================================

# select unique individuals for geocoding based on voter ID -----------------------------
sample_geo <- sample %>%
  select(id, place, street, zip) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(lon = NA, lat = NA, address = NA, confidence = NA)

# geocode via Bing Maps API -------------------------------------------------------------
# max 50,000 queries per day, increase and continue on subsequent days
sample_geo <- geoLocate2(data = sample_geo, range = 1:50000, lon_column = "lon", 
                         lat_column = "lat", city_column = "place", 
                         street_column = "street", zip_column = "zip", 
                         accuracy_column = "confidence", address_column = "address", 
                         bing_maps_key = "API KEY")

# correct wrong entries -----------------------------------------------------------------
sample_geo <- sample_geo[-which(is.na(sample_geo$id)),]
replacement_idx <- which(sample_geo$lon < -88)
sample_geo[replacement_idx,]$lon <- c(-80.6385468, -84.9474902, -84.8818836,
                                      -84.8596937, -84.8844377, -84.8844377,
                                      -84.8132677, -84.8711027, -84.8603053,
                                      -84.8096447, -84.8420686, -84.8135987,
                                      -84.8052947, -84.9590907, -81.9460589,
                                      -80.5434836, -80.4991641, -82.3998626,
                                      -82.4005025, -82.4005025)
sample_geo[replacement_idx,]$lat <- c(28.0512909, 29.623627, 29.6571821,
                                      29.664011, 29.655584, 29.655584,
                                      29.680678, 29.65961, 29.6655132,
                                      29.681499, 29.671944, 29.680546,
                                      29.681635, 29.614449, 28.8500833,
                                      24.998213, 25.0271467, 27.1440441,
                                      27.1434492, 27.1434492)
replacement_idx <- which(sample_geo$lat > 31)
sample_geo[replacement_idx,]$lon <- c(-82.3485418, -82.3485418, -82.3485418, -82.3680588, 
                                      -81.5090455, -82.3533876, -81.7318225, -86.5727846, 
                                      -82.4563284)
sample_geo[replacement_idx,]$lat <- c(29.6483139, 29.6483139, 29.6483139, 29.6418663,
                                      27.5987507, 27.9016406, 28.5880415, 30.750341,
                                      27.1345784)
sample_geo$lon[c(315, 736, 1628, 1997, 5660, 43130, 43131, 43134, 43137, 43138, 43140, 43143,
             43163, 43170, 43172, 43178, 43191, 43192, 44816, 47363, 55402, 56214, 66195,
             66486, 70063, 100372, 100575, 100591, 100592)] <- c(-82.348675,-82.348675,
                                          -82.348675,-82.365892,-80.636390,-84.946396,
                                          -84.879691,-84.857537,-84.882197,-84.882197,
                                          -84.811111,-84.868968,-84.858181,-84.807402,
                                          -84.839901,-84.811453,-84.802658,-84.956934,
                                          -81.506725,-82.350887,-81.729752,-81.943795,
                                          -80.541286,-80.496969,-86.570617,-82.454129,
                                          -82.397652,-82.398389,-82.398389)
sample_geo$lat[c(315, 736, 1628, 1997, 5660, 43130, 43131, 43134, 43137, 43138, 43140, 43143,
                   43163, 43170, 43172, 43178, 43191, 43192, 44816, 47363, 55402, 56214, 66195,
                   66486, 70063, 100372, 100575, 100591, 100592)] <- c(29.648589,29.648589,
                                        29.648589,29.642165,28.051587,29.623916,29.657363,
                                        29.664281,29.655833,29.655833,29.681032,29.659946,
                                        29.665793,29.681797,29.672214,29.680891,29.681392,
                                        29.614747,27.599041,27.901943,28.588564,28.850356,
                                        24.998345,25.027312,30.750535,27.134836,27.144350,
                                        27.143707,27.143707)
sample_geo <- sample_geo[-43122,]

# save data to disk
saveRDS(sample_geo, "./data/auxiliary/sample_geo")


#### COLLECT CENSUS DATA ================================================================

# assign API key for US census bureau API -----------------------------------------------
census_api_key <- "API KEY"

# import sample_geo ---------------------------------------------------------------------
sample_geo <- readRDS("./data/auxiliary/sample_geo")

# retrieve census block codes via coordinates of residence ------------------------------
sample_geo$censusblock <- NA
for (i in 1:dim(sample_geo)[1]) {
  sample_geo$censusblock[i] <- call_geolocator_latlon(lat = sample_geo$lat[i], 
                                                      lon = sample_geo$lon[i])
  cat(".")
  if (i%%1000 == 0)
    cat("\n", i, "\n")
}

# extract fips county codes from census block -------------------------------------------
sample_geo$county <- str_extract(sample_geo$censusblock, 
                                 "(?<=^[[:digit:]]{2})[[:digit:]]{3}")

# shorten census block to census block-group code ---------------------------------------
sample_geo$censusblock_group <- str_replace(sample_geo$censusblock, "[[:digit:]]{3}$", 
                                            "")

# collect census block group data for PER CAPITA INCOME IN THE PAST 12 MONTHS -----------
iter <- 1
for (i in 1:length(unique(sample_geo$county))) {
  cat(i,"\n")
  censusdata <- getCensus(name = "acs/acs5", vintage = 2017, key = census_api_key, 
                          region = "block group:*",
                          regionin = str_c("state:12+county:", 
                                           unique(sample_geo$county)[i]),
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
income <- select(income, censusblock_group, B19301_001E)

# collect census block group data for MEDIAN HOUSEHOLD INCOME ---------------------------
iter <- 1
for (i in 1:length(unique(sample_geo$county))) {
  cat(i,"\n")
  censusdata <- getCensus(name = "acs/acs5", vintage = 2017, key = census_api_key, 
                          region = "block group:*",
                          regionin = str_c("state:12+county:", 
                                           unique(sample_geo$county)[i]),
                          vars = c("B19013_001E"))
  if (iter == 1) {
    income2 <- censusdata
  } else {
    income2 <- rbind(income2,censusdata)
  }
  iter <- iter + 1
  rm(censusdata)
}
income2$censusblock_group <- str_c(income2$state, income2$county, income2$tract, 
                                  income2$block_group)
income2 <- select(income2, censusblock_group, B19013_001E)

# join census data to sample ------------------------------------------------------------
sample_geo <- left_join(x = sample_geo, y = income, by = "censusblock_group")
sample_geo[which(sample_geo$B19301_001E == -666666666),]$B19301_001E <- NA
colnames(sample_geo)[13] <- "income_pci"
sample_geo <- left_join(x = sample_geo, y = income2, by = "censusblock_group")
sample_geo[which(sample_geo$B19013_001E == -666666666),]$B19013_001E <- NA
colnames(sample_geo)[14] <- "income_household"

# save data to disk ---------------------------------------------------------------------
saveRDS(sample_geo, "./data/auxiliary/sample_geo")
