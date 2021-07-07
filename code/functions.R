# ---------------------------------------------------------------------------------------
# VOTING AND SOCIAL-MEDIA BASED POLITICAL PARTICIPATION
# Sascha Goebel
# Functions script
# April 2019
# ---------------------------------------------------------------------------------------


# content -------------------------------------------------------------------------------
cat(underline("FUNCTIONS"),"
Line 31 - resample
Line 39 - %ni%
Line 46 - is.not.null
Line 53 - switchWin
Line 67 - impMatchBind
Line 129 - geoLocate
Line 199 - mailToTwitter
Line 731 - twitterApiFollowers
Line 754 - twitterApiFriends
Line 777 - twitterApiLikes
Line 800 - twitterApiStatuses
Line 823 - twitterLookup
Line 1283 - twitterCollector
Line 2464 - sqlToR
Line 2650 - voteHist
Line 2696 - applyDict
Line 2761 - mlAMEs
")


#### resample ===========================================================================

# 'resample' modifies the 'sample' function to allow sampling from a pool with varying 
# length

resample <- function(x, ...) x[sample.int(length(x), ...)]


#### %ni% ===============================================================================

# '%ni%' yields the negated version of %in%, i.e., "not in"

"%ni%" <- Negate("%in%")


#### is.not.null ========================================================================

# yields the negated version of is.null

is.not.null <- function(x) ! is.null(x)


#### switchWin ==========================================================================

# due to changes in the underlying API of the driver (geckodriver) which Firefox uses, 
# window handles diverge and RSelenium's '$switchToWindow' doesn't work. 'SwitchWin' 
# implements a solution to enable switching through Windows. The function takes 'remDr', 
# the remote driver that was setup, and a 'windowID'; the output of RSelenium's 
# getWindowHandles() as arguments.

switchWin <- function (remDr, windowId) {
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}


#### impMatchBind =======================================================================

# 'impMatchBind' imports voter records and twitter user data collected by applying the 
# 'mailToTwitter'function to those records and first matches then binds them together and
# stores all merged data frames in the global environment. The function takes the folders
# storing the twitter user data ('folder_a') and the voter records ('folder_b') as 
# arguments.

impMatchBind <- function(folder_a, folder_b, store_records = FALSE, 
                         store_matched = FALSE, store_twitter = FALSE, 
                         bind_record = FALSE) {
  varlabs <- c("county", "id", "surname", "sufname", "forename", "midname", "exempt",
               "street", "addresssup", "place", "state", "zip", "mail1", "mail2", "mail3",
               "mailplace", "mailstate", "mailzip", "mailcountry", "gender", "race",
               "birth", "regist", "party", "precinct", "precinctgroup", "precinctsplit",
               "precinctsuff", "status", "congdist", "housdist", "sendist", "comdist",
               "schooldist", "areacode", "phonenum", "phoneext", "email")
  varlabs <- varlabs[-c(7,11,13:19,26:28)]
  files_a <- list.files(folder_a)
  files_b <- list.files(folder_b)
  ab_all <- data.frame(matrix(nrow = 0, ncol = 31, 
                              dimnames = list(c(), c("Name", "Given Name", "email", "t_handle", 
                                                     "t_names", "t_ids", "county", "id", "surname", 
                                                     "sufname", 
                                                     "forename", "midname", "street", "addresssup", 
                                                     "place", "zip", "gender", 
                                                     "race", "birth", "regist", "party", "precinct", 
                                                     "status", "congdist", "housdist", "sendist", 
                                                     "comdist", "schooldist", "areacode", "phonenum", 
                                                     "phoneext"))))
  b_all <- data.frame(matrix(nrow = 0, ncol = 26,
                             dimnames = list(c(), varlabs)))
  for (i in 1:length(files_a)) {
    a <- readRDS(paste0(folder_a, files_a[i]))
    colnames(a)[3] <- "email" 
    cat(i, "\n")
    b <- fread(input = paste0(folder_b, files_b[i]), header = FALSE,
               stringsAsFactors = FALSE, col.names = varlabs,
               drop = c(7,11,13:19,26:28), na.strings = c("", "*"),
               quote = "")
    ab <- left_join(x = a, y = b, by = "email")
    if (store_twitter == TRUE) {
      assign(paste0(files_a[i], "_twitter"), a, envir=globalenv())
    }
    if (store_records == TRUE) {
      assign(paste0(files_a[i], "_record"), b, envir=globalenv())
    }
    if (store_matched == TRUE) {
      assign(paste0(files_a[i], "_matched"), ab, envir=globalenv())
    }
    if (bind_record == TRUE) {
    b_all  <- rbind(b_all, b)
    }
    ab_all <- rbind(ab_all, ab)
  }
  assign("sample", ab_all, envir = globalenv())
  if (bind_record == TRUE) {
    assign("records", b_all, envir = globalenv())
  }
}


#### geoLocate ==========================================================================

# 'geoLocate' uses the googlemaps or Bing API to determine geographic coordinates 
# (latitude, longitude) of valid addresses. The function takes the data frame including 
# the (initially empty) columns storing latitude and longitude values as well as the
# column containing addresses as argument in 'data'. The arguments 'lon_column`,
# 'lat_column', and 'address_column' specify the names of the respective columns.
# The function heeds the API rate limits (2500 queries daily) and terminates execution 
# if met. When restarting after the rate limit has been reset, the function will continue 
# with addresses for which coordinates have not been determined previously.

geoLocate <- function(data, lon_column, lat_column, address_column) {
  amount <- geocodeQueryCheck()
  for (i in 1:nrow(data)) {
    if (is.na(data[i,lon_column])) {
      result <- geocode(data[i,address_column])
      data[i,lon_column] <- result[1]
      data[i,lat_column] <- result[2]
      amount <- amount - 1
      Sys.sleep(1)
      cat(str_c("at row: ", i, "\n", "remaining queries: ", amount, "\n"))
    }
    if (amount == 0){
      break
    }
  }
  return(data)
}

geoLocate2 <- function(data, range, lon_column, lat_column, city_column, street_column, 
                       zip_column, accuracy_column, address_column, bing_maps_key) {
  cat("Geocoding ", length(range), " addresses\n")
  progress <- txtProgressBar(min = min(range), max = max(range), style = 3)
  skipped <- 0
  for (i in range) {
    if (is.na(data[i,lon_column])) {
      result <- try(response <- httr::RETRY("GET", 
                                    "http://dev.virtualearth.net/REST/v1/Locations?",
                                    query = list(countryRegion = "US",
                                                 locality = data[i, city_column],
                                                 postalCode = data[i, zip_column],
                                                 addressLine = data[i, street_column],
                                                 maxResults = 1,
                                                 key = bing_maps_key),
                                    times = 1000,
                                    quiet = FALSE),
            silent = TRUE)
      if (result$status_code == 429) {
        cat("Rate limit reached, returning current state")
        return(data)
      }
      if (headers(result)$`x-ms-bm-ws-info` == 1 | result$status_code != 200) {
        cat("\nSomething went wrong, proceeding with next iteration\n",
            "--- x-ms-bm-ws-info = ", headers(result)$`x-ms-bm-ws-info`, "\n",
            "--- status code = ", result$status_code, "\n")
        next
      }
      #Sys.sleep(1)
      data[i, lon_column] <- httr::content(result)$resourceSets[[1]]$resources[[1]]$point$coordinates[[2]]
      data[i, lat_column] <- httr::content(result)$resourceSets[[1]]$resources[[1]]$point$coordinates[[1]]
      data[i, accuracy_column] <- httr::content(result)$resourceSets[[1]]$resources[[1]]$confidence
      data[i, address_column] <- httr::content(result)$resourceSets[[1]]$resources[[1]]$address$formattedAddress
      skipped <- 0
      setTxtProgressBar(progress, i)
    }
  }
  return(data)
}


#### mailToTwitter ======================================================================

# 'mailToTwitter' uses RSelenium to identify Twitter IDs via email addresses. The 
# function requires a working Twitter account and takes the associated email address and 
# password as login credentials in the arguments 'email2' and 'pass2'. A working gmail 
# account used within the Twitter account is further required, the respective email 
# address and password of which are passed to the arguments 'email' and 'pass'. The 
# 'mails' argument takes a data frame with email addresses, surnames, and forenames. The 
# 'folder' argument specifies the path where email batches are temporarily stored. See
# the supplementary materials for the exact algorithm.

mailToTwitter <- function(email, pass, email2, pass2, mails, folder) {
  remDr1$navigate("https://mail.google.com")
      # navigate to gmail login
  remDr2$navigate("https://twitter.com/login")
      # navigate to twitter login
  login1a <- remDr1$findElement(using = "id", "identifierId")
      # locate email field on gmail
  login2a <- remDr2$findElement(using = "xpath", "//input[@class = 'js-username-field email-input js-initial-focus']")
      # locate email field on twitter
  login1a$sendKeysToElement(list(email2, key = 'enter'))
      # enter email address on gmail and continue
  error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "id", "password")),silent = TRUE))
  while (class(error_check) == "try-error") {
    Sys.sleep(2)
    error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "id", "password")),silent = TRUE))
  }
  #while (length(remDr1$executeScript(script = "return document.getElementById('password');", 
  #                                   args = list())) == 0) {
  #  # check if element completely loaded on remDr1 (gmail)
  #  Sys.sleep(2)
  #  # if not wait and check again
  #}
  login2a$sendKeysToElement(list(email))
      # enter email address on twitter
  login1b <- remDr1$findElement(using = "xpath", "//input[@type = 'password']")
      # locate password field on gmail
  login2b <- remDr2$findElement(using = "xpath", "//input[@class = 'js-password-field']")
      # locate password field on twitter
  login1b$sendKeysToElement(list(pass2, key = 'enter'))
      # enter password on gmail and continue
  login2b$sendKeysToElement(list(pass, key = 'enter'))
      # enter password on twitter and continue
  error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "id", ":i")),silent = TRUE))
  while (class(error_check) == "try-error") {
    Sys.sleep(4)
    error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "id", ":i")),silent = TRUE))
  }
  #while (length(remDr1$executeScript(script = "return document.getElementById(':i');", 
  #                                  args = list())) == 0) {
  #    # check if element completely loaded
  #  Sys.sleep(4)
  #      # if not wait and check again
  #}
  # visit contacts page
  # remDr1$findElement(using = "id", ":i")$clickElement() # OUTDATED
  # remDr1$findElement(using = "id", ":13")$clickElement() # OUTDATED
  remDr1$navigate("https://www.google.com/contacts/?hl=en&tab=mC#contacts")
  mails <- mutate(mails, surname = tolower(surname), forename = tolower(forename))
      # make names in mails list lower-case
  poolidx <- 1:nrow(mails)
      # index through list of mails
  run <- 1
  batches <- NULL
  t_data_all <- NULL
  matched <- NULL
  folder <- str_c(str_replace_all(folder, "/", "\\\\"), "\\", "batch_out.csv")
  ###### ENTER LOOP
  Sys.sleep(sample(5:10, 1))
  # while (length(poolidx) >= 1000) {
  while (run <= 6 & length(poolidx) > 0) {
    cat(run, length(poolidx))
    #delcount <- remDr1$findElement(using = "xpath", value = "//a[@class = 'J-Ke n0']") # OUTDATED
    delcount <- remDr1$findElement(using = "xpath", value = "//a[@class = 'VIpgJd-hSRGPd ceRIQe']")
    delcount <- str_extract(delcount$getElementText(), pattern = "[[:digit:]]+")
    while (!is.na(delcount)) {
      remDr1$findElement(using = "xpath", "//div[@type = 'checkbox']")$clickElement()
      # error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", "//div[@class = 'T-I J-J5-Ji T-I-ax7 T-I-Js-Gs L3'][2]")),silent = TRUE)) # OUTDATED
      error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "id", ":ra")),silent = TRUE))
      iter <- 0
      while (class(error_check) == "try-error") {
        Sys.sleep(6)
        # error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", "//div[@class = 'T-I J-J5-Ji T-I-ax7 T-I-Js-Gs L3'][2]")),silent = TRUE)) # OUTDATED
        error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "id", ":ra")),silent = TRUE))
        if (iter > 2) {
          remDr1$refresh()
          Sys.sleep(10)
          remDr1$findElement(using = "xpath", "//div[@type = 'checkbox']")$clickElement()
          Sys.sleep(5)
        # error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", "//div[@class = 'T-I J-J5-Ji T-I-ax7 T-I-Js-Gs L3'][2]")),silent = TRUE)) # OUTDATED
          error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "id", ":ra")),silent = TRUE))
        }
        iter <- iter + 1 
      }
      remDr1$findElement(using = "id", ":ra")$clickElement()
      Sys.sleep(2)
      if (delcount > 1) {
        # remDr1$findElement(using = "xpath", "//div[@class = 'J-N LJOhwe'][2]")$clickElement() # old "Delete contacts" = 'J-N ou' # OUTDATED
        remDr1$findElement(using = "xpath", "//div[@class = 'VIpgJd-j7LFlb LJOhwe'][2]")$clickElement()
        # remDr1$findElement(using = "xpath", "//div[@class = 'J-N LJOhwe'][1]")$clickElement() # old "Delete contacts" = 'J-N ou' # OUTDATED
        remDr1$findElement(using = "xpath", "//div[@class = 'VIpgJd-j7LFlb LJOhwe'][1]")$clickElement()
      }
      Sys.sleep(5)
      # delcount <- remDr1$findElement(using = "xpath", value = "//a[@class = 'J-Ke n0']") # OUTDATED
      delcount <- remDr1$findElement(using = "xpath", value = "//a[@class = 'VIpgJd-hSRGPd ceRIQe']")
      delcount <- str_extract(delcount$getElementText(), pattern = "[[:digit:]]+")
    }
    if (run == 1) {
      cat(str_c("    sub-iteration ", run, ": running\n"))
    } else {
      wait <- sample(30:120, 1)
      cat("    ... waiting ", wait, " seconds before continuing with sub-iteration", 
      run, "...\n")
      Sys.sleep(wait)
      cat(str_c("    sub-iteration ", run, ": running\n"))
    }
    ## step 1
    if (length(poolidx) >= 1000) {
      batchidx <- sample(poolidx, size = 1000, replace = FALSE)
          # sample 1000 records out of records pool
    } else {
      batchidx <- poolidx
    }
    testdupl <- batchidx[-c(which(duplicated(mails[batchidx,3])), 
                            which(duplicated(mails[batchidx,4])))]
    if (length(testdupl) != 0) {
      batchidx <- testdupl
    }
        # drop records in sample with duplicate names
    batch <- mails[batchidx,]
        # prepare email batch
    if (is.null(batches)){
      batches <- list(batch)
    } else {
      batches <- c(batches, list(batch))
    }
    poolidx <- poolidx[-which(poolidx %in% batchidx)]
    # add break if data does merely contain unique name elements either fore or surname
    # import
    batch_out <- data.table("Name" = batch$surname, "Given Name" = batch$forename,
                        "E-mail 1 - Value" = batch$email)
    write.csv(batch_out, file = "batch_out.csv", row.names = FALSE, quote = FALSE)
    # error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", value = "//div[@class = 'RM LJOhwe'][2]")),silent = TRUE)) # old "Import contacts" =  'RM ou' # OUTDATED
    error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", value = "//a[@class = 'NMrsfe']")),silent = TRUE))
    while (class(error_check) == "try-error") {
      Sys.sleep(6)
      # error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", value = "//div[@class = 'RM LJOhwe'][2]")),silent = TRUE)) # OUTDATED
      error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", value = "//div[@class = 'yWE1y LJOhwe'][2]")),silent = TRUE))
    }
    except <- 1
    while (except == 1) {
      # remDr1$findElement(using = "xpath", value = "//div[@class = 'RM LJOhwe'][2]")$clickElement() # OUTDATED
      remDr1$findElement(using = "xpath", value =  "//div[@class = 'yWE1y LJOhwe'][2]")$clickElement()
      Sys.sleep(sample(5:10, 1))
      import <- remDr1$findElement(using = "xpath", "//input[@type = 'file']")
      import$sendKeysToElement(list(folder))
      Sys.sleep(sample(5:10, 1))
      remDr1$findElement(using = "xpath", value = "//button[@name = 'ok']")$clickElement()
      Sys.sleep(sample(1:2, 1))
      error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", value = "//div[@class = 'jARnrf']")),silent = TRUE))
      if (class(error_check) == "try-error") {
        except <- 0
      } else {
        remDr1$findElement(using = "xpath", value = "//span[@class = 'Kj-JD-K7-Jq']")$clickElement()
        Sys.sleep(sample(5:10, 1))
      }
    }
    remDr2$findElement(using = "id", value = "user-dropdown-toggle")$clickElement()
    # remDr2$findElement(using = "xpath", value = "//li[@class = 'current-user']")$clickElement()
    remDr2$findElement(using = "xpath", value = "//li[@data-name = 'lists']")$clickElement()
    Sys.sleep(sample(5:10, 1))
    error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'js-find-friends-link js-nav']")),silent = TRUE))
    while (class(error_check) == "try-error") {
      Sys.sleep(6)
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'js-find-friends-link js-nav']")),silent = TRUE))
    }
    remDr2$findElement(using = "xpath", value = "//a[@class = 'js-find-friends-link js-nav']")$clickElement()
    Sys.sleep(sample(5:10, 1))
    error_check <-  suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'contacts-link']")),silent = TRUE))
    iter <- 0
    while (class(error_check) == "try-error") {
      Sys.sleep(6)
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'contacts-link']")),silent = TRUE))
      iter <- iter + 1
      if (iter >1) {
        remDr2$refresh()
        Sys.sleep(6)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'js-find-friends-link js-nav']")),silent = TRUE))
        if (class(error_check) != "try-error") {
        remDr2$findElement(using = "xpath", value = "//a[@class = 'js-find-friends-link js-nav']")$clickElement()
        } else {
          remDr2$findElement(using = "id", value = "user-dropdown-toggle")$clickElement()
          remDr2$findElement(using = "xpath", value = "//li[@data-name = 'lists']")$clickElement()
          Sys.sleep(6)
          error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'js-find-friends-link js-nav']")),silent = TRUE))
          if (class(error_check) != "try-error") {
            remDr2$findElement(using = "xpath", value = "//a[@class = 'js-find-friends-link js-nav']")$clickElement()
          }
        }
        Sys.sleep(10)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'contacts-link']")),silent = TRUE))
      }
    }
    Sys.sleep(sample(5:10, 1))
    remDr2$findElement(using = "xpath", value = "//a[@class = 'contacts-link']")$clickElement()
    if (run == 1) {
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "user_data_dashboard_auth_password")), silent = TRUE))
      while (class(error_check) == "try-error") {
        Sys.sleep(6)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "user_data_dashboard_auth_password")),silent = TRUE))
      }
      login3b <- remDr2$findElement(using = "id", value = "user_data_dashboard_auth_password")
      Sys.sleep(5)
      login3b$sendKeysToElement(list(pass, key = 'enter'))
    } else  {
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "user_data_dashboard_auth_password")), silent = TRUE))
      while (class(error_check) == "try-error") {
        Sys.sleep(6)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "user_data_dashboard_auth_password")),silent = TRUE))
        error_check2 <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'EdgeButton EdgeButton--primary contacts-import-btn']")),silent = TRUE))
        if (class(error_check2) != "try-error") {
          break
        }
      }
      if (class(error_check) != "try-error") {
        login3b <- remDr2$findElement(using = "id", value = "user_data_dashboard_auth_password")
        Sys.sleep(5)
        login3b$sendKeysToElement(list(pass, key = 'enter'))
      }
    }
    Sys.sleep(sample(5:10, 1))
    error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//div[@class = 'contacts-operation']")),silent = TRUE))
    while (class(error_check) == "try-error") {
      Sys.sleep(6)
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "////div[@class = 'contacts-operation']")),silent = TRUE))
    }
    remDr2$findElement(using = "xpath", value = "//div[@class = 'contacts-operation']/button")$clickElement()
    Sys.sleep(sample(5:10, 1))
    error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "wipe-addressbook-dialog-body")),silent = TRUE))
    iterx <- 0
    while (class(error_check) == "try-error") {
      Sys.sleep(3)
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "wipe-addressbook-dialog-body")),silent = TRUE))
      iterx <- iterx + 1
      if (iterx > 2) {
          break
      }
    }
    if (class(error_check) != "try-error") {
      remDr2$findElement(using = "xpath", value = "//div[@class = 'modal-footer']/button[2]")$clickElement()
    }
    error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'EdgeButton EdgeButton--primary contacts-import-btn']")),silent = TRUE))
    while (class(error_check) == "try-error") {
      Sys.sleep(6)
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'EdgeButton EdgeButton--primary contacts-import-btn']")),silent = TRUE))
    }
    remDr2$findElement(using = "xpath", value = "//a[@class = 'EdgeButton EdgeButton--primary contacts-import-btn']")$clickElement()
    Sys.sleep(sample(5:10, 1))
    remDr2$findElement(using = "xpath", value = "//li[@data-service = 'gmail']/button[@type = 'button']")$clickElement()
    Sys.sleep(sample(5:10, 1))
    winhan1 <- remDr2$getWindowHandles()[[1]]
    winhan2 <- remDr2$getWindowHandles()[[2]]
    switchWin(remDr = remDr2, windowId = winhan2)
    Sys.sleep(sample(5:10, 1))
    if (run == 1) {
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", "identifierId")),silent = TRUE))
      while (class(error_check) == "try-error") {
        Sys.sleep(2)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", "identifierId")),silent = TRUE))
      }
      #while (length(remDr2$executeScript(script = "return document.getElementById('identifierId');", 
      #                                   args = list())) == 0) {
      #  # check if element completely loaded on remDr1 (gmail)
      #  Sys.sleep(2)
      #  # if not wait and check again
      #}
      login3a <- remDr2$findElement(using = "id", "identifierId")
      login3a$sendKeysToElement(list(email2, key = 'enter'))
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", "password")),silent = TRUE))
      while (class(error_check) == "try-error") {
        Sys.sleep(2)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", "password")),silent = TRUE))
      }
      #while (length(remDr2$executeScript(script = "return document.getElementById('password');", 
      #                                 args = list())) == 0) {
      ## check if element completely loaded on remDr1 (gmail)
      #  Sys.sleep(2)
      ## if not wait and check again
      #  }
      login3b <- remDr2$findElement(using = "xpath", "//input[@type = 'password']")
      Sys.sleep(sample(5:10, 1))
      login3b$sendKeysToElement(list(pass2, key = 'enter'))
      # enter password on gmail and continue
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "submit_approve_access")), silent = TRUE))
      while (class(error_check) == "try-error") {
        Sys.sleep(6)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "submit_approve_access")),silent = TRUE))
      }
      Sys.sleep(5)
      remDr2$findElement(using = "id", value = "submit_approve_access")$clickElement()
    } else {
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//div[@class = 'TnvOCe k6Zj8d XraQ3b']")),silent = TRUE))
      iter <- 0
      while (class(error_check) == "try-error") {
        Sys.sleep(6)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//div[@class = 'TnvOCe k6Zj8d XraQ3b']")),silent = TRUE))
        iter <- iter + 1
        if (iter >3) {
          remDr2$refresh()
          Sys.sleep(6)
          error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//div[@class = 'TnvOCe k6Zj8d XraQ3b']")),silent = TRUE))
          if (class(error_check) != "try-error") {
            break
          }
        }
      }
      remDr2$findElement(using = "xpath", value = "//div[@class = 'TnvOCe k6Zj8d XraQ3b']")$clickElement()
      Sys.sleep(sample(5:10, 1))
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "submit_approve_access")),silent = TRUE))
      iter <- 0
      while (class(error_check) == "try-error") {
        Sys.sleep(6)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "submit_approve_access")),silent = TRUE))
        iter <- iter + 1
        if (iter >3) {
          remDr2$refresh()
          Sys.sleep(6)
          error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "submit_approve_access")),silent = TRUE))
          if (class(error_check) != "try-error") {
            break
          }
        }
      }
      remDr2$findElement(using = "id", value = "submit_approve_access")$clickElement()
    }
    switchWin(remDr = remDr2, windowId = winhan1)
    error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "timeline")),silent = TRUE))
    while (class(error_check) == "try-error") {
      Sys.sleep(6)
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "timeline")),silent = TRUE))
    }
    error_check <- suppressMessages(try(unlist(remDr2$findElement(using= "xpath", value = "//div[@class = 'selector compensate-for-scrollbar']")),silent = TRUE))
    if (class(error_check) != "try-error") {
      catch_target <- 0
      catch_have <- 1
      while (catch_have != catch_target) {
        scroll <- remDr2$findElement(using = "id", value = "timeline")
        rep(scroll$sendKeysToElement(list(key = "end")), 5)
        Sys.sleep(5)
        elem <- remDr2$findElement(using = "id", value = "timeline")
        elemtxt <- elem$getElementAttribute("outerHTML")[[1]] %>%
          htmlParse()
        Sys.sleep(sample(5:10, 1))
        t_data <- data.table(handles = xpathSApply(elemtxt, "//div[@class = 'stream-item-content account listview-find-friends-result js-actionable-user selected']", xmlGetAttr, "data-screen-name"),
                             ids = xpathSApply(elemtxt, "//div[@class = 'stream-item-content account listview-find-friends-result js-actionable-user selected']", xmlGetAttr, "data-user-id"),
                             names = xpathSApply(elemtxt, "//strong[@class = 'fullname u-textTruncate']", xmlValue))
        catch_target <- remDr2$findElement(using = "xpath", value = "//div[@class = 'selector compensate-for-scrollbar']/label")
        catch_target <- catch_target$getElementAttribute("outerHTML")[[1]] %>%
          htmlParse()
        catch_target <- xpathSApply(catch_target, "//label", xmlValue) %>%
          str_extract("[[:digit:]]+") %>%
          as.numeric()
        catch_have <- dim(t_data)[1]
      }
      if (is.null(t_data_all)){
        t_data_all <- list(t_data)
        } else {
          t_data_all <- c(t_data_all, list(t_data))
          }
      #### NAME MATCHING
      t_names <- t_data$names %>%
        str_split(" |_") %>%
        lapply(tolower)
      first <- lapply(t_names, head, n = 1L) %>% unlist
      position1 <- match(first, batch_out[[2]], nomatch=0)
      if (any(position1 != 0)) {
        position2 <- which(position1 != 0)
        batch_out[position1[position1 != 0], "t_handle" :=  t_data[position2,]$handles]
        batch_out[position1[position1 != 0], "t_names" := t_data[position2,]$names]
        batch_out[position1[position1 != 0], "t_ids" := t_data[position2,]$ids]
        t_names <- t_names[-position2]
        t_data <- t_data[-position2,]
        }
      last <- lapply(t_names, tail, n = 1L) %>% unlist
      position1 <- match(last, batch_out[[1]], nomatch=0)
      if (any(position1 != 0)) {
        position2 <- which(position1 != 0)
        batch_out[position1[position1 != 0], "t_handle" :=  t_data[position2,]$handles]
        batch_out[position1[position1 != 0], "t_names" := t_data[position2,]$names]
        batch_out[position1[position1 != 0], "t_ids" := t_data[position2,]$ids]
        t_names <- t_names[-position2]
        t_data <- t_data[-position2,]
        }
      second <- lapply(t_names, head, n = 2L) %>%
        lapply(., function(x) x[2]) %>% 
        unlist
      position1 <- match(second, batch_out[[1]], nomatch=0)
      if (any(position1 != 0)) {
        position2 <- which(position1 != 0)
        batch_out[position1[position1 != 0], "t_handle" :=  t_data[position2,]$handles]
        batch_out[position1[position1 != 0], "t_names" := t_data[position2,]$names]
        batch_out[position1[position1 != 0], "t_ids" := t_data[position2,]$ids]
        t_names <- t_names[-position2]
        t_data <- t_data[-position2,]
        }
      second <- lapply(t_names, head, n = 2L) %>%
        lapply(., function(x) x[2]) %>% 
        unlist
      position1 <- match(second, batch_out[[2]], nomatch=0)
      if (any(position1 != 0)) {
        position2 <- which(position1 != 0)
        batch_out[position1[position1 != 0], "t_handle" :=  t_data[position2,]$handles]
        batch_out[position1[position1 != 0], "t_names" := t_data[position2,]$names]
        batch_out[position1[position1 != 0], "t_ids" := t_data[position2,]$ids]
        t_data <- t_data[-position2,]
        }
      t_names <- str_split(t_data$handles, "\\.|_|(?=([[:upper:]]=?))")
      last <- lapply(t_names, tail, n = 1L) %>% 
        lapply(tolower) %>% 
        unlist
      position1 <- match(last, batch_out[[1]], nomatch=0)
      if (any(position1 != 0)) {
        position2 <- which(position1 != 0)
        batch_out[position1[position1 != 0], "t_handle" :=  t_data[position2,]$handles]
        batch_out[position1[position1 != 0], "t_names" := t_data[position2,]$names]
        batch_out[position1[position1 != 0], "t_ids" := t_data[position2,]$ids]
      }
      if (dim(batch_out)[2] == 6) {
        if (is.null(matched)) {
          matched <- na.omit(batch_out)
          } else {
            matched <- rbind(matched, na.omit(batch_out))
        }
      }
    }
    remDr2$goBack()
    Sys.sleep(sample(5:10, 1))
    error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'contacts-link']")),silent = TRUE))
    iter <- 1
    while (class(error_check) == "try-error") {
      Sys.sleep(6)
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'contacts-link']")),silent = TRUE))
      if (iter >3) {
        remDr2$goBack()
        remDr2$goForward()
        Sys.sleep(6)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//a[@class = 'contacts-link']")),silent = TRUE))
        if (class(error_check) != "try-error") {
          break
        }
      }
      iter <- iter+1
    }
    remDr2$findElement(using = "xpath", value = "//a[@class = 'contacts-link']")$clickElement()
    error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "user_data_dashboard_auth_password")), silent = TRUE))
    while (class(error_check) == "try-error") {
        Sys.sleep(6)
        error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", value = "user_data_dashboard_auth_password")),silent = TRUE))
        error_check2 <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//div[@class = 'contacts-operation']")),silent = TRUE))
        if (class(error_check2) != "try-error") {
          break
        }
      }
      if (class(error_check) != "try-error") {
        login3b <- remDr2$findElement(using = "id", value = "user_data_dashboard_auth_password")
        Sys.sleep(5)
        login3b$sendKeysToElement(list(pass, key = 'enter'))
      }
    error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "//div[@class = 'contacts-operation']")),silent = TRUE))
    while (class(error_check) == "try-error") {
      Sys.sleep(6)
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "xpath", value = "////div[@class = 'contacts-operation']")),silent = TRUE))
    }
    remDr2$findElement(using = "xpath", value = "//div[@class = 'contacts-operation']/button")$clickElement()
    Sys.sleep(sample(5:10, 1))
    error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", "wipe-addressbook-dialog-body")),silent = TRUE))
    while (class(error_check) == "try-error") {
      Sys.sleep(2)
      error_check <- suppressMessages(try(unlist(remDr2$findElement(using = "id", "wipe-addressbook-dialog-body")),silent = TRUE))
    }
    #while (length(remDr2$executeScript(script = "return document.getElementById('wipe-addressbook-dialog-body');", 
    #                                   args = list())) == 0) {
    #  # check if element completely loaded on remDr1 (gmail)
    #  Sys.sleep(2)
    #  # if not wait and check again
    #}
    remDr2$findElement(using = "xpath", value = "//div[@class = 'modal-footer']/button[2]")$clickElement()
    # delete
    delcount <- 0
    Sys.sleep(sample(5:10, 1))
    while (!is.na(delcount)) {
      remDr1$findElement(using = "xpath", "//div[@type = 'checkbox']")$clickElement()
      # error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", "//div[@class = 'T-I J-J5-Ji T-I-ax7 T-I-Js-Gs L3'][2]")),silent = TRUE)) # OUTDATED
      error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "id", ":ra")),silent = TRUE))
      iter <- 0
      while (class(error_check) == "try-error") {
        Sys.sleep(6)
        # error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", "//div[@class = 'T-I J-J5-Ji T-I-ax7 T-I-Js-Gs L3'][2]")),silent = TRUE)) # OUTDATED
        error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "id", ":ra")),silent = TRUE))
        if (iter > 2) {
          remDr1$refresh()
          Sys.sleep(10)
          remDr1$findElement(using = "xpath", "//div[@type = 'checkbox']")$clickElement()
          Sys.sleep(5)
          # error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "xpath", "//div[@class = 'T-I J-J5-Ji T-I-ax7 T-I-Js-Gs L3'][2]")),silent = TRUE)) # OUTDATED
          error_check <- suppressMessages(try(unlist(remDr1$findElement(using = "id", ":ra")),silent = TRUE))
        }
        iter <- iter + 1 
      }
      # remDr1$findElement(using = "xpath", "//div[@class = 'T-I J-J5-Ji T-I-ax7 T-I-Js-Gs L3'][2]")$clickElement() # OUTDATED
      remDr1$findElement(using = "id", ":ra")$clickElement()
      Sys.sleep(2)
      if (dim(batch_out)[1] > 1) {
        # remDr1$findElement(using = "xpath", "//div[@class = 'J-N LJOhwe'][2]")$clickElement() # OUTDATED
        remDr1$findElement(using = "xpath", "//div[@class = 'VIpgJd-j7LFlb LJOhwe'][2]")$clickElement()
      } else {
        # remDr1$findElement(using = "xpath", "//div[@class = 'J-N LJOhwe'][1]")$clickElement() # OUTDATED
        remDr1$findElement(using = "xpath", "//div[@class = 'VIpgJd-j7LFlb LJOhwe'][1]")$clickElement()
      }
      Sys.sleep(5)
      # delcount <- remDr1$findElement(using = "xpath", value = "//a[@class = 'J-Ke n0']") # OUTDATED
      delcount <- remDr1$findElement(using = "xpath", value = "//a[@class = 'VIpgJd-hSRGPd ceRIQe']")
      delcount <- str_extract(delcount$getElementText(), pattern = "[[:digit:]]+")
    }
    cat(str_c("    sub-iteration ", run, ": completed\n"))
    run <- run + 1
  }
  residual <- mails[poolidx,]
  result <- list(matched, batches, t_data_all, residual)
  return(result)
}

#### twitterApiFollowers ================================================================

# 'twitterApiFollowers' sends GET requests to the Twitter REST API in order to retrieve
# IDs of followers of specified Twitter users. The function takes a list of query 
# parameters in the 'query' argument and a signed OAuth request in the 'signature' 
# argument. Upon response codes 500 (Internal Server Error), 502 (Bad Gateway), 503 
# (Service Unavailable), and 504 (Gateway timeout) the function retries up to a 1000 
# times with an exponential waiting time.

twitterApiFollowers <- function (query, signature) {
  try(response <- httr::RETRY("GET", 
                              "https://api.twitter.com/1.1/followers/ids.json?",
                              query = query,
                              config = signature,
                              times = 1000,
                              quiet = FALSE,
                              terminate_on = c(200, 304, 400, 401, 403, 404, 406, 410, 
                                               422, 429)),
      silent = TRUE)
  return(response)
}


#### twitterApiFriends ==================================================================

# 'twitterApiFriends' sends GET requests to the Twitter REST API in order to retrieve
# IDs of friends of specified Twitter users. The function takes a list of query 
# parameters in the 'query' argument and a signed OAuth request in the 'signature' 
# argument. Upon response codes 500 (Internal Server Error), 502 (Bad Gateway), 503 
# (Service Unavailable), and 504 (Gateway timeout) the function retries up to a 1000 
# times with an exponential waiting time.

twitterApiFriends <- function (query, signature) {
  try(response <- httr::RETRY("GET", 
                              "https://api.twitter.com/1.1/friends/ids.json?",
                              query = query,
                              config = signature,
                              times = 1000,
                              quiet = FALSE,
                              terminate_on = c(200, 304, 400, 401, 403, 404, 406, 410, 
                                               422, 429)),
      silent = TRUE)
  return(response)
}


#### twitterApiLikes ====================================================================

# 'twitterApiLikes' sends GET requests to the Twitter REST API in order to retrieve Tweet 
# objects of status updates liked by specified Twitter users. The function takes a list 
# of query parameters in the 'query' argument and a signed OAuth request in the 
# 'signature' argument. Upon response codes 500 (Internal Server Error), 502 
# (Bad Gateway), 503 (Service Unavailable), and 504 (Gateway timeout) the function 
# retries up to a 1000 times with an exponential waiting time.

twitterApiLikes <- function (query, signature) {
  try(response <- httr::RETRY("GET", 
                              "https://api.twitter.com/1.1/favorites/list.json?",
                              query = query,
                              config = signature,
                              times = 1000,
                              quiet = FALSE,
                              terminate_on = c(200, 304, 400, 401, 403, 404, 406, 410, 
                                               422, 429)),
      silent = TRUE)
  return(response)
}


#### twitterApiStatuses =================================================================

# 'twitterApiStatuses' sends GET requests to the Twitter REST API in order to retrieve   
# Twitter objects of status updates by specified Twitter users. The function takes a list 
# of query parameters in the 'query' argument and a signed OAuth request in the 
# 'signature' argument. Upon response codes 500 (Internal Server Error), 502 
# (Bad Gateway), 503 (Service Unavailable), and 504 (Gateway timeout) the function 
# retries up to a 1000 times with an exponential waiting time.

twitterApiStatuses <- function (query, signature) {
  try(response <- httr::RETRY("GET", 
                              "https://api.twitter.com/1.1/statuses/user_timeline.json?",
                              query = query,
                              config = signature,
                              times = 1000,
                              quiet = FALSE,
                              terminate_on = c(200, 304, 400, 401, 403, 404, 406, 410, 
                                               422, 429)),
      silent = TRUE)
  return(response)
}


#### twitterLookup ======================================================================

# 'twitterLookup' collects basic information on Twitter users as well as activity counts 
# and computes changes to previous lookups (if available). The function distuingishes 
# between first and subsequent lookups. For the first lookup, the 'record' argument
# takes a data frame containing the columns 't_ids' (twitter ids) and 't_handle' (twitter
# handles). From the second lookup onwards the 'record' argument expects a list with
# multiple data frames as in part generated by the first lookup as well as a necessary
# call to the follow-up function 'twitterCollector'. The optional 'credentials' argument 
# takes a data frame with columns 'app', 'consumer_key', 'consumer_secret', 'access_token',
# 'access_secret', 'status_lookup', and 'reset_lookup'. The argument is not optional in
# the first lookup.

lookup <- function(record, credentials, connection) {
  # check if this is the first lookup (1L) based on whether record is still a single data.frame
  first_lookup <- ifelse(is.data.frame(record), 1L, 0L)
  # if this is not the first lookup, pass record[[2]] to credentials
  if (first_lookup == 0L & missing(credentials)) {
    credentials <- record[[2]]
  } 
  # open all windows with rate limits reset since last use
  credentials$status_lookup[which(is.na(credentials$reset_lookup))] <- "open"
  credentials$status_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- "open"
  credentials$reset_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- NA
  # if no window open, do:
  while (length(which(credentials$status_lookup == "open")) == 0) {
    # wait until the first window opens
    Sys.sleep(ifelse(min(credentials$reset_lookup - Sys.time(), na.rm = TRUE) < 0, 
                     0, 
                     min(credentials$reset_lookup - Sys.time(), na.rm = TRUE)))
    # open all windows with rate limits reset
    credentials$status_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- "open"
  }
  # randomly select app-user credentials
  app_user <- resample((1:nrow(credentials))[which(credentials$status_lookup == "open")], 1)
  # close respective window
  credentials[app_user,]$status_lookup <- "closed"
  # suppress caching in this session
  options(httr_oauth_cache = FALSE)
  # create OAuth application
  application <- oauth_app(appname = "twitter",
                           key = credentials$consumer_key[app_user], 
                           secret = credentials$consumer_secret[app_user])
  # sign OAuth request
  signature <- sign_oauth1.0(application, 
                             token = credentials$access_token[app_user],
                             token_secret = credentials$access_secret[app_user],
                             httr_oauth_cache = FALSE)
  # set up request counter
  request_counter <- 0
  # setup retry frame
  retry_record <- as.data.frame(matrix(nrow = 0, ncol = 2, dimnames = list(c(), c("index", "attempts"))))
  # if first lookup, do
  if (first_lookup == 1L) {
    # build index of units to query
    index <- 1:nrow(record)
    # create request_mode, protected, verified, and geo_enabled columns in record
    record$request_mode <- NA
    record[, c("protected", "verified", "geo_enabled")] <- NA
    # create beginning data (static)
    beginning_followers <- data.frame(t_ids = record$t_ids, 
                                      date = Sys.Date(), 
                                      count = NA, 
                                      ids = NA,
                                      stringsAsFactors = FALSE)
    beginning_friends <- data.frame(t_ids = record$t_ids, 
                                    date = Sys.Date(), 
                                    count = NA,
                                    ids = NA,
                                    stringsAsFactors = FALSE)
    beginning_likes <- data.frame(t_ids = record$t_ids, 
                                  date = Sys.Date(), 
                                  count = NA,
                                  ids = NA,
                                  text = NA,
                                  source = NA,
                                  created = NA,
                                  stringsAsFactors = FALSE)
    beginning_statuses <- data.frame(t_ids = record$t_ids, 
                                     date = Sys.Date(),
                                     count = NA,
                                     ids = NA,
                                     text = NA,
                                     reply = NA,
                                     reply_ids = NA,
                                     created = NA,
                                     stringsAsFactors = FALSE)
    # create tracker data (updated every lookup)
    tracker_followers <- data.frame(t_ids = record$t_ids, 
                                    count_previous = NA,
                                    count_current = NA,
                                    change = NA,
                                    ids = NA,
                                    stringsAsFactors = FALSE)
    tracker_friends <- data.frame(t_ids = record$t_ids, 
                                  count_previous = NA,
                                  count_current = NA,
                                  change = NA,
                                  ids = NA,
                                  stringsAsFactors = FALSE)
    tracker_likes <- data.frame(t_ids = record$t_ids,
                                count_previous = NA,
                                count_current = NA,
                                change = NA,
                                ids = NA,
                                since_ids = NA,
                                stringsAsFactors = FALSE)
    tracker_statuses <- data.frame(t_ids = record$t_ids,
                                   count_previous = NA,
                                   count_current = NA,
                                   change = NA,
                                   since_ids = NA,
                                   stringsAsFactors = FALSE)
  } else {
    # build index of units to query
    index <- 1:nrow(record[[1]])
  }
  # While the index is not empty, do:
  while (length(index) > 0) {
    # sample from index 100 units (max. 100 units per request)
    if (length(index) >= 100) {
      request_batch <- sample(index, size = 100, replace = FALSE)
    } else {
      request_batch <- index
    }
    # update the main index
    index <- index[-which(index %in% request_batch)]
    if (first_lookup == 1L) {
      # retrieve request_batch specific user ids from user data
      user_ids <- record$t_ids[request_batch]
      # build comma separated list of user_ids
      user_ids <- paste0(user_ids, collapse = ",")
      rate_limit_counter <- character(0)
      while(length(rate_limit_counter) == 0) {
        try(response <- httr::RETRY("GET", 
                                    "https://api.twitter.com/1.1/users/lookup.json?",
                                    query = list(user_id = user_ids),
                                    config = signature,
                                    times = 1000,
                                    quiet = FALSE,
                                    terminate_on = c(200, 304, 400, 401, 403, 404, 406, 410, 422, 429)
          ),
        silent = TRUE
        )
        rate_limit_counter <- as.integer(headers(response)$`x-rate-limit-remaining`)
        if(length(rate_limit_counter) == 0) {
          Sys.sleep(2)
        }
      }
      if (as.integer(str_replace(headers(response)$status, " .+", "")) %in% c(400, 401, 403, 406, 410, 422, 429)) {
        break
      }
      if (as.integer(str_replace(headers(response)$status, " .+", "")) == 200) {
        response_content <- httr::content(response)
      } else {
        response_content <- list(NA)
      }
    } else {
      # retrieve request_batch specific user ids from user data with request mode == 0
      user_ids <- record[[1]]$t_ids[request_batch[which(record[[1]]$request_mode[request_batch] == 0)]]
      # build comma separated list of user_ids
      user_ids <- paste0(user_ids, collapse = ",")
      # set screen_names to empty string
      screen_names <- ""
      # retrieve request_batch specific screen names from user data if any request mode == 0
      if (any(record[[1]]$request_mode[request_batch] == 1)) {
        # retrieve request_batch specific screen names from user data with request mode == 1
        screen_names <- record[[1]]$t_handle[request_batch[which(record[[1]]$request_mode[request_batch] == 1)]]
        # build comma separated list of user_ids
        screen_names <- paste0(screen_names, collapse = ",")
      }
      rate_limit_counter <- character(0)
      while(length(rate_limit_counter) == 0) {
        try(response <- httr::RETRY("GET", 
                                    "https://api.twitter.com/1.1/users/lookup.json?",
                                    query = list(user_id = user_ids,
                                                 screen_name = screen_names),
                                    config = signature,
                                    times = 1000,
                                    quiet = FALSE,
                                    terminate_on = c(200, 304, 400, 401, 403, 404, 406, 410, 422, 429)
          ),
        silent = TRUE
        )
        rate_limit_counter <- as.integer(headers(response)$`x-rate-limit-remaining`)
        if(length(rate_limit_counter) == 0) {
          Sys.sleep(2)
        }
      }
      if (as.integer(str_replace(headers(response)$status, " .+", "")) %in% c(400, 401, 403, 406, 410, 422, 429)) {
        break
      }
      if (as.integer(str_replace(headers(response)$status, " .+", "")) == 200) {
        response_content <- httr::content(response)
      } else {
        response_content <- list(NA)
      }
    }
    # increment request counter
    request_counter <- request_counter + 1
    # record http status code
    status <- headers(response)$status
    # record remaining requests for app_token combination
    rate_limit_counter <- as.integer(headers(response)$`x-rate-limit-remaining`)
    # print info to console 
    #cat(request_batch)
    cat(request_counter, ". request:\n--- http status code = ", status, "\n--- remaining requests = ", rate_limit_counter, "\n--- app-user combination = ", app_user, "\n--- remaining units = ", length(index), "\n\n", sep = "")
    if (rate_limit_counter == 0) {
      # open all windows with rate limits reset, except, if open again, the current
      credentials$status_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- "open"
      # record for current app-user combination time rate limit is reset
      credentials$reset_lookup[app_user] <- with_tz(as.POSIXct(as.integer(headers(response)$`x-rate-limit-reset`), origin = "1970-01-01", tz="GMT"), tz = "CET")
      if (length(which(credentials$status_lookup == "open")) == 0) {
        # wait until the first window opens
        Sys.sleep(ifelse(min(credentials$reset_lookup - Sys.time(), na.rm = TRUE) < 0, 
                         0, 
                         min(credentials$reset_lookup - Sys.time(), na.rm = TRUE)))
        # open all windows with rate limits reset
        credentials$status_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- "open"
      }
      # select new app-user combination
      app_user <- resample((1:nrow(credentials))[which(credentials$status_lookup == "open")], 1)
      # close respective window
      credentials[app_user,]$status_lookup <- "closed"
      # create OAuth application
      application <- oauth_app(appname = "twitter",
                               key = credentials$consumer_key[app_user], 
                               secret = credentials$consumer_secret[app_user])
      # sign OAuth request
      signature <- sign_oauth1.0(application, 
                                 token = credentials$access_token[app_user],
                                 token_secret = credentials$access_secret[app_user])
    }
    # if first look up, do:
    if (first_lookup == 1L) {
      # which element in request batch was captured by the unique id
      success <- str_split(string = user_ids, pattern = ",", simplify = TRUE) %in%
        sapply(httr::content(response), `$.data.frame`, "id_str")
      # record request mode in user_data, 0L = user id, 1L = screen name
      record$request_mode[request_batch] <- ifelse(success, 0L, 1L)
      if (length(request_batch[!success]) > 0) {
        # retrieve request_batch specific screen names from user data
        screen_names <- record$t_handle[request_batch[!success]]
        # build comma separated list of screen names
        screen_names <- paste0(screen_names, collapse = ",")
        rate_limit_counter <- character(0)
        while(length(rate_limit_counter) == 0) {
          try(response_2 <- httr::RETRY("GET", 
                                        "https://api.twitter.com/1.1/users/lookup.json?",
                                        query = list(screen_name = screen_names),
                                        config = signature,
                                        times = 1000,
                                        quiet = FALSE,
                                        terminate_on = c(200, 304, 400, 401, 403, 404, 406, 410, 422, 429)
          ),
          silent = TRUE
          )
          rate_limit_counter <- as.integer(headers(response_2)$`x-rate-limit-remaining`)
          if(length(rate_limit_counter) == 0) {
            Sys.sleep(2)
          }
        }
        if (as.integer(str_replace(headers(response_2)$status, " .+", "")) %in% c(400, 401, 403, 406, 410, 422, 429)) {
          break
        }
        # increment request counter
        request_counter <- request_counter + 1
        # record http status code
        status <- headers(response_2)$status
        # record remaining requests for app_token combination
        rate_limit_counter <- as.integer(headers(response_2)$`x-rate-limit-remaining`)
        # print info to console
        cat(request_counter, ". request (2nd try):\n--- http status code = ", status, "\n--- remaining requests = ", rate_limit_counter, "\n--- app-user combination = ", app_user, "\n--- remaining units = ", length(index), "\n\n", sep = "")
        if (rate_limit_counter == 0) {
          # open all windows with rate limits reset, except, if open again, the current
          credentials$status_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- "open"
          # record for current app-user combination time rate limit is reset
          credentials$reset_lookup[app_user] <- with_tz(as.POSIXct(as.integer(headers(response)$`x-rate-limit-reset`), origin = "1970-01-01", tz="GMT"), tz = "CET")
          if (length(which(credentials$status_lookup == "open")) == 0) {
            # wait until the first window opens
            Sys.sleep(ifelse(min(credentials$reset_lookup - Sys.time(), na.rm = TRUE) < 0, 
                             0, 
                             min(credentials$reset_lookup - Sys.time(), na.rm = TRUE)))
            # open all windows with rate limits reset
            credentials$status_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- "open"
          }
          # select new app-user combination
          app_user <- resample((1:nrow(credentials))[which(credentials$status_lookup == "open")], 1)
          # close respective window
          credentials[app_user,]$status_lookup <- "closed"
          # create OAuth application
          application <- oauth_app(appname = "twitter",
                                   key = credentials$consumer_key[app_user], 
                                   secret = credentials$consumer_secret[app_user])
          # sign OAuth request
          signature <- sign_oauth1.0(application, 
                                     token = credentials$access_token[app_user],
                                     token_secret = credentials$access_secret[app_user])
        }
        if (status == "200 OK") {
          ids2 <- sapply(httr::content(response_2), `$.data.frame`, "id_str")
          if (any(ids2 %in% record$t_ids[request_batch] == TRUE)) {
            # which element in request batch was captured by the screen name
            success_2 <- str_split(string = screen_names, pattern = ",", simplify = TRUE) %in%
              sapply(httr::content(response_2), `$.data.frame`, "screen_name")
            # record request mode in user_data, 1L = screen name, NA = not found
            record$request_mode[request_batch[!success]] <- ifelse(success_2 & ids2 %in% record$t_ids[request_batch], 1L, NA)
            response_content <- c(response_content, httr::content(response_2)[ids2 %in% record$t_ids[request_batch]])
            # RETRY THOSE !Success, that ultimately didn't result in a response
            if (length(request_batch[!success][!success_2]) > 0) {
              for (e in 1:length(request_batch[!success][!success_2])) {
                if (request_batch[!success][!success_2][e] %in% retry_record$index) {
                  retry_record[retry_record$index == request_batch[!success][!success_2][e], "attempts"] <- retry_record[retry_record$index == request_batch[!success][!success_2][e], "attempts"] + 1 
                } else {
                  retry_record <- rbind(retry_record, data.frame(index = request_batch[!success][!success_2][e], attempts = 1))
                }
                if (retry_record[retry_record$index == request_batch[!success][!success_2][e], "attempts"] < 3) {
                  index <- c(index, retry_record$index[e])
                }
              }
            }
          }
        } else {
          record$request_mode[request_batch[!success]] <- NA
        }
      }
    }
    if (first_lookup == 1L) {
      ids <- sapply(response_content, `$.data.frame`, "id_str")
      beginning_followers$count[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "followers_count")
      beginning_friends$count[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "friends_count")
      beginning_likes$count[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "favourites_count")
      beginning_statuses$count[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "statuses_count")
      tracker_followers$count_previous[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "followers_count")
      tracker_friends$count_previous[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "friends_count")
      tracker_likes$count_previous[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "favourites_count")
      tracker_statuses$count_previous[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "statuses_count")
      record$protected[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "protected")
      record$verified[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "verified")
      record$geo_enabled[match(ids, record$t_ids)] <- sapply(response_content, `$.data.frame`, "geo_enabled")
    } else {
      ids <- sapply(response_content, `$.data.frame`, "id_str")
      record[[3]]$count_current[match(ids, record[[3]]$t_ids)] <- sapply(response_content, `$.data.frame`, "followers_count")
      record[[4]]$count_current[match(ids, record[[4]]$t_ids)] <- sapply(response_content, `$.data.frame`, "friends_count")
      record[[5]]$count_current[match(ids, record[[5]]$t_ids)] <- sapply(response_content, `$.data.frame`, "favourites_count")
      record[[6]]$count_current[match(ids, record[[6]]$t_ids)] <- sapply(response_content, `$.data.frame`, "statuses_count")
      record[[1]]$protected[match(ids, record[[1]]$t_ids)] <- sapply(response_content, `$.data.frame`, "protected")
    }
  }
  if (first_lookup == 1L) {
    # build sql_placeholder to record terminated, protected, and verified accounts
    terminated <- data.frame(t_ids = filter(record, is.na(request_mode) | is.na(protected) | protected == TRUE)$t_ids,
                             date = Sys.Date(),
                             protected = filter(record, is.na(request_mode) | is.na(protected) | protected == TRUE)$protected,
                             stringsAsFactors = FALSE)
    # remove unreachable accounts (protected or NA for request mode or protected, i.e., failed request as account not reachable, 
    # cannot be used for further data collection)
    beginning_followers <- beginning_followers[-match(terminated$t_ids, record$t_ids),]
    beginning_friends <- beginning_friends[-match(terminated$t_ids, record$t_ids),]
    beginning_likes <- beginning_likes[-match(terminated$t_ids, record$t_ids),]
    beginning_statuses <- beginning_statuses[-match(terminated$t_ids, record$t_ids),]
    tracker_followers <- tracker_followers[-match(terminated$t_ids, record$t_ids),]
    tracker_friends <- tracker_friends[-match(terminated$t_ids, record$t_ids),]
    tracker_likes <- tracker_likes[-match(terminated$t_ids, record$t_ids),]
    tracker_statuses <- tracker_statuses[-match(terminated$t_ids, record$t_ids),]
    record <- record[-match(terminated$t_ids, record$t_ids),]
  } else {
    # build sql_placeholder to record terminated and protected accounts
    if (length(filter(record[[1]], is.na(request_mode) | is.na(protected) | protected == TRUE)$t_ids) > 0) {
      terminated <- data.frame(t_ids = filter(record[[1]], is.na(request_mode) | is.na(protected) | protected == TRUE)$t_ids,
                               date = Sys.Date(),
                               protected = filter(record[[1]], is.na(request_mode) | is.na(protected) | protected == TRUE)$protected,
                               stringsAsFactors = FALSE)
      # remove unreachable accounts (protected or NA for request mode,i.e., failed request, cannot be used for further data collection)
      record[[3]] <- record[[3]][-match(terminated$t_ids, record[[3]]$t_ids),]
      record[[4]] <- record[[4]][-match(terminated$t_ids, record[[4]]$t_ids),]
      record[[5]] <- record[[5]][-match(terminated$t_ids, record[[5]]$t_ids),]
      record[[6]] <- record[[6]][-match(terminated$t_ids, record[[6]]$t_ids),]
      record[[1]] <- record[[1]][-match(terminated$t_ids, record[[1]]$t_ids),]
    }
    # update change columns by checking for differences between next and previous
    record[[3]]$change <- ifelse(record[[3]]$count_current - record[[3]]$count_previous != 0, TRUE, FALSE)
    record[[4]]$change <- ifelse(record[[4]]$count_current - record[[4]]$count_previous != 0, TRUE, FALSE)
    record[[5]]$change <- ifelse(record[[5]]$count_current - record[[5]]$count_previous != 0, TRUE, FALSE)
    record[[6]]$change <- ifelse(record[[6]]$count_current - record[[6]]$count_previous != 0, TRUE, FALSE)
    if (any(record[[3]]$change == TRUE)) {
      change_followers <- data.frame(t_ids = filter(record[[3]], change == TRUE)$t_ids,
                                     date = Sys.Date(),
                                     change = filter(record[[3]], change == TRUE)$count_current -
                                       filter(record[[3]], change == TRUE)$count_previous,
                                     ids_added = NA,
                                     ids_removed = NA,
                                     stringsAsFactors = FALSE)
    } else {
      change_followers <- NA
    }
    if (any(record[[4]]$change == TRUE)) {
      change_friends <- data.frame(t_ids = filter(record[[4]], change == TRUE)$t_ids,
                                   date = Sys.Date(),
                                   change = filter(record[[4]], change == TRUE)$count_current -
                                     filter(record[[4]], change == TRUE)$count_previous,
                                   ids_added = NA,
                                   ids_removed = NA,
                                   stringsAsFactors = FALSE)
    } else {
      change_friends <- NA
    }
    if (any(record[[5]]$change == TRUE)) {
      change_likes <- data.frame(t_ids = filter(record[[5]], change == TRUE)$t_ids,
                                 date = Sys.Date(),
                                 change = filter(record[[5]], change == TRUE)$count_current -
                                   filter(record[[5]], change == TRUE)$count_previous,
                                 ids_added = NA,
                                 text = NA,
                                 source = NA,
                                 created = NA,
                                 stringsAsFactors = FALSE)
    } else {
      change_likes <- NA
    }
    if (any(record[[6]]$change == TRUE)) {
      change_statuses <- data.frame(t_ids = filter(record[[6]], change == TRUE)$t_ids,
                                    date = Sys.Date(),
                                    change = filter(record[[6]], change == TRUE)$count_current -
                                      filter(record[[6]], change == TRUE)$count_previous,
                                    ids = NA,
                                    text = NA,
                                    reply = NA,
                                    reply_ids = NA,
                                    created = NA,
                                    stringsAsFactors = FALSE)
    } else {
      change_statuses <- NA
    }
    # update tracker for next lookup, i.e., current count to previous count
    record[[3]]$count_previous <- record[[3]]$count_current 
    record[[4]]$count_previous <- record[[4]]$count_current
    record[[5]]$count_previous <- record[[5]]$count_current
    record[[6]]$count_previous <- record[[6]]$count_current
  }
  credentials$reset_lookup[app_user] <- with_tz(as.POSIXct(as.integer(headers(response)$`x-rate-limit-reset`), origin = "1970-01-01", tz="GMT"), tz = "CET")
  if (first_lookup == 1L) {
    dbWriteTable(conn = connection, name = "terminated",
                 value = terminated, append = TRUE)
    # return list of results
    return(list(record = record, credentials = credentials, 
                beginning_followers = beginning_followers, beginning_friends = beginning_friends, beginning_likes = beginning_likes, beginning_statuses = beginning_statuses,
                tracker_followers = tracker_followers, tracker_friends = tracker_friends, tracker_likes = tracker_likes, tracker_statuses = tracker_statuses))
  } else {
    if (exists("terminated")) {
    dbWriteTable(conn = connection, name = "terminated",
                 value = terminated, append = TRUE)
    }
    record[[2]] <- credentials
    # return record + change
    return(c(record, list(change_followers = change_followers, change_friends = change_friends, change_likes = change_likes, change_statuses = change_statuses)))
  }
}


#### twitterCollector ===================================================================

# 'twitterCollector' takes the output from 'twitterLookup' in the 'record' arguments and 
# collects various data on Twitter users based on the 'twitterAPI...' functions. The 
# function distinguishes between the first lookup and subsequent lookups as of the 
# 'twitterLookup' function. For the first lookup, the function gathers pretty much all 
# available information. For subsequent lookups the function gather only new information.

collector <- function(record, connection, connection_2) {
  # check if this is the first lookup (1L) based on whether beginning appears in record
  first_lookup <- ifelse("beginning_followers" %in% names(record), 1L, 0L)
  # if this is not the first lookup, pass record[[2]] to credentials
  credentials <- record[[2]]
  # open all windows with rate limits reset since last use
  credentials$status_lookup[which(is.na(credentials$reset_lookup))] <- "open"
  credentials$status_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- "open"
  credentials$reset_lookup[which(credentials$status_lookup == "open")] <- NA
  # if no window open, do:
  while (length(which(credentials$status_lookup == "open")) == 0) {
    # wait until the first window opens
    cat("waiting for an open window")
    Sys.sleep(ifelse(min(credentials$reset_lookup - Sys.time(), na.rm = TRUE) < 0, 
                     0, 
                     min(credentials$reset_lookup - Sys.time(), na.rm = TRUE)))
    # open all windows with rate limits reset
    credentials$status_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- "open"
  }
  # randomly select app-user credentials
  app_user <- resample((1:nrow(credentials))[which(credentials$status_lookup == "open")], 1)
  # close respective window
  credentials[app_user,]$status_lookup <- "closed"
  # create OAuth application
  application <- oauth_app(appname = "twitter",
                           key = credentials$consumer_key[app_user], 
                           secret = credentials$consumer_secret[app_user])
  # sign OAuth request
  signature <- sign_oauth1.0(application, 
                             token = credentials$access_token[app_user],
                             token_secret = credentials$access_secret[app_user])
  # if this is the first time the collector is activated:
  if (first_lookup == 1L) {
    # build indices of units to query (all that elicit any activity)
    index_followers <- which(record[[3]]$count > 0)
    index_friends <- which(record[[4]]$count > 0)
    index_likes <- which(record[[5]]$count > 0)
    index_statuses <- which(record[[6]]$count > 0)
    continue_tracker <- rep(0, times = length(record[[6]]$count))
  } else {
    # build indices of units to query (all that elicit any change in activity)
    index_followers <- which(record[[3]]$change == TRUE)
    index_friends <- which(record[[4]]$change == TRUE)
    index_likes <- which(record[[5]]$change == TRUE)
    index_statuses <- which(record[[6]]$change == TRUE)
  }
  # set up request counters
  request_counter_followers <- 0
  request_counter_friends <- 0
  request_counter_likes <- 0
  request_counter_statuses <- 0
  # set up premises
  cursor_followers <- "-1"
  cursor_friends <- "-1"
  ids_collector_followers <- ""[-1]
  idx_collector_followers <- ""[-1]
  ids_collector_friends <- ""[-1]
  idx_collector_friends <- ""[-1]
  time_start_2 <- Sys.time()
  try_again_followers <- 0
  try_again_friends <- 0
  try_again_likes <- 0
  try_again_statuses <- 0
  skip_followers <- 0
  skip_friends <- 0
  skip_likes <- 0
  skip_statuses <- 0
  errors_followers <- as.data.frame(matrix(nrow = 0, ncol = 3, dimnames = list(c(), c("t_id", "code", "attempts"))))
  errors_friends <- as.data.frame(matrix(nrow = 0, ncol = 3, dimnames = list(c(), c("t_id", "code", "attempts"))))
  errors_likes <- as.data.frame(matrix(nrow = 0, ncol = 3, dimnames = list(c(), c("t_id", "code", "attempts"))))
  errors_statuses <- as.data.frame(matrix(nrow = 0, ncol = 3, dimnames = list(c(), c("t_id", "code", "attempts"))))
  stop_followers <- FALSE
  stop_friends <- FALSE
  stop_likes <- FALSE
  stop_statuses <- FALSE
######################################  
  while (length(c(index_followers, index_friends, index_likes, index_statuses)) > 0) {
    ############################ RETRIEVE FOLLOWERS DATA #############################
    if (length(index_followers) > 0 | length(idx_collector_followers) > 0) {
      # determine requests (max. 15 requests per window)
      if (try_again_followers > 0 & try_again_followers < 3) {
        request_followers <- request_followers
      } else if (length(idx_collector_followers) == 0 & length(index_followers) > 15) {
        request_followers <- sample(index_followers, size = 15, replace = FALSE)
        index_followers <- index_followers[-which(index_followers %in% request_followers)]
      } else if (length(idx_collector_followers) > 0 & length(index_followers) > 15) {
        request_size <- 15 - length(idx_collector_followers)
        request_followers <- sample(index_followers, size = request_size, replace = FALSE)
        if (length(request_followers) > 0) {
          index_followers <- index_followers[-which(index_followers %in% request_followers)]
        }
        request_followers <- c(as.integer(idx_collector_followers), request_followers)
        cursor_followers <- c(cursor_followers, rep("-1", request_size))
      } else if (length(idx_collector_followers) == 0 & length(index_followers) <= 15) {
        request_followers <- index_followers
        index_followers <- index_followers[-which(index_followers %in% request_followers)]
      } else if (length(idx_collector_followers) > 0 & length(index_followers) <= 15 & length(index_followers) > 0) {
        request_size <- 15 - length(idx_collector_followers)
        request_followers <- c(as.integer(idx_collector_followers), na.omit(index_followers[1:request_size]))
        cursor_followers <- c(cursor_followers, rep("-1", length(na.omit(index_followers[1:request_size]))))
        index_followers <- index_followers[-c(1:request_size)]
      } else if (length(idx_collector_followers) > 0 & length(index_followers) == 0) {
        request_followers <- as.integer(idx_collector_followers)
      }
      # build the 15 queries (or less, if finalizing)
      cat("DEBUG STEP 0\n")
      cat(paste0(cursor_followers, sep = ", "))
      query <- mapply(list, 
                      ifelse(record[[1]]$request_mode[request_followers] == 0, 
                             record[[1]][request_followers, "t_ids"], 
                             record[[1]][request_followers, "screen_name"]), 
                      cursor = cursor_followers, 
                      stringify_ids = "true", 
                      SIMPLIFY = FALSE)
      # check request type - user_id or screen_name
      request_type <- ifelse(record[[1]]$request_mode[request_followers] == 0, "user_id", "screen_name")
      # name list component according to request type for api to understand the call
      query <- mapply(function(x,y) {names(x)[1] <- y
      x}, query, request_type, SIMPLIFY = FALSE)
      # call API and measure partial execution time
      time_start <- Sys.time()
      response_followers <- lapply(query, twitterApiFollowers, signature)
      time_end <- Sys.time()
      execution_time <- difftime(time1 = time_end, time2 =time_start, units = "secs")
      test_success <- suppressMessages(try(headers(tail(response_followers, n = 1)[[1]])$`x-rate-limit-reset`, silent = TRUE))
      if (class(test_success) != "try-error") {
        try_again_followers <- 0
        # record response statuses
        cat("DEBUG STEP 1\n")
        cat(paste0(request_followers, sep = ", "))
        statuses <- as.integer(str_replace(
          string = unname(sapply(lapply(response_followers, headers), `$.data.frame`, "status")), 
          pattern = " .+", ""))
        # check for error
        if (any(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))) {
          error_idx_followers <- which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))
          error_ids_followers <- record[[1]]$t_ids[request_followers[which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))]]
          error_code_followers <- statuses[which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))]
          for (e in 1:length(error_ids_followers)) {
            if (error_ids_followers[e] %in% errors_followers$t_id) {
              errors_followers[errors_followers$t_id == error_ids_followers[e], "attempts"] <- errors_followers[errors_followers$t_id == error_ids_followers[e], "attempts"] + 1 
            } else {
              errors_followers <- rbind(errors_followers, data.frame(t_id = error_ids_followers[e], code = error_code_followers[e], attempts = 1))
            }
            if (errors_followers[errors_followers$t_id == error_ids_followers[e], "attempts"] < 3) {
              index_followers <- c(index_followers, request_followers[error_idx_followers[e]])
            }
          }
          #####################################################################################################
          if (any(request_followers[error_idx_followers] %in% as.integer(idx_collector_followers))) {
            # remove NAs from collector
            ids_collector_followers <- ids_collector_followers[-na.omit(match(request_followers[error_idx_followers], 
                                                                      as.integer(idx_collector_followers)))]
            idx_collector_followers <- idx_collector_followers[-na.omit(match(request_followers[error_idx_followers], 
                                                                      as.integer(idx_collector_followers)))]
            if (length(ids_collector_followers) == 0) {
              ids_collector_followers <- ""[-1]
            }
            if (length(idx_collector_followers) == 0) {
              idx_collector_followers <- ""[-1]
            }
          }
          #####################################################################################################
          response_followers <- response_followers[-error_idx_followers]
          request_followers <- request_followers[-error_idx_followers]
          statuses <- statuses[-error_idx_followers]
          cat(yellow("\nATTENTION: ERRORS ", paste(error_code_followers, collapse = ", "), " ENCOUNTERED"))
          cat("DEBUG STEP 2\n")
          cat(paste0(request_followers, sep = ", "))
        }
        # store response content in separate object
        response_content_followers <- ifelse(statuses == 200, lapply(response_followers, httr::content), NA)
        # if any NAs in response_content_followers, do
        if (any(is.na(response_content_followers))) {
          # drop NAs in response_content_followers and respective entries in relevant objects
          response_followers <- response_followers[!is.na(response_content_followers)]
          request_followers <- request_followers[!is.na(response_content_followers)]
          response_content_followers <- response_content_followers[!is.na(response_content_followers)]
        }
        # store follower ids in separate object
        ids_followers_current <-  lapply(lapply(response_content_followers, `$.data.frame`, "ids"), unlist)
        if (length(response_followers) != 0) {
          window_reset_time <- headers(tail(response_followers, n = 1)[[1]])$`x-rate-limit-reset`
          latest_reset <- with_tz(as.POSIXct(as.integer(window_reset_time), origin = "1970-01-01", tz="GMT"), tz = "CET")
          request_counter_followers <- request_counter_followers + 1
          # get page status of responses
          continue_status_followers <- unlist(lapply(response_content_followers, `$.data.frame`, "next_cursor_str"))
          # record request_followers with page status 0
          continue_status_followers_0 <- request_followers[which(continue_status_followers == "0")]
          # record request_followers with other page status
          continue_status_followers_1 <- request_followers[which(continue_status_followers != "0")]
          # determine if one of the previous continues is completed (status 0)
          if (any(continue_status_followers_0 %in% idx_collector_followers)) {
            # determine which request_follower with continue status 0 is completed
            continue_completed_followers <- continue_status_followers_0[continue_status_followers_0 %in% 
                                                                        idx_collector_followers]
            # append respective ids_followers_current to ids_followers_collector and replace in ids_followers_current
            ids_followers_current[match(continue_completed_followers, request_followers)] <- mapply(c,
                                                                                                ids_collector_followers[match(continue_completed_followers, idx_collector_followers)],
                                                                                                ids_followers_current[match(continue_completed_followers, request_followers)], 
                                                                                                SIMPLIFY = FALSE)
            # remove completed from continue_followers_previous, from idx_collector and from ids_collector       
            #continue_followers_previous <- continue_followers_previous[-match(continue_completed_followers, 
            #                                                                  continue_followers_previous)]
            ids_collector_followers <- ids_collector_followers[-match(continue_completed_followers, 
                                                                      idx_collector_followers)]
            idx_collector_followers <- idx_collector_followers[-match(continue_completed_followers, 
                                                                      idx_collector_followers)]
          }
          # determine if one of the continues needs to be updated
          if (any(continue_status_followers_1 %in% idx_collector_followers)) {
            # determine which request_follower with continue status other than zero needs to be updated
            continue_update_followers <- continue_status_followers_1[continue_status_followers_1 %in% 
                                                                       idx_collector_followers]
            # append respective ids_followers_current to ids_followers_collector
            ids_collector_followers[match(continue_update_followers, idx_collector_followers)] <- mapply(c,
                                                                                                         ids_collector_followers[match(continue_update_followers, idx_collector_followers)],
                                                                                                         ids_followers_current[match(continue_update_followers, request_followers)],
                                                                                                        SIMPLIFY = FALSE)
          }
         # determine if one of the continues needs to be freshly added to the collector
         if (any(continue_status_followers_1 %ni% idx_collector_followers)) {
            # determine which request_follower with continue status other than zero must be added
            continue_add_followers <- continue_status_followers_1[continue_status_followers_1 %ni%
                                                                 idx_collector_followers]
            # add respective request_follower to idx_collector_followers
            idx_collector_followers <- c(idx_collector_followers, continue_add_followers)
            # add respective ids_followers_current to ids_followers_collector
            ids_collector_followers <- c(ids_collector_followers, ids_followers_current[request_followers %in% continue_add_followers])
         }
          # determine if some of the current requests require another lookup/continue
          if (length(idx_collector_followers) != 0) {
            # record the respective cursors
            cursor_followers <- continue_status_followers[match(idx_collector_followers, request_followers)]
            # remove from request_followers and ids_followers_current those, that
            # require a lookup, so that the remainder can simply be passed to the database
            request_followers <- request_followers[-which(continue_status_followers != "0")]
            ids_followers_current <- ids_followers_current[-which(continue_status_followers != "0")]
          } else {
            # set the cursor to the default again, and subsequently simply pass everything in the response
            # to the database
            cursor_followers <- "-1"
          }
          # if this is the first lookup, do
          if (first_lookup == 1L) {
            #cat("DEBUG STEP 3")
            #cat(paste0(request_followers, sep = ", "))
            # take the completed follower collections and insert them in the beginning [[3]] and tracker [[7]] data
            record[[3]][request_followers, "ids"] <- unlist(lapply(ids_followers_current, paste0, collapse = ","))
            record[[7]][request_followers, "ids"] <- unlist(lapply(ids_followers_current, paste0, collapse = ","))
            # if not, do
            #cat("DEBUG STEP 4")
            #cat(paste0(request_followers, sep = ", "))
          } else {
            cat("DEBUG STEP 3\n")
            cat(paste0(request_followers, sep = ", "))
            # from the tracker data, get the ids as of the last lookup
            ids_followers_previous <- str_split(record[[3]][request_followers, "ids"], pattern = ",")
            if (length(ids_followers_current) > 1) {
              # add the newly added IDs to the change data
              record[[7]][match(record[[1]]$t_ids[request_followers], 
                                record[[7]]$t_ids), "ids_added"] <- unlist(lapply(mapply(`[`, ids_followers_current, 
                                                                                         mapply(`%ni%`, ids_followers_current, 
                                                                                                ids_followers_previous, SIMPLIFY = FALSE), SIMPLIFY = FALSE),
                                                                                  paste0, collapse = ","))
              # add the newly removed IDs to the change data 
              record[[7]][match(record[[1]]$t_ids[request_followers], 
                                record[[7]]$t_ids), "ids_removed"] <- unlist(lapply(mapply(`[`, ids_followers_previous, 
                                                                                           mapply(`%ni%`, ids_followers_previous, 
                                                                                                  ids_followers_current, SIMPLIFY = FALSE), SIMPLIFY = FALSE),
                                                                                    paste0, collapse = ",")) 
              record[[3]][request_followers, "ids"] <- unlist(lapply(ids_followers_current, paste0, collapse = ","))
            } 
            if (length(ids_followers_current) == 1) {
              record[[7]][match(record[[1]]$t_ids[request_followers], 
                                record[[7]]$t_ids), "ids_added"] <- paste0(ids_followers_current[[1]][mapply(`%ni%`, ids_followers_current, 
                                                                                                              ids_followers_previous)[,1]], collapse = ",")
              record[[7]][match(record[[1]]$t_ids[request_followers], 
                                record[[7]]$t_ids), "ids_removed"] <- paste0(ids_followers_previous[[1]][mapply(`%ni%`, ids_followers_previous, 
                                                                                                               ids_followers_current)[,1]], collapse = ",")
              record[[3]][request_followers, "ids"] <- unlist(lapply(ids_followers_current, paste0, collapse = ","))
            }
          }
          # print progress info to console
          cat("\n##### FOLLOWERS ##### ")
          cat(green("SUCCESS"))
          cat("\n--- request: ", request_counter_followers,
              "\n--- http status codes: ", paste0(unique(statuses), collapse = ", "), 
              "\n--- app-user combination: ", app_user, 
              "\n--- remaining requests: ", length(index_followers) + length(idx_collector_followers),
              "\n--- partial execution time: ", execution_time, " seconds",
              "\n--- total_execution time: ", difftime(Sys.time(), time_start_2, units = "secs")/3600, " hours",
              "\n--- memory usage: ", round(mem_used()/1000000), "MB",
              "\n", sep = "")
          if (length(index_statuses) == 0 & length(index_likes) == 0 & length(index_friends) == 0 & length(idx_collector_friends) == 0) {
            cat("\n---------------------------------------------------------\n", sep = "")
          }
          skip_followers <- 0
        } else {
          skip_followers <- 1
          ids_collector_followers <- ""[-1]
          idx_collector_followers <- ""[-1]
          cursor_followers <- "-1"
          cat("\n##### FOLLOWERS ##### ")
          cat(yellow("\n--- SOMETHING WENT WRONG, SKIPPING TO NEXT SAMPLE"))
        }
      } else {
        try_again_followers <- try_again_followers + 1
        if (try_again_followers > 0 & try_again_followers < 3) {
          cat("\n##### FOLLOWERS ##### ")
          cat(yellow("\n--- SOMETHING WENT WRONG, TRYING AGAIN NEXT ITERATION"))
        } else {
          try_again_followers <- 0
          cat("\n##### FOLLOWERS ##### ")
          cat(yellow("\n--- RETRIES EXHAUSTED, SKIPPING TO NEXT SAMPLE"))
          ids_collector_followers <- ""[-1]
          idx_collector_followers <- ""[-1]
          cursor_followers <- "-1"
        }
      }
    } else {
      stop_followers <- TRUE
    }
    if (stop_likes == FALSE) {
      abit <- sample(8:12,1)
      cat("\nwaiting ", abit, " seconds\n")
      Sys.sleep(abit)
    } else if (stop_likes == TRUE & stop_friends == TRUE) {
      abit <- sample(50:60,1)
      cat("\nwaiting ", abit, " seconds\n")
      Sys.sleep(abit)
    } else {
      abit <- sample(30:40,1)
      cat("\nwaiting ", abit, " seconds\n")
      Sys.sleep(abit)
    }
    ############################ RETRIEVE FRIENDS DATA #############################
    if (length(index_friends) > 0 | length(idx_collector_friends) > 0) {
      # determine requests (max. 15 requests per window)
      if (try_again_friends > 0 & try_again_friends < 3) {
        request_friends <- request_friends
      } else if (length(idx_collector_friends) == 0 & length(index_friends) > 15) {
        request_friends <- sample(index_friends, size = 15, replace = FALSE)
        index_friends <- index_friends[-which(index_friends %in% request_friends)]
      } else if (length(idx_collector_friends) > 0 & length(index_friends) > 15) {
        request_size <- 15 - length(idx_collector_friends)
        request_friends <- sample(index_friends, size = request_size, replace = FALSE)
        if (length(request_friends) > 0) {
          index_friends <- index_friends[-which(index_friends %in% request_friends)]
        }
        request_friends <- c(as.integer(idx_collector_friends), request_friends)
        cursor_friends <- c(cursor_friends, rep("-1", request_size))
      } else if (length(idx_collector_friends) == 0 & length(index_friends) <= 15) {
        request_friends <- index_friends
        index_friends <- index_friends[-which(index_friends %in% request_friends)]
      } else if (length(idx_collector_friends) > 0 & length(index_friends) <= 15 & length(index_friends) > 0) {
        request_size <- 15 - length(idx_collector_friends)
        request_friends <- c(as.integer(idx_collector_friends), na.omit(index_friends[1:request_size]))
        cursor_friends <- c(cursor_friends, rep("-1", length(na.omit(index_friends[1:request_size]))))
        index_friends <- index_friends[-c(1:request_size)]
      } else if (length(idx_collector_friends) > 0 & length(index_friends) == 0) {
        request_friends <- as.integer(idx_collector_friends)
      }
      # build the 15 queries (or less, if finalizing)
      cat("DEBUG STEP 0\n")
      cat(paste0(cursor_friends, sep = ", "))
      query <- mapply(list, 
                      ifelse(record[[1]]$request_mode[request_friends] == 0, 
                             record[[1]][request_friends, "t_ids"], 
                             record[[1]][request_friends, "screen_name"]), 
                      cursor = cursor_friends, 
                      stringify_ids = "true", 
                      SIMPLIFY = FALSE)
      # check request type - user_id or screen_name
      request_type <- ifelse(record[[1]]$request_mode[request_friends] == 0, "user_id", "screen_name")
      # name list component according to request type for api to understand the call
      query <- mapply(function(x,y) {names(x)[1] <- y
      x}, 
      query, request_type, SIMPLIFY = FALSE)
      # call API and measure partial execution time
      time_start <- Sys.time()
      response_friends <- lapply(query, twitterApiFriends, signature)
      time_end <- Sys.time()
      execution_time <- difftime(time1 = time_end, time2 =time_start, units = "secs")
      test_success <- suppressMessages(try(headers(tail(response_friends, n = 1)[[1]])$`x-rate-limit-reset`, silent = TRUE))
      if (class(test_success) != "try-error") {
        try_again_friends <- 0
         # record response statuses
        cat("DEBUG STEP 1\n")
        cat(paste0(request_friends, sep = ", "))
        statuses <- as.integer(str_replace(
            string = unname(sapply(lapply(response_friends, headers), `$.data.frame`, "status")), 
            pattern = " .+", ""))
        cat(statuses)
          # check for error
          if (any(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))) {
            error_idx_friends <- which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))
            error_ids_friends <- record[[1]]$t_ids[request_friends[which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))]]
            error_code_friends <- statuses[which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))]
            for (e in 1:length(error_ids_friends)) {
              if (error_ids_friends[e] %in% errors_friends$t_id) {
                errors_friends[errors_friends$t_id == error_ids_friends[e], "attempts"] <- errors_friends[errors_friends$t_id == error_ids_friends[e], "attempts"] + 1 
              } else {
                errors_friends <- rbind(errors_friends, data.frame(t_id = error_ids_friends[e], code = error_code_friends[e], attempts = 1))
              }
              if (errors_friends[errors_friends$t_id == error_ids_friends[e], "attempts"] < 3) {
                index_friends <- c(index_friends, request_friends[error_idx_friends[e]])
              }
            }
            #####################################################################################################
            if (any(request_friends[error_idx_friends] %in% as.integer(idx_collector_friends))) {
              # remove NAs from collector
              ids_collector_friends <- ids_collector_friends[-na.omit(match(request_friends[error_idx_friends], 
                                                                                as.integer(idx_collector_friends)))]
              idx_collector_friends <- idx_collector_friends[-na.omit(match(request_friends[error_idx_friends], 
                                                                                as.integer(idx_collector_friends)))]
              if (length(ids_collector_friends) == 0) {
                ids_collector_friends <- ""[-1]
              }
              if (length(idx_collector_friends) == 0) {
                idx_collector_friends <- ""[-1]
              }
            }
            #####################################################################################################
            response_friends <- response_friends[-error_idx_friends]
            request_friends <- request_friends[-error_idx_friends]
            statuses <- statuses[-error_idx_friends]
            cat(yellow("\nATTENTION: ERRORS ", paste(error_code_friends, collapse = ", "), " ENCOUNTERED"))
            cat("DEBUG STEP 2\n")
            cat(paste0(request_friends, sep = ", "))
          }
          # store response content in separate object
          response_content_friends <- ifelse(statuses == 200, lapply(response_friends, httr::content), NA)
          # if any NAs in response_content_friends, do
          if (any(is.na(response_content_friends))) {
            # drop NAs in response_content_friends and respective entries in relevant objects
            response_friends <- response_friends[!is.na(response_content_friends)]
            request_friends <- request_friends[!is.na(response_content_friends)]
            response_content_friends <- response_content_friends[!is.na(response_content_friends)]
          }
          # store friend ids in separate object
          ids_friends_current <-  lapply(lapply(response_content_friends, `$.data.frame`, "ids"), unlist)
          if (length(response_friends) != 0) {
            window_reset_time <- headers(tail(response_friends, n = 1)[[1]])$`x-rate-limit-reset`
            latest_reset <- with_tz(as.POSIXct(as.integer(window_reset_time), origin = "1970-01-01", tz="GMT"), tz = "CET")
            request_counter_friends <- request_counter_friends + 1
            # get page status of responses
            continue_status_friends <- unlist(lapply(response_content_friends, `$.data.frame`, "next_cursor_str"))
            # record request_friends with page status 0
            continue_status_friends_0 <- request_friends[which(continue_status_friends == "0")]
            # record request_friends with other page status
            continue_status_friends_1 <- request_friends[which(continue_status_friends != "0")]
            # determine if one of the previous continues is completed (status 0)
          if (any(continue_status_friends_0 %in% idx_collector_friends)) {
            # determine which request_friends with continue status 0 is completed
            continue_completed_friends <- continue_status_friends_0[continue_status_friends_0 %in% 
                                                                    idx_collector_friends]
            # append respective ids_friends_current to ids_friends_collector and replace in ids_friends_current
            ids_friends_current[match(continue_completed_friends, request_friends)] <- mapply(c,
                                                                                            ids_collector_friends[match(continue_completed_friends, idx_collector_friends)],
                                                                                            ids_friends_current[match(continue_completed_friends, request_friends)], 
                                                                                            SIMPLIFY = FALSE)
            # remove completed from idx_collector and from ids_collector       
            ids_collector_friends <- ids_collector_friends[-match(continue_completed_friends, 
                                                                  idx_collector_friends)]
            idx_collector_friends <- idx_collector_friends[-match(continue_completed_friends, 
                                                                  idx_collector_friends)]
          }
          # determine if one of the continues needs to be updated
          if (any(continue_status_friends_1 %in% idx_collector_friends)) {
            # determine which request_friends with continue status other than zero needs to be updated
            continue_update_friends <- continue_status_friends_1[continue_status_friends_1 %in% 
                                                                   idx_collector_friends]
            # append respective ids_friends_current to ids_friends_collector
            ids_collector_friends[match(continue_update_friends, idx_collector_friends)] <- mapply(c,
                                                                                               ids_collector_friends[match(continue_update_friends, idx_collector_friends)],
                                                                                               ids_friends_current[match(continue_update_friends, request_friends)],
                                                                                               SIMPLIFY = FALSE)
          }
          # determine if one of the continues needs to be freshly added to the collector
          if (any(continue_status_friends_1 %ni% idx_collector_friends)) {
            # determine which request_friends with continue status other than zero must be added
            continue_add_friends <- continue_status_friends_1[continue_status_friends_1 %ni%
                                                                idx_collector_friends]
            # add respective request_friends to idx_collector_friends
            idx_collector_friends <- c(idx_collector_friends, continue_add_friends)
            # add respective ids_friends_current to ids_friends_collector
            ids_collector_friends <- c(ids_collector_friends, ids_friends_current[request_friends %in% continue_add_friends])
         }
         # determine if some of the current requests require another lookup/continue
         if (length(idx_collector_friends) != 0) {
            # record the respective cursors
            cursor_friends <- continue_status_friends[match(idx_collector_friends, request_friends)]
            # remove from request_friends and ids_friends_current those that
            # require a lookup, so that the remainder can simply be passed to the database
            request_friends <- request_friends[-which(continue_status_friends != "0")]
            ids_friends_current <- ids_friends_current[-which(continue_status_friends != "0")]
         } else {
            # set the cursor to the default again, and subsequently simply pass everything in the response
            # to the database
            cursor_friends <- "-1"
         }
          # if this is the first lookup, do
        if (first_lookup == 1L) {
          if(!is.null(warnings())) {
            print(warnings())
          }
          #cat("\n--- USERS: ", paste0(request_friends, sep = ", "))
          # take the completed friends collections and insert them in the beginning [[4]] and tracker [[8]] data
          record[[4]][request_friends, "ids"] <- unlist(lapply(ids_friends_current, paste0, collapse = ","))
          record[[8]][request_friends, "ids"] <- unlist(lapply(ids_friends_current, paste0, collapse = ","))
          if(!is.null(warnings())) {
            print(warnings())
          }
          # if not, do
        } else {
          cat("DEBUG STEP 3\n")
          cat(paste0(request_friends, sep = ", "))
          # from the tracker data, get the ids as of the last lookup
          ids_friends_previous <- str_split(record[[4]][request_friends, "ids"], pattern = ",")
          if (length(ids_friends_current) > 1) {
            # add the newly added IDs to the change data
            # add the newly added IDs to the change data [[8]], tracker is now [[4]]
            record[[8]][match(record[[1]]$t_ids[request_friends], 
                              record[[8]]$t_ids), "ids_added"] <- unlist(lapply(mapply(`[`, ids_friends_current, 
                                                                                       mapply(`%ni%`, ids_friends_current, 
                                                                                              ids_friends_previous, SIMPLIFY = FALSE), SIMPLIFY = FALSE),
                                                                                paste0, collapse = ","))
            # add the newly removed IDs to the change data 
            record[[8]][match(record[[1]]$t_ids[request_friends], 
                              record[[8]]$t_ids), "ids_removed"] <- unlist(lapply(mapply(`[`, ids_friends_previous, 
                                                                                         mapply(`%ni%`, ids_friends_previous, 
                                                                                                ids_friends_current, SIMPLIFY = FALSE), SIMPLIFY = FALSE),
                                                                                  paste0, collapse = ","))
            record[[4]][request_friends, "ids"] <- unlist(lapply(ids_friends_current, paste0, collapse = ","))
          } 
          if (length(ids_friends_current) == 1) {
            record[[8]][match(record[[1]]$t_ids[request_friends], 
                              record[[8]]$t_ids), "ids_added"]  <- paste0(ids_friends_current[[1]][mapply(`%ni%`, ids_friends_current, 
                                                                                                           ids_friends_previous)[,1]], collapse = ",")
            record[[8]][match(record[[1]]$t_ids[request_friends], 
                              record[[8]]$t_ids), "ids_removed"] <- paste0(ids_friends_previous[[1]][mapply(`%ni%`, ids_friends_previous, 
                                                                                                              ids_friends_current)[,1]], collapse = ",")
            record[[4]][request_friends, "ids"] <- unlist(lapply(ids_friends_current, paste0, collapse = ","))
          }
          # record current ids in tracker for next lookup
        }
        # print progress info to console
        cat("\n##### FRIENDS ##### ")
        cat(green("SUCCESS"))
        cat("\n--- request: ", request_counter_friends,
            "\n--- http status codes = ", paste0(unique(statuses), collapse = ", "), 
            "\n--- app-user combination = ", app_user, 
            "\n--- remaining requests = ", length(index_friends) + length(idx_collector_friends),
            "\n--- partial execution time = ", execution_time, " seconds",
            "\n--- total_execution time = ", difftime(Sys.time(), time_start_2, units = "secs")/3600, " hours",
            "\n--- memory usage: ", round(mem_used()/1000000), "MB",
            "\n", sep = "")
        if (length(index_statuses) == 0 & length(index_likes) == 0) {
          cat("\n---------------------------------------------------------\n", sep = "")
        }
        skip_friends <- 0
      } else {
            skip_friends <- 1
            cat("\n##### FRIENDS ##### ")
            cat(yellow("\n--- SOMETHING WENT WRONG, SKIPPING TO NEXT SAMPLE"))
            ids_collector_friends <- ""[-1]
            idx_collector_friends <- ""[-1]
            cursor_friends <- "-1"
          }
      } else {
        try_again_friends <- try_again_friends + 1
        if (try_again_friends > 0 & try_again_friends < 3) {
          cat("\n##### FRIENDS ##### ")
          cat(yellow("\n--- SOMETHING WENT WRONG, TRYING AGAIN NEXT ITERATION"))
        } else {
          try_again_friends <- 0
          cat("\n##### FRIENDS ##### ")
          cat(yellow("\n--- RETRIES EXHAUSTED, SKIPPING TO NEXT SAMPLE"))
          ids_collector_friends <- ""[-1]
          idx_collector_friends <- ""[-1]
          cursor_friends <- "-1"
        }
      }
    } else {
      stop_friends <- TRUE
    }
    if (stop_likes == FALSE) {
      abit <- sample(8:12,1)
      cat("\nwaiting ", abit, " seconds\n")
      Sys.sleep(abit)
    } else if (stop_likes == TRUE & stop_followers == TRUE) {
      abit <- sample(50:60,1)
      cat("\nwaiting ", abit, " seconds\n")
      Sys.sleep(abit)
    } else {
      abit <- sample(30:40,1)
      cat("\nwaiting ", abit, " seconds\n")
      Sys.sleep(abit)
    }
    ############################ RETRIEVE LIKES DATA #############################
    if (length(index_likes) > 0) {
      # determine requests (max. 75)
      if (try_again_likes > 0 & try_again_likes < 3) {
        request_likes <- request_likes
      } else if (length(index_likes) > 75) {
        request_likes <- sample(index_likes, size = 75, replace = FALSE)
        index_likes <- index_likes[-which(index_likes %in% request_likes)]
      } else {
        request_likes <- index_likes
        index_likes <- index_likes[-which(index_likes %in% request_likes)]
      }
      if (first_lookup == 1L) {
        # build the 75 queries (or less, if finalizing)
        query <- mapply(list, 
                        ifelse(record[[1]]$request_mode[request_likes] == 0, 
                               record[[1]][request_likes, "t_ids"], 
                               record[[1]][request_likes, "screen_name"]), 
                        count = 200, 
                        include_entities = "false",
                        tweet_mode = "extended", 
                        SIMPLIFY = FALSE)
      } else {
        # build the 75 queries (or less, if finalizing)
        record[[5]][request_likes, "since_ids"] <- ifelse(is.na(record[[5]][request_likes, "since_ids"]), "1", record[[5]][request_likes, "since_ids"]) 
        query <- mapply(list, 
                        ifelse(record[[1]]$request_mode[request_likes] == 0, 
                               record[[1]][request_likes, "t_ids"],                                
                               record[[1]][request_likes, "screen_name"]),
                        since_id = record[[5]][request_likes, "since_ids"],
                        count = 200,
                        trim_user = "true",
                        tweet_mode = "extended", 
                        SIMPLIFY = FALSE)
      }
      # check request type - user_id or screen_name
      request_type <- ifelse(record[[1]]$request_mode[request_likes] == 0, "user_id", "screen_name")
      # name list component according to request type for api to understand the call
      query <- mapply(function(x,y) {names(x)[1] <- y
      x}, 
      query, request_type, SIMPLIFY = FALSE)
      # call API and measure partial execution time
      time_start <- Sys.time()
      response_likes <- lapply(query, twitterApiLikes, signature)
      time_end <- Sys.time()
      execution_time <- difftime(time1 = time_end, time2 =time_start, units = "secs")
      test_success <- suppressMessages(try(headers(tail(response_likes, n = 1)[[1]])$`x-rate-limit-reset`, silent = TRUE))
      if (class(test_success) != "try-error") {
        try_again_likes <- 0
        # record response statuses
        #cat("DEBUG STEP 1")
        #cat(paste0(request_likes, sep = ", "))
        statuses <- as.integer(str_replace(
          string = unname(sapply(lapply(response_likes, headers), `$.data.frame`, "status")), 
          pattern = " .+", ""))
        cat(statuses)
        # check for error
        if (any(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))) {
          error_idx_likes <- which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))
          error_ids_likes <- record[[1]]$t_ids[request_likes[which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))]]
          error_code_likes <- statuses[which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))]
          for (e in 1:length(error_ids_likes)) {
            if (error_ids_likes[e] %in% errors_likes$t_id) {
              errors_likes[errors_likes$t_id == error_ids_likes[e], "attempts"] <- errors_likes[errors_likes$t_id == error_ids_likes[e], "attempts"] + 1 
            } else {
              errors_likes <- rbind(errors_likes, data.frame(t_id = error_ids_likes[e], code = error_code_likes[e], attempts = 1))
            }
            if (errors_likes[errors_likes$t_id == error_ids_likes[e], "attempts"] < 3) {
              index_likes <- c(index_likes, request_likes[error_idx_likes[e]])
            }
          }
          response_likes <- response_likes[-error_idx_likes]
          request_likes <- request_likes[-error_idx_likes]
          statuses <- statuses[-error_idx_likes]
          cat(yellow("\nATTENTION: ERRORS ", paste(error_code_likes, collapse = ", "), " ENCOUNTERED"))
          #cat("DEBUG STEP 2")
          #cat(paste0(request_likes, sep = ", "))
        }
        # store response content in separate object
        response_content_likes <- ifelse(statuses == 200, lapply(response_likes, httr::content), NA)
        # if any NAs in response_content_friends, do
        if (any(is.na(response_content_likes))) {
          # drop NAs in response_content_friends and respective entries in relevant objects
          response_likes <- response_likes[!is.na(response_content_likes)]
          request_likes <- request_likes[!is.na(response_content_likes)]
          response_content_likes <- response_content_likes[!is.na(response_content_likes)]
        }
        if (any(lengths(response_content_likes) == 0)) {
          # drop 0 length elements in response_constent_statuses and respective entries in relevant objects
          response_likes <- response_likes[which(lengths(response_content_likes) != 0)]
          request_likes <- request_likes[which(lengths(response_content_likes) != 0)]
          response_content_likes <- response_content_likes[which(lengths(response_content_likes) != 0)]
        }
        if (length(response_likes) != 0) {
          # store likes ids, creation time, text, and source in separate objects
          ids_likes_current <- lapply(lapply(response_content_likes, function(x) lapply(x, `$.data.frame`, "id_str")), unlist)
          text_likes <- lapply(lapply(response_content_likes, function(x) lapply(x, `$.data.frame`, "full_text")), unlist)
          source_likes <- lapply(lapply(response_content_likes, function(x) lapply(x, `$.data.frame`, "user") %>%
                                        lapply(`$.data.frame`, "id_str")), unlist)
          created_likes <- lapply(lapply(response_content_likes, function(x) lapply(x, `$.data.frame`, "created_at")), unlist)
          created_likes <- lapply(created_likes, parse_date_time, orders = "%a %b %d %H:%M:%S %z %Y", locale = "C")
          since_id_likes <- unlist(lapply(ids_likes_current, head, n = 1))
          window_reset_time <- headers(tail(response_likes, n = 1)[[1]])$`x-rate-limit-reset`
          latest_reset <- with_tz(as.POSIXct(as.integer(window_reset_time), origin = "1970-01-01", tz="GMT"), tz = "CET")
          request_counter_likes <- request_counter_likes + 1
          # if this is the first lookup, do
          if (first_lookup == 1L) {
            if(!is.null(warnings())) {
              print(warnings())
            }
            #cat("\n--- USERS: ", paste0(request_likes, sep = ", "))
            # take the completed friends collections and insert them in the beginning [[5]] and tracker [[9]] data
            record[[5]][request_likes, "ids"] <- unlist(lapply(ids_likes_current, paste0, collapse = ","))
            record[[5]][request_likes, "text"] <- list(text_likes)
            record[[5]][request_likes, "source"] <- unlist(lapply(source_likes, paste0, collapse = ","))
            record[[5]][request_likes, "created"] <- list(created_likes)
            record[[9]][request_likes, "since_ids"] <- since_id_likes
            record[[9]][request_likes, "ids"] <- unlist(lapply(ids_likes_current, paste0, collapse = ","))
            if(!is.null(warnings())) {
              print(warnings())
            }
            # if not, do
          } else {
            #cat("DEBUG STEP 3")
            #cat(paste0(request_likes, sep = ", "))
            # from the tracker data, get the ids as of the last lookup
            ids_likes_previous <- str_split(record[[5]][request_likes, "ids"], pattern = ",")
            # add the IDs of the new likes to the change data [[9]], tracker is now [[5]]
            record[[9]][match(record[[1]]$t_ids[request_likes],
                              record[[9]]$t_ids), "ids_added"] <- unlist(lapply(mapply(`[`, ids_likes_current,
                                                                                      mapply(`%ni%`, ids_likes_current,
                                                                                             ids_likes_previous, SIMPLIFY = FALSE), SIMPLIFY = FALSE),
                                                                               paste0, collapse = ","))
            # add the text of the new likes to the change data [[9]], tracker is now [[5]]
            record[[9]][match(record[[1]]$t_ids[request_likes],
                           record[[9]]$t_ids), "text"] <- list(mapply(`[`, text_likes,
                                                                      mapply(`%ni%`, ids_likes_current,
                                                                             ids_likes_previous, SIMPLIFY = FALSE), SIMPLIFY = FALSE))
            # add the sources of the new likes to the change data [[9]], tracker is now [[5]]
            record[[9]][match(record[[1]]$t_ids[request_likes],
                            record[[9]]$t_ids), "source"] <- unlist(lapply(mapply(`[`, source_likes,
                                                                                  mapply(`%ni%`, ids_likes_current,
                                                                                         ids_likes_previous, SIMPLIFY = FALSE), SIMPLIFY = FALSE),
                                                                           paste0, collapse = ","))
            # add the creation data of the new likes to the change data [[9]], tracker is now [[5]]
            record[[9]][match(record[[1]]$t_ids[request_likes],
                              record[[9]]$t_ids), "created"] <- list(mapply(`[`, created_likes,
                                                                          mapply(`%ni%`, ids_likes_current,
                                                                                 ids_likes_previous, SIMPLIFY = FALSE), SIMPLIFY = FALSE))
            # record current ids in tracker [[5]] for next lookup
            record[[5]][request_likes, "ids"] <- unlist(lapply(ids_likes_current, paste0, collapse = ","))
            record[[5]][request_likes, "since_ids"] <- since_id_likes
         }
        # print progress info to console
         cat("\n##### LIKES ##### ")
         cat(green("SUCCESS"))
         cat("\n--- request: ", request_counter_likes,
             "\n--- http status codes = ", paste0(unique(statuses), collapse = ", "), 
             "\n--- app-user combination = ", app_user, 
             "\n--- remaining requests = ", length(index_likes),
             "\n--- partial execution time = ", execution_time, " seconds",
             "\n--- total_execution time = ", difftime(Sys.time(), time_start_2, units = "secs")/3600, " hours",
             "\n--- memory usage: ", round(mem_used()/1000000), "MB",
             "\n", sep = "")
         if (length(index_statuses) == 0) {
           cat("\n---------------------------------------------------------\n", sep = "")
         }
         skip_likes <- 0
        } else {
          skip_likes <- 1
          cat("\n##### LIKES ##### ")
          cat(yellow("\n--- EMPTY RESPONSE, SKIPPING TO NEXT SAMPLE"))
        }
      } else {
        try_again_likes <- try_again_likes + 1
        if (try_again_likes > 0 & try_again_likes < 3) {
          cat("\n##### LIKES ##### ")
          cat(yellow("\n--- SOMETHING WENT WRONG, TRYING AGAIN NEXT ITERATION"))
        } else {
          try_again_likes <- 0
          cat("\n##### LIKES ##### ")
          cat(yellow("\n--- RETRIES EXHAUSTED, SKIPPING TO NEXT SAMPLE"))
        }
      }
    } else {
      stop_likes <- TRUE
    }
    cat("\nwaiting a bit\n")
    Sys.sleep(sample(8:12,1))
    ############################ RETRIEVE STATUSES DATA #############################
    if (length(index_statuses) > 0) {
      if (try_again_statuses > 0 & try_again_statuses < 3) {
        request_statuses <- request_statuses
      # determine requests (max. 900)
      } else if (length(index_statuses) > 900) {
        request_statuses <- sample(index_statuses, size = 900, replace = FALSE)
        index_statuses <- index_statuses[-which(index_statuses %in% request_statuses)]
      } else {
        request_statuses <- index_statuses
        index_statuses <- index_statuses[-which(index_statuses %in% request_statuses)]
      }
      if (first_lookup == 1L) {
        # build the 900 queries (or less, if finalizing)
        query <- mapply(list, 
                        ifelse(record[[1]]$request_mode[request_statuses] == 0, 
                               record[[1]][request_statuses, "t_ids"],                                
                               record[[1]][request_statuses, "screen_name"]), 
                        count = 200,
                        trim_user = "true",
                        tweet_mode = "extended", 
                        SIMPLIFY = FALSE)
      } else {
        # build the 900 queries (or less, if finalizing)
        record[[6]][request_statuses, "since_ids"] <- ifelse(is.na(record[[6]][request_statuses, "since_ids"]), "1", record[[6]][request_statuses, "since_ids"]) 
        query <- mapply(list, 
                        ifelse(record[[1]]$request_mode[request_statuses] == 0, 
                               record[[1]][request_statuses, "t_ids"],                                
                               record[[1]][request_statuses, "screen_name"]),
                        since_id = record[[6]][request_statuses, "since_ids"],
                        count = 200,
                        trim_user = "true",
                        tweet_mode = "extended", 
                        SIMPLIFY = FALSE)
      }
      # check request type - user_id or screen_name
      request_type <- ifelse(record[[1]]$request_mode[request_statuses] == 0, "user_id", "screen_name")
      # name list component according to request type for api to understand the call
      query <- mapply(function(x,y) {names(x)[1] <- y
                                      x}, 
                        query, request_type, SIMPLIFY = FALSE)
      # call API and measure partial execution time
      time_start <- Sys.time()
      response_statuses <- lapply(query, twitterApiStatuses, signature)
      time_end <- Sys.time()
      execution_time <- difftime(time1 = time_end, time2 =time_start, units = "secs")
      test_success <- suppressMessages(try(headers(tail(response_statuses, n = 1)[[1]])$`x-rate-limit-reset`, silent = TRUE))
      if (class(test_success) != "try-error") {
        try_again_statuses <- 0
        # record response statuses
        #cat("DEBUG STEP 1")
        #cat(paste0(request_statuses, sep = ", "))
        statuses <- as.integer(str_replace(
          string = unname(sapply(lapply(response_statuses, headers), `$.data.frame`, "status")), 
          pattern = " .+", ""))
        # check for error
        if (any(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))) {
          error_idx_statuses <- which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))
          error_ids_statuses <- record[[1]]$t_ids[request_statuses[which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))]]
          error_code_statuses <- statuses[which(statuses %in% c(400, 401, 403, 404, 406, 410, 422, 429, NA, "NA"))]
          for (e in 1:length(error_ids_statuses)) {
            if (error_ids_statuses[e] %in% errors_statuses$t_id) {
              errors_statuses[errors_statuses$t_id == error_ids_statuses[e], "attempts"] <- errors_statuses[errors_statuses$t_id == error_ids_statuses[e], "attempts"] + 1 
            } else {
              errors_statuses <- rbind(errors_statuses, data.frame(t_id = error_ids_statuses[e], code = error_code_statuses[e], attempts = 1))
            }
            if (errors_statuses[errors_statuses$t_id == error_ids_statuses[e], "attempts"] < 3) {
              index_statuses <- c(index_statuses, request_statuses[error_idx_statuses[e]])
            }
          }
          response_statuses <- response_statuses[-error_idx_statuses]
          request_statuses <- request_statuses[-error_idx_statuses]
          statuses <- statuses[-error_idx_statuses]
          cat(yellow("\nATTENTION: ERRORS ", paste(error_code_statuses, collapse = ", "), " ENCOUNTERED"))
          #cat("DEBUG STEP 2")
          #cat(paste0(request_statuses, sep = ", "))
        } 
        # store response content in separate object
        response_content_statuses <- ifelse(statuses == 200, lapply(response_statuses, httr::content), NA)
        # if any NAs in response_content_statuses, do
        if (any(is.na(response_content_statuses))) {
          # drop NAs in response_content_statuses and respective entries in relevant objects
          response_statuses <- response_statuses[!is.na(response_content_statuses)]
          request_statuses <- request_statuses[!is.na(response_content_statuses)]
          response_content_statuses <- response_content_statuses[!is.na(response_content_statuses)]
        }
        if (any(lengths(response_content_statuses) == 0)) {
          # drop 0 length elements in response_constent_statuses and respective entries in relevant objects
          response_statuses <- response_statuses[which(lengths(response_content_statuses) != 0)]
          request_statuses <- request_statuses[which(lengths(response_content_statuses) != 0)]
          response_content_statuses <- response_content_statuses[which(lengths(response_content_statuses) != 0)]
        }
        if (length(response_statuses) != 0) {
          # store tweet ids, creation time, text, and source etc. in separate objects, split 
          ids_statuses <- lapply(lapply(response_content_statuses, function(x) lapply(x, `$.data.frame`, "id_str")), unlist)
          created_statuses <- lapply(lapply(response_content_statuses, function(x) lapply(x, `$.data.frame`, "created_at")), unlist)
          created_statuses <- lapply(created_statuses, parse_date_time, orders = "%a %b %d %H:%M:%S %z %Y", locale = "C")
          text_statuses <- lapply(lapply(response_content_statuses, function(x) lapply(x, `$.data.frame`, "full_text")), unlist)
          retweet_logical <- lapply(lapply(lapply(response_content_statuses, function(x) lapply(x, `$.data.frame`, "retweeted_status")), function(x) lapply(x, is.not.null)), unlist)
          retweet_at <- lapply(lapply(text_statuses, str_extract, "^RT @.+?\\:"), na.omit)
          if (any(lengths(retweet_at)) > 0) {
            rt_index <- lapply(retweet_at, length) > 0
            retweet_at2 <- retweet_at[lapply(retweet_at, length) > 0]
            retweet_statuses <- lapply(lapply(lapply(response_content_statuses, function(x) lapply(x, `$.data.frame`, "retweeted_status")), 
                                              function(x) lapply(x, `$.data.frame`, "full_text")), unlist)
            if (!isTRUE(all.equal(lapply(retweet_statuses, is.not.null) == TRUE, lapply(retweet_at, length) > 0))) {
              rt_turn <- which((lapply(retweet_statuses, is.not.null) == TRUE) != (lapply(retweet_at, length) > 0))
              rt_index <- lapply(retweet_at, length) > 0
              rt_index[rt_turn] <- FALSE
              retweet_at2 <- retweet_at[rt_index]
            }
            cat("DEBUG STEP 1")
            cat(paste0(request_statuses, sep = ", "))
            retweet_statuses <- retweet_statuses[lapply(retweet_statuses, is.not.null) == TRUE]
            retweet_statuses <- mapply(paste0, retweet_at2, " ", retweet_statuses)
            retweet_logical <- lapply(retweet_logical[rt_index], which, TRUE) 
            text_statuses[rt_index] <- mapply(replace, text_statuses[rt_index], retweet_logical, retweet_statuses)
          }
          reply_statuses <- lapply(lapply(response_content_statuses, function(x) lapply(x, `$.data.frame`, "in_reply_to_status_id_str")), lengths)
          reply_ids_statuses <- lapply(lapply(response_content_statuses, function(x) lapply(x, `$.data.frame`, "in_reply_to_status_id_str")), unlist)
          reply_ids_statuses[unlist(lapply(reply_ids_statuses, is.null))] <- NA
          # store since ids in tracker [[10]] instead of al ids as in previous calls
          since_id_statuses <- unlist(lapply(ids_statuses, head, n = 1))
          window_reset_time <- headers(tail(response_statuses, n = 1)[[1]])$`x-rate-limit-reset`
          latest_reset <- with_tz(as.POSIXct(as.integer(window_reset_time), origin = "1970-01-01", tz="GMT"), tz = "CET")
          request_counter_statuses <- request_counter_statuses + 1
          # if this is the first lookup, do
          if (first_lookup == 1L) {
            if(!is.null(warnings())) {
              print(warnings())
            }
            #cat("\n--- USERS: ", paste0(request_statuses, sep = ", "))
            # take the completed statuses collections and insert them in the beginning [[6]] and tracker [[10]] data
            record[[6]][request_statuses, "ids"] <- unlist(lapply(ids_statuses, paste0, collapse = ","))
            record[[6]][request_statuses, "text"] <- list(text_statuses)
            record[[6]][request_statuses, "reply"] <- list(reply_statuses)
            record[[6]][request_statuses, "reply_ids"] <- unlist(lapply(reply_ids_statuses, paste0, collapse = ","))
            record[[6]][request_statuses, "created"] <- list(created_statuses)
            record[[10]][request_statuses, "since_ids"] <- since_id_statuses
            if(!is.null(warnings())) {
              print(warnings())
            }
          # if not, do
          } else {
            #cat("DEBUG STEP 3")
            #cat(paste0(request_statuses, sep = ", "))
            # do the same as above but pass to change[[10]] and tracker[[6]] data, differences need not be computed
            # since the provision of the since_id ensures that only timeline content newer then the since_id will be returned
            record[[10]][match(record[[1]]$t_ids[request_statuses],
                               record[[10]]$t_ids), "ids"] <- unlist(lapply(ids_statuses, paste0, collapse = ","))
            record[[10]][match(record[[1]]$t_ids[request_statuses],
                               record[[10]]$t_ids), "text"] <- list(text_statuses)
            record[[10]][match(record[[1]]$t_ids[request_statuses],
                               record[[10]]$t_ids), "reply"] <- list(reply_statuses)
            record[[10]][match(record[[1]]$t_ids[request_statuses],
                               record[[10]]$t_ids), "reply_ids"] <- unlist(lapply(reply_ids_statuses, paste0, collapse = ","))
            record[[10]][match(record[[1]]$t_ids[request_statuses],
                               record[[10]]$t_ids), "created"] <- list(created_statuses)
            # record current since_id in tracker for next lookup
            record[[6]][request_statuses, "since_ids"] <- since_id_statuses
          }
          # print progress info to console
          cat("\n##### STATUSES ##### ")
          cat(green("SUCCESS"))
          cat("\n--- request: ", request_counter_statuses,
              "\n--- http status codes = ", paste0(unique(statuses), collapse = ", "), 
              "\n--- app-user combination = ", app_user, 
              "\n--- remaining requests = ", length(index_statuses),
              "\n--- partial execution time = ", execution_time, " seconds",
              "\n--- total_execution time = ", difftime(Sys.time(), time_start_2, units = "secs")/3600, " hours",
              "\n--- memory usage: ", round(mem_used()/1000000), "MB",
              "\n\n---------------------------------------------------------\n", sep = "")
          skip_statuses <- 0
        } else {
          skip_statuses <- 1
          cat("\n##### STATUSES ##### ")
          cat(yellow("\n--- NOTHING TO COLLECT FROM RESPONSE, SKIPPING TO NEXT SAMPLE"))
          cat(statuses)
        }
      } else {
        try_again_statuses <- try_again_statuses + 1
        if (try_again_statuses > 0 & try_again_statuses < 3) {
          cat("\n##### STATUSES ##### ")
          cat(yellow("\n--- SOMETHING WENT WRONG, TRYING AGAIN NEXT ITERATION"))
        } else {
          try_again_statuses <- 0
          cat("\n##### STATUSES ##### ")
          cat(yellow("\n--- RETRIES EXHAUSTED, SKIPPING TO NEXT SAMPLE"))
        }
      }
    } else {
      stop_statuses <- TRUE
    }
    if ((try_again_statuses != 0 & try_again_likes != 0 & try_again_friends != 0 & try_again_followers != 0) |
        (skip_statuses != 0 | skip_likes != 0 | skip_friends != 0 | skip_followers != 0)) {
      latest_reset <- Sys.time() + 930
      credentials$reset_lookup[app_user] <- latest_reset
    } else {
      # set latest reset as reset time of this app-token window 
      credentials$reset_lookup[app_user] <- latest_reset
    }
    # open all windows with rate limits reset since last use
    credentials$status_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- "open"
    credentials$reset_lookup[which(credentials$status_lookup == "open")] <- NA
    # if no window open, do:
    while (length(which(credentials$status_lookup == "open")) == 0) {
      wait <- min(difftime(credentials$reset_lookup, Sys.time(), units = "secs"), na.rm = TRUE)
      #if (stop_friends == TRUE & stop_likes == TRUE & stop_statuses == TRUE) {
      #  wait <- wait + sample(30:60,1)
      #}
      if (stop_statuses == TRUE) {
        wait <- wait + sample(30:60,1)
      }
      cat("\n No open app-token windows at the moment, waiting", wait/60, "\n minutes for a window to open\n")
      wait2 <- rep(wait/50, 50)
      progress <- txtProgressBar(min = 0, max = 50, style = 3)
      # wait until the first window opens
      for (i in 1:50) {
        Sys.sleep(wait2[i])
        setTxtProgressBar(progress, i)
      }
      # open all windows with rate limits reset
      credentials$status_lookup[which(credentials$reset_lookup - Sys.time() < 0)] <- "open"
    }
    if (length(which(credentials$status_lookup == "open")) != 0 & stop_friends == TRUE & stop_likes == TRUE & stop_statuses == TRUE) {
      wait <- 50
      wait2 <- rep(wait/50, 50)
      cat("\n Waiting", wait/60, "minute\n")
      progress <- txtProgressBar(min = 0, max = 50, style = 3)
      # wait until the first window opens
      for (i in 1:50) {
        Sys.sleep(wait2[i])
        setTxtProgressBar(progress, i)
      }
    }
    # randomly select app-user credentials
    app_user <- resample((1:nrow(credentials))[which(credentials$status_lookup == "open")], 1)
    # close respective window
    credentials[app_user,]$status_lookup <- "closed"
    # create OAuth application
    application <- oauth_app(appname = "twitter",
                             key = credentials$consumer_key[app_user], 
                             secret = credentials$consumer_secret[app_user])
    # sign OAuth request
    signature <- sign_oauth1.0(application, 
                               token = credentials$access_token[app_user],
                               token_secret = credentials$access_secret[app_user])
    # if this is the first time the collector runs
    if (first_lookup == 1L) {
      if (try_again_followers == 0 & stop_followers == FALSE) {
        # append the gathered follower data to respective table in sql file
        dbWriteTable(conn = connection, name = "beginning_followers",
                     value = record[[3]][request_followers,], append = TRUE)
        # replace information in function-internal data with NA to free memory
        record[[3]][request_followers, "ids"] <- NA
        if(!is.null(warnings())) {
          print(warnings())
        }
      }
      if (try_again_friends == 0 & stop_friends == FALSE) {
        # append the gathered friends data to respective table in sql file
        dbWriteTable(conn = connection, name = "beginning_friends",
                     value = record[[4]][request_friends,], append = TRUE)
        # replace information in function-internal data with NA to free memory
        record[[4]][request_friends, "ids"] <- NA
        # serialze text and date information in likes data
        if(!is.null(warnings())) {
          print(warnings())
        }
      }
      if (try_again_likes == 0 & stop_likes == FALSE & skip_likes == 0) {
        record[[5]][request_likes, c("text", "created")] <- 
          lapply(record[[5]][request_likes, c("text", "created")],
                 function(x) lapply(x, serialize, connection = NULL, ascii = FALSE))
        # append the gathered friends data to respective table in sql file   
        dbWriteTable(conn = connection, name = "beginning_likes",
                     value = record[[5]][request_likes,], append = TRUE)
        # replace information in function-internal data with NA to free memory
        record[[5]][request_likes, c("ids", "text", "source", "created")] <- NA
        if(!is.null(warnings())) {
          print(warnings())
        }
      }
      if (try_again_statuses == 0 & stop_statuses == FALSE) {
        # serialze text and date information in likes data
        record[[6]][request_statuses, c("text", "reply", "created")] <- 
          lapply(record[[6]][request_statuses, c("text", "reply", "created")],
                 function(x) lapply(x, serialize, connection = NULL, ascii = FALSE))
        # append the gathered friends data to respective table in sql file   
        dbWriteTable(conn = connection, name = "beginning_statuses",
                     value = record[[6]][request_statuses,], append = TRUE)
        # replace information in function-internal data with NA to free memory
        record[[6]][request_statuses, c("ids", "text", "reply", "reply_ids", "created")] <- NA
        if(!is.null(warnings())) {
          print(warnings())
        }
      }
    } else {
      if (try_again_followers == 0 & stop_followers == FALSE & length(ids_followers_current) > 0 & skip_followers == 0) {
        # append the gathered follower data to respective table in sql file
        record_7 <- record[[7]][match(record[[1]]$t_ids[request_followers], 
                                      record[[7]]$t_ids),]
        record_7$ids_added <- ifelse(record_7$ids_added == "", NA, record_7$ids_added)
        record_7$ids_removed <- ifelse(record_7$ids_removed == "", NA, record_7$ids_removed)
        record_7 <- filter(record_7, !is.na(ids_added) | !is.na(ids_removed))
        dbWriteTable(conn = connection, name = "change_followers",
                     value = record_7, append = TRUE)
        # replace information in function-internal data with NA to free memory
        record[[7]][match(record[[1]]$t_ids[request_followers], 
                          record[[7]]$t_ids), c("ids_added", "ids_removed")] <- NA
        cat(green("DONE"))
      }
      if (try_again_friends == 0 & stop_friends == FALSE & length(ids_friends_current) > 0 & skip_friends == 0) {
        # append the gathered friends data to respective table in sql file
        record_8 <- record[[8]][match(record[[1]]$t_ids[request_friends], 
                                      record[[8]]$t_ids),]
        record_8$ids_added <- ifelse(record_8$ids_added == "", NA, record_8$ids_added)
        record_8$ids_removed <- ifelse(record_8$ids_removed == "", NA, record_8$ids_removed)
        record_8 <- filter(record_8, !is.na(ids_added) | !is.na(ids_removed))
        dbWriteTable(conn = connection, name = "change_friends",
                     value = record_8, append = TRUE)
        # replace information in function-internal data with NA to free memory
        record[[8]][match(record[[1]]$t_ids[request_friends], 
                          record[[8]]$t_ids), c("ids_added", "ids_removed")] <- NA
        cat(green("DONE"))
      }
      if (try_again_likes == 0 & stop_likes == FALSE & skip_likes == 0) {
        # serialze text and date information in likes data
        record[[9]][match(record[[1]]$t_ids[request_likes],
                          record[[9]]$t_ids), c("text", "created")] <- 
          lapply(record[[9]][match(record[[1]]$t_ids[request_likes],
                                   record[[9]]$t_ids), c("text", "created")],
                 function(x) lapply(x, serialize, connection = NULL, ascii = FALSE))
        # append the gathered friends data to respective table in sql file   
        dbWriteTable(conn = connection, name = "change_likes",
                     value = record[[9]][match(record[[1]]$t_ids[request_likes],
                                               record[[9]]$t_ids),], append = TRUE)
        # replace information in function-internal data with NA to free memory
        record[[9]][match(record[[1]]$t_ids[request_likes],
                          record[[9]]$t_ids), c("ids_added", "text", "source", "created")] <- NA
      }
      if (try_again_statuses == 0 & stop_statuses == FALSE & skip_statuses == 0) {
        # serialze text and date information in likes data
        record[[10]][match(record[[1]]$t_ids[request_statuses],
                           record[[10]]$t_ids), c("text", "reply", "created")] <- 
          lapply(record[[10]][match(record[[1]]$t_ids[request_statuses],
                                    record[[10]]$t_ids), c("text", "reply", "created")],
                 function(x) lapply(x, serialize, connection = NULL, ascii = FALSE))
        # append the gathered friends data to respective table in sql file   
        dbWriteTable(conn = connection, name = "change_statuses",
                     value = record[[10]][match(record[[1]]$t_ids[request_statuses],
                                                record[[10]]$t_ids),], append = TRUE)
        # replace information in function-internal data with NA to free memory
        record[[10]][match(record[[1]]$t_ids[request_statuses],
                           record[[10]]$t_ids), c("ids", "text", "reply", "reply_ids", "created")] <- NA
      }
    }
  }
  if (nrow(errors_followers) > 0) {
    dbWriteTable(conn = connection_2, name = "errors_followers",
                 value = errors_followers, append = TRUE)
  }
  if (nrow(errors_friends) > 0) {
    dbWriteTable(conn = connection_2, name = "errors_friends",
                 value = errors_friends, append = TRUE)
  }
  if (nrow(errors_likes) > 0) {
    dbWriteTable(conn = connection_2, name = "errors_likes",
                 value = errors_likes, append = TRUE)
  }
  if (nrow(errors_statuses) > 0) {
    dbWriteTable(conn = connection_2, name = "errors_statuses",
                 value = errors_statuses, append = TRUE)
  }
  record[[2]] <- credentials
  if (first_lookup == 1L) {
    record <- record[-c(3:6)]
  } else {
    record <- record[-c(7:10)]
  }
  return(record)
}


#### sqlToR =============================================================================

# 'sqlToR' connects to an SQLLite database, processes the data, and brings it into 
# readily analyzable panel format. The 'connection' argument takes a connection to
# a DBMS. The 'table' argument specifies the name of the SQLLite table to be retrieved.
# 'table' must be one of "beginning_statuses", "beginning_likes", "change_statuses",
# "change_likes". The 'column' argument specifies the variables to be kept, using either
# a vector of position values or variable names. 'range' filters rows by date range,
# whereby a date vector including all days to be retrieved must be passed. 'recipients'
# applies to statuses only and includes a column with message recipients if TRUE. 
# If 'language' is TRUE, a column indicating message language is added. If 'rm_meta'
# is set to FALSE, columns indicating mentions, replies, and retweets are omitted, 
# pertains only to statuses. 'rm_emoji', 'rm_urls', 'rm_mention', 'rm_rt', remove the
# respective information from the text, if TRUE. 'rm_rt' pertains to the retweet tag.

sqlToR <- function(connection, table, columns = NULL, range = NULL, recipients = FALSE,
                   language = FALSE, rm_meta = FALSE, rm_emoji = FALSE, rm_urls = FALSE, 
                   rm_mention = FALSE, rm_rt = FALSE) {
  cat(yellow("PROCESSING SQL DATA \n"))
  # establish reference to sql data
  sql_ref <- dplyr::tbl(src = connection, table)
  cat(white("   connection established \n"))
  # optionally subset sql data
  if (!is.null(range)) {
    range <- as.numeric(range)
    sql_ref <- dplyr::filter(sql_ref, date %in% range)
  }
  if (!is.null(columns)) {
    columns <- colnames(sql_ref)[columns]
    sql_ref <- dplyr::select(sql_ref, columns)
  }
  # retrieve data into local data frame
  sql_dat <- dplyr::collect(sql_ref)
  cat(white("   data transfered \n"))
  # stop execution and print error if no data recovered from SQL
  if (dim(sql_dat)[1] == 0) {
    stop("Empty data frame. The provided date range is not covered by the data.")
  }
  if (table != "search_stream") {
    # generate random string
    unique_string <- str_c(" ", stringi::stri_rand_strings(n = 1, length = 13, pattern = "[0-9A-Za-z]"), "  ")
    # unserialize all serialized columns
    for (i in 1:length(colnames(sql_dat))) {
      # if column is not serialized skip to next
      if (lapply(sql_dat, class)[[i]] != "blob") {
        next
      }
      # unserialize cells into collapsed strings and replace "," with unique string
      sql_dat[,i] <- sql_dat[,i] %>%
        rowwise %>%
        mutate(!!colnames(sql_dat)[i] := paste0(str_replace_all(unserialize(unlist(
          !!rlang::parse_quosure(colnames(sql_dat)[i]))), ",", unique_string), collapse = ","))
      cat(white("   column", i, "unserialized \n"))
    }
    # bring into panel format, i.e., separate collapsed columns into multiple rows
    sql_dat <- tidyr::separate_rows(sql_dat, colnames(sql_dat)[-1], sep = ",")
    cat(white("   data reshaped into long format \n"))
    # remove duplicate entries 
    # --issues at beginning of data collection led to multiple collection of same entries
    if (table %in% c("change_statuses", "beginning_statuses")) {
      sql_dat <- dplyr::distinct(sql_dat, ids, .keep_all = TRUE)
    }
    if (table %in% c("change_likes")) {
      sql_dat <- dplyr::distinct(sql_dat, t_ids, ids_added, .keep_all = TRUE)
    }
    if (table %in% c("beginning_likes")) {
      sql_dat <- dplyr::distinct(sql_dat, t_ids, ids, .keep_all = TRUE)
    }
    # replace unique_string with ","
    sql_dat$text <- stringr::str_replace_all(sql_dat$text, unique_string, ",")
  }
  # transform timestamp to date
  sql_dat$created <- lubridate::as_date(sql_dat$created)
  # remove line breaks and weird characters
  sql_dat$text <- stringr::str_replace_all(sql_dat$text, "\\n|\\U0001d409", " ")
  sql_dat$text <- stringr::str_squish(sql_dat$text)
  # replace ampersand with and
  sql_dat$text <- stringr::str_replace_all(sql_dat$text, "&amp;", "and")
  # if table pertains to statuses
  if (table %in% c("change_statuses", "beginning_statuses")) {
    if (!isTRUE(rm_meta)) {
      # make reply column logical
      sql_dat$reply <- ifelse(sql_dat$reply == 1, TRUE, FALSE)
      # create logical indicator for retweets
      sql_dat$retweet <- stringr::str_detect(sql_dat$text, "^RT @")
      # add white space to the right of text
      sql_dat$text <- stringr::str_c(sql_dat$text, " ")
      # identify @mentions
      sql_dat$mention <- stringr::str_detect(sql_dat$text, "@.+? ")
      sql_dat$mention <- ifelse(sql_dat$reply == TRUE | sql_dat$retweet == TRUE, FALSE, sql_dat$mention)
      cat(white("   reply, retweet, and mention indicators added \n"))
    }
    # record recipients (optional)
    if (isTRUE(recipients)) {
      # record recipient
      sql_dat$recipients <- unlist(lapply(lapply(lapply(stringr::str_extract_all(sql_dat$text, "@.+? "), stringr::str_trim), 
                                                 stringr::str_replace_all, "\\.|\\:|\\,", ""), paste0, collapse = ","))
      sql_dat$recipients <- ifelse(sql_dat$recipients == "", NA, sql_dat$recipients)
      cat(white("   recipients recorded \n"))
    }
  }
  if (table %in% c("change_statuses", "beginning_statuses",
                   "change_likes", "beginning_likes", "search_stream")) {
    # remove retweet tag from text
    if (isTRUE(rm_rt)) {
      sql_dat$text <- stringr::str_replace_all(sql_dat$text, "^RT ", "")
      cat(white("   retweet tag removed from text \n"))
    }
    # remove mentions (optional)
    if (isTRUE(rm_mention)) {
      sql_dat$text <- stringr::str_c(sql_dat$text, " ")
      sql_dat$text <- stringr::str_replace_all(sql_dat$text, "@.+? ", "")
      cat(white("   @mentions removed from text \n"))
    }
    # remove urls (optional)
    if (isTRUE(rm_urls)) {
      sql_dat$text <- stringr::str_c(sql_dat$text, " ")
      sql_dat$text <- stringr::str_replace_all(sql_dat$text, 
                                      "https:.+? |bit.ly.+? |http:.+? ", "")
      cat(white("   urls removed from text \n"))
    }
    # remove emojis (optional)
    if (isTRUE(rm_emoji)) {
      # compile emoji lists - requires Internet access
      emojis <- "https://www.fileformat.info/info/emoji/list.htm" %>% read_html %>% html_table()
      # https://www.qhmit.com/character_sets/emoji/emoji_v3.0/unicode_emoji_v3.0_characters_all.cfm # new site
      emojis3 <- "https://www.quackit.com/character_sets/emoji/emoji_v3.0/unicode_emoji_v3.0_characters_all.cfm" %>% read_html %>% html_table()
      emojis4 <- "https://de.piliapp.com/twitter-symbols/" %>% read_html %>% 
        html_nodes(xpath = "//div[@class = 'emojis']/span") %>% html_attrs() %>% lapply(extract2, 2) %>%
        str_replace_all("emoji emoji", "") %>% str_split("-") %>% lapply(str_replace_all,"^", "0x") %>% lapply(intToUtf8) %>% unlist
      emojis5 <- list.files("./data/twitter_emojis/") %>% str_replace_all("\\.svg", "") %>% str_split("-") %>% lapply(str_replace_all,"^", "0x") %>% lapply(intToUtf8) %>% unlist
      emojis5 <- emojis5[-c(2625,2817,2825,2828,2829,2832:2838)]
      emojis3 <- emojis3[[1]]$Hexadecimal
      emojis3 <- stringr::str_replace_all(emojis3, "\\&\\#", "0")
      emojis3 <- stringr::str_replace_all(emojis3, ";", "")
      emojis3 <- str_split(emojis3, pattern = " ")
      emojis3 <- unlist(lapply(emojis3, intToUtf8))
      emojis <- emojis[[2]]$Unicode
      emojis <- emojis[emojis != ""]
      emojis <- stringr::str_replace_all(emojis, "U\\+", "0x")
      emojis <- unlist(stringr::str_split(emojis, " "))
      emojis <- unlist(lapply(emojis, intToUtf8))
      emojis2 <- remoji::emoji(list_emoji())
      emojis <- unlist(unique(c(emojis2, emojis)))[-c(846,649,848:857,637:646)]
      emojis <- unique(c(emojis, emojis3))[-c(1615:1625)]
      emojis <- c(emojis, intToUtf8(0x1f92f), intToUtf8(0x1f9d0))
      emojis <- c(emojis, emojis4[-c(1161:1162,1115:1124)],emojis5)
      emojis <- paste0(emojis, collapse = "|")
      sql_dat$text <- stringr::str_replace_all(sql_dat$text, pattern = emojis, " ")
      cat(white("   emojis removed from text \n"))
    }
    # trim and remove redundant whitespace
    sql_dat$text <- stringr::str_trim(sql_dat$text)
    sql_dat$text <- stringr::str_squish(sql_dat$text)
    # record language (optional)
    if (isTRUE(language)) {
      # detect language googles compact language detector 2
      sql_dat$lang <- cld2::detect_language(sql_dat$text, plain_text = TRUE, lang_code = FALSE)
      sql_dat$lang <- ifelse(sql_dat$lang %in% c("AFRIKAANS", "ALBANIAN", "AZERBAIJANI", 
                                                 "BASQUE", "BOSNIAN", "CEBUANO", "CHEROKEE",
                                                 "CROATIAN", "DANISH", "ESTONIAN", "FINNISH",
                                                 "GANDA", "HMONG", "ICELANDIC", "IRISH",
                                                 "JAVANESE", "KAZAKH", "KURDISH", "LATVIAN",
                                                 "LITHUANIAN", "MALAGASY", "MALAY", "NORWEGIAN",
                                                 "NYANJA", "ROMANIAN", "SCOTS_GAELIC", "SESOTHO",
                                                 "SLOVAK", "SLOVENIAN", "SUNDANESE", "SWAHILI",
                                                 "SWEDISH", "TAGALOG", "UKRAINIAN", "UZBEK",
                                                 "VIETNAMESE", "WELSH", "X_INHERITED"), "ENGLISH", 
                             ifelse(sql_dat$lang %in% c("CATALAN", "GALICIAN", "X_BOPOMOFO"), "SPANISH",
                                    ifelse(sql_dat$lang %in% c("CHINESET"), "CHINESE",
                                           ifelse(is.na(sql_dat$lang), "ENGLISH", sql_dat$lang))))
      cat(white("   language recorded \n"))
    }
  }
  if (table %in% c("change_statuses", "beginning_statuses", "search_stream", "beginning_likes")) {
    sql_dat <- dplyr::select(sql_dat, t_ids, created, ids, everything())
  }
  if (table %in% c("change_likes")) {
    sql_dat <- dplyr::select(sql_dat, t_ids, created, ids_added, everything())
  }
  cat(white("   data formated \n"))
  cat(green("DONE \n"))
  return(sql_dat)
}


#### voteHist ===========================================================================

# 'voteHist' takes a sample of voter record entries in the 'sample' argument and a 
# filepath pointing to the .txt files that store the respective voting histories in the
# 'histpath' argument. For each county specific voting history file the function looks 
# up each observation, thus assigning whether the observation voted in a specific 
# election or not. County specific explicit mentions of "N" (= not voted) are removed 
# initialy to ensure that nonvoters are not falsly classified as voters in a specific 
# election. The function returns a data frame with and "id" column storing an 
# observation's voter ID and election*year specific columns indicating "TRUE" if a
# voter voted in the specific election and "FALSE" if not. The function does only
# consider general and primary elections as these are contained in the voting history
# files of every county.

voteHist <- function(histpath, sample) {
  files <- list.files(histpath)
  elections <- c("gen2006", "gen2008", "gen2010", "gen2012", "gen2014",
                 "gen2016", "gen2018",
                 "pri2006", "pri2008", "pri2010", "pri2012", "pri2014",
                 "pri2016", "pri2018")
  dates <- c("11/07/2006", "11/04/2008", "11/02/2010", "11/06/2012", "11/04/2014",
             "11/08/2016", "11/06/2018", "09/05/2006", "08/26/2008", "08/24/2010",
             "08/14/2012", "08/14/2012", "08/26/2014", "08/30/2016", "08/28/2018")
  for (i in 1:length(files)) {
    search <- fread(input = paste0(histpath, files[i]), 
                    header = FALSE,
                    stringsAsFactors = FALSE, 
                    col.names = c("county", "id", "date", "election", "mode"),
                    na.strings = c("", "*")) %>%
      as.data.frame()
    search <- filter(search, election == "GEN"| election == "PRI") %>%
      filter(mode != "N")
    for (j in 1:length(elections)) {
      sub_search <- search %>%
        filter(date == dates[j]) %>%
        dplyr::select(id, date)
      sample <- left_join(sample, sub_search, by = "id")
      sample <- sample %>%
        mutate("{elections[j]}" := ifelse(!is.na(date), TRUE, !!sym(elections[j]))) %>%
        dplyr::select(-date)
    }
  }
  return(sample)
}


#### applyDict ==========================================================================

# 'applyDict' takes a dataframe with preprocessed text in its 'data' argument and applies
# the uni-, bi-, and trigram dictionaries supplied in the 'dictionary_uni', 
# 'dictionary_bi' and 'dictionary_tri' arguments. The column with preprocessed text in 
# 'data' must be named 'text_pro'. The 'varname' argument specifies the name of the 
# columns that yield the result of the classification procedure. At max, three columns
# are returned with the original data, depending on whether bi-, and trigram dictionaries
# were supplied in addition to a unigram dictionary, varname is then appended with
# '_uni', '_bi', and '_tri'. The dictionaries must be of class 'dictionary2', as produced
# with quanteda's dfm function. The 'slicesize' argument determines the number of rows
# that are processed at once.

applyDict <- function(data, varname, slicesize, dictionary_uni, dictionary_bi = NULL, 
                      dictionary_tri = NULL) {
  # generates slices of data in steps of slicesize
  from <- sapply(split(1:nrow(data), ceiling(seq_along(1:nrow(data))/slicesize)), `[[`, 1)
  to <- sapply(split(1:nrow(data), ceiling(seq_along(1:nrow(data))/slicesize)), tail, n = 1)
  # add empty columns for classification results to data
  varname_uni <- paste(varname, "uni" , sep="_")
  data <- mutate(data, !!varname_uni := NA)
  if (class(dictionary_bi) == "dictionary2") {
    varname_bi <- paste(varname, "bi" , sep="_")
    data <- mutate(data, !!varname_bi := NA)
  }
  if (class(dictionary_tri) == "dictionary2") {
    varname_tri <- paste(varname, "tri" , sep="_")
    data <- mutate(data, !!varname_tri := NA)
  }
  cat(yellow("APPLYING DICTIONARY TO DATA\n"))
  # loop over data slices
  for (i in 1:length(to)) {
    # construct a corpus object
    corp <- corpus(data[from[i]:to[i],],
                   docid_field = "doc_id",
                   text_field = "text_pro")
    cat("rows", from[i], "to", to[i], "tranformed to corpus\n")
    # construct a sparse document-feature matrix from corpus object with ngrams according
    # to the dictionary that shall be applied and apply the respective dictionary to the
    # tokens (for uni- bi- and trigram dictionaries)
    dfm_uni <- dfm(corp, ngrams = 1L, dictionary = dictionary_uni)
    dfm_uni <- convert(dfm_uni, to = "data.frame")
    colnames(dfm_uni)[2] <- paste0(varname, "_uni")
    data[from[i]:to[i],varname_uni] <- dfm_uni[,varname_uni]
    cat("----unigram dictionary applied\n")
    if (class(dictionary_bi) == "dictionary2") {
      dfm_bi <- dfm(corp, ngrams = 2L, dictionary = dictionary_bi)
      dfm_bi <- convert(dfm_bi, to = "data.frame")
      colnames(dfm_bi)[2] <- paste0(varname, "_bi")
      data[from[i]:to[i],varname_bi] <- dfm_bi[,varname_bi]
      cat("----bigram dictionary applied\n")
    }
    if (class(dictionary_tri) == "dictionary2") {
      dfm_tri <- dfm(corp, ngrams = 3L, dictionary = dictionary_tri)
      dfm_tri <- convert(dfm_tri, to = "data.frame")
      colnames(dfm_tri)[2] <- paste0(varname, "_tri")
      data[from[i]:to[i],varname_tri] <- dfm_tri[,varname_tri]
      cat("----trigram dictionary applied\n")
    }
  }
  cat(green("DONE \n"))
  return(data)
}


#### mlAMEs ==========================================================================

# 'mlAMEs'produces population-averaged predictions for multilevel models. The data 
# argument takes a matrix or data.frame with variables that are indexed when 
# evaluating the linear predictor to assign the relevant value of a parameter for 
# each observation in the data. This can but must not include variables fixed at 
# specific values. The exact specification depends on the specification of the linear 
# predictor. The levels argument specifies the steps or changes along a specific 
# covariate for which predictions shall be obtained. This must be a named list 
# including at least one vector named ‘x’, which appears in the linear predictor 
# and sets changes of interest at the scale of the respective covariate. This can 
# also be used to introduce dummy vectors of covariate values in the data for a 
# different specification of the linear predictor. The draws argument takes a numeric 
# specifying the number of draws set to obtain the posterior (simulations). The 
# posterior argument takes a named list of vectors or matrices/data.frames, each 
# representing the (simulated) posterior of an estimated parameter. The names must 
# align with the specification of the linear predictor. The linear_predictor argument 
# takes a character string describing the linear predictor. Parameters are named as 
# specified in the named list passed to the posterior argument. Parameter draws are 
# indexed with n, changes in x with k, data is indexed using the variable/column 
# names in the matrix or data.frame passed to the data argument. The type argument 
# takes one of linear or logit. For other generalized linear multilevel models the 
# link function can be substituted within the mlAMEs() function.

mlAMEs <- function(data, levels, draws, posterior, linear_predictor, type) {
  # throw error if type is not one of linear or logit
  if (!(type %in% c("linear", "logit")))
    stop ("mlAMEs works with 'type' = 'linear' or 'logit' only.")
  # translate all factor variables to character
  data[sapply(data, is.factor)] <- lapply(data[sapply(data, is.factor)], 
                                          as.character)
  # create ID column denoting unique combinations of variables in the data
  data <- data.table::as.data.table(data)
  data[, predID := .GRP, by = names(data)]
  # record the number of times each unique combination of variables occurs in the data
  predID_count <- count(data, predID)$n
  # assign the unique combinatiions of variables in the data to a computation dataset
  data_comp <- distinct(data, .keep_all = TRUE)
  # all pieces of data in a list (similar to rstan) and assign into the environment
  data_list <- c(list(n_draws = draws),
                 levels,
                 as.list(data_comp),
                 posteriors)
  for (i in 1:length(data_list)) {
    assign(names(data_list)[i], data_list[[i]])
  }
  # modify the linear predictor such that is evaluates to elementwise addition for 
  # matrices and column-wise recycling of vectors added to or multiplied with vectors.
  # In the matrices that are arithmetically evaluated, rows are posterior draws and 
  # columns are data values for the specific unique combinations of variables in the data
  linear_predictor <- linear_predictor %>%
    str_replace_all("\\[n", str_c("\\[1\\:", n_draws))
  # parse the modified linear predictor so that in can be evaluated
  linear_predictor <- parse(text = linear_predictor)
  # iterate over the predetermined steps of x (parallelized)
  ames_full <- foreach(k = 1:length(x), .packages = c("arm", "purrr"), 
                       .verbose = FALSE, .export = c(names(data_list)[-1])) %dopar% {  
                         # evaluate the linear predictor and transform using the inverse of the link 
                         # function if type == "logit", else evaluate without transforming
                         if (type == "logit") {
                           prediction <- arm::invlogit(eval(linear_predictor))
                         } else {
                           prediction <- eval(linear_predictor)
                         }
                         # for each posterior draw expand the predictions for unique combinations
                         # of variables in the data according to their occurence in the data, i.e.,
                         # a prediction for each individual in the data. Then average over the predictions,
                         # producing a population-averaged prediction for each draw and step of x
                         ames <- 1:n_draws %>%
                           purrr::map(~{
                             y <- mean(rep(prediction[.x,], times = predID_count))
                           }) %>%
                           unlist
                         return(ames)
                       }
  # tranform output to tidy format for easy visualization via ggplot2
  ames_full <- data.table::transpose(as.data.table(ames_full))
  ames_full <- melt(data = ames_full, measure.vars = 1:n_draws,
                    value.name = "y", variable.name = "draw")
  ames_full$x <- rep(x, n_draws)
  #ames_full$draw <- as.integer(str_replace(ames_full$draw, "V", ""))
  return(ames_full)
}


