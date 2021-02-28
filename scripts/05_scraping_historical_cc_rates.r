# 02 Ensure all pkgs in this script are installed ####
pkgs <-
    c('xml2',
    'rvest')

activatePkgs(pkgs)

# Load requirements
requirements_df <-
  data.frame(
    required_rda_files =
      c("outputs/02_dates_of_interest.rda"),
    
    source_scripts = 
      c("scripts/02_dates_of_interest.r"),
    
    stringsAsFactors = F)

load_source_data()

# Establish which days to scrape for CoinMarketCap
days_for_coinmarketcap <- #make sequence of days
  seq(as.POSIXlt(start_date),
      as.POSIXlt(end_date),
      by = "day") %>% #convert to date
  as.Date %>% #clean the strings
  gsub("-", "", .)


# Load RSelenium driver

startRd()

# Make directory in output folder
makeDirIfDoesNotExist("outputs/05_cmc")

# For all days of interest will do a loop to capture the opening prices for
# the day of interest (note that the website may show the previous date in the
# header, which is why these are taken as opening prices)
for (i in 1 : length(days_for_coinmarketcap)) {
  temp_df_name <- paste0("cmc_", days_for_coinmarketcap[i])
  
  file_path <- paste0("outputs/05_cmc/", days_for_coinmarketcap[i], ".rda")
  
  # If the data has already been scraped, will skip to next iteration of loop
  ifelse(test = file.exists(file_path),
         {cat(paste0("File ", file_path,
         " exists. Skipping to next iteration.\n\n"))
           next},
         
         #If the data has not already been scraped, will scrape and save it
         {cat(paste0("File ", file_path,
         " not found. Navigating to page to scrape data.\n\n"))
           
           rd$client$navigate(paste0("https://coinmarketcap.com/historical/", days_for_coinmarketcap[i]))
           #Give time to load page before proceeding
           Sys.sleep(3)
           
           button_exists <- T
           try <- 1
           #While there are more crypto data to load, they will be loaded
           while (button_exists & try <= 10) {
             tryCatch(
               {
                 cat(paste0("For ", temp_df_name, ": finding 'load more' button.\n\n"))
                 load_more_button <-
                  rd$client$findElement(using = "css selector",
                  value =
                  ".cmc-table-listing__loadmore .cmc-button--color-default")
                 load_more_button$sendKeysToElement(list(key = "end", key = "page_up"))
                 #Give time between scrolling before clicking
                 Sys.sleep(1)
                 cat(paste0("For ", temp_df_name, ": clicking 'load more' button.\n\n"))
                 load_more_button$clickElement()
                 cat(paste0("For ", temp_df_name, ": clicked 'load more' button ", try, " times.\n\n"))
                 if (try == 10) cat("Final try, will not load more after this try.")
                 try <- try + 1
                 #Give time to load new data before repeating loop
                 Sys.sleep(3)
               },
               #Update button_exists when it no longer exists to escape the loop
               error = function(cond) {
                 cat(paste0("For ", temp_df_name, ": no more 'load more' button.\n\n"))
                 assign("button_exists", F, envir = .GlobalEnv)
               },
               finally = NA
             )
           }
           
           #After all data has been loaded to the page, the page should be
           # scraped
           
           #Link that provided helpful hints:
           #https://callumgwtaylor.github.io/blog/2018/02/01/using-rselenium-and-docker-to-webscrape-in-r-using-the-who-snake-database/
           cat(paste0("For ", temp_df_name, ": capturing 'temp_df'.\n\n"))
           
           temp_df <-
             rd$client$getPageSource()[[1]] %>% #converts full page to pure html now that all info is loaded from the javascript interactions
             xml2::read_html(.) %>% #reads the html
             rvest::html_table(.) %>% #pulls tables
             .[[3]] #the third table is the table I care to scrape
           
           cat(paste0("For ", temp_df_name, ": naming 'temp_df' ", temp_df_name, ".\n\n"))
           
           assign(temp_df_name, temp_df)
           
           cat(paste0("For ", temp_df_name, ": saving as ", file_path, ".\n\n"))
           
           #This was for saving as .rda
           save(list = temp_df_name,
                file = file_path)
           
           cat("Cleaning memory before next iteration.\n\n")
           
           rm(list = temp_df_name, button_exists, file_path, i, load_more_button, temp_df, temp_df_name)
           
           cat("Moving to next iteration.\n\n")
           
           next
         }
        )
  }


 # Remove the remote Driver, clean up the "garbage" to free memory, and kill any processes still keeping the port open.
 
stopRd()