# Load requirements
requirements_df <-
  data.frame(
    required_rda_files =
      c("outputs/02_dates_of_interest.rda"),
    
    source_scripts = 
      c("scripts/02_dates_of_interest.r"),
    
    stringsAsFactors = F)

load_source_data()

# 02 Ensure all pkgs in this script are installed ####
pkgs <-
    c('openxlsx')

activatePkgs(pkgs)

#Load RSelenium driver
startRd()

#Navigate to the page
rd$client$navigate(paste0(
  # "https://www.riksbank.se/sv/statistik/sok-rantor--valutakurser/?g130-SEKEURPMI=on&g130-SEKUSDPMI=on&from=",
  "https://www.riksbank.se/sv/statistik/sok-rantor--valutakurser/?g130-SEK",
  "CHF",
  "PMI=on&g130-SEK",
  "EUR",
  "PMI=on&g130-SEK",
  "USD",
  "PMI=on&from=",
  start_date,
  "&to=",
  end_date,
  "&f=Day&c=cAverage&s=Dot")) #take rsd to webpage for fiat x rates for EUR and USD for my selected dates

#Scrape the data
riksbanken_exchange_rates <-
  rd$client$getPageSource()[[1]] %>% #converts full page to pure html now that all info is loaded 
  xml2::read_html(.) %>% #reads the html
  rvest::html_table(., fill = T) %>% #pulls tables
  .[[2]] %>% #the second table is the table I care to scrape
  .[2 : nrow(.) , 2:5] #keeps the columns I care about

# Remove the remote Driver, clean up the "garbage" to free memory, and kill any processes still keeping the port open.
stopRd()

#Modify the df so it is in the format I need
names(riksbanken_exchange_rates) <- list("Date", "CHF_SEK_", "EUR_SEK_", "USD_SEK_")

# Riksbanken lists per 100 CHF, so that needs to be modified
riksbanken_exchange_rates %<>%
  mutate(CHF_SEK_ = as.numeric(CHF_SEK_) / 100)

#Impute missing NAs
riksbanken_exchange_rates$CHF_SEK_ %<>%
  imputeTS::na_interpolation(.)

riksbanken_exchange_rates[["SEK_CHF_"]] <-
  1 / riksbanken_exchange_rates[["CHF_SEK_"]]

riksbanken_exchange_rates[["Date"]] %<>% #format for later merging
  as.Date %>%
  lubridate::ymd(.)

riksbanken_exchange_rates[["EUR_SEK_"]] %<>%
  as.character %<>%
  as.numeric 

riksbanken_exchange_rates[["SEK_EUR_"]] <-
  1 / riksbanken_exchange_rates[["EUR_SEK_"]]

riksbanken_exchange_rates[["USD_SEK_"]] %<>%
  as.character %<>%
  as.numeric

riksbanken_exchange_rates[["SEK_USD_"]] <-
  1 / riksbanken_exchange_rates[["USD_SEK_"]]

riksbanken_exchange_rates[["CHF_USD_"]] <-
  riksbanken_exchange_rates[["SEK_USD_"]] / riksbanken_exchange_rates[["SEK_CHF_"]]

riksbanken_exchange_rates[["USD_CHF_"]] <-
  1 / riksbanken_exchange_rates[["CHF_USD_"]]

riksbanken_exchange_rates[["EUR_USD_"]] <-
  riksbanken_exchange_rates[["SEK_USD_"]] / riksbanken_exchange_rates[["SEK_EUR_"]]

riksbanken_exchange_rates[["USD_EUR_"]] <-
  1 / riksbanken_exchange_rates[["EUR_USD_"]]

riksbanken_exchange_rates %<>%
  merge(dates_of_interest,
        all = T)

# Pull in the irs exchange rates
irs_exchange_rates <-
  read.xlsx("inputs/03_irs_exchange_rates.xlsx")

#save the data
save(riksbanken_exchange_rates, irs_exchange_rates, file = "outputs/03_fiat_exchange_rates.rda")
