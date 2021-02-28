# Load requirements
requirements_df <-
  data.frame(
    required_rda_files =
      c("outputs/02_dates_of_interest.rda",
        "outputs/03_fiat_exchange_rates.rda",
        "outputs/04_usd_xau__rates.rda"),
    
    source_scripts = 
      c("scripts/02_dates_of_interest.r",
        "scripts/03_fiat_exchange_rates.r",
        "scripts/04_usd_xau__rates.r"),
    
    stringsAsFactors = F)

load_source_data()

# 02 Ensure all pkgs in this script are installed ####
pkgs <-
    c('openxlsx')

activatePkgs(pkgs)

#Load data from my ccs
my_ccs <-
  read.xlsx("inputs/06_cc_ticker_lookup_table.xlsx", sheet = "CoinMarketCap")

my_bitfinex_ccs <-
  read.xlsx("inputs/06_cc_ticker_lookup_table.xlsx", sheet = "Bitfinex")

bitfinex_deps_wtdrws_trnsfrs <- 
  read.xlsx(xlsxFile = "inputs/06_cc_ticker_lookup_table.xlsx", sheet = "Bitfinex_deps_wtdrws_trnsfrs")

my_gatehub_ccs <-
  read.xlsx("inputs/06_cc_ticker_lookup_table.xlsx", sheet = "Gatehub")

gatehub_deps_wtdrws_trnsfrs <- #load df from excel file
  read.xlsx(xlsxFile = "inputs/06_cc_ticker_lookup_table.xlsx", sheet = "Gatehub_deps_wtdrws_trnsfrs")

my_transferwise_ccs <-
  read.xlsx("inputs/06_cc_ticker_lookup_table.xlsx", sheet = "Transferwise")

my_kraken_ccs <-
  read.xlsx("inputs/06_cc_ticker_lookup_table.xlsx", sheet = "kraken")

kraken_deps_wtdrws_trnsfrs <- #load df from excel file
  read.xlsx(xlsxFile = "inputs/06_cc_ticker_lookup_table.xlsx", sheet = "Kraken_deps_wtdrws_trnsfrs")

#Consolidate the objects into a single df
cmc_files <-
  list.files(path = "outputs/05_cmc/")

for (i in 1 : length(cmc_files)) {
  
  cat(paste0("i is ", i, " of ", length(cmc_files), " cmc_files.\n\n"))
  
  #load the .rda file as a n object
  load(paste0("outputs/05_cmc/", cmc_files[i]))
  
  #specify date
  df_date <-
    cmc_files[i] %>%
    gsub(".rda", "", x = .)
  
  #specify the name of the object
  obj_name <- 
    df_date %>%
    paste0("cmc_", .)
  
  #Create a temporary df and add the date column
  temp_df <- get(obj_name)

  #rename column to Rank if just named as # to begin with
  if(names(temp_df)[1] == "#") {
    names(temp_df)[1] <- 'Rank'
  }

  #Add missing columns
  temp_df[["Date"]] <-
    df_date %>%
    lubridate::ymd(.)
  
  temp_df["coinmarketcapTickerPlusNameForRMatching"] <-
    paste0(temp_df[["Symbol"]], temp_df[["Name"]]) %>%
    gsub("\n| ", "", x = .)

  #Rename a column
  temp_df[["USD_"]] <- temp_df[["Price"]]
  temp_df[["Price"]] <- NULL

  #create data_frame if it does not already exist
  ifelse(exists("usd__all_rates", envir = .GlobalEnv) == T,
         {usd__all_rates %<>% rbind(temp_df)},
         {usd__all_rates <<- temp_df})
  
  #remove the object so as to save memory
  rm(list = obj_name)
  
  #remove other created variables
  rm(i, temp_df, obj_name, df_date)

  
}


# Drop column if no name
names(usd__all_rates)
for (i in seq_along(colnames(usd__all_rates))) {
  if (i == 1) {
    j <- 0
  }
  i <- i - j
  if (colnames(usd__all_rates)[i] == '') {
    usd__all_rates[[i]] <- NULL
    j <- j + 1
  }
}
names(usd__all_rates)

#After creating one big data frame I need to join and organize this with other data.

usd__all_rates %<>% #only keep rows matching my CCs
  .[.[["coinmarketcapTickerPlusNameForRMatching"]] %in%
      my_ccs[["coinmarketcapTickerPlusNameForRMatching"]], ] %>% #bring in fourLetterTicker
  merge(., my_ccs) %>% #subset to only columns I want to keep
  .[ , c("Date", "fourLetterTicker", "USD_")] %>% 
  arrange(Date) %>%
  distinct # deduplicate rows

usd__all_rates[["Date"]] %<>%
  as.Date %>%
  lubridate::ymd(.) #converts to right type for later merging operations

usd__all_rates[["USD_"]] %<>% #Remove $ sign
  gsub("[^[:alnum:][:space:].]","", .) %<>%
  as.numeric #convert to numeric

usd__all_rates %<>% #reshape so Date is left column, fourLetterTickers are column headers, USD are values
  reshape(v.names = "USD_",
          timevar= "fourLetterTicker",
          idvar = "Date",
          direction = "wide",
          sep = "") %>% #add rows where no dates with no CC x rate data. Merging will duplicate date rows if they are characters in one df but factors in another
  merge(x = ., y = dates_of_interest)

# Reorder columns so that sorted alphabetically
usd__all_rates %<>%
  select(
    c(
      "Date",
      colnames(usd__all_rates) %>% .[. %notIn% "Date"] %>% sort
      )
    )

# Add in any dates where data was not successfully scraped
dates <-
  data.frame(
    Date =
      as.Date(seq(as.POSIXlt(start_date), as.POSIXlt(end_date), by = "day"))
    )

usd__all_rates %<>%
  left_join(dates, .) %>% #arrange by date
  arrange(Date)

colnames(usd__all_rates) <- #renames columns based on selling 1 crypto to yield x number of USD. This will not pipe easily!
  paste0(
    stringr::str_sub(colnames(usd__all_rates), -4, -1),
    stringr::str_sub(colnames(usd__all_rates), 1, 4))

colnames(usd__all_rates)[1] <- "Date" #rename Date column since previous operation made it odd

usd__all_rates %<>% #add XAU exchange data
  merge(x = usd_xau__rates[ , c("Date", "XAU_USD_")],
        y = .) %>% #add fiat exchange data
  merge(x = riksbanken_exchange_rates[ , c("Date", "CHF_USD_", "EUR_USD_", "SEK_USD_")],
        y = .) %>% #impute missing data
  imputeTS::na_interpolation(.)

#Now to create a similar dataframe for sek
#create df with desired structure
sek__all_rates <- usd__all_rates

names(sek__all_rates) %<>% #rename columns
  sub("USD_", "SEK_", .)

for (i in 2 : length(names(sek__all_rates))) { #loop to convert SEK_CC from USD_CC based on SEK_USD_
  
  sek__all_rates[ , i] <-
    sek__all_rates[ , i] *
    riksbanken_exchange_rates[["USD_SEK_"]]
  
}

sek__all_rates %<>% #drop fiat exchanges
  .[ , colnames(.) %notIn% c("CHF_SEK_", "EUR_SEK_", "SEK_SEK_")] %>% #add fiat exchange data
  merge(x = riksbanken_exchange_rates[ , c("Date", "CHF_SEK_", "EUR_SEK_", "USD_SEK_")],
        y = .) %>% #impute missing data
  imputeTS::na_interpolation(.)

#This will be used in a future script
ref_table <-
  read.xlsx(xlsxFile = "inputs/06_cc_ticker_lookup_table.xlsx",
    sheet = "All")

save(my_ccs, my_bitfinex_ccs, bitfinex_deps_wtdrws_trnsfrs, my_gatehub_ccs, gatehub_deps_wtdrws_trnsfrs, my_transferwise_ccs, my_kraken_ccs, kraken_deps_wtdrws_trnsfrs, usd__all_rates, sek__all_rates, ref_table, file = "outputs/06_all_exchange_rates.rda")
