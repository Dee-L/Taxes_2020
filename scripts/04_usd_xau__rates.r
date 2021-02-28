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

#Reference saved file
usd_xau__rates <- #make df
  read.xlsx("inputs/04_goldprices.xlsx", #Login and download data from https://www.gold.org/data/gold-price
            "Daily",
            startRow = 8,
            skipEmptyCols = T,
            detectDates = T,
            skipEmptyRows = T) %>% #pulls out relevant data from file
  .[ , 1:2] %>%
  (. %<>%
     rename(Date = Name) %<>%
     rename(troyozgold_USD = US.dollar))

usd_xau__rates[["Date"]] %<>% #formatting for merging
  as.Date %>%
  lubridate::ymd(.)

usd_xau__rates %<>% #limit to USD column and rename it
  merge(dates_of_interest,
        all = T) %>% #add dates of interest
  .[.[["Date"]] %in% dates_of_interest[["Date"]], ] #limit to dates of interest

usd_xau__rates[["XAU_USD_"]] <-
  usd_xau__rates[["troyozgold_USD"]]/31.1034768

usd_xau__rates[["USD_XAU_"]] <-
  1 / usd_xau__rates[["XAU_USD_"]]

save(usd_xau__rates, file = "outputs/04_usd_xau__rates.rda")