# Project root
projectRoot <- "D:/Coding/Projects/Taxes_2020/"

setwd(projectRoot)

# Load requirements
source(paste0(projectRoot, "scripts/01_my_fxns_this_project.r"))

# 02 Ensure all pkgs in this script are installed ####
pkgs <-
    c('svDialogs')

activatePkgs(pkgs)

##
dates_of_interest_question <-
  dlgInput("TRUE/FALSE: you have updated '02_years_of_interest.xlsx' with the years you want to process data for AND moved the file to the 'inputs' directory.", Sys.info()["user"])$res %>% as.logical

if(!identical(dates_of_interest_question, TRUE)) {
  dlgMessage("YOU CANNOT PROCEED!")
  
  dlgMessage("You must update '02_years_of_interest.xlsx' with the years you want to process data for AND move the file to the 'inputs' directory.")
  
  dlgMessage("ABORTING R SESSION!")
  
  quit()
}


## Still need to download latest exchange rates after 2020 concludes
irs_exchange_rates_question <-
  dlgInput("TRUE/FALSE: you have updated '03_irs_exchange_rates.xlsx' with this year's rates AND moved the file to the 'inputs' directory.", Sys.info()["user"])$res %>% as.logical

if(!identical(irs_exchange_rates_question, TRUE)) {
  dlgMessage("YOU CANNOT PROCEED!")
  
  dlgMessage("You must update '03_irs_exchange_rates.xlsx' with this year's rates AND move the file to the 'inputs' directory.")
  
  dlgMessage("ABORTING R SESSION!")
  
  quit()
}

## Still need to download latest gold prices after 2020 concludes
gold_rate_question <-
  dlgInput("TRUE/FALSE: you have moved '04_usd_xau_rates.rda' to the outputs directory OR moved '04_goldprices.xlsx' to the inputs directory.", Sys.info()["user"])$res %>% as.logical

if(!identical(gold_rate_question, TRUE)) {
  dlgMessage("YOU CANNOT PROCEED!")
  
  dlgMessage("You must move '04_usd_xau_rates.rda' to the outputs directory OR move '04_goldprices.xlsx' to the inputs directory.")
  
  dlgMessage("ABORTING R SESSION!")
  
  quit()
}


## Still need to verify that cmc scraper is working
scraping_cmc_question <-
  dlgInput("TRUE/FALSE: you have moved '05_cmc/' to the outputs directory.", Sys.info()["user"])$res %>% as.logical

if(!identical(scraping_cmc_question, TRUE)) {
  dlgMessage("YOU CANNOT PROCEED!")
  
  dlgMessage("You must move '05_cmc/' to the outputs directory or else you will waste hours scraping data you already scraped last year.")
  
  dlgMessage("ABORTING R SESSION!")
  
  quit()
}


##
cc_ticker_lookup_question <-
  dlgInput("TRUE/FALSE: you have moved '06_cc_ticker_lookup_table.xlsx' to the inputs directory.", Sys.info()["user"])$res %>% as.logical

if(!identical(cc_ticker_lookup_question, TRUE)) {
  dlgMessage("YOU CANNOT PROCEED!")
  
  dlgMessage("You must move '06_cc_ticker_lookup_table.xlsx' to the inputs directory.")
  
  dlgMessage("ABORTING R SESSION!")
  
  quit()
}


##
all_exchanges_all_years_question <-
  dlgInput("TRUE/FALSE: you have moved '09_skatteverket_parameters.xlsx' to the 'inputs' directory.", Sys.info()["user"])$res %>% as.logical

if(!identical(all_exchanges_all_years_question, TRUE)) {
  dlgMessage("YOU CANNOT PROCEED!")
  
  dlgMessage("You must move '09_skatteverket_parameters.xlsx' to the 'inputs' directory")
  
  dlgMessage("ABORTING R SESSION!")
  
  quit()
}


##
skatteverket_parameters_question <-
  dlgInput("TRUE/FALSE: You have moved '07_all_exchanges_all_years.rda' to the outputs directory OR moved ALL OF THESE TO THE 'inputs/07_all_exchanges_all_years' directory; 'bitfinex_deposits.csv', 'bitfinex_withdrawals.csv', 'bitfinex_trades.csv', 'gatehub_2016-12-31_2020-01-01.csv', 'transferwise_transfers.csv', 'kraken_ledgers.csv'.", Sys.info()["user"])$res %>% as.logical

if(!identical(skatteverket_parameters_question, TRUE)) {
  dlgMessage("YOU CANNOT PROCEED!")
  
  dlgMessage("You must move '07_all_exchanges_all_years.rda' to the outputs directory OR move ALL OF THESE TO THE 'inputs/07_all_exchanges_all_years' directory; 'bitfinex_deposits.csv', 'bitfinex_withdrawals.csv', 'bitfinex_trades.csv', 'gatehub_2016-12-31_2020-01-01.csv', 'transferwise_transfers.csv', 'kraken_ledgers.csv'")
  
  dlgMessage("ABORTING R SESSION!")
  
  quit()
}


dlgMessage("Will now attempt to generate the worbook for the accountant with data arranged both for Skatteverket and for IRS on different tabs.")

source("10_make_wb_for_accountant.r")

dlgMessage("If the job succeeded, you should find '10_wb_for_accountant.xlsx' in the 'outputs/' directory.")