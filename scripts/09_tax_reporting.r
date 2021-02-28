# Load requirements
requirements_df <-
  data.frame(
    required_rda_files =
      c("outputs/02_dates_of_interest.rda",
        "outputs/03_fiat_exchange_rates.rda",
        "outputs/06_all_exchange_rates.rda",
        "outputs/08_fifo.rda"),
    
    source_scripts =
      c("scripts/02_dates_of_interest.r",
        "scripts/03_fiat_exchange_rates.r",
        "scripts/06_all_exchange_rates.r",
        "scripts/08_fifo.r"),
    
    stringsAsFactors = F)

load_source_data()

# 02 Ensure all pkgs in this script are installed ####
pkgs <-
    c('openxlsx')

activatePkgs(pkgs)

#Parameters from Skatteverket
Rate_to_find_deductableportionof_8.4 <- read.xlsx("inputs/09_skatteverket_parameters.xlsx")[["Rate_to_find_deductableportionof_8.4"]]

Taxrate_on_profits <- read.xlsx("inputs/09_skatteverket_parameters.xlsx")[["Taxrate_on_profits"]]

#Pull in a ref table to indicate how to treat different assets in terms of Swedish tax rules
ref_table %<>% .[ , c("tickerToReport",
                      "profitboxVinstruta",
                      "lossboxFörlustruta")]

#Read in fifo data for the relevant years
first_year <- earliest_year
final_year <- filing_year

#preparing empty list where objects will be appended for saving later
objs_to_save_in_rda <- c()

for (year in first_year : final_year) {
  
  # Get data that will be used for reporting to Skatteverket
  all_trades <- get(paste0("fifo_", year))
  
  # Subset to only include sales for reporting to the IRS
  all_sales <- all_trades[all_trades[["Action_Händelse"]] == "Sell", ]
  
  #Now focus on producing tables used for Skatteverket only and will return to tables for the IRS later.
  #only keeping columns necessary for Skatteverket
  all_trades %<>% 
    .[ , c("Rownumber",
           "Ticker_Beteckning",
           "Transaction_Datum",
           "Action_Händelse",
           "Number_Antal",
           "Totalnumber_Totalantal",
           "SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp",
           "TotalSEKspent_Totaltoutnyttjatomkostnadsbeloppförkvarvarandevärdepapper",
           "AverageSEKperunit_Genomsnittligtomkostnadsbeloppförkvarvarandevärdepapper",
            "Purchase_price_used_in_this_sale")]

  obj_name <- paste0(year, "_skv_1_all_trades")
  
  assign(obj_name, all_trades)
  
  objs_to_save_in_rda %<>% c(obj_name)
  
  #Creating the aggregated table that shows information I need to enter into K4 documents for Skatteverket
  distinct_assets <- 
    all_trades[["Ticker_Beteckning"]] %>% unique %>% sort %>% data.frame(Ticker_Beteckning = .)
  
  k4_c_for_fiat_or_d_for_crypto_table <- sqldf::sqldf("
                                                      select
                                                      sum(Number_Antal) as Number_Antal,
                                                      Ticker_Beteckning,
                                                      sum(SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp) as Salesprice_Försäljningspris,
                                                      sum(Number_Antal * AverageSEKperunit_Genomsnittligtomkostnadsbeloppförkvarvarandevärdepapper) as Purchaseprice_Omkostnadsbelopp
                                                      from all_trades
                                                      where Action_Händelse = 'Sell'
                                                      group by Ticker_Beteckning")
  
  k4_c_for_fiat_or_d_for_crypto_table[["Profit_vinst"]] <-
    ifelse(
      (k4_c_for_fiat_or_d_for_crypto_table[["Salesprice_Försäljningspris"]] - k4_c_for_fiat_or_d_for_crypto_table[["Purchaseprice_Omkostnadsbelopp"]]) > 0,
      k4_c_for_fiat_or_d_for_crypto_table[["Salesprice_Försäljningspris"]] - k4_c_for_fiat_or_d_for_crypto_table[["Purchaseprice_Omkostnadsbelopp"]],
      0)
  
  k4_c_for_fiat_or_d_for_crypto_table[["Loss_Förlust"]] <-
    ifelse(
      (k4_c_for_fiat_or_d_for_crypto_table[["Salesprice_Försäljningspris"]] - k4_c_for_fiat_or_d_for_crypto_table[["Purchaseprice_Omkostnadsbelopp"]]) < 0,
      k4_c_for_fiat_or_d_for_crypto_table[["Purchaseprice_Omkostnadsbelopp"]] - k4_c_for_fiat_or_d_for_crypto_table[["Salesprice_Försäljningspris"]],
      0)                     
  
  k4_c_for_fiat_or_d_for_crypto_table <-
    sqldf::sqldf("
                 select *
                 from k4_c_for_fiat_or_d_for_crypto_table as s
                 join ref_table as r
                 on s.Ticker_Beteckning = r.tickerToReport") %>%
    subset(select = -c(tickerToReport))
  
  obj_name <- paste0(year, "_skv_2_aggregated_for_k4")
  
  assign(obj_name, k4_c_for_fiat_or_d_for_crypto_table)
  
  objs_to_save_in_rda %<>% c(obj_name)
  
  #Creating a summary table to capture all the high-level information.
  
  summary_table <-
    cbind(
      sqldf::sqldf("
                   select sum(Profit_Vinst) as 'Profit_7.2_fiat'
                   from k4_c_for_fiat_or_d_for_crypto_table
                   where profitboxVinstruta = 7.2"),
      
      sqldf::sqldf("
                   select sum(Profit_Vinst) as 'Profit_7.5_crypto'
                   from k4_c_for_fiat_or_d_for_crypto_table
                   where profitboxVinstruta = 7.5"),
      
      sqldf::sqldf("
                   select sum(Loss_Förlust) as 'Loss_8.1_fiat'
                   from k4_c_for_fiat_or_d_for_crypto_table
                   where lossboxFörlustruta = 8.1"),
      
      sqldf::sqldf("
                   select sum(Loss_Förlust) as 'Loss_8.4_crypto'
                   from k4_c_for_fiat_or_d_for_crypto_table
                   where lossboxFörlustruta = 8.4")
      )
  
  summary_table[["Rate_to_find_deductableportionof_8.4"]] <- Rate_to_find_deductableportionof_8.4
  
  summary_table[["Deductableportionof_8.4"]] <-
    Rate_to_find_deductableportionof_8.4 * summary_table[["Loss_8.4_crypto"]]
  
  summary_table[["RealProfitorLossFiat_ÖverskottellerUnderskottFiat"]] <-
    summary_table[["Profit_7.2_fiat"]] - summary_table[["Loss_8.1_fiat"]]
  
  summary_table[["RealProfitorLossCC_ÖverskottellerUnderskottCC"]] <- 
    summary_table[["Profit_7.5_crypto"]] - summary_table[["Deductableportionof_8.4"]]
  
  summary_table[["RealProfitorLossCCMarkets_ÖverskottellerUnderskottCCMarknader"]] <- 
    summary_table[["RealProfitorLossFiat_ÖverskottellerUnderskottFiat"]] + summary_table[["RealProfitorLossCC_ÖverskottellerUnderskottCC"]]
  
  summary_table[["Taxrate_on_profits"]] <- Taxrate_on_profits
  
  summary_table[["Taxesowed"]] <- 
    ifelse(summary_table[["RealProfitorLossCCMarkets_ÖverskottellerUnderskottCCMarknader"]] > 0,
           summary_table[["RealProfitorLossCCMarkets_ÖverskottellerUnderskottCCMarknader"]] * Taxrate_on_profits,
           0)
  
  summary_table[["Absoluteprofit"]] <-
    summary_table[["Profit_7.2_fiat"]] +
    summary_table[["Profit_7.5_crypto"]] -
    summary_table[["Loss_8.1_fiat"]] -
    summary_table[["Loss_8.4_crypto"]]
  
  summary_table[["Effectivetaxrate"]] <-
    summary_table[["Taxesowed"]] / 
    summary_table[["Absoluteprofit"]]
  
  summary_table[["USD_Rate_avg_from_IRS"]] <-
    irs_exchange_rates[["rate_from_irs"]][irs_exchange_rates[["year"]] == year]
  
  summary_table[["Total Profit in USD"]] <-
    sum(summary_table[["Profit_7.2_fiat"]], summary_table[["Profit_7.5_crypto"]]) /
    summary_table[["USD_Rate_avg_from_IRS"]]
  
  summary_table[["Total Loss in USD"]] <-
    sum(summary_table[["Loss_8.1_fiat"]], summary_table[["Loss_8.4_crypto"]]) /
    summary_table[["USD_Rate_avg_from_IRS"]]
  
  summary_table[["Overall in USD"]] <-
    summary_table[["Total Profit in USD"]] -
    summary_table[["Total Loss in USD"]]
  
  #Transposing the table since it's only one row.
  summary_table %<>% t %>% as.data.frame
  
  summary_table[['Measure']] <- row.names(summary_table)
  
  row.names(summary_table) <- NULL
  
  summary_table[['Value']] <- summary_table[['V1']]
  
  summary_table %<>% .[ , c('Measure', 'Value')]
  
  obj_name <- paste0(year, "_skv_3_summary_of_profits_losses_taxes")
  
  assign(obj_name, summary_table)
  
  objs_to_save_in_rda %<>% c(obj_name)

  
  #Changing focus to producing the tables used for reporting to the IRS
  #Add columns needed for IRS filing.
  all_sales[["Description of Property"]] <-
    paste0(all_sales[["Number_Antal"]], " ", all_sales[["Ticker_Beteckning"]])
  
  all_sales[["Date Acquired"]] <- all_sales[["Date_of_purchase_for_final_unit_of_this_sale"]]
  
  all_sales[["Date Sold"]] <- all_sales[["Transaction_Datum"]]
  
  all_sales[["Proceeds (Sales Price)"]] <- all_sales[["SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp"]]
  
  all_sales[["Cost/Basis"]] <- all_sales[["Purchase_price_used_in_this_sale"]]
  
  #I do not use the exchange rate for USD/SEK for the date acquired, but rather for the date sold for both Proceeds and Sales Price. Using different exchange rates for the two will lead to inconsistencies in how numbers are reported to Skatteverket and the IRS. It makes no sense to use different rates for different years when reporting to Skatteverket because all trades are calculated with SEK as the base, and SEK:SEK rates are 1:1 every year. Thus, although SEK:USD rates fluctuate each year, SEK is always the base currency that I use for my accounting since I reside in Sweden, so I should only usd the SEK:USD rate that applies for the year of the sale.
  for (row in 1 : nrow(all_sales)) {
    all_sales[["Proceeds (Sales Price)"]][row] <- all_sales[["Proceeds (Sales Price)"]][row] /
      irs_exchange_rates[["rate_from_irs"]][irs_exchange_rates[["year"]] == year]
  }
  
  for (row in 1 : nrow(all_sales)) {
    all_sales[["Cost/Basis"]][row] <- all_sales[["Cost/Basis"]][row] /
      irs_exchange_rates[["rate_from_irs"]][irs_exchange_rates[["year"]] == year]
  }
  
  all_sales[["Gain or (Loss)"]] <- all_sales[["Proceeds (Sales Price)"]] - all_sales[["Cost/Basis"]]
  
  all_sales[["Length"]] <- all_sales[["Length_Asset_Held"]]
  
  #only keeping columns necessary for IRS
  all_sales %<>% 
    .[ , c("Description of Property",
           "Date Acquired",
           "Date Sold",
           "Proceeds (Sales Price)",
           "Cost/Basis",
           "Gain or (Loss)",
           "Length")]
  
  obj_name <- paste0(year, "_irs_1_all_sales")
  
  assign(obj_name, all_sales)
  
  objs_to_save_in_rda %<>% c(obj_name)
  
  #Make summary table
  all_sales_summary <- 
    cbind(
      sqldf::sqldf('
                   select sum("Gain or (Loss)") as "Sum of Gains"
                   from all_sales
                   where "Gain or (Loss)" > 0'),
      sqldf::sqldf('
                   select sum("Gain or (Loss)") as "Sum of Losses"
                   from all_sales
                   where "Gain or (Loss)" < 0'),
      sqldf::sqldf('
                   select sum("Gain or (Loss)") as "Total Gain or Loss"
                   from all_sales')
      )
  
  #Transposing the table since it's only one row.
  all_sales_summary %<>% t %>% as.data.frame
  
  all_sales_summary[['Measure']] <- row.names(all_sales_summary)
  
  row.names(all_sales_summary) <- NULL
  
  all_sales_summary[['Value']] <- all_sales_summary[['V1']]
  
  all_sales_summary %<>% .[ , c('Measure', 'Value')]
  
  #Save summary table
  obj_name <- paste0(year, "_irs_2_all_sales_summary")
  
  assign(obj_name, all_sales_summary)
  
  objs_to_save_in_rda %<>% c(obj_name)
  
  #Make aggregated by asset type table for it's own worksheet
  all_sales_agg <- all_sales
  
  all_sales_agg[["Description of Property"]] <-
    all_sales_agg[["Description of Property"]] %>%
    gsub(pattern = "[^A-Z]",
         replacement = "",
         x = .)
  
  all_sales_agg <-
    sqldf::sqldf('select
                 "Description of Property" as Asset,
                 sum("Gain or (Loss)") as "Total Gain or Loss"
                 from all_sales_agg
                 group by "Description of Property"')
  
  #Save the aggregated table
  
  obj_name <- paste0(year, "_irs_3_sales_agg_by_asset")
  
  assign(obj_name, all_sales_agg)
  
  objs_to_save_in_rda %<>% c(obj_name)
  
  #Make summary of the aggregated table
  all_sales_agg_summary <- 
    cbind(
      sqldf::sqldf('
                   select sum("Total Gain or Loss") as "Sum of profitable assets"
                   from all_sales_agg
                   where "Total Gain or Loss" > 0'),
      sqldf::sqldf('
                   select sum("Total Gain or Loss") as "Sum of loss assets"
               from all_sales_agg
               where "Total Gain or Loss" < 0'),
      sqldf::sqldf('
                   select sum("Total Gain or Loss") as "Total Gain or Loss"
               from all_sales_agg')
    )
  
  
  #Transposing the table since it's only one row.
  all_sales_agg_summary %<>% t %>% as.data.frame
  
  all_sales_agg_summary[['Measure']] <- row.names(all_sales_agg_summary)
  
  row.names(all_sales_agg_summary) <- NULL
  
  all_sales_agg_summary[['Value']] <- all_sales_agg_summary[['V1']]
  
  all_sales_agg_summary %<>% .[ , c('Measure', 'Value')]
  
  #Save summary table
  obj_name <- paste0(year, "_irs_4_sales_agg_summary")
  
  assign(obj_name, all_sales_agg_summary)
  
  objs_to_save_in_rda %<>% c(obj_name)
  
}

save(list = eval(objs_to_save_in_rda), file = "outputs/09_tax_reporting.rda")
