# Load requirements
requirements_df <-
  data.frame(
    required_rda_files =
      c("outputs/02_dates_of_interest.rda",
        "outputs/07_all_trades_all_years.rda"),
    
    source_scripts =
      c("02_dates_of_interest.r",
        "07_all_trades_all_years.r"),
    
    stringsAsFactors = F)

load_source_data()

# 02 Ensure all pkgs in this script are installed ####
pkgs <-
    c('flifo')

activatePkgs(pkgs)

#Read in all exchange data
fifo_all <- all_trades_all_years


#Add columns I will populate
fifo_all[["TotalSEKspent_Totaltoutnyttjatomkostnadsbeloppförkvarvarandevärdepapper"]] <- NA
fifo_all[["AverageSEKperunit_Genomsnittligtomkostnadsbeloppförkvarvarandevärdepapper"]] <- NA
fifo_all[["Purchase_price_used_in_this_sale"]] <- NA
fifo_all[["Date_of_purchase_for_this_sale_if_all_short_term"]] <- lubridate::as_datetime(NA)
fifo_all[["Number_bought_up_to_this_date"]] <- NA
fifo_all[["Number_sold_up_to_this_date"]] <- NA
fifo_all[["Date_of_purchase_for_final_unit_of_this_sale"]] <- lubridate::as_datetime(NA)
fifo_all[["Length_Asset_Held"]] <- NA

#Sort the assets alphabetically for looping later
asset_types <- fifo_all[["Ticker_Beteckning"]] %>% unique %>% sort

for (asset in asset_types) {
  
  #set a counter for printing
  ifelse(exists("i"),
         i <- i + 1,
         i <- 1)
  
  message <- paste0(asset, ' is asset ', i, ' of ', length(asset_types), '.\n\n')
  
  #Parse out assets into their own dfs.
  df_for_asset <- fifo_all[fifo_all[["Ticker_Beteckning"]] == asset, ]

  datum_stack <- flifo::fifo()
  number_bought_after_this_transaction <- 0
  number_sold_after_this_transaction <- 0
  
  for (row in 1 : nrow(df_for_asset)) {
    
    cat(paste0(message, 'Row is ', row, ' of ', nrow(df_for_asset), ' rows.\n\n'))
    
    data_subset <-
      df_for_asset[(df_for_asset[["Rownumber"]] <= df_for_asset[["Rownumber"]][row]), ]
    
    #Lots of recursion needed. Base case is the first "Buy" action.
    ifelse(
      df_for_asset[["Action_Händelse"]][row] == "Buy",
      {
        #Add to the stack for FIFO.
        flifo::push(datum_stack, df_for_asset[["Transaction_Datum"]][row])
        
        #Populate the columns in the base case
        number_bought_after_this_transaction <-
          number_bought_after_this_transaction + df_for_asset[["Number_Antal"]][row]
        
        df_for_asset[["Number_bought_up_to_this_date"]][row] <-
          number_bought_after_this_transaction
        
        total_sekspent <-
          sum(
            data_subset[["SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp"]][
              data_subset[["Action_Händelse"]] == "Buy"], na.rm = T) -
          sum(
            data_subset[["Purchase_price_used_in_this_sale"]][
              data_subset[["Action_Händelse"]] == "Sell"], na.rm = T)
        
        df_for_asset[["TotalSEKspent_Totaltoutnyttjatomkostnadsbeloppförkvarvarandevärdepapper"]][row] <-
          total_sekspent
        
        df_for_asset[["AverageSEKperunit_Genomsnittligtomkostnadsbeloppförkvarvarandevärdepapper"]][row] <-
          df_for_asset[["TotalSEKspent_Totaltoutnyttjatomkostnadsbeloppförkvarvarandevärdepapper"]][row]/
          df_for_asset[["Totalnumber_Totalantal"]][row]
      },
      {
        #Populate the columns somewhat differently if "Sell" action
        number_sold_after_this_transaction <-
          number_sold_after_this_transaction + df_for_asset[["Number_Antal"]][row]
        
        df_for_asset[["Number_sold_up_to_this_date"]][row] <-
          number_sold_after_this_transaction
        
        data_subset <- data_subset[data_subset[["Rownumber"]] < df_for_asset[["Rownumber"]][row], ]
        
        df_for_asset[["AverageSEKperunit_Genomsnittligtomkostnadsbeloppförkvarvarandevärdepapper"]][row] <-
          df_for_asset[["AverageSEKperunit_Genomsnittligtomkostnadsbeloppförkvarvarandevärdepapper"]][
            df_for_asset[["Rownumber"]] == max(data_subset[["Rownumber"]])]
        
        total_sekspent <-
          df_for_asset[["TotalSEKspent_Totaltoutnyttjatomkostnadsbeloppförkvarvarandevärdepapper"]][
            df_for_asset[["Rownumber"]] == max(data_subset[["Rownumber"]])] -
          df_for_asset[["Number_Antal"]][row] *
          df_for_asset[["AverageSEKperunit_Genomsnittligtomkostnadsbeloppförkvarvarandevärdepapper"]][row]
        
        df_for_asset[["TotalSEKspent_Totaltoutnyttjatomkostnadsbeloppförkvarvarandevärdepapper"]][row] <- 
          total_sekspent
        
        df_for_asset[["Purchase_price_used_in_this_sale"]][row] <-
          df_for_asset[["Number_Antal"]][row] *
          df_for_asset[["AverageSEKperunit_Genomsnittligtomkostnadsbeloppförkvarvarandevärdepapper"]][row]
        
        #Deeper_subset and column for "Date_of_purchase_for_this_sale_if_all_short_term" were used in 2018 since I presumed it would be easier and very few of my sales would be long-term positions.
        deeper_subset <-
          data_subset[data_subset[["Action_Händelse"]] == "Buy", ]

        df_for_asset[["Date_of_purchase_for_this_sale_if_all_short_term"]] <-
          df_for_asset[["Transaction_Datum"]][
            df_for_asset[["Rownumber"]] == max(deeper_subset[["Rownumber"]])]
        
        #Referring to the stack created earlier for proper FIFO accounting
        ref_date <- as.list(datum_stack)[[1]]
        
        bought_up_to_ref_date <-
          sum(
            df_for_asset[["Number_bought_up_to_this_date"]][
              (df_for_asset[["Transaction_Datum"]] == ref_date) &
                (df_for_asset[["Action_Händelse"]] == "Buy")],
            na.rm = T)
        
        #If the number sold > bought_up_to_date, then need to refer to the next purchase date.
        while(number_sold_after_this_transaction > bought_up_to_ref_date) {
          cat(paste0("number_sold > bought_up_to_ref_date. Popping stack.\n\n"))
          backup_ref_date <- as.list(datum_stack)[[1]]
          
          #There may be edge cases where you pop the final date from the stack but you still supposedly sold more than you had. When examined, these are always fractions of fractions of cryptocurrencies and are likely just cryptodust accounting errors and can be safely treated as worthless. The tryCatch loop below should account for these edge cases.
          tryCatch(
            {
              flifo::pop(datum_stack)
              ref_date <<- as.list(datum_stack)[[1]]
              
              bought_up_to_ref_date <<-
                sum(
                  df_for_asset[["Number_bought_up_to_this_date"]][
                    (df_for_asset[["Transaction_Datum"]] == ref_date) &
                      (df_for_asset[["Action_Händelse"]] == "Buy")],
                  na.rm = T)
            },
            error = function(err) {
              cat(paste0(err, "\nPresume this was due to crypto dust."))
              
              ref_date <<- backup_ref_date
              
              bought_up_to_ref_date <<- number_sold_after_this_transaction
            },
            finally = NA
                  )
        }
        
        df_for_asset[["Date_of_purchase_for_final_unit_of_this_sale"]][row] <-
          ref_date
      }
    )
    
    #Creating the column indicating of positions were long or short.
    df_for_asset[["Length_Asset_Held"]][row] <-
      ifelse(
        (df_for_asset[["Transaction_Datum"]][row] -
           df_for_asset[["Date_of_purchase_for_final_unit_of_this_sale"]][row]) < 365.25,
        "Short",
        "Long")
    
    #cleaning up namespace
    if(row == nrow(df_for_asset)) {
      rm(row, total_sekspent, data_subset, deeper_subset, backup_ref_date, bought_up_to_ref_date, ref_date)
    }
    
  }
  assign(paste0("df_for_", asset), df_for_asset)
  
  #cleaning up namespace
  if(asset == asset_types[length(asset_types)]) {
    rm(i, asset, message, df_for_asset, datum_stack, number_bought_after_this_transaction, number_sold_after_this_transaction)
  }
  
}

#Consolidating the dfs back together
for(asset in asset_types) {

  ifelse(exists("temp_df"),
         temp_df %<>% rbind(get(paste0("df_for_", asset))),
         {
           temp_df <- get(paste0("df_for_", asset))
         })
  
  rm(list = paste0("df_for_", asset))

  #saving to my df and cleaning up namespace
  if (asset == asset_types[length(asset_types)]) {
    fifo_all <- temp_df
    rm(asset, temp_df, asset_types)
  }
}


#Sorting them by date
fifo_all %<>% .[order(.[["Rownumber"]]), ]

#Cleaning up crypto dust. These instances should always be fractions of fractions of assets and thus should be treated as worthless.
fifo_all[["Totalnumber_Totalantal"]][fifo_all[["Totalnumber_Totalantal"]] < 0] <- 0

#Clearing up the underscores since no longer need to match to my reference tables.
fifo_all[["Ticker_Beteckning"]] %<>% gsub(pattern = "_", replacement = "", x =.)



#Subsetting the fifo data by years
objs_to_save_in_rda <- c("fifo_all")

for (year in earliest_year : (filing_year - 1)) {
  print(year)
  create_two_fifo_subsets(fifo_all, year, year + 1)
  objs_to_save_in_rda %<>% c(paste0("fifo_", year))
  
  if (year == (filing_year - 1)) {
    objs_to_save_in_rda %<>% c(paste0("fifo_", year + 1))
  }
}


save(list = eval(objs_to_save_in_rda), file = 'outputs/08_fifo.rda')
