# Load requirements
requirements_df <-
  data.frame(
    required_rda_files =
      c("outputs/06_all_exchange_rates.rda"),
    
    source_scripts =
      c("06_all_exchange_rates.r"),
    
    stringsAsFactors = F)

load_source_data()

# 02 Ensure all pkgs in this script are installed ####
pkgs <-
    c('svDialogs')

activatePkgs(pkgs)

#Load deposits data
bitfinex_deposits <-
  as.data.frame(readr::read_csv("inputs/07_all_trades_all_years/bitfinex_deposits.csv"))

bitfinex_deposits %<>% .[!(duplicated(.)), ]


#Add rows to df that were not included in the downloaded data

df_of_missing_rows_from_Bf_records <- #create df structure
  bitfinex_deposits[NULL, ]

original_BCH_distribution <- #Create the row
  list("No_Number",
       "17-08-02 00:58:18.000",
       "BCH",
       "COMPLETED",
       6.14457768,
       0,
       "BTC_to_BCH_split",
       "No_Transaction_ID_given")

missing_rows <- #Make list of all missing rows
  list(original_BCH_distribution)

for (i in 1 : length(missing_rows)){ #Create df from rows
  df_of_missing_rows_from_Bf_records[i ,] <-
    missing_rows[[i]]
}

bitfinex_deposits %<>%
  rbind(., df_of_missing_rows_from_Bf_records)


#Resuming after adding the missing rows
bitfinex_deposits$Type <- "Deposit_to_bitfinex"

bitfinex_withdrawals <-
  as.data.frame(readr::read_csv("inputs/07_all_trades_all_years/bitfinex_withdrawals.csv"))

bitfinex_withdrawals %<>% .[!(duplicated(.)), ]

bitfinex_withdrawals$Type <- "Withdrawal_from_bitfinex"

bitfinex_depos_and_withdrawals <-
  rbind(bitfinex_deposits, bitfinex_withdrawals)

rm(bitfinex_deposits, bitfinex_withdrawals)

bitfinex_depos_and_withdrawals %<>% .[.$STATUS == "COMPLETED" , ]

bitfinex_dw_column_order <- #save column order for later
  bitfinex_depos_and_withdrawals %>% colnames

bitfinex_depos_and_withdrawals$DATE %<>% #convert time stamp
  strptime("%y-%m-%d %H:%M:%S") %>%
  strftime("%Y-%m-%d %H:%M:%S")

bitfinex_depos_and_withdrawals$Date <-
  bitfinex_depos_and_withdrawals$DATE %>%
  as.Date %>%
  lubridate::ymd(.)

bitfinex_dw_column_order %<>% c(., "Date")

bitfinex_depos_and_withdrawals %<>% #add column for matching ccs next
  merge(x = .,
        y = my_bitfinex_ccs[ , c("bitfinexTicker", "fourLetterTicker")],
        by.x = "CURRENCY",
        by.y = "bitfinexTicker",
        all.x = T)

bitfinex_dw_column_order %<>% c(., "fourLetterTicker")

refTable <- sek__all_rates #pull in reference table for looking up exchanges

names(refTable) %<>% #modify names so can be matched
  gsub(pattern = "SEK_", replacement = "", x = .)

row.names(refTable) <- refTable$Date #assign Date to row names for easy indexing

unique_ccs_in_bitfinex <- #create vector for matching
  unique(bitfinex_depos_and_withdrawals$fourLetterTicker) %>%
  sort

ccs_in_bitfinex_not_matched_in_ref_table <-
  unique_ccs_in_bitfinex[unique_ccs_in_bitfinex %notIn% names(refTable)] %>%
  sort


#Simple test to make sure all of your CCs are accounted for
if(length(ccs_in_bitfinex_not_matched_in_ref_table) == 0) {
  dlgMessage("All CCs accounted for. You should proceed.")
} else {
  dlgMessage("All CCs are NOT accounted for; check the fourLetterTickers.")
  stop()
}

#Simple test to see if any missing fourLetterTickers
if(!any(is.na(bitfinex_depos_and_withdrawals$CURRENCY))) {
  dlgMessage("No missing, you should proceed.")
} else {
  dlgMessage("Some fourLetterTicker data are missing; check them and fix before moving on.")
  stop()
}

bitfinex_depos_and_withdrawals$Date_as_character <- #create column for matching to refTable row.names
  bitfinex_depos_and_withdrawals$Date %>% 
  as.character

bitfinex_dw_column_order %<>% c(., "Date_as_character")

bitfinex_depos_and_withdrawals$SEK_rate <- #lookup values in refTable. Cannot use "$Date" since cbind converts "date" classes and the lookup will fail.
  refTable[cbind(
    bitfinex_depos_and_withdrawals$Date_as_character,
    bitfinex_depos_and_withdrawals$fourLetterTicker)] %>%
  as.numeric

bitfinex_dw_column_order %<>% c(., "SEK_rate")

bitfinex_depos_and_withdrawals$SEK_costoftransaction <- #The Gatehub formats differ from Kraken and bitfinex; if I paid SEK to deposit EUR into an account, the SEK_cost must be postive. If I withdrew EUR to receive SEK, the SEK_cost should be negative (negative cost = gain in SEK).
  bitfinex_depos_and_withdrawals$AMOUNT * bitfinex_depos_and_withdrawals$SEK_rate 

bitfinex_dw_column_order %<>% c(., "SEK_costoftransaction")


##Interrupting workflow to fix inaccurate costs from deposits and withdrawals

bitfinex_depos_and_withdrawals %<>% #bring in columns from df
  merge(x = .,
        y = bitfinex_deps_wtdrws_trnsfrs[ , c("TRANSACTION.ID", "Actual_SEK_from_Handelsbanken_records", "Bitfinex_deposit_withdrawal_transfer_note")],
        by.x = c("TRANSACTION ID"),
        by.y = c("TRANSACTION.ID"),
        all = T)

bitfinex_dw_column_order %<>%
  c(.,
    "Actual_SEK_from_Handelsbanken_records",
    "Bitfinex_deposit_withdrawal_transfer_note")

for (i in 1 : nrow(bitfinex_depos_and_withdrawals)) { #update the SEK_costoftransaction
  if (is.na(bitfinex_depos_and_withdrawals$Actual_SEK_from_Handelsbanken_records[i])) {
    next
  } else {
    bitfinex_depos_and_withdrawals$SEK_costoftransaction[i] <-
      bitfinex_depos_and_withdrawals$Actual_SEK_from_Handelsbanken_records[i]
  }
}


#Resuming workflow after fixing inaccurate costs.

bitfinex_depos_and_withdrawals$SEK_costoffee <- #The bitfinex fees differ from Kraken. They show up as negative numbers in the raw data, but should be tabulated as a positive SEK_costoffee since a positive number means it cost me SEK.
  bitfinex_depos_and_withdrawals$FEES * bitfinex_depos_and_withdrawals$SEK_rate * -1

bitfinex_dw_column_order %<>% c(., "SEK_costoffee")

#bitfinex, unlike Gatehub and Kraken, does not give updated balances on the deposits/withdrawals .csv.

bitfinex_depos_and_withdrawals %<>% #Bring in SEK_USD_ rates
  merge(x = .,
        y = usd__all_rates[ , c("Date", "SEK_USD_")],
        all.x = T)

bitfinex_dw_column_order %<>% c(., "SEK_USD_")

bitfinex_depos_and_withdrawals$USD_rate <-
  bitfinex_depos_and_withdrawals$SEK_rate * bitfinex_depos_and_withdrawals$SEK_USD_

bitfinex_dw_column_order %<>% c(., "USD_rate")

bitfinex_depos_and_withdrawals$USD_costoftransaction <-
  bitfinex_depos_and_withdrawals$SEK_costoftransaction * bitfinex_depos_and_withdrawals$SEK_USD_

bitfinex_dw_column_order %<>% c(., "USD_costoftransaction")

bitfinex_depos_and_withdrawals$USD_costoffee <- #The bitfinex fees differ from Kraken. They show up as negative numbers in the raw data, but should be tabulated as a positive SEK_costoffee since a positive number means it cost me SEK.
  bitfinex_depos_and_withdrawals$FEES * bitfinex_depos_and_withdrawals$USD_rate * -1

bitfinex_dw_column_order %<>% c(., "USD_costoffee")

#bitfinex, unlike Gatehub and Kraken, does not give updated balances on the deposits/withdrawals .csv.

bitfinex_depos_and_withdrawals$Date_time <-
  bitfinex_depos_and_withdrawals$DATE %>%
  lubridate::as_datetime(.)

bitfinex_dw_column_order %<>% c(., "Date_time")

bitfinex_depos_and_withdrawals %<>% #Reorder column to match source 
  .[ , bitfinex_dw_column_order]



#Read in the bitfinex trades data
bitfinex_trades <-
  as.data.frame(readr::read_csv("inputs/07_all_trades_all_years/bitfinex_trades.csv"))

#interrupting workflow to fix inaccurate trades from Bitfinex.

#According to the Bitfinex records I somehow sold more BTG and OMG than I ever had. Their records are inaccurate for these two. To adjust, I added the amounts of missing crypto to the first time I acquired the assets

bitfinex_trades[["AMOUNT"]][bitfinex_trades[["#"]] == 84830405] <-
  bitfinex_trades[["AMOUNT"]][bitfinex_trades[["#"]] == 84830405] + 0.3147

bitfinex_trades[["AMOUNT"]][bitfinex_trades[["#"]] == 66227715] <-
  bitfinex_trades[["AMOUNT"]][bitfinex_trades[["#"]] == 84830405] + 38.38596

#Resuming the workflow after fixing those inaccurate costs.

bitfinex_trades_column_order <- #save column order for later
  bitfinex_trades %>% colnames

bitfinex_trades %<>% .[!(duplicated(.)), ]

bitfinex_trades$DATE %<>% #convert time stamp
  strptime("%y-%m-%d %H:%M:%S") %>%
  strftime("%Y-%m-%d %H:%M:%S")

bitfinex_trades$Date <-
  bitfinex_trades$DATE %>%
  as.Date %>%
  lubridate::ymd(.)

bitfinex_trades_column_order %<>% c(., "Date")

bitfinex_trades %<>% #This part is unique to bitfinex. I must split the pairs column.
  tidyr::separate(PAIR,
                  sep = "/",
                  into = c("start_if_neg",
                           "start_if_pos"),
                  remove = F)

bitfinex_trades_column_order %<>% c(., "start_if_neg", "start_if_pos")

bitfinex_trades %<>% #create columns to indicate starting and ending currencies and changes in amounts
  mutate(Start =
                  ifelse(AMOUNT < 0,
                         start_if_neg,
                         start_if_pos),
                End =
                  ifelse(Start == start_if_neg,
                         start_if_pos,
                         start_if_neg),
                Change_in_Start =
                  ifelse(AMOUNT > 0,
                         AMOUNT * PRICE * -1,
                         AMOUNT),
                Change_in_End =
                  ifelse(AMOUNT < 0,
                         AMOUNT * PRICE * -1,
                         AMOUNT)) %<>% #Sorts by DateTime
  arrange(DATE)

bitfinex_trades_column_order %<>% c(., "Start", "End", "Change_in_Start", "Change_in_End")

bitfinex_trades %<>% #add columns for matching ccs next
  merge(x = .,
        y = my_bitfinex_ccs[ , c("bitfinexTicker", "fourLetterTicker")],
        by.x = "Start",
        by.y = "bitfinexTicker") %>%
  rename(Start_fourLetterTicker = fourLetterTicker) %>%
  merge(x = .,
        y = my_bitfinex_ccs[ , c("bitfinexTicker", "fourLetterTicker")],
        by.x = "End",
        by.y = "bitfinexTicker") %>%
  rename(End_fourLetterTicker = fourLetterTicker) %>%
  merge(x = .,
        y = my_bitfinex_ccs[ , c("bitfinexTicker", "fourLetterTicker")],
        by.x = "FEE CURRENCY",
        by.y = "bitfinexTicker") %>%
  rename(Fee_fourLetterTicker = fourLetterTicker)

bitfinex_trades_column_order %<>% c(., "Start_fourLetterTicker", "End_fourLetterTicker", "Fee_fourLetterTicker")

refTable <- sek__all_rates #pull in reference table for looking up exchanges

names(refTable) %<>% #modify names so can be matched
  gsub(pattern = "SEK_", replacement = "", x = .)

row.names(refTable) <- refTable$Date #assign Date to row names for easy indexing

unique_ccs_in_bitfinex <- #create vector for matching
  unique(c(bitfinex_trades$Start_fourLetterTicker, bitfinex_trades$End_fourLetterTicker, bitfinex_trades$Fee_fourLetterTicker)) %>%
  sort

ccs_in_bitfinex_not_matched_in_ref_table <-
  unique_ccs_in_bitfinex[unique_ccs_in_bitfinex %notIn% names(refTable)] %>%
  sort

#Simple test to make sure all of your CCs are accounted for
if(length(ccs_in_bitfinex_not_matched_in_ref_table) == 0) {
  dlgMessage("All CCs accounted for. You should proceed.")
} else {
  dlgMessage("All CCs are NOT accounted for; check the fourLetterTickers.")
  stop()
}

#Simple test to see if any missing fourLetterTickers
if(!any(is.na(bitfinex_trades$Start), is.na(bitfinex_trades$End))) {
  dlgMessage("No missing, you should proceed.")
} else {
  dlgMessage("Some fourLetterTicker data are missing; check them and fix before moving on.")
  stop()
}

bitfinex_trades$Date_as_character <- #create column for matching to refTable row.names
  bitfinex_trades$Date %>% 
  as.character

bitfinex_trades_column_order %<>% c(., "Date_as_character")

bitfinex_trades$Start_SEK_rate <- #lookup values in refTable. Cannot use "$Date" since cbind converts "date" classes and the lookup will fail.
  refTable[cbind(
    bitfinex_trades$Date_as_character,
    bitfinex_trades$Start_fourLetterTicker)] %>%
  as.numeric

bitfinex_trades_column_order %<>% c(., "Start_SEK_rate")

bitfinex_trades$End_SEK_rate <- #lookup values in refTable. Cannot use "$Date" since cbind converts "date" classes and the lookup will fail.
  refTable[cbind(
    bitfinex_trades$Date_as_character,
    bitfinex_trades$End_fourLetterTicker)] %>%
  as.numeric

bitfinex_trades_column_order %<>% c(., "End_SEK_rate")

bitfinex_trades$Fee_SEK_rate <- #lookup values in refTable. Cannot use "$Date" since cbind converts "date" classes and the lookup will fail.
  refTable[cbind(
    bitfinex_trades$Date_as_character,
    bitfinex_trades$Fee_fourLetterTicker)] %>%
  as.numeric

bitfinex_trades_column_order %<>% c(., "Fee_SEK_rate")

bitfinex_trades$Start_SEK_costoftransaction <- 
  bitfinex_trades$Change_in_Start * bitfinex_trades$Start_SEK_rate 

bitfinex_trades$End_SEK_costoftransaction <- #The Gatehub formats differ from Kraken and bitfinex; if I paid SEK to deposit EUR into an account, the SEK_cost must be postive. If I withdrew EUR to receive SEK, the SEK_cost should be negative (negative cost = gain in SEK).
  bitfinex_trades$Change_in_End * bitfinex_trades$End_SEK_rate 

bitfinex_trades$Fee_SEK_costoftransaction <- #The bitfinex fees differ from Kraken. They show up as negative numbers in the raw data, but should be tabulated as a positive SEK_costoffee since a positive number means it cost me SEK.
  bitfinex_trades$FEE * bitfinex_trades$Fee_SEK_rate * -1

bitfinex_trades_column_order %<>% c(., "Start_SEK_costoftransaction", "End_SEK_costoftransaction", "Fee_SEK_costoftransaction")

#bitfinex, unlike Gatehub and Kraken, does not give updated balances on the deposits/withdrawals .csv.

bitfinex_trades %<>% #Bring in SEK_USD_ rates
  merge(x = .,
        y = usd__all_rates[ , c("Date", "SEK_USD_")],
        all.x = T)

bitfinex_trades_column_order %<>% c(., "SEK_USD_")

bitfinex_trades$Start_USD_rate <-
  bitfinex_trades$Start_SEK_rate * bitfinex_trades$SEK_USD_

bitfinex_trades$End_USD_rate <-
  bitfinex_trades$End_SEK_rate * bitfinex_trades$SEK_USD_

bitfinex_trades$Fee_USD_rate <-
  bitfinex_trades$Fee_SEK_rate * bitfinex_trades$SEK_USD_

bitfinex_trades_column_order %<>% c(., "Start_USD_rate", "End_USD_rate", "Fee_USD_rate")

bitfinex_trades$Start_USD_costoftransaction <-
  bitfinex_trades$Start_SEK_costoftransaction * bitfinex_trades$SEK_USD_

bitfinex_trades$End_USD_costoftransaction <-
  bitfinex_trades$End_SEK_costoftransaction * bitfinex_trades$SEK_USD_

bitfinex_trades$Fee_USD_costoftransaction <-
  bitfinex_trades$Fee_SEK_costoftransaction * bitfinex_trades$SEK_USD_

bitfinex_trades_column_order %<>% c(., "Start_USD_costoftransaction", "End_USD_costoftransaction", "Fee_USD_costoftransaction")

#bitfinex, unlike Gatehub and Kraken, does not give updated balances on the deposits/withdrawals .csv.

bitfinex_trades$Date_time <-
  bitfinex_trades$DATE %>%
  lubridate::as_datetime(.)

bitfinex_trades_column_order %<>% c(., "Date_time")

bitfinex_trades %<>% #Reorder column to match source 
  .[ , bitfinex_trades_column_order]


#Read in gatehub data
gatehub_balance_changes <-
  as.data.frame(readr::read_csv("inputs/07_all_trades_all_years/gatehub_2016-12-31_2020-01-01.csv"))

gatehub_column_order <- #save column order for later
  gatehub_balance_changes %>% colnames

gatehub_balance_changes %<>% .[!(duplicated(.)), ]

#Interrupting workflow to add rows to df that were not given in the raw data

df_of_missing_rows_from_Gh_records <- #create df structure
  gatehub_balance_changes[NULL, ]

original_XRP_signup_bonus <- #Create the row
  list("Jan 14, 2017, 12:00",
       NA,
       "payment",
       29.999865,
       "XRP",
       NA,
       NA,
       29.999865)

missing_rows <- #Make list of all missing rows
  list(original_XRP_signup_bonus)

for (i in 1 : length(missing_rows)){ #Create df from rows
  df_of_missing_rows_from_Gh_records[i ,] <-
    missing_rows[[i]]
}

gatehub_balance_changes %<>%
  rbind(., df_of_missing_rows_from_Gh_records)

#Formatting columns
gatehub_balance_changes[["Time"]] %<>% #convert time stamp
  strptime("%b %d, %Y, %H:%M") %>%
  strftime("%Y-%m-%d %H:%M:%S")

gatehub_balance_changes[["Date"]] <- #create date column
  gatehub_balance_changes[["Time"]] %>%
  as.Date %>%
  lubridate::ymd(.)

gatehub_column_order %<>% c(., "Date")

#Pulling in data for CCs from this exchange
gatehub_balance_changes %<>% #add column for matching ccs next
  merge(x = .,
        y = my_gatehub_ccs[ , c("gatehubTicker", "fourLetterTicker")],
        by.x = "Currency",
        by.y = "gatehubTicker",
        all.x = T)

gatehub_column_order %<>% c(., "fourLetterTicker")

refTable <- sek__all_rates #pull in reference table for looking up exchanges

names(refTable) %<>% #modify names so can be matched
  gsub(pattern = "SEK_", replacement = "", x = .)

row.names(refTable) <- refTable[["Date"]] #assign Date to row names for easy indexing

unique_ccs_in_gh <- #create vector for matching
  unique(gatehub_balance_changes[["fourLetterTicker"]]) %>%
  sort

ccs_in_gh_not_matched_in_ref_table <-
  unique_ccs_in_gh[unique_ccs_in_gh %notIn% names(refTable)] %>%
  sort

#Simple test to make sure all of your CCs are accounted for
if(length(ccs_in_gh_not_matched_in_ref_table) == 0) {
  dlgMessage("All CCs accounted for. You should proceed.")
} else {
  dlgMessage("All CCs are NOT accounted for; check the fourLetterTickers.")
  stop()
}


#Simple test to see if any missing fourLetterTickers
if(!any(is.na(gatehub_balance_changes$Currency))) {
  dlgMessage("No missing, you should proceed.")
} else {
  dlgMessage("Some fourLetterTicker data are missing; check them and fix before moving on.")
  stop()
}

#If the tests above were successful, proceed with formatting, etc.
gatehub_balance_changes[["Date_as_character"]] <- #create column for matching to refTable row.names
  gatehub_balance_changes[["Date"]] %>% 
  as.character

gatehub_column_order %<>% c(., "Date_as_character")

gatehub_balance_changes[["SEK_rate"]] <- #lookup values in refTable. Cannot use "$Date" since cbind converts "date" classes and the lookup will fail.
  refTable[cbind(
    gatehub_balance_changes[["Date_as_character"]],
    gatehub_balance_changes[["fourLetterTicker"]])] %>%
  as.numeric

gatehub_column_order %<>% c(., "SEK_rate")

gatehub_balance_changes[["Date_time"]] <-
  gatehub_balance_changes[["Time"]] %>%
  lubridate::as_datetime(.)

gatehub_column_order %<>% c(., "Date_time")

##
##Gatehub Amount column was not usable in previous reports from Gatehub- it gave me 700,000 more XRP than it should! Thus, I calculated the amount myself (Davids_calculated_amount). However, I pointed the error out to them after 2018 and they seemed to have fixed it, so I now have a usable Amount column but have kept the code block before to have a comparison.
##

ccs_in_gatehub <- gatehub_balance_changes[["fourLetterTicker"]] %>% unique

for (i in 1 : length(ccs_in_gatehub)) {
  if (i == 1) {
    final_df <- data.frame()
  }
  ith_cc_df <-
    sqldf::sqldf(paste0('
                        select *
                        from gatehub_balance_changes
                        where fourLetterTicker = "', ccs_in_gatehub[i], '"
                        '))
  
  ith_cc_df %<>% arrange(Date_time)
  ith_cc_df[["Davids_calculated_amount"]] <- NA
  ith_cc_df[["New_balance"]] <- NA
  for (j in 1 : nrow(ith_cc_df)) {
    if (j == 1) {
      ith_cc_df[["Davids_calculated_amount"]][j] <-
        ith_cc_df[["Balance"]][j]
      ith_cc_df[["New_balance"]][j] <-
        ith_cc_df[["Balance"]][j]
    } else {
      ith_cc_df[["Davids_calculated_amount"]][j] <-
        ith_cc_df[["Balance"]][j] - ith_cc_df[["Balance"]][j - 1]
    }
  }
  
  ith_cc_df[["New_balance"]] <-
    cumsum(ith_cc_df[["Davids_calculated_amount"]])
  final_df %<>%
    rbind(., ith_cc_df) %<>%
    arrange(Date_time)
}

gatehub_balance_changes <- final_df

gatehub_column_order %<>% c(., "Davids_calculated_amount")

#The Gatehub formats differ from Kraken and Bitfinex; if I paid SEK to deposit EUR into an account, the SEK_cost must be postive. If I withdrew EUR to receive SEK, the SEK_cost should be negative (negative cost = gain in SEK). Stupidly, Gatehub shows the amount differently based on the "type". If it is a "ripple_network_fee" type, the amount is negative, thus SEKcostoftransaction must be positive and the fee must be multiplied by -1. For other types, it must not be multiplied by -1 since it represents a purchase when positive (cost SEK) and a sale when negative (earned SEK = -SEK cost).
gatehub_balance_changes[["SEK_costoftransaction"]] <-
  ifelse(gatehub_balance_changes[["Type"]] == "ripple_network_fee",
         gatehub_balance_changes[["Amount"]] * gatehub_balance_changes[["SEK_rate"]] * -1,
         gatehub_balance_changes[["Amount"]] * gatehub_balance_changes[["SEK_rate"]])

gatehub_column_order %<>% c(., "SEK_costoftransaction")


##Interrupting workflow to fix inaccurate costs associated with deposits and withdrawals.

gatehub_balance_changes <- #bring in columns from df
  merge(x = gatehub_balance_changes,
        y = gatehub_deps_wtdrws_trnsfrs[ , c("TX.hash", "Type", "Actual_SEK_from_Handelsbanken_records", "Gatehub_deposit_withdrawal_transfer_note")],
        by.x = c("TX hash", "Type"),
        by.y = c("TX.hash", "Type"),
        all = T)

gatehub_column_order %<>%
  c(.,
    "Actual_SEK_from_Handelsbanken_records",
    "Gatehub_deposit_withdrawal_transfer_note")

for (i in 1 : nrow(gatehub_balance_changes)) { #update the SEK_costoftransaction
  if (is.na(gatehub_balance_changes[["Actual_SEK_from_Handelsbanken_records"]][i])) {
    next
  } else {
    gatehub_balance_changes[["SEK_costoftransaction"]][i] <-
      gatehub_balance_changes[["Actual_SEK_from_Handelsbanken_records"]][i]
  }
}


#After fixing the inaccurate costs associated with deposits and withdrawals, will calculate the SEK value and USD value of the remaining balance.
gatehub_balance_changes[["SEK_Value_of_Balance"]] <- 
  gatehub_balance_changes[["Balance"]] * gatehub_balance_changes[["SEK_rate"]]

gatehub_column_order %<>% c(., "SEK_Value_of_Balance")

gatehub_balance_changes %<>% #Bring in SEK_USD_ rates
  merge(x = .,
        y = usd__all_rates[ , c("Date", "SEK_USD_")],
        all.x = T)

gatehub_column_order %<>% c(., "SEK_USD_")

gatehub_balance_changes[["USD_rate"]] <-
  gatehub_balance_changes[["SEK_rate"]] * gatehub_balance_changes[["SEK_USD_"]]

gatehub_column_order %<>% c(., "USD_rate")

gatehub_balance_changes[["USD_costoftransaction"]] <-
  gatehub_balance_changes[["SEK_costoftransaction"]] * gatehub_balance_changes[["SEK_USD_"]]

gatehub_column_order %<>% c(., "USD_costoftransaction")

gatehub_balance_changes[["USD_Value_of_Balance"]] <- 
  gatehub_balance_changes[["SEK_Value_of_Balance"]] * gatehub_balance_changes[["SEK_USD_"]]

gatehub_column_order %<>% c(., "USD_Value_of_Balance")

gatehub_balance_changes %<>% #Reorder column to match source 
  .[ , gatehub_column_order]

gatehub_balance_changes %<>% arrange(Date_time)




#Read in the transferwise data
transferwise_transfers <-
  as.data.frame(readr::read_csv("inputs/07_all_trades_all_years/transferwise_transfers.csv"))

transferwise_column_order <- #save column order for later
  transferwise_transfers %>% colnames

transferwise_transfers %<>% .[!(duplicated(.)), ]

transferwise_transfers %<>% #Drop columns that aren't needed
  .[.$Status == "transferred", ] %<>%
  .[(.$Reference %notIn% c('UPS packag', 'VERIFYBANK')), ]


##Interrupting workflow to add rows to df that were not originally caught by transferwise
df_of_missing_rows_from_transferwise_records <- #create df structure
  transferwise_transfers[NULL, ]

original_USD_Purchase_with_SEK <- #Create the row
  list("No_ID",
       "2017/01/01 01:09:31",
       "personal",
       "transferred",
       "SEK",
       318793.8,
       0,
       0,
       0,
       "USD",
       35050,
       35050/318793.8,
       "2017/01/01 01:09:31",
       "2017/01/01 01:09:31",
       "Redacted",
       "Redacted",
       "Initial USD purchase")

missing_rows <- #Make list of all missing rows
  list(original_USD_Purchase_with_SEK)

for (i in 1 : length(missing_rows)){ #Create df from rows
  df_of_missing_rows_from_transferwise_records[i ,] <-
    missing_rows[[i]]
}

transferwise_transfers %<>%
  rbind(., df_of_missing_rows_from_transferwise_records)

##Resuming workflow after having added rows to df

transferwise_transfers$`Submit time` %<>%
  strptime("%Y/%m/%d %H:%M:%S") %>%
  strftime("%Y-%m-%d %H:%M:%S")

transferwise_transfers$Date <- #create date column
  transferwise_transfers$`Submit time` %>%
  as.Date %>%
  lubridate::ymd(.)

transferwise_column_order %<>% c(., "Date")

transferwise_transfers$Start <-
  transferwise_transfers$`Source currency`

transferwise_transfers$End <-
  transferwise_transfers$`Target currency`

transferwise_transfers$Change_in_Start <-
  transferwise_transfers$`Amount paid by`

transferwise_transfers$Change_in_End <-
  transferwise_transfers$`Converted and sent to`

transferwise_column_order %<>% c(., "Start", "End", "Change_in_Start", "Change_in_End")

transferwise_transfers %<>% #add columns for matching CCs next
  merge(x = .,
        y = my_transferwise_ccs[ , c("transferwiseTicker", "fourLetterTicker")],
        by.x = "Start",
        by.y = "transferwiseTicker") %>%
  rename(Start_fourLetterTicker = fourLetterTicker) %>%
  merge(x = .,
        y = my_transferwise_ccs[ , c("transferwiseTicker", "fourLetterTicker")],
        by.x = "End",
        by.y = "transferwiseTicker") %>%
  rename(End_fourLetterTicker = fourLetterTicker)

transferwise_column_order %<>% c(., "Start_fourLetterTicker", "End_fourLetterTicker")

transferwise_transfers$Date_as_character <- #create column for matching to refTable row.names
  transferwise_transfers$Date %>% 
  as.character

transferwise_column_order %<>% c(., "Date_as_character")

transferwise_transfers$SEK_costoftransaction <- #transferwise has the exchange rates in the csv directly, so no need to look them up.
  ifelse(transferwise_transfers$`Target currency` == "SEK",
         transferwise_transfers$`Converted and sent to` * -1,
         ifelse(transferwise_transfers$`Source currency` == "SEK", transferwise_transfers$`Amount paid by`, "NA"))

transferwise_column_order %<>% c(., "SEK_costoftransaction")

transferwise_transfers %<>% #Bring in SEK_USD_ rates
  merge(x = .,
        y = usd__all_rates[ , c("Date", "SEK_USD_")],
        all.x = T)

transferwise_column_order %<>% c(., "SEK_USD_")

transferwise_transfers$USD_costoftransaction <-
  ifelse(transferwise_transfers$`Target currency` == "USD",
         transferwise_transfers$`Converted and sent to` * -1,
         ifelse(transferwise_transfers$`Source currency` == "USD", transferwise_transfers$`Amount paid by`, "NA"))

transferwise_column_order %<>% c(., "USD_costoftransaction")

transferwise_transfers$Date_time <-
  transferwise_transfers$Date %>%
  lubridate::as_datetime(.)

transferwise_column_order %<>% c(., "Date_time")

transferwise_transfers %<>% #Reorder column to match source 
  .[ , transferwise_column_order]


#Read in kraken data
kraken_ledgers <-
  as.data.frame(readr::read_csv("inputs/07_all_trades_all_years/kraken_ledgers.csv"))

#If Kraken asset for Swiss Franc is only shown as CHF, it must be changed to
# ZCHF for matching to other records
kraken_ledgers %<>%
  mutate(asset = ifelse(asset == "CHF", "ZCHF", asset))

###Should check the date format since the way Kraken exports dates may have changed!

kraken_column_order <- #save column order for later
  kraken_ledgers %>% colnames

kraken_ledgers %<>% .[!(duplicated(.)), ]

##Interrupting workflow to test for duplicate info

df_for_testing_uniqueness_of_kraken_rows <- kraken_ledgers

df_for_testing_uniqueness_of_kraken_rows[["amount"]] %<>% formatC(format = "e", digits = 10)

df_for_testing_uniqueness_of_kraken_rows[["fee"]] %<>% formatC(format = "e", digits = 10)

rows_without_txid <-
  which(is.na(df_for_testing_uniqueness_of_kraken_rows[["txid"]]))

rows_without_balance <-
  which(is.na(df_for_testing_uniqueness_of_kraken_rows[["balance"]]))


#Simple test to make sure the two lists of rows above are identical.
if(sum(rows_without_txid != rows_without_balance) == 0) {
  dlgMessage("Can use either 'rows_without_txid' or 'rows_without_balance' for subsetting. You should proceed.\n\n")
} else {
  dlgMessage("The two lists are not identical - you must understand why and fix it before proceeding.")
  stop()
}

rows_with_txid <-
  which(row(df_for_testing_uniqueness_of_kraken_rows) %notIn%
          rows_without_txid)

columns_to_concatenate <- #timestamps vary on some of these rows although all other data seems to be identical
  colnames(df_for_testing_uniqueness_of_kraken_rows)[
    colnames(df_for_testing_uniqueness_of_kraken_rows) %notIn%
      c("txid", "balance", "time")]

vector_of_strings_without_txid <-
  apply(df_for_testing_uniqueness_of_kraken_rows[rows_without_txid, columns_to_concatenate], 1, paste0, collapse = "-")

vector_of_strings_with_txid <-
  apply(df_for_testing_uniqueness_of_kraken_rows[rows_with_txid, columns_to_concatenate], 1, paste0, collapse = "-")

#Simple test to try to identify rows to be ignored
if(!(any(vector_of_strings_without_txid %notIn% vector_of_strings_with_txid))) {
  dlgMessage("Information is duplicated when txid is blank and those rows should be ignored.")
} else {
  dlgMessage("It is unclear which rows should be ignored. You must explore the data more before proceeding.")
  stop()
}


#Dropping rows where txid is blank and then formatting the date columns.
kraken_ledgers %<>% .[!is.na(.$txid), ]

kraken_ledgers[["time"]] %<>%
  lubridate::as_datetime(.)

kraken_ledgers[["Date"]] <-
  kraken_ledgers[["time"]] %>%
  as.Date %>%
  lubridate::ymd(.)

kraken_column_order %<>% c(., "Date")

#Bringing in data about my kraken ccs.
kraken_ledgers %<>% #add column for matching ccs next
  merge(x = .,
        y = my_kraken_ccs[ , c("krakenLedgerTicker", "fourLetterTicker")],
        by.x = "asset",
        by.y = "krakenLedgerTicker",
        all.x = T)

kraken_column_order %<>% c(., "fourLetterTicker")

#Dropping rows for "KFEE" asset since it is just a bonus given by Kraken and has no cash value outside of Kraken. It just offsets some fees.
kraken_ledgers %<>% .[.[["asset"]] != "KFEE", ]

refTable <- sek__all_rates #pull in reference table for looking up exchanges

names(refTable) %<>% #modify names so can be matched
  gsub(pattern = "SEK_", replacement = "", x = .)

row.names(refTable) <- refTable$Date #assign Date to row names for easy indexing

unique_ccs_in_kraken <- #create vector for matching
  unique(kraken_ledgers$fourLetterTicker) %>%
  sort

ccs_in_kraken_not_matched_in_ref_table <-
  unique_ccs_in_kraken[unique_ccs_in_kraken %notIn% names(refTable)] %>%
  sort

#Simple test to make sure all of your CCs are accounted for
if(length(ccs_in_kraken_not_matched_in_ref_table) == 0) {
  dlgMessage("All CCs accounted for. You should proceed.\n\n")
} else {
  dlgMessage("All CCs are NOT accounted for; check the fourLetterTickers.")
  stop()
}

#Simple test to see if any missing fourLetterTickers
if(!any(is.na(kraken_ledgers$fourLetterTicker))) {
  dlgMessage("No missing, you should proceed.\n\n")
} else {
  dlgMessage("Some fourLetterTicker data are missing; check them and fix before moving on.")
  stop()
}


#If the tests above were successful, then continue on with formatting, etc.
kraken_ledgers[["Date_as_character"]] <- #create column for matching to refTable row.names
  kraken_ledgers[["Date"]] %>% 
  as.character

kraken_column_order %<>% c(., "Date_as_character")

kraken_ledgers[["SEK_rate"]] <- #lookup values in refTable. Cannot use "$Date" since cbind converts "date" classes and the lookup will fail.
  refTable[cbind(
    kraken_ledgers[["Date_as_character"]],
    kraken_ledgers[["fourLetterTicker"]])] %>%
  as.numeric

kraken_column_order %<>% c(., "SEK_rate")

kraken_ledgers[["SEK_costoftransaction"]] <- #The Gatehub formats differ from Kraken and Bitfinex; if I paid SEK to deposit EUR into an account, the SEK_cost must be postive. If I withdrew EUR to receive SEK, the SEK_cost should be negative (negative cost = gain in SEK).
  kraken_ledgers[["amount"]] * kraken_ledgers[["SEK_rate"]] 

kraken_column_order %<>% c(., "SEK_costoftransaction")


##Interrupting workflow to fix inaccurate costs associated with deposits and withdrawals.

#Dropping rows for "KFEE" asset since it is just a bonus given by Kraken and has no cash value outside of Kraken. It just offsets some fees.
kraken_deps_wtdrws_trnsfrs %<>% .[.[["Kraken_deposit_withdrawal_transfer_note"]] != "KFEE_Bonus_from_Kraken", ]

kraken_ledgers <- #bring in columns from df
  merge(x = kraken_ledgers,
        y = kraken_deps_wtdrws_trnsfrs[ , c("txid", "refid", "Actual_SEK_from_Handelsbanken_records", "Kraken_deposit_withdrawal_transfer_note")],
        all = T)

kraken_column_order %<>%
  c(.,
    "Actual_SEK_from_Handelsbanken_records",
    "Kraken_deposit_withdrawal_transfer_note")

for (i in 1 : nrow(kraken_ledgers)) { #update the SEK_costoftransaction
  if (is.na(kraken_ledgers[["Actual_SEK_from_Handelsbanken_records"]][i])) {
    next
  } else {
    kraken_ledgers[["SEK_costoftransaction"]][i] <-
      kraken_ledgers[["Actual_SEK_from_Handelsbanken_records"]][i]
  }
}


#After fixing the inaccurate costs associated with deposits and withdrawals, will calculate the SEK value and USD value of the remaining balance.

kraken_ledgers[["SEK_costoffee"]] <- #The Bitfinex fees differ from Kraken. They show up as negative numbers in the raw data, but should be tabulated as a positive SEK_costoffee since a positive number means it cost me SEK.
  kraken_ledgers[["fee"]] * kraken_ledgers[["SEK_rate"]]

kraken_column_order %<>% c(., "SEK_costoffee")

kraken_ledgers[["SEK_Value_of_Balance"]] <- #Bitfinex, unlike Gatehub and Kraken, does not give updated balances on the deposits/withdrawals .csv.
  kraken_ledgers[["balance"]] * kraken_ledgers[["SEK_rate"]]

kraken_column_order %<>% c(., "SEK_Value_of_Balance")

kraken_ledgers %<>% #Bring in SEK_USD_ rates
  merge(x = .,
        y = usd__all_rates[ , c("Date", "SEK_USD_")],
        all.x = T)

kraken_column_order %<>% c(., "SEK_USD_")

kraken_ledgers[["USD_rate"]] <-
  kraken_ledgers[["SEK_rate"]] * kraken_ledgers[["SEK_USD_"]]

kraken_column_order %<>% c(., "USD_rate")

kraken_ledgers[["USD_costoftransaction"]] <-
  kraken_ledgers[["SEK_costoftransaction"]] * kraken_ledgers[["SEK_USD_"]]

kraken_column_order %<>% c(., "USD_costoftransaction")

kraken_ledgers[["USD_costoffee"]] <- #The Bitfinex fees differ from Kraken. They show up as negative numbers in the raw data, but should be tabulated as a positive SEK_costoffee since a positive number means it cost me SEK.
  kraken_ledgers[["fee"]] * kraken_ledgers[["USD_rate"]]

kraken_column_order %<>% c(., "USD_costoffee")

kraken_ledgers[["USD_Value_of_Balance"]] <- #Bitfinex, unlike Gatehub and Kraken, does not give updated balances on the deposits/withdrawals .csv.
  kraken_ledgers[["SEK_Value_of_Balance"]] * kraken_ledgers[["SEK_USD_"]]

kraken_column_order %<>% c(., "USD_Value_of_Balance")

kraken_ledgers[["Date_time"]] <-
  kraken_ledgers[["time"]] %>%
  lubridate::as_datetime(.)

kraken_column_order %<>% c(., "Date_time")

kraken_ledgers %<>% #Reorder column to match source 
  .[ , kraken_column_order]




#Consolidating all the different data into a single dataframe

##
##Starting with bitfinex
##


#First for the deposits and withdrawals

bitfinex_for_skatteverket_01_depos_and_withdrawals <-
  bitfinex_depos_and_withdrawals

bitfinex_for_skatteverket_01_depos_and_withdrawals %<>% #Remove cancelled orders
  .[.$STATUS %in% "COMPLETED", ]

bitfinex_for_skatteverket_01_depos_and_withdrawals$Ticker_Beteckning <-
  bitfinex_for_skatteverket_01_depos_and_withdrawals$fourLetterTicker

bitfinex_for_skatteverket_01_depos_and_withdrawals$Transaction_Datum <-
  bitfinex_for_skatteverket_01_depos_and_withdrawals$Date_time

bitfinex_for_skatteverket_01_depos_and_withdrawals$Action_Händelse <- 
  ifelse(bitfinex_for_skatteverket_01_depos_and_withdrawals$AMOUNT >= 0,
         "Buy",
         "Sell")

##
##Interrupting workflow to append fees as independent sells for 0 SEK
##


bitfinex_for_skatteverket_01_depos_and_withdrawals_fees <-
  bitfinex_for_skatteverket_01_depos_and_withdrawals[bitfinex_for_skatteverket_01_depos_and_withdrawals$FEES < 0, ]

bitfinex_for_skatteverket_01_depos_and_withdrawals_fees$AMOUNT <-
  bitfinex_for_skatteverket_01_depos_and_withdrawals_fees$FEES

bitfinex_for_skatteverket_01_depos_and_withdrawals_fees$Action_Händelse <- "Sell"

bitfinex_for_skatteverket_01_depos_and_withdrawals_fees$SEK_costoftransaction <-
  0

bitfinex_for_skatteverket_01_depos_and_withdrawals %<>%
  rbind(., bitfinex_for_skatteverket_01_depos_and_withdrawals_fees)


##
##Resuming workflow after appending fees as independent sells for 0 SEK
##


bitfinex_for_skatteverket_01_depos_and_withdrawals$Number_Antal <-
  bitfinex_for_skatteverket_01_depos_and_withdrawals$AMOUNT %>%
  abs

bitfinex_for_skatteverket_01_depos_and_withdrawals$SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp <- 
  bitfinex_for_skatteverket_01_depos_and_withdrawals$SEK_costoftransaction %>%
  abs

#Then for the trades

bitfinex_for_skatteverket_02_trades <-
  bitfinex_trades

bitfinex_for_skatteverket_02_trades$Transaction_Datum <-
  bitfinex_for_skatteverket_02_trades$Date_time


##
##Interrupting workflow to treat fees, start, and end CC as independent transactions. Also making sure fees are sells for 0 SEK.
##


bitfinex_for_skatteverket_02_trades_fees <-
  bitfinex_for_skatteverket_02_trades[bitfinex_for_skatteverket_02_trades$FEE < 0, ]

bitfinex_for_skatteverket_02_trades_fees$Ticker_Beteckning <-
  bitfinex_for_skatteverket_02_trades_fees$Fee_fourLetterTicker

bitfinex_for_skatteverket_02_trades_fees$AMOUNT <-
  bitfinex_for_skatteverket_02_trades_fees$FEE

bitfinex_for_skatteverket_02_trades_fees$SEK_costoftransaction <-
  0

bitfinex_for_skatteverket_02_trades_start <-
  bitfinex_for_skatteverket_02_trades

bitfinex_for_skatteverket_02_trades_start$Ticker_Beteckning <-
  bitfinex_for_skatteverket_02_trades_start$Start_fourLetterTicker

bitfinex_for_skatteverket_02_trades_start$AMOUNT <-
  bitfinex_for_skatteverket_02_trades_start$Change_in_Start

bitfinex_for_skatteverket_02_trades_start$SEK_costoftransaction <-
  bitfinex_for_skatteverket_02_trades_start$Start_SEK_costoftransaction

bitfinex_for_skatteverket_02_trades_end <-
  bitfinex_for_skatteverket_02_trades

bitfinex_for_skatteverket_02_trades_end$Ticker_Beteckning <-
  bitfinex_for_skatteverket_02_trades_end$End_fourLetterTicker

bitfinex_for_skatteverket_02_trades_end$AMOUNT <-
  bitfinex_for_skatteverket_02_trades_end$Change_in_End

bitfinex_for_skatteverket_02_trades_end$SEK_costoftransaction <-
  bitfinex_for_skatteverket_02_trades_end$End_SEK_costoftransaction

bitfinex_for_skatteverket_02_trades <-
  rbind(bitfinex_for_skatteverket_02_trades_fees,
        bitfinex_for_skatteverket_02_trades_start,
        bitfinex_for_skatteverket_02_trades_end)

##
##Resuming workflow after treating fees, start, and end CC as independent transactions. Also made sure that fees are sells for 0 SEK.
##


bitfinex_for_skatteverket_02_trades$Action_Händelse <- 
  ifelse(bitfinex_for_skatteverket_02_trades$SEK_costoftransaction <= 0,
         "Sell",
         "Buy")

bitfinex_for_skatteverket_02_trades$Number_Antal <-
  bitfinex_for_skatteverket_02_trades$AMOUNT %>%
  abs

bitfinex_for_skatteverket_02_trades$SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp <- 
  bitfinex_for_skatteverket_02_trades$SEK_costoftransaction %>%
  abs

rm(bitfinex_depos_and_withdrawals, bitfinex_for_skatteverket_01_depos_and_withdrawals_fees, bitfinex_for_skatteverket_02_trades_end, bitfinex_for_skatteverket_02_trades_fees, bitfinex_for_skatteverket_02_trades_start, bitfinex_trades)


##
##Continuing with transferwise
##


transferwise_for_skatteverket <-
  transferwise_transfers

transferwise_for_skatteverket$Ticker_Beteckning <-
  "USD_"

transferwise_for_skatteverket$Transaction_Datum <-
  transferwise_for_skatteverket$Date_time

transferwise_for_skatteverket$Action_Händelse <- 
  ifelse(transferwise_for_skatteverket$Start_fourLetterTicker == "SEK_",
         "Buy",
         "Sell")

if (is.null(transferwise_for_skatteverket$Converted.and.sent.to)) {
  transferwise_for_skatteverket$Converted.and.sent.to <-
    transferwise_for_skatteverket$`Converted and sent to`
}

if (is.null(transferwise_for_skatteverket$Amount.paid.by)) {
  transferwise_for_skatteverket$Amount.paid.by <-
    transferwise_for_skatteverket$`Amount paid by`
}

transferwise_for_skatteverket$Number_Antal <-
  ifelse(transferwise_for_skatteverket$Start_fourLetterTicker == "SEK_",
         transferwise_for_skatteverket$Converted.and.sent.to,
         transferwise_for_skatteverket$Amount.paid.by)

transferwise_for_skatteverket$SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp <- 
  transferwise_for_skatteverket$SEK_costoftransaction %>%
  as.numeric %>%
  abs

rm(transferwise_transfers)

##
##Continuing with gatehub
##


gatehub_for_skatteverket <-
  gatehub_balance_changes

gatehub_for_skatteverket %<>% #Remove duplicated rows
  .[!(duplicated(.)), ]

gatehub_for_skatteverket$Ticker_Beteckning <-
  gatehub_for_skatteverket$fourLetterTicker

gatehub_for_skatteverket$Transaction_Datum <-
  gatehub_for_skatteverket$Date_time

gatehub_for_skatteverket$Action_Händelse <- 
  ifelse(gatehub_for_skatteverket$Amount >= 0,
         "Buy",
         "Sell")

gatehub_for_skatteverket$Number_Antal <-
  gatehub_for_skatteverket$Amount %>%
  abs

gatehub_for_skatteverket$SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp <- 
  gatehub_for_skatteverket$SEK_costoftransaction %>%
  abs

#Fixing so that fees show up as "sells" for 0 SEK

gatehub_for_skatteverket$Action_Händelse %<>%
  ifelse(gatehub_for_skatteverket$Amount == "ripple_network_fee", "Sell", .)

gatehub_for_skatteverket$SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp %<>% #Since the Amount column is not actually accurate (I have to calculate it earlier using the balance column), it means that sometimes the "Type" == "ripple_network_fee" lines up with large deposits/withdrawals that gatehub does for some random reason. I have to make sure the SEKcost is accurate on these, and not counted as 0, or else it may look like I got a lot of cryptocurrency for free
  ifelse(gatehub_for_skatteverket$Type == "ripple_network_fee" &
           gatehub_for_skatteverket$Amount == 0.000015, 0, .)

rm(gatehub_balance_changes)

##
##Continuing with kraken
##


kraken_for_skatteverket <-
  kraken_ledgers

kraken_for_skatteverket %<>% #Remove rows where txid is blank since established these are duplicates
  .[!is.na(.$txid), ]

kraken_for_skatteverket$Ticker_Beteckning <-
  kraken_for_skatteverket$fourLetterTicker

kraken_for_skatteverket$Transaction_Datum <-
  kraken_for_skatteverket$Date_time

kraken_for_skatteverket$Action_Händelse <- 
  ifelse(kraken_for_skatteverket$amount >= 0,
         "Buy",
         "Sell")


##
##Interrupting workflow to append fees as independent sells for 0 SEK
##


kraken_for_skatteverket_fees <-
  kraken_for_skatteverket[kraken_for_skatteverket$fee > 0, ]

kraken_for_skatteverket_fees$Action_Händelse <-
  "Sell"

kraken_for_skatteverket_fees$amount <-
  kraken_for_skatteverket_fees$fee

kraken_for_skatteverket_fees$SEK_costoftransaction <-
  0

kraken_for_skatteverket %<>%
  rbind(., kraken_for_skatteverket_fees)


##
##Resuming workflow after appending fees as independent sells for 0 SEK
##


kraken_for_skatteverket$Number_Antal <-
  kraken_for_skatteverket$amount %>%
  abs

kraken_for_skatteverket$SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp <- 
  kraken_for_skatteverket$SEK_costoftransaction %>%
  abs

rm(kraken_for_skatteverket_fees, kraken_ledgers)




##
##Bringing together all exchanges
##


columns_to_rbind <-
  c("Ticker_Beteckning",
    "Transaction_Datum",
    "Action_Händelse",
    "Number_Antal",
    "SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp")

all_trades_all_years <-
  rbind(
    bitfinex_for_skatteverket_01_depos_and_withdrawals[ , c(columns_to_rbind)],
    bitfinex_for_skatteverket_02_trades[ , c(columns_to_rbind)],
    gatehub_for_skatteverket[ , c(columns_to_rbind)],
    transferwise_for_skatteverket[ , c(columns_to_rbind)],
    kraken_for_skatteverket[ , c(columns_to_rbind)])

all_trades_all_years$Helpercolumn_Changeintotalnumber <-
  ifelse(all_trades_all_years$Action_Händelse == "Buy",
         all_trades_all_years$Number_Antal,
         all_trades_all_years$Number_Antal * -1)

all_trades_all_years %<>%
  arrange(Action_Händelse) %<>%
  arrange(Transaction_Datum)

# re-establishing rownames after reordering
rownames(all_trades_all_years) <- NULL

all_trades_all_years$Rownumber <-
  rownames(all_trades_all_years) %>%
  as.numeric

all_trades_all_years$Totalnumber_Totalantal <-
  ave(all_trades_all_years$Helpercolumn_Changeintotalnumber,
      all_trades_all_years$Ticker_Beteckning,
      FUN = cumsum)

all_trades_all_years %<>%
  .[ , c("Rownumber",
         "Ticker_Beteckning",
         "Transaction_Datum",
         "Action_Händelse",
         "Number_Antal",
         "Totalnumber_Totalantal",
         "SEKcostoftransaction_Inköpsprisrespektiveutnyttjatomkostnadsbelopp",
         "Helpercolumn_Changeintotalnumber")] #I added the helpercolumn here so I don't have to repeat some operations in a later script

all_trades_all_years %<>%
  arrange(Action_Händelse) %<>%
  arrange(Transaction_Datum)


# save(bitfinex_depos_and_withdrawals, file = "outputs/bitfinex_depos_and_withdrawals.rda")
# save(bitfinex_trades, file = "outputs/bitfinex_trades.rda")
# save(gatehub_balance_changes, file = "outputs/gatehub_balance_changes.rda")
# save(transferwise_transfers, file = "outputs/transferwise_transfers.rda")
# save(kraken_ledgers, file = "outputs/kraken_ledgers.rda")
save(bitfinex_for_skatteverket_01_depos_and_withdrawals, bitfinex_for_skatteverket_02_trades, gatehub_for_skatteverket, transferwise_for_skatteverket, kraken_for_skatteverket, all_trades_all_years, file = "outputs/07_all_trades_all_years.rda")
