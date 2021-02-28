# Load requirements
requirements_df <-
  data.frame(
    required_rda_files =
      c("outputs/09_tax_reporting.rda"),
    
    source_scripts =
      c("scripts/09_tax_reporting.r"),
    
    stringsAsFactors = F)

load_source_data()

# 02 Ensure all pkgs in this script are installed ####
pkgs <-
    c('openxlsx')

activatePkgs(pkgs)

#get list of dataframes that will put into a workbook
rm(requirements_df)

dfs_to_write_to_wb <-
  eapply(.GlobalEnv,is.data.frame) %>%
  unlist %>%
  which %>%
  names %>%
  sort

wb_for_accountant <- createWorkbook()

for(df in dfs_to_write_to_wb) {
  
  temp_obj <- get(df)
  
  df %<>% substr(1, 31)
  
  addWorksheet(wb = wb_for_accountant, sheetName = df)
  
  writeData(wb = wb_for_accountant, sheet = df, x = temp_obj)
  
}

saveWorkbook(wb_for_accountant, file = "outputs/10_wb_for_accountant.xlsx", overwrite = T)
