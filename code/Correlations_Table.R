Correlations_Table <- function() {
  # Load the data from githubusercontent
  dailydata_raw <- 
    read_csv("https://raw.githubusercontent.com/Nicktz/ExDat/master/extdata/findata.csv", col_types = cols(.default = "d", Date = "D"))
  # Create returns columns
  rtn <- 
    (diff(log(dailydata_raw %>% arrange(Date) %>% 
    tbl_xts()), lag = 1)) * 100
  # Remove the first row containing zeros
  rtn <- 
    rtn[-1, ]
  # Rename the columns
  colnames(rtn) <- 
    colnames(rtn) %>% 
    gsub("JSE.", "", .) %>% 
    gsub(".Close", "", .)
  # Create a correlation matrix and display it in a table
  cormat <- 
    cor(rtn %>% xts_tbl() %>% dplyr::select(-date))
  table3 <- 
    xtable(cormat, caption = "Unconditional Correlations \\label{Correlations}")
    print.xtable(table3,
              floating = TRUE,
              table.placement = 'H', 
              comment = FALSE,
              caption.placement = 'bottom')
}