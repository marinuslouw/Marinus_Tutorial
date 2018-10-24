Pairs_panels <- function() {
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
  # Display pairs panels using the psych package's pairs.panels function
  pairs.panels(rtn %>% xts_tbl() %>% dplyr::select(-date))
} 