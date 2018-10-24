Load_data <- function() {
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
  # Create a squared returns column
  rtndata <- 
    rtn %>% 
    xts_tbl() %>% 
    gather(Ticker, Return, -date) %>% 
    mutate(Return_Sqd = Return^2)
}
