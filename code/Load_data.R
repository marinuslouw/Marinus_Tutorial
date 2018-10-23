Load_data <- function() {
  dailydata_raw <- read_csv("https://raw.githubusercontent.com/Nicktz/ExDat/master/extdata/findata.csv", 
                            col_types = cols(.default = "d", Date = "D"))
  rtn <- (diff(log(dailydata_raw %>% arrange(Date) %>% tbl_xts()), 
               lag = 1)) * 100
  rtn <- rtn[-1, ]
  colnames(rtn) <- colnames(rtn) %>% gsub("JSE.", "", .) %>% gsub(".Close", "", .)
  rtndata <- rtn %>% xts_tbl() %>% gather(Ticker, Return, -date) %>% 
    mutate(Return_Sqd = Return^2)
}
