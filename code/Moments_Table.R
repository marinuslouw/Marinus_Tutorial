Moments_Table <- function() {
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
  # Filter the data to the periods 2006-2008 and 2010-2013 and pipe them into psych package's describe function
  rtndata1 <- 
    rtndata %>% 
    filter(date < ymd(20090101)) %>% 
    dplyr::select(c(Return, Return_Sqd)) %>% 
    psych::describe()
  rtndata2 <- 
    rtndata %>% 
    filter(date >= ymd(20100101) & date < ymd(20140101)) %>%
    dplyr::select(c(Return, Return_Sqd)) %>% 
    psych::describe()
  # Create an xtable for both sample periods
  table1 <- 
    xtable(rtndata1, caption = "1st and 2nd Moments (2006-2008) \\label{Moments-A}")
    print.xtable(table1,
               floating = TRUE,
               table.placement = 'H',
               scalebox = 0.8,
               comment = FALSE,
               caption.placement = 'bottom')
  table2 <- 
    xtable(rtndata2, caption = "1st and 2nd Moments (2010-2013) \\label{Moments-B}")
    print.xtable(table2, 
               floating = TRUE,
               table.placement = 'H', 
               scalebox = 0.8,
               comment = FALSE,
               caption.placement = 'bottom')
}

