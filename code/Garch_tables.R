Garch_tables <- function() {
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
  # Scale, clean and log the already logged returns
  rtn3 <- scale(rtn, center = T, scale = F)
  rtn3 <- Return.clean(rtn3, method = c("none", "boudt", "geltner")[2], alpha = 0.01)
  Rtn3 <- log(1 + rtn3/100) * 100
  # Specify the GARCH model
  garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[2], 
              garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
              distribution.model = c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp", "jsu")[1])
  # Fit each stock's GARCH model
  garchfit_ABSP = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "ABSP"]))
  garchfit_BVT = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "BVT"]))
  garchfit_FSR = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "FSR"]))
  garchfit_NBKP = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "NBKP"]))
  garchfit_RMH = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "RMH"]))
  garchfit_SBK = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "SBK"]))
  garchfit_SLM = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "SLM"]))
  # Call the variance matrix from Sanlam's GARCH model 
  vxreg1 <- as.matrix(garchfit_SLM@fit$var)
  # Specify ABSA's GARCH model with an external regressor (Sanlam's variance matrix)
  garch11_vxreg1 <- ugarchspec(variance.model = list(model = "gjrGARCH", external.regressors = vxreg1, 
                    garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                    distribution.model = c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp", "jsu")[1])
  # Fit ABSA's GARCH model with the external regressor
  garchfit_ABSP_vxreg1 = ugarchfit(spec = garch11_vxreg1, data = as.matrix(Rtn3[ , "ABSP"]))
  # Display the coefficients of ABSA's GARCH model without the external regressor in a table
  table4 <- 
    xtable(garchfit_ABSP@fit$matcoef, caption = "Coefficients: ABSA GARCH11 \\label{GARCH-A}")
  # Display the coefficients of ABSA's GARCH model with an external regressor in a table
  table5 <- 
    xtable(garchfit_ABSP_vxreg1@fit$matcoef, caption = "Coefficients: ABSA GARCH11 with Sanlam external regressor  \\label{GARCH-B}")
    print.xtable(table4,
             floating = TRUE,
             table.placement = 'H', 
             comment = FALSE,
             caption.placement = 'bottom')
    print.xtable(table5,
             floating = TRUE,
             table.placement = 'H',
             comment = FALSE,
             caption.placement = 'bottom')
    # Display the inforcriteria of the 2 models we want to compare
    table6 <- 
      xtable(infocriteria(garchfit_ABSP), caption = "Infocriteria: ABSA GARCH11 \\label{GARCH-C}")
    table7 <- 
      xtable(infocriteria(garchfit_ABSP_vxreg1), caption = "Infocriteria: GARCH11 with Sanlam external regressor  \\label{GARCH-D}")
    print.xtable(table6,
                 floating = TRUE,
                 table.placement = 'H', 
                 comment = FALSE,
                 caption.placement = 'bottom')
    print.xtable(table7,
                 floating = TRUE,
                 table.placement = 'H',
                 comment = FALSE,
                 caption.placement = 'bottom')
}