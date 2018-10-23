Garch_tables <- function() {
  dailydata_raw <- read_csv("https://raw.githubusercontent.com/Nicktz/ExDat/master/extdata/findata.csv", 
                          col_types = cols(.default = "d", Date = "D"))

rtn <- (diff(log(dailydata_raw %>% arrange(Date) %>% tbl_xts()), lag = 1)) * 100
rtn <- rtn[-1, ]
colnames(rtn) <- colnames(rtn) %>% gsub("JSE.", "", .) %>% gsub(".Close", "", .)
rtn3 <- scale(rtn, center = T, scale = F)
rtn3 <- Return.clean(rtn3, method = c("none", "boudt", "geltner")[2], alpha = 0.01)
Rtn3 <- log(1 + rtn3/100) * 100

garch11 <- ugarchspec(variance.model = list(model = c("sGARCH", 
                                                      "gjrGARCH", "eGARCH", "fGARCH", "apARCH")[2], garchOrder = c(1, 
                                                                                                                   1)), mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                      distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                             "sged", "nig", "ghyp", "jsu")[1])
garchfit_ABSP = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "ABSP"]))
garchfit_BVT = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "BVT"]))
garchfit_FSR = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "FSR"]))
garchfit_NBKP = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "NBKP"]))
garchfit_RMH = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "RMH"]))
garchfit_SBK = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "SBK"]))
garchfit_SLM = ugarchfit(spec = garch11, data = as.matrix(Rtn3[ , "SLM"]))

vxreg1 <- as.matrix(garchfit_SLM@fit$var)
garch11_vxreg1 <- ugarchspec(variance.model = list(model = "gjrGARCH", external.regressors = vxreg1, garchOrder = c(1, 
                                                                                                                    1)), mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                             distribution.model = c("norm", "snorm", "std", "sstd", "ged", 
                                                    "sged", "nig", "ghyp", "jsu")[1])
garchfit_ABSP_vxreg1 = ugarchfit(spec = garch11_vxreg1, data = as.matrix(Rtn3[ , "ABSP"]))

table4 <- xtable(garchfit_ABSP@fit$matcoef, caption = "GARCH11 \\label{GARCH-A}")
table5 <- xtable(garchfit_ABSP_vxreg1@fit$matcoef, caption = "GARCH11 with SLM external regressor  \\label{GARCH-B}")
print.xtable(table4, 
             # tabular.environment = "longtable",
             floating = TRUE,
             table.placement = 'H', 
             # scalebox = 0.3, 
             comment = FALSE,
             caption.placement = 'bottom'
)
print.xtable(table5, 
             # tabular.environment = "longtable",
             floating = TRUE,
             table.placement = 'H', 
             # scalebox = 0.3, 
             comment = FALSE,
             caption.placement = 'bottom'
)
}