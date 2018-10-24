# Financial Econometrics
## Marinus_Tutorial
### 24 October 2018

## Purpose
##### The objective of this tutorial is to accumulate experience in working with Texevier and RMarkdown. The tutorial was written in a README with accompanying R Scripts in order to neaten the process. The tutorial answers the following questions:
* Create a summary table showing the first and second moments of the returns of these stocks for the following periods:
+ 2006 - 2008
+ 2010 - 2013
* Calculate the unconditional (full sample) correlations between the stocks.
* Plot the univariate GARCH ht processes for each of the series.
* Plot the cumulative returns series of a portfolio that is equally weighted to each of the stocks - reweighted each year on the last day of June.
* See if including the GARCH11 conditional volatility series of SLM (ht,Sanlam) improves the GARCH11 model fit of ABSP (interpret the p-value of the regressor).

## Methodology
##### The tutorial questions are answered by writing the necessary code in functions within the following R scripts:
* Packages.R - containing the necessary packages for each chunk of code.
* Load_data.R - a function to load the findata from https://raw.githubusercontent.com/Nicktz
* Moments_Table.R - a function containing the 1st and 2nd moments of 2006-2008 and 2010-2013
* Correlations_Table.R - a function creating a correlations table using base R's cor function
* Pairs_panels.R - a function creating visual pairs panels using the psych package's pairs.panels function to supplement the Correlations_Table.R
* Garch_table.R - a function creating the GARCH specifications, fits, model coefficients tables and information criteria tables
