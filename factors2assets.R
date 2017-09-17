# script to explore modelling factors to assets
#
# 16 Sept 2017

library(RStudioAMI)
library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(RFtools)


# load data
path <- "/home/rstudio/Dropbox/data/"
factors <-read.csv(paste(path, "factors.csv", sep = "")) 
factors$Date <- as.Date(factors$Date, "%d/%m/%Y")
factors <- df2xts(factors)

assets <- read.csv(paste(path, "assets.csv", sep = ""))                    
assets$Dates <- as.Date(assets$Dates, "%d/%m/%Y")
assets <- df2xts(assets)

# create EM factor separate to DM equities
fit <- lm(formula = EM ~ Equities, data = factors)
EMx <- as.data.frame(residuals(fit))
EMx$Dates <- rownames(EMx)
names(EMx) <- c("EMx", "Dates")
row.names(EMx) <-1:nrow(EMx)
EMx <- EMx[,c("Dates", "EMx")]
EMx$Dates <- as.Date(EMx$Date, "%Y-%m-%d")
EMx <- df2xts(EMx)

factors <- merge.xts(factors, EMx)
factors <- factors[, colnames(factors) != "EM"]
factors <- factors[, c("Equities", "RealRates", "Inflation", "IG", "Comm", "EMx")] 

# find common start date
didx <- max(firstNonNAindex(factors))
factors <- factors[paste(index(factors)[didx], "/", sep = "")]

# size assets to factors
assets <- merge.xts(assets, factors, join = "right", fill = NA)

# regressing assets to factors
assetRegress <- function(x, factors = factors){
  
  y <- merge.xts(x, factors)
  
  form.text <- paste(names(x), "~", paste(names(factors), collapse = " + "), collapse = " ")
  form <- as.formula(form.text)
  
  fit <- lm(formula = form, data = y)
  EMx <- as.data.frame(residuals(fit))
  EMx$Dates <- rownames(EMx)
  names(EMx) <- c("EMx", "Dates")
  row.names(EMx) <-1:nrow(EMx)
  EMx <- EMx[,c("Dates", "EMx")]
  EMx$Dates <- as.Date(EMx$Date, "%Y-%m-%d")
  EMx <- df2xts(EMx)
  
  
  
  
}
