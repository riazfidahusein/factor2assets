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
names(EMx) <- c("Date", "EMx")

EMx <- df2xts(EMx)

factors <- df2xts(factors)
