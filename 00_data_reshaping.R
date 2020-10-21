## Recoding MSOA death counts

## while(!try(require(tidyverse), silent = TRUE)) install.packages("tidyverse")
## while(!try(require(readxl), silent = TRUE)) install.packages("readxl")  


library(tidyverse)
library(readxl)
library(reshape2)

setwd('C:/00RESEARCH/repo/COVID_spatial_inequalities/')

#Read in data
df <- read_xlsx("death_counts_MSOA.xlsx", sheet=8, skip=11)
#Drop weird empty ONS columns
df <- df[-c(4,11,18)]

#rename columns to replicate
names <- c("cd", "ons", "nm", "cov3", "cov4", "cov5", "cov6", "cov7", "covtot", "ncov3", "ncov4", "ncov5", "ncov6", "ncov7", "ncovtot", "tot3", "tot4", "tot5", "tot6", "tot7", "tottot")
colnames(df) <- names

#ditch metavar colnames
df <- df[-1,]

#ditch cols not needed for this analysis
df <- df[,-c(2,3)]
#melt data for single row per observation, can group by variables otherswise
mdf <- melt(df, id="cd")

age <- read_xlsx("2018_pop_ests.xlsx", sheet =4, skip=4)
