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

## Read age data
age <- read_xlsx("covid_mort_spatseg/2018_pop_ests.xlsx", sheet =4, skip=4)
# Create age-band proportions <25, 25-44, 45-65, 65-75, 75+
age$`<25` <- rowSums(age[,4:8])
age$`25-44` <- rowSums(age[,9:12])
age$`45-64` <- rowSums(age[,13:16])
age$`65-74` <- rowSums(age[,17:18])
age$`75+` <- rowSums(age[19:22])

# Generate proportion variables
age$`<25prop` <- age$`<25`/age$`All Ages`
age$`25-44prop` <- age$`25-44`/age$`All Ages`
age$`45-64prop` <- age$`45-64`/age$`All Ages`
age$`65-74prop` <- age$`65-74`/age$`All Ages`
age$`75+prop` <- age$`75+`/age$`All Ages`

# drop constituent cols
age <- age[,-c(2,4:27)]

# Copy age profile for each MSOA 
master <- left_join(mdf, age, by=c("cd"="Area Codes"))

# Write outfile
write_csv(master, "MSOAmort.csv")
