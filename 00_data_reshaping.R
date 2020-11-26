## Recoding MSOA death counts

## while(!try(require(tidyverse), silent = TRUE)) install.packages("tidyverse")
## while(!try(require(readxl), silent = TRUE)) install.packages("readxl")  


library(tidyverse)
library(readxl)
library(reshape2)

setwd('C:/00RESEARCH/repo/COVID_spatial_inequalities/covid_mort_spatseg/')

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
age <- read_xlsx("2018_pop_ests.xlsx", sheet =4, skip=4)
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

mort <- master %>% group_by(variable) %>% 
  ### first find total population
  mutate(total_pop = sum(`All Ages`),
         ### then total deaths
         total_death = sum(as.numeric(value)),
         ### expected value is total deaths divided by total population times population of msoa
         expected = (total_death/total_pop)*`All Ages`,
         ### offset is log of this value
         offset = log(expected))

#######
# Integrate with STP shapefiles.
#######


### read in file linking lsoa to stp
lsoa_ccg <- read_csv("lsoa_ccg.csv")
#### read in the IMD data
IMD <- read_csv("IMD.csv")
lsoa_ccg <- inner_join(lsoa_ccg, IMD, by = c("LSOA11CD" = "LSOA code (2011)"))
lsoa_ccg <- lsoa_ccg %>% 
  group_by(STP19CD) %>% 
  mutate(avgIMDstp = mean(`Index of Multiple Deprivation (IMD) Rank`))

### a file of imd data just for stp
stp_imd <- lsoa_ccg %>% 
  select(STP19CD, avgIMDstp) %>% 
  distinct(STP19CD, .keep_all = TRUE)

### read in file linking msoa to lsoa
msoa_lsoa <- read_csv("OAtoLSOAtoMSOAtoLAD.csv") %>% 
  distinct(LSOA11CD, .keep_all = T) %>% 
  select(LSOA11CD,MSOA11CD, MSOA11NM,LAD17CD, LAD17NM, RGN11CD,RGN11NM)


msoa_lsoa <- left_join(msoa_lsoa, IMD, by = c("LSOA11CD" = "LSOA code (2011)"))

### add the deprivation data
msoa_lsoa <- msoa_lsoa %>% 
  group_by(RGN11CD) %>% 
  mutate(avgIMDreg = mean(`Index of Multiple Deprivation (IMD) Rank`)) %>% 
  ungroup() %>% 
  group_by(LAD17CD) %>% 
  mutate(avgIMDlad = mean(`Index of Multiple Deprivation (IMD) Rank`)) %>%
  ungroup() %>% 
  group_by(MSOA11CD) %>% 
  mutate(avgIMDmsoa = mean(`Index of Multiple Deprivation (IMD) Rank`))



### join two together usin lsoa as key
msoa_stp <- left_join(lsoa_ccg,msoa_lsoa, by = "LSOA11CD")



### there is a slight problem here because 17 MSOAs are in different STPs
### because this is a very small number and to make life easier/
### for the purpose of this analysis we will asign each MSOA to the STP/
### within within which the majority of the lsoas within that msoa are part of
### this code does this and matches each MSOA with exactly one STP
msoa_stp <- msoa_stp %>% 
  group_by(MSOA11CD) %>% 
  count(STP19CD, STP19NM) %>% 
  ungroup() %>% 
  arrange(MSOA11CD,-n) %>% 
  distinct(MSOA11CD, .keep_all = T)

msoa_stp <- left_join(msoa_stp, stp_imd, by = "STP19CD")



### we can now rejoin this with the MSOA data
areas <- left_join(msoa_lsoa,msoa_stp, by = "MSOA11CD") %>% 
  distinct(MSOA11CD, .keep_all = T) %>% 
  select(MSOA11CD, MSOA11NM, RGN11CD, RGN11NM,STP19CD, STP19NM, LAD17CD, LAD17NM, avgIMDreg, avgIMDlad, avgIMDmsoa, avgIMDstp)

### stps are for england for wales there are health boards
wales_healthboard <- read_csv("wales_lad_healthboard.csv")

areas <- left_join(areas, wales_healthboard, by = c("LAD17CD" = "UA19CD"))

areas$STP19CD[is.na(areas$STP19CD)] <- areas$LHB19CD[is.na(areas$STP19CD)]
areas$STP19NM[is.na(areas$STP19NM)] <- areas$LHB19NM[is.na(areas$STP19NM)]
areas <- areas %>% select(-c(FID,UA19NM, LHB19NM, LHB19CD, LHB19NM)) %>% 
  filter(RGN11NM != "Scotland")

### Join with MSOA deaths data
mortareas <- left_join(mort,areas, by = c("cd"  = "MSOA11CD"))




### create the offset (grouping by month and type of death)

mortareas <- mortareas %>% group_by(variable) %>% 
  ### first find total population
  mutate(total_pop = sum(`All Ages`),
         ### then total deaths
         total_death = sum(as.numeric(value)),
         ### expected value is total deaths divided by total population times population of msoa
         expected = (total_death/total_pop)*`All Ages`,
         ### offset is log of this value
         offset = log(expected))

#######
### Integrate Care Home counts from NOMIS (KS405EW)
#######

carehomes <- read.csv('careHomes.csv', skip=8, h=T) %>% drop_na()
colnames(carehomes) <- c("nm", "cd", "lach", "othnurs", "othnonurs")

### Create total population in carehomes in MSOA

carehomes <- carehomes %>% mutate(chtotal = lach + othnurs + othnonurs)

### Merge with master dataset

modeldata <- merge(mortareas, carehomes[,c(2,6)], by="cd", all.x=TRUE)
modeldata <- modeldata %>% mutate(percCH = (chtotal/All.Ages)*100)

### write to csv
write_csv(modeldata, "model_data_imd.csv")



