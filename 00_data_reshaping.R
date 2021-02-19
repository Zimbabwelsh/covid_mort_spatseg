## Recoding MSOA death counts

## while(!try(require(tidyverse), silent = TRUE)) install.packages("tidyverse")
## while(!try(require(readxl), silent = TRUE)) install.packages("readxl")  


library(tidyverse)
library(readxl)
library(reshape2)
library(sf)

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

## To generate Summary counts graphic
# counts <- mort %>% group_by(variable) %>% count(total_death) %>% select(-n)
# counts <- counts[-c(6,12:18),]
# counts <- counts %>% mutate(group = case_when(grepl("n", variable) ~ "Non-COVID",
#                                              TRUE ~ "COVID-19"))
# counts$month <- factor(rep(c("March", "April", "May", "June", "July"),2),
#                        levels = c("March", "April", "May", "June", "July"))
# 
# mort_counts <- ggplot(data=counts, aes(month, total_death))+
#                       geom_col(colour="black", fill="#3b5a9d")+
#                       facet_wrap(~group)+
#                       labs(x="Month", y = "Count of Deaths")
  #




#######
# Integrate with STP shapefiles.
#######

### read in file linking lsoa to stp
lsoa_ccg <- read_csv("lsoa_ccg.csv") %>% 
  select(LSOA11CD, LSOA11NM, LAD19CD, LAD19NM, STP19CD, STP19NM)
### stps are for england for wales there are health boards
wales_healthboard <- read_csv("wales_lad_healthboard.csv") %>% 
  rename("STP19CD" = "LHB19CD",
         "STP19NM" = "LHB19NM",
         "LAD17CD" = "UA19CD",
         "LAD17NM" = "UA19NM") %>% 
  select(-FID)
area_lookups <- read_csv("OAtoLSOAtoMSOAtoLAD.csv") %>% 
  distinct(LSOA11CD, .keep_all = T) %>% 
  select(LSOA11CD, LSOA11NM, LAD17CD, LAD17NM)

wales_healthboard <- inner_join(wales_healthboard, area_lookups, by = c("LAD17CD", "LAD17NM")) %>% 
  rename("LAD19CD" = "LAD17CD",
         "LAD19NM" = "LAD17NM")
lsoa_ccg <- bind_rows(lsoa_ccg,wales_healthboard)

#### read in the IMD data and join to stp data
IMD <- read_csv("IMD.csv")
lsoa_ccg <- left_join(lsoa_ccg, IMD, by = c("LSOA11CD" = "LSOA code (2011)"))

### read in UK IMD and also join
UKIMD <- read_csv("UK_IMD_scores.csv") %>%
  select(area_code, uk_imd_england_score)
lsoa_ccg <- left_join(lsoa_ccg, UKIMD, by = c("LSOA11CD" = "area_code"))

### create stp level IMD
lsoa_ccg <- lsoa_ccg %>% 
  group_by(STP19CD) %>% 
  mutate(avgIMDstp = mean(`Index of Multiple Deprivation (IMD) Rank`),
         avgUKIMDstp = mean(uk_imd_england_score))


### a file of imd data just for stp
stp_imd <- lsoa_ccg %>% 
  select(STP19CD, avgIMDstp, avgUKIMDstp) %>% 
  distinct(STP19CD, .keep_all = TRUE)



### read in file linking msoa to lsoa
msoa_lsoa <- read_csv("OAtoLSOAtoMSOAtoLAD.csv") %>% 
  distinct(LSOA11CD, .keep_all = T) %>% 
  select(LSOA11CD,MSOA11CD, MSOA11NM,LAD17CD, LAD17NM, RGN11CD,RGN11NM)


### join ttwas
ttwas <- read_csv("LSOA2011toTTWA2011.csv")
ttwas <- ttwas %>% select(LSOA11CD, TTWA11CD, TTWA11NM)
msoa_lsoa <- left_join(msoa_lsoa, ttwas, by = "LSOA11CD")

### join with deprviation data
msoa_lsoa <- left_join(msoa_lsoa, IMD, by = c("LSOA11CD" = "LSOA code (2011)"))
msoa_lsoa <- left_join(msoa_lsoa, UKIMD, by = c("LSOA11CD" = "area_code"))

### add the deprivation data
msoa_lsoa <- msoa_lsoa %>% 
  group_by(RGN11CD) %>% 
  mutate(avgIMDreg = mean(`Index of Multiple Deprivation (IMD) Rank`),
         avgUKIMDreg = mean(uk_imd_england_score)) %>% 
  ungroup() %>% 
  group_by(LAD17CD) %>% 
  mutate(avgIMDlad = mean(`Index of Multiple Deprivation (IMD) Rank`),
         avgUKIMDlad = mean(uk_imd_england_score)) %>%
  ungroup() %>% 
  group_by(TTWA11CD) %>% 
  mutate(avgIMDttwa = mean(`Index of Multiple Deprivation (IMD) Rank`, na.rm =T),
         avgUKIMDttwa = mean(uk_imd_england_score)) %>%
  ungroup() %>% 
  group_by(MSOA11CD) %>% 
  mutate(avgIMDmsoa = mean(`Index of Multiple Deprivation (IMD) Rank`),
         avgUKIMDmsoa = mean(uk_imd_england_score))



### join two together usin lsoa as key
msoa_stp <- left_join(msoa_lsoa, lsoa_ccg, by = "LSOA11CD")



### there is a slight problem here because 17 MSOAs are in different STPs
### for the purpose of this analysis we will assign each MSOA to the STP/
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
  select(MSOA11CD, MSOA11NM, RGN11CD, RGN11NM,STP19CD, STP19NM, LAD17CD, LAD17NM, TTWA11CD, TTWA11NM, 
         avgIMDreg, avgIMDlad, avgIMDmsoa, avgIMDstp, avgIMDttwa, avgUKIMDreg, avgUKIMDlad, avgUKIMDmsoa, avgUKIMDstp, avgUKIMDttwa) %>% 
  filter(RGN11NM != "Scotland")


### Join with MSOA deaths data
mort <- read_csv("MSOAmort.csv")
mortareas <- left_join(mort,areas, by = c("cd"  = "MSOA11CD"))

#### add care home data
care_homes <- st_read("Geolytix_UK_Care_Homes_2020.shp")
#### transform to british national grid to match with msoa data
care_homes <- st_transform(care_homes,27700)
### read in msoa shapefile
msoa <- st_read("Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp")
### join each care home with an msoa
data <- st_join(care_homes, msoa, join = st_within)

### count how many care home are in each msoa
care_home_msoa <- data %>% 
  as_tibble() %>% 
  group_by(msoa11cd) %>% 
  tally()

#### join count in each msoa back to msoa data
care_home_msoa <- left_join(msoa, care_home_msoa, by = "msoa11cd") %>% 
  as_tibble() %>% 
  select(msoa11cd,n) %>% 
  rename(MSOA11CD = msoa11cd,
         care_homes = n) %>% 
  ### msoas with no care homes currently read NA so replace this with 0
  replace(is.na(.), 0)

#### join this with the rest of the data
mortareas <- left_join(mortareas, care_home_msoa, by = c("cd" = "MSOA11CD"))

### write to csv
write_csv(mortareas, "model_data_ukimd.csv")

