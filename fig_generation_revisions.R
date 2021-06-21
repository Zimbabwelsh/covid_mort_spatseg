library(data.table)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(gridExtra)
library(viridis)

setwd("C:/Users/gg9824/Dropbox/00ESRC Fellowship/Projects/COVID19/COVID Inequalities/Final Models/Finalfinal Models/No STP")
MCMC <- fread("TTWA 2b ENGIMD output.csv")

### For calculation
# N of iterations in MCMC samplers
iter <- 500000
# N of structural levels
levels <- 3
# Name levels
levnames <- c("MSOA", "TTWA", "Region")
# N of fixed coefficients
fcoefs <- 24
# N of random coefficients
rcoefs <- 5
# Random Coefficient Names
rcoefnames <- c("Mar", "Apr", "May", "Jun", "Jul")
# Total coefs
coefs <- rcoefs+fcoefs

# Calc number of parameter estimates in iteration
ests <- nrow(MCMC)/iter

# Calc size of variance matrix minus diagonal
size <- ((rcoefs*(rcoefs+1))/2)-rcoefs

# Calc N of random estimates
rests <- size*levels
  
# Reshape data
df <- as.data.frame(matrix(MCMC$V1, iter, ests, byrow=TRUE))

# Drop final col as it only contains level 1 (constant)
df <- df[,-(ncol(df))]


## Create variance list for omission/inclusion 
varlist <- seq(ncol(df))
# Mask based on MLwiN output
# Ignore beta estimates
varlist <- varlist[-c(seq(coefs))] 
# Create mask for random effect variances
variances <- vector()
maxval=ncol(df)

# Generate function for triangle numbers to index variances
trino <- function(n){
  sapply(1:n, function(x) sum(1:x))
}

basevariances <-  trino(rcoefs)
variances <- basevariances

# Repeat for all levels
repeat{  
  variances <- append(variances, basevariances+max(variances))
  if (length(variances)==(rcoefs*levels)){
    break
  }
}

# Take just variance estimates, and calculate quantiles
quants <- c(0.025,0.05,0.50,0.95,0.975)

# Dataset of solely variance iteration estimates
rawvars <- df[,varlist[variances]]
# Take quantiles from "quants" and apply to rawvars
varests <- as.data.frame(apply(rawvars, 2, quantile, probs = quants))
varests <- varests %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value)
varests <- varests[,-1]
# Rename Cols to something sensible
colnames(varests) <- c("Lower", "Lower 0.05", "Median", "Upper 0.95", "Upper")


################# 
### MRR Calcs ###
#################


MRRests <- exp(sqrt(2*varests)*0.6745)
# Generate level-names variable and ensure factor to maintain order for plotting
MRRests$level <- rev(rep(levnames, each=rcoefs))
MRRests$level <- factor(MRRests$level, levels = levnames)
# Generate months
MRRests$Month <- rep(2+(1:rcoefs), levels)

### replace region MRR NAs with 1.0 
# MRRests$Lower <- MRRests$Lower %>% replace_na(1)#

MRRests$type <- "Non COVID-19"

ncovMRRs <- MRRests


########################
# VPC Calculations ###
#########################

### Need to extract fixed part estimates for random coefficients

raw_fixed_ests <- df[,1:rcoefs]

fixed_ests <- as.data.frame(apply(raw_fixed_ests, 2, quantile, probs = quants))
fixed_ests <- fixed_ests %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value)
fixed_ests <- fixed_ests[,-1]
colnames(fixed_ests) <- c("Lower", "Lower 0.05", "Median", "Upper 0.95", "Upper")
fixed_ests$level <- (rcoefnames)
fixed_slim <- fixed_ests %>% select(c("Median"))
lv3_slim <- varests[1:rcoefs,] %>% select(c("Median"))
lv2_slim <- varests[(rcoefs+1):(2*rcoefs),] %>% select("Median")
lv1_slim <- varests[((2*rcoefs)+1):(3*rcoefs),] %>% select("Median")

VPC_data <- cbind(fixed_slim, lv3_slim, lv2_slim, lv1_slim)
VPC_data$level <- rcoefnames

VPC_colnames <- c("expected", rev(levnames), "month")
colnames(VPC_data) <- VPC_colnames

#Construct Marginal expectation (in average context)
VPC_data$mu_1 <- exp(VPC_data$expected + (VPC_data$Region/2) + (VPC_data$TTWA/2) + (VPC_data$MSOA/2))

#Construct Level-3 component of total variance
VPC_data$lv3var <- ((VPC_data$mu_1)^2)*(exp(VPC_data$Region)-1)

#Construct Level-2 component of total variance
VPC_data$lv2var <- ((VPC_data$mu_1)^2)*(exp(VPC_data$Region))*(exp(VPC_data$TTWA)-1)

#Construct level-1 component of total variance
VPC_data$lv1var <- (VPC_data$mu_1)+((VPC_data$mu_1)^2)*exp(VPC_data$Region + VPC_data$TTWA)*(exp(VPC_data$MSOA)-1)

# Relative contribution of Level 3 variance 
VPC_data$VPC3 <- (VPC_data$lv3var)/(VPC_data$lv3var+VPC_data$lv2var+VPC_data$lv1var)

#Relative contribution of Level 2 variance
VPC_data$VPC2 <- (VPC_data$lv2var)/(VPC_data$lv3var+VPC_data$lv2var+VPC_data$lv1var)

# Relative contribution of Level 1 variance
VPC_data$VPC1 <- (VPC_data$lv1var)/(VPC_data$lv3var+VPC_data$lv2var+VPC_data$lv1var)

VPC_data$totalvar <- (VPC_data$mu_1)+(((VPC_data$mu_1)^2)*(exp(VPC_data$Region + VPC_data$TTWA + VPC_data$MSOA)-1))

# write.csv(VPC_data, "VPC_data.csv")

MRRests$VPC <- c(VPC_data$VPC3, VPC_data$VPC2, VPC_data$VPC1)

ncovVPCs <- MRRests

#########################
### Correlation Calcs ###
#########################

covarlist <- varlist[-variances]
rawcovars <- df[covarlist]

# Create first list to index first variances for correlation calculation
# Function to create repeating ascending list (1,1,2,1,2,3...)
cvseq <- function(n){
  seq(1, n, 1) %>%
    map(~seq(1, ., 1)) %>%
    unlist()
}

diag <- trino(rcoefs)

# Covarseq gives the first variance location for each covariance 
covarseq1 <- diag[cvseq(rcoefs-1)]

# Add total size of v/cv matrix and repeat until list is 40 elements 
repeat{
  covarseq1 <- append(covarseq1, tail(covarseq1, size)+(size+rcoefs))
  if (length(covarseq1)==rests){
    break
  }
}

covarseq1 <- covarseq1+coefs

# Create second list to index second variances for correlation calculation

# Take elements of triangle numbers in form 1, 2, 2, 3, 3, 3...
covarseq2 <- diag[(rep(1:(rcoefs-1), 1:(rcoefs-1))+1)]

# Extend using same approach as above
repeat{
  covarseq2 <- append(covarseq2, tail(covarseq2, size)+(size+rcoefs))
  if (length(covarseq2)==rests){
    break
  }
}

covarseq2 <- covarseq2+coefs


rawcorrs <- as.data.frame(matrix(0, iter, rests))

for (i in 1:length(covarlist)){
  rawcorrs[i] <- df[,covarlist[i]]/(sqrt(df[,covarseq1[i]])*sqrt(df[,covarseq2[i]]))
}

corrests <- as.data.frame(apply(rawcorrs, 2, quantile, probs = quants))
corrests <- corrests %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value)
corrests <- corrests[,-1]


############# To here works for generating correlation estimates without variances 

colnames(corrests) <- c("Lower", "Lower 0.05", "Median", "Upper 0.95", "Upper")
# Generate level-names variable and ensure factor to maintain order for plotting
corrests$level <- rev(rep(levnames, each=size))
corrests$level <- factor(corrests$level, levels = levnames)


corrests$month1 <- factor(rep(c("March","March","April","March","April","May","March","April","May","June"),levels), 
                          levels = c("March", "April", "May", "June", "July"))
corrests$month2 <- factor(rep(c("April","May","May","June","June","June","July","July","July","July"),levels),
                          levels = c("March", "April", "May", "June", "July"))
### crreate column for type of death
corrests$type <- "Non COVID-19"

# Save out correlation estimates for COVID/NonCOVID
ncovcorrs <- corrests
# write.csv(ncovcorrs, "TTWA 2borrs.csv")


##########################
### Combined MRR plots ###
##########################

MRRestsMod1 <- bind_rows(list(covMRRs, ncovMRRs))

# Produce ggplot graphic
areavars <- ggplot(data=MRRestsMod1, aes(x=Month,y=Median, colour=level))+
  geom_line()+
  scale_color_manual(values=c("#1b9e77","#d95f02", "#7570b3"))+
  ylim(1,8)+
  geom_ribbon(aes(ymin=Lower, ymax=Upper), linetype=3, alpha=0.1) +
  facet_grid(type~level)

areavars

ggsave("Mod2ENGIMDMRRs.tiff")


##########################
### Combined VPC plots ###
##########################

VPCests <- bind_rows(list(covVPCs, ncovVPCs))

VPCplot <- ggplot(data=VPCests, aes(x=Month, y=VPC, colour=level)) +
  geom_line(size=1)+
  ylim(0,1) +
  ylab("Variance Partitioning Coefficient") +
  scale_x_continuous(labels=c("3" = "Mar", "4" = "Apr", "5" = "May",
                              "6" = "Jun", "7" = "Jul")) +
  facet_grid(type~level)

VPCplot

ggsave("Mod2ENGIMDVPCplot.tiff")


###########################
### Combined Corr plots ###
###########################

### combine covid and other deaths together
correstsMod2 <-  bind_rows(list(covcorrs, ncovcorrs))

### make the plot
ggplot(data = correstsMod2,
       aes(month1,
           fct_rev(month2), 
           fill=Median, 
           label=sprintf("%.2f", round(Median,2)))) +
  geom_tile() +
  labs(x = NULL,
       y = NULL,
       fill = "Pearson's\nCorrelation", 
       title="Multiscale monthly mortality correlations") + 
  ### add colour scale
  scale_fill_gradient2(mid="#FBFEF9",low="#A63446",high="#0C6291", limits=c(-1,1)) +
  ### add in the numbers on the tiles
  geom_text() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme_minimal() +
  ### change text sizes
  theme(axis.text=element_text(size=12, colour = "black"), 
        strip.text = element_text(size=12),
        legend.title=element_text(size=12),
        plot.title = element_text(size = 16)) +
  ### create 2 by 4 plot with each level as row starting with region (fct_rev)
  facet_grid(fct_rev(level)~type) 

ggsave("Mod2ENGIMDMortCorrs.tiff")
 



