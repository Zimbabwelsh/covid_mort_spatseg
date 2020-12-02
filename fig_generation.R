library(data.table)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(gridExtra)

setwd("C:/Users/gg9824/Dropbox/00ESRC Fellowship/Projects/COVID19/COVID Inequalities/Final Models")
MCMC <- fread("1b output.csv")

### For calculation
# N of iterations in MCMC samplers
iter <- 500000
# N of structural levels
levels <- 4
# Name levels
levnames <- c("MSOA", "LAD", "STP", "Region")
# N of fixed coefficients
fcoefs <- 4
# N of random coefficients
rcoefs <- 5
# Random Coefficient Names
rcoefnames <- c("March", "April", "May", "June", "July")
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

variances <-  trino(rcoefs)

# Repeat for all levels
repeat{  
  variances <- append(variances, variances+max(variances))
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
# 
MRRests <- exp(sqrt(2*varests)*0.6745)
# Generate level-names variable and ensure factor to maintain order for plotting
MRRests$level <- rev(rep(levnames, each=rcoefs))
MRRests$level <- factor(MRRests$level, levels = levnames)
# Generate months
MRRests$Month <- rep(3:7, levels)

# Produce ggplot graphic
areavars <- ggplot(data=MRRests, aes(x=Month,y=Median, colour=level))+
  geom_line()+ 
  ylim(1,4)+
  geom_ribbon(aes(ymin=Lower, ymax=Upper), linetype=3, alpha=0.1) +
  facet_grid(~level)

areavars

ggsave("1bVariances.png")



### Producing and plotting correlations 
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

##########
# Combine lists to calculate correlations for each covariance estimate and store in rawcorrs

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


############# To here seems to work BUT some nonsensical correlations (V16) - check code in morning.

colnames(corrests) <- c("Lower", "Lower 0.05", "Median", "Upper 0.95", "Upper")
# Generate level-names variable and ensure factor to maintain order for plotting
corrests$level <- rev(rep(levnames, each=size))
corrests$level <- factor(corrests$level, levels = levnames)

# Generate list of matrices to iterate over for output
covarmatlist <- vector("list", length(levels))

# Generate lower-triangle covariance matrix, without diagonal
for (i in 1:levels){
  covarmat <- matrix(0,rcoefs,rcoefs)
  covarmat[upper.tri(covarmat)] <- corrests$Median[((i-1)*size)+1:(size*i)]
  # diag(covarmat) <- varests$Median[(((i-1)*rcoefs)+1):(rcoefs*i)]
  rownames(covarmat) <- rcoefnames
  covarmatlist[[i]] <- t(covarmat)
}

for (element in covarmatlist){
  corrplot(element, type = "lower", is.corr=TRUE)
}

 par(mfrow=c(4,1))
 corrplot(covarmatlist[[1]], type = "lower", is.corr=TRUE)
 title("Region", line = -2, adj=0.6)
 corrplot(covarmatlist[[2]], type = "lower", is.corr=TRUE)
 title("STP", line = -2, adj=0.6)
 corrplot(covarmatlist[[3]], type = "lower", is.corr=TRUE)
 title("LAD", line = -2, adj=0.6)
 corrplot(covarmatlist[[4]], type = "lower", is.corr=TRUE)
 title("MSOA", line = -2, adj=0.6)

 
 
 ### CYS CODE
# ## Create empty matrix
# rawcorrs <- matrix(0, nrow(rawcovars), ncol(rawcovars))
# 
# ## Generate sequences for looping
# start_var <- seq(1, ncol(rawvars), by=rcoefs)
# end_var <- seq(rcoefs, ncol(rawvars), by=rcoefs)
# 
# start_covar <- seq(1,ncol(rawcovars), by=((rcoefs*(rcoefs+1))/2)-rcoefs)
# end_covar <- seq(((rcoefs*(rcoefs+1))/2)-rcoefs,ncol(rawcovars), by=((rcoefs*(rcoefs+1))/2)-rcoefs)
# 
# ptm <- proc.time()
# 
# for (i in 1:nrow(rawcovars)){
#   for(j in 1:levels){
#     
#     cor_mat <- matrix(0,rcoefs,rcoefs)
#     
#     var_mat <- diag(rawvars[i,start_var[j]:end_var[j]], rcoefs, rcoefs)
#     
#     var_mat[upper.tri(var_mat, diag = F)] <- as.numeric(rawcovars[i,start_covar[j]:end_covar[j]])
#     
#     rand_cor <- cov2cor(t(var_mat))
#     
#     cor_byrow <- t(rand_cor)
#     
#     rawcorrs[i,start_covar[j]:end_covar[j]] <- cor_byrow[upper.tri(cor_byrow, diag = F)]
#     
#   }
# }
# 
# RunTime<-proc.time() - ptm


# plots = lapply(covarmatlist, function(.x) corrplot(covarmat, type = "lower", is.corr=TRUE))
# 
# 

# 
# cor_mat<-matrix(0, nrow(rawcovars), ncol(rawcovars))
# rawcovars <- (df[covarlist])
# 
# for(k in 1:ncol(rawcolvars))
#   for(j in 1: ncol(rawcovars)){
#     {
#       cor_mat[,1]<-
#     }
#   }
# {
#   
# }
# rawcovars[1,1]
# 
# 
# for (i in 1:ncol(rawcovars)){
#     n <- rawcovars[,[i]]/sqrt(rawvars[paste("V",covarlist[i], sep="")])*sqrt(rawvars[,3])
#       df$corr[i] <- n
# }
# 
# 
# var1 <- list()
# for (i in 1:5){
#   for (j in 1:i){
#     append(j)
#   }
# }
# repeat{  
#   variances <- append(variances, variances+max(variances))
#   if (length(variances)==(rcoefs*levels)){
#     break
#   }
# }
