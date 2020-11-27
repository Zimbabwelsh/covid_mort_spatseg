library(data.table)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(gridExtra)

setwd("C:/Users/gg9824/Dropbox/00ESRC Fellowship/Projects/COVID19/COVID Inequalities/Final Models")
MCMC <- fread("1a output.csv")

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

# Generate vector of variance locations for one level
for (i in 1:rcoefs){
  val <- sum(seq(1:i))
  variances[[length(variances)+1]] <- val
}
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
varests <- transpose(as.data.frame(apply(rawvars, 2, quantile, probs = quants)))
# Rename Cols to something sensible
colnames(varests) <- c("Lower", "Lower 0.05", "Median", "Upper 0.95", "Upper")
# Generate level-names variable and ensure factor to maintain order for plotting
varests$level <- rev(rep(levnames, each=rcoefs))
varests$level <- factor(varests$level, levels = levnames)
# Generate months
varests$Month <- rep(3:7, levels)

# Produce ggplot graphic
areavars <- ggplot(data=varests, aes(x=Month,y=Median, colour=level))+
  geom_line()+ 
  ylim(0,2)+
  geom_ribbon(aes(ymin=Lower, ymax=Upper), linetype=3, alpha=0.1) +
  facet_grid(~level)

areavars

ggsave("1aVariances.png")

### CYS CODE

### Producing and plotting correlations 
covarlist <- varlist[-variances]
rawcovars <- df[covarlist]

## Create empty matrix
rawcorrs <- matrix(0, nrow(rawcovars), ncol(rawcovars))

## Generate sequences for looping
start_var <- seq(1, ncol(rawvars), by=rcoefs)
end_var <- seq(rcoefs, ncol(rawvars), by=rcoefs)

start_covar <- seq(1,ncol(rawcovars), by=((rcoefs*(rcoefs+1))/2)-rcoefs)
end_covar <- seq(((rcoefs*(rcoefs+1))/2)-rcoefs,ncol(rawcovars), by=((rcoefs*(rcoefs+1))/2)-rcoefs)

ptm <- proc.time()

for (i in 1:nrow(rawcovars)){
  for(j in 1:levels){
    
    cor_mat <- matrix(0,rcoefs,rcoefs)
    
    var_mat <- diag(rawvars[i,start_var[j]:end_var[j]], rcoefs, rcoefs)
    
    var_mat[upper.tri(var_mat, diag = F)] <- as.numeric(rawcovars[i,start_covar[j]:end_covar[j]])
    
    rand_cor <- cov2cor(t(var_mat))
    
    cor_byrow <- t(rand_cor)
    
    rawcorrs[i,start_covar[j]:end_covar[j]] <- cor_byrow[upper.tri(cor_byrow, diag = F)]
    
  }
}

RunTime<-proc.time() - ptm

covarests <- transpose(as.data.frame(apply(rawcorrs, 2, quantile, probs = quants)))

colnames(covarests) <- c("Lower", "Lower 0.05", "Median", "Upper 0.95", "Upper")
# Generate level-names variable and ensure factor to maintain order for plotting
covarests$level <- rev(rep(levnames, each=size))
covarests$level <- factor(covarests$level, levels = levnames)

# Generate list of matrices to iterate over for output
covarmatlist <- vector("list", length(levels))

for (i in 1:levels){
  covarmat <- matrix(0,5,5)
  covarmat[upper.tri(covarmat)] <- covarests$Median[((i-1)*10)+1:(10*i)]
  covarmatlist[[i]] <- t(covarmat)
}

for (element in covarmatlist){
  corrplot(element, type = "lower", is.corr=TRUE)
}


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
