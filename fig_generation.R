library(data.table)
library(ggplot2)

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
# Total coefs
coefs <- rcoefs+fcoefs

# Calc number of parameter estimates in iteration
ests <- nrow(MCMC)/iter

# Calc size of variance matrix
size <- (rcoefs*(rcoefs+1))/2

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

ggsave("1bVariances.png")

### Producing and plotting correlations 
covarlist <- varlist[-variances]

