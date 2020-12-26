cell_rad <- read.csv("http://users.stat.ufl.edu/~winner/data/cellphnrad.csv")
attach(cell_rad);    names(cell_rad)

# Part 1: Summarizing Data

mean(rads)                    # Compute the mean for the variable rads
sd(rads)                      # Compute the sample standard deviation of rads
(lq <- quantile(rads,.25))    # lq is assigned the 25th percentile
(uq <- quantile(rads,.75))    # uq is assigned the 75th percentile
(med.rads <- median(rads))    # med.rads is assigned the median
(iqr <- uq - lq)              # iqr is assigned the interquartile range

tapply(rads, brand, mean)     # Compute the mean seperately for each brand
tapply(rads, brand, median)   # Compute the median seperately for each brand
tapply(rads, brand, sd)       # Compute the Std Dev seperately for each brand

# Obtain histogram, freq=F makes it a relative frequency histogram 
# (heights sum to 1)
### win.graph "holds plot" in a window and will not overwrite with 
###   subsequent plots
win.graph(height=5.5, width=7.0)    
hist(rads,main="",xlab="Radiation",freq=F)
lines(density(rads))

# Part 2: Probability

n <- length(rads)      # Total number of cell phone models

sum(rads >= 1.0)/n     # Prob(rads>=1)

rads.u.8 <- ifelse(rads <= .80,1,0)            # rads <= 0.8
rads.g.7 <- ifelse(rads >= .70,1,0)            # rads >= 0.7
rads.u.med <- ifelse(rads <= median(rads),1,0) # rads <= median(rads)


# Cross-tabulation (aka contingency table) for each brand by criteria
(brand.rad.8 <- table(brand,rads.u.8))        # Brand=Rows   0,1=Columns
(brand.rad.7 <- table(brand,rads.g.7))
(brand.rad.med <- table(brand,rads.u.med))

# Frequencies converted to various types of proportions
# Event: Rad <= 0.80
prop.table(brand.rad.8,1)                     # P(0|Brand), P(1|Brand)
margin.table(brand.rad.8,1)/sum(brand.rad.8)  # P(Brand) 
prop.table(brand.rad.8,2)                     # P(Brand|0), P(Brand|1)
margin.table(brand.rad.8,2)/sum(brand.rad.8)  # P(0), P(1)
brand.rad.8/sum(brand.rad.8)                  # P(0,Brand), P(1,Brand)

# Event: Rad >= 0.70
prop.table(brand.rad.7,1)                     # P(0|Brand), P(1|Brand)
margin.table(brand.rad.7,1)/sum(brand.rad.7)  # P(Brand) 
prop.table(brand.rad.7,2)                     # P(Brand|0), P(Brand|1)
margin.table(brand.rad.7,2)/sum(brand.rad.7)  # P(0), P(1)
brand.rad.7/sum(brand.rad.7)                  # P(0,Brand), P(1,Brand)

# Event: Rad <= Median
prop.table(brand.rad.med,1)                       # P(0|Brand), P(1|Brand)
margin.table(brand.rad.med,1)/sum(brand.rad.med)  # P(Brand)
prop.table(brand.rad.med,2)                       # P(Brand|0), P(Brand|1)
margin.table(brand.rad.med,2)/sum(brand.rad.med)  # P(0), P(1)
brand.rad.med/sum(brand.rad.med)                  # P(0,Brand), P(1,Brand)


# Part 4: Sampling Distribution of Ybar

# POPULATION Mean and Variance saved to variables
MU.rads <- mean(rads)
SIGMA2.rads <- (length(rads)-1)*var(rads)/length(rads)

sampmean <- numeric(10000)   # Initialize a vector to save sample means

# 10000 Random Samples of size n=36 (Sampling without replacement)
for (i in 1:10000) {
ransamp <- sample(rads,36)            # Take a sample of n=36 from the rads
sampmean[i] <- mean(ransamp)              # Compute sample mean and save
}

# Summary statistics for the 10000 sample Means 
mean(sampmean)
var(sampmean)
sd(sampmean)
min(sampmean)
max(sampmean)


# Histogram of Sampling Distribution of Y-bar with Normal Super-Imposed 
win.graph(height=5.5, width=7.0)
hist(sampmean[(sampmean > 0.80)&(sampmean<1.30)],freq=F,breaks=seq(0.80,1.30,0.0125),ylim=c(0,8),
xlab="ybar",main="Sampling Distribution of Y-bar")
lines(seq(0.8,1.30,0.001),
dnorm(seq(0.8,1.30,0.001),MU.rads,sqrt(SIGMA2.rads/36)))
