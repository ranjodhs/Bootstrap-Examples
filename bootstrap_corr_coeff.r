###################################################################################################
# Program Description: Demonstrate Bootstrap technique for estimating the s.e. of a correlation coefficient
# Date and version: 21st July 2016
# Email: ranjodh.singh@gmail.com
###################################################################################################
# Load MASS library
library(MASS)

mu <- rep(0,2)
mu
sigma <- matrix(0.8,nrow=2,ncol=2) + diag(2)*2
sigma
gvr <- mvrnorm(n=10000,mu=mu,Sigma=sigma)

# check population
cov(gvr)
cor(gvr)

# We only have a sample of 2500 observations from the population
s <- 2500
y <- sample(1:s,size=s,replace=TRUE)
oursample <- gvr[y,]

# Sample estimates
cov(oursample)
cor(oursample)

# bootstrap
B <- 200                  #number of replicates
n <- nrow(oursample)      #sample size
R <- numeric(B)           #storage for replicates

#bootstrap estimate of standard error of R
for (b in 1:B) {
	#randomly select the indices
	i <- sample(1:n, size = n, replace = TRUE)
	tempsample <- oursample[i,]       #i is a vector of indices
	R[b] <- cor(tempsample)[2]
	}
	#output
	print(se.R <- sd(R))
	hist(R, prob = TRUE,col='blue')

# Same thing using the boot package
library(boot)       
obj <- boot(data = oursample, statistic = cor, R = 2500)
obj
y <- obj$t
sd(y)


