###################################################################################################
# Program Description: Demonstrate bootstrap concept in R
# Date and version: 14th July 2016 v1
# Email: ranjodhsingh@gmail.com
###################################################################################################

rm(list=ls())

x <- seq(-3,3,0.01)	# regressor
u <- rnorm(length(x),0,2)	# normal errors
y <- 1.5 + 3*x + u	# simulated dependant variable

plot(x,y)	

fit <- lm(y~x)

resi <- fit$residuals
replicates <- 1000
results <- array(0,dim=c(replicates,2))
for (i in 1:replicates) {
	newy <- y + sample(resi,replace=T)
	repfit <- lm(newy ~ x)
	results[i,] <- repfit$coefficients
}




