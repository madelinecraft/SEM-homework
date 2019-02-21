#1
lower <- '1.00,
	   -0.25, 1.00, 
	   0.11, -0.14, 1.00,
	   0.25, -0.22, 0.21, 1.00,
	   0.18, -0.15, 0.19, 0.53, 1.00'
0.33^2
0.62^2
1.00^2
0.54^2
0.47^2
var <- c(0.11, 0.38, 1, 0.29, 0.22)
install.packages("lavaan")
library("lavaan")
cov <- getCov( lower, names = c("RMBI", "FES", "FEQ", "DERS", "SCL"))
install.packages("propagate")
library("propagate")
cor <- cor2cov(cov, var)
library("lavaan")
mod1 <- 'SCL ~ DERS
	   DERS ~ RMBI + FES + FEQ'
fit1 <- sem(mod1,  sample.cov=cov,  sample.nobs=676)
summary(fit1)
summary(fit1, standardized=TRUE)
#2
y <- inspect(fit1, "sampstat")$cov[5,]
y
#3
z <- inspect(fit1, "sigma")[5,]
z
#4
y-z
#5
resid(fit1)$cov[5,]
#6
mod2 <- 'SCL ~ DERS + RMBI + FES + FEQ
	   DERS ~ RMBI + FES + FEQ'
fit2 <- sem(mod2,  sample.cov=cov,  sample.nobs=676)
summary(fit2)
#7
lower <- '1.00,
	   0.16, 1.00, 
	   0.61, 0.23, 1.00,
	   -0.46, -0.16, -0.06, 1.00,
	   0.34, 0.03, 0.50, 0.06, 1.00
	   -0.17, -0.14, 0.09, 0.49, 0.45, 1.00
	   0.18, 0.08, 0.28, -0.07, 0.37, 0.11, 1.00'
1.22^2
1.74^2
1.38^2
1.42^2
1.78^2
1.93^2
1.87^2
var <- c(1.49, 3.03, 1.90, 2.02, 3.17, 3.72, 3.50)
cor <- getCov( lower, names = c("mc", "ma", "ms", "ws", "msc", "wsc", "aae"))
cov <- cor2cov(cor, var)
cov
inspect(fit1, "sigma")
mod1 <- 'aae ~ msc + wsc
	   msc ~ ms
	   wsc ~ ws
	   ms ~ mc + ma
	   ws ~ ma + mc
	   ms ~~ ws
	   msc ~~ wsc'
fit1 <- sem(mod1,  sample.cov=cov,  sample.nobs=105, std.lv=TRUE, fixed.x=FALSE)
summary(fit1)
inspect(fit1, "sigma")
inspect(fit1, "sampstat")
#15
mod2 <- 'aae ~ msc + 0*wsc
	   msc ~ ms
	   wsc ~ ws
	   ms ~ mc + ma
	   ws ~ 0*ma + mc
	   ms ~~ ws
	   msc ~~ wsc
	   mc ~~ 0*ma'
fit2 <- sem(mod2,  sample.cov=cov,  sample.nobs=105, std.lv=TRUE)
summary(fit2)
inspect(fit2, "sigma")
anova(fit1, fit2)
#16
pchisq(4.2364, 3, lower.tail=FALSE)
#18
getwd()
setwd("/Users/madelinecraft/Downloads")
mydata <- read.table ("empathy.dat.txt")
head(mydata)
g.cov = cov(mydata, use="complete")
dim(mydata)
cfa.model <- '
			pt =~ V1+V2+V3+V4+V5+V6+V7
			pt ~~ 1*pt
			ec =~ V8+V9+V10+V11+V12+V13+V14
			ec ~~ 1*ec
			pd =~ V15+V16+V17+V18+V19+V20+V21
			pd ~~ 1*pd
			'
cfa.fit <- sem(cfa.model, sample.cov = g.cov, sample.nobs = 164, std.lv = TRUE)
summary(cfa.fit, standardized = TRUE)
#20
inspect(cfa.fit, "coefficients")$psi
#21
mod_ind <- summary(cfa.fit, standardized = TRUE, modindices=T)
head(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], 10)
#V12~~V13 is the largest fit index, mi=27.09. This means that if we added a parameter for the covariance between V12 and V13, the chi-sqared difference statistic would decrease by 27.088, which is a good thing because we don't want significance in sem. These two items must be highly related over and above being apart of the same latent factor.  
#epc can be interpretted as the estimate of the parameter if that parameter were added to the model. 
#V15~~V17 is the second largest.
#ec=~V21 is the third largest.

#22
#There are particularly low loadings for V2, V4, V5, V8, and V9.

#23
cfa.model2 <- '
			pt =~ V1+V2+V3+V4+V5+V6+V7
			pt ~~ 1*pt
			ec =~ V8+V9+V10+V11+V12+V13+V14
			ec ~~ 1*ec
			pd =~ V15+V16+V17+V18+V19+V20+V21
			pd ~~ 1*pd
			V12~~V13
			V15~~V17
			'
cfa.fit2 <- sem(cfa.model2, sample.cov = g.cov, sample.nobs = 164, std.lv = TRUE)
summary(cfa.fit2, standardized=TRUE)
#df=184
#Chi-squared=337.95
#p-value=0.000
#This model does not fit well either. 
#24
inspect(cfa.fit2, "coefficients")$psi
#There were small changes. cor(ec, pt) changed from .397 to .373, cor(pd, pt) changed from -.078 to -.048, and cor(pd, ec) changed from 0.067 to 0.107.







