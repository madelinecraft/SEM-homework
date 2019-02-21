Ouwens, M.A., va Strien, T., & van Leeuwe, J.F. (2009). Possible pathways between depression, emotional and external eating. A structural equation model. Appetite, 53(2), 245-248.

#EmE: emotional eating
#ExE: external eating
#DIF: difficulty identifying feelings
#Imp: impulsivity
#Dep: depression

#Data
library("lavaan")
lower <-		'1,
					 0.43, 1,
					 0.34, 0.10, 1,
					 0.36, 0.17, 0.60, 1,
					 0.38, 0.12, 0.60, 0.68, 1'
sd <- c(.84, .61, .94, 6.36, .86)
.84^2
.61^2
.94^2
6.36^2
.86^2
var <- c(0.7056, 0.3721, 0.8836, 40.4496, 0.7396)
cor <- getCov(lower,  names = c("EmE", "ExE", "DIF", "Imp", "Dep"))
cor
library("propagate")
cov <- cor2cov(cor, var)
cov

#Model from figure 1
mod1 <- 'DIF ~ Dep
				Imp ~ Dep
				EmE ~ DIF + Dep + Imp
				ExE ~ Imp + Dep + DIF
				DIF ~~ Imp'
fit1 <- sem(mod1, sample.cov = cov, sample.nobs = 549, fixed.x = F)
summary(fit1, standardized = T)
fitMeasures (fit1)

#Model from figure 2
mod2 <- 'DIF ~ Dep
				Imp ~ Dep
				EmE ~ DIF + Dep + Imp
				ExE ~ Imp 
				DIF ~~ Imp
				ExE ~~ EmE'
fit2 <- sem(mod2, sample.cov = cov, sample.nobs = 549, fixed.x = F)
summary(fit2, standardized = T)
fitMeasures (fit2, c("GFI", "NFI", "RMSEA"))

#Power Analysis
popmod <-	'
					DIF ~ .656*Dep
					Imp ~ 5.029*Dep
					EmE ~ .121*DIF + .196*Dep + .019*Imp
					ExE ~ .016*Imp 	
					Dep ~~ .738*Dep
					Imp ~~ 21.706*Imp
					DIF ~~ .564*DIF
					ExE ~~ .361*ExE
					EmE ~~ .582*EmE
					DIF ~~ 1.146*Imp
					ExE ~~ .188*EmE
					'
fitmod <- 'DIF ~ Dep
				Imp ~ Dep
				EmE ~ DIF + Dep + Imp
				ExE ~ Imp 
				DIF ~~ Imp
				ExE ~~ EmE'
results <- NULL
for (i in 1:1000)	{
	data <- simulateData(popmod, sample.nobs = 549)
	data <- as.data.frame(data)
	fit <- sem(model = fitmod, data = data)
	results <- rbind(results, parameterEstimates(fit)[4,])}
head(results)
mean(results$pvalue < .05)

