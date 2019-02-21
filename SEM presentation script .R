mydata = read.table("/Users/madelinecraft/Desktop/Classes/204B/dental.txt", header = FALSE)

names(mydata) = c("ID", "Gender", "Measure1", "Measure2", "Measure3", "Measure4")
head(mydata)
dim(mydata)

long.mydata = reshape(mydata, idvar = "ID", varying = list(3:6), v.names = "Response", timevar = "Time", direction = "long")
long.mydata = long.mydata[order(long.mydata$ID),]
head(long.mydata, 20)

#MLM
#Data in long format
library(lme4)
mod1<-lmer(Response ~ Time  + (1 + Time | ID), data = long.mydata)
summary(mod1)
#wrong estimator (REML)?
#wrong error covariance structure?

#SEM
#Data in wide format
library(lavaan)
model = 	'i =~ 1*Measure1 + 1*Measure2 + 1*Measure3 + 1*Measure4
				s =~ 0*Measure1 + 1*Measure2 + 2*Measure3 + 3*Measure4
				Measure1 ~~ label("Ve")*Measure1
				Measure2 ~~ equal("Ve")*Measure2
				Measure3 ~~ equal("Ve")*Measure3
				Measure4 ~~ equal("Ve")*Measure4'
mod2 = growth(model, data = mydata)
summary(mod2) 

######################################
#######Adding a Level 2 Predictor#########
#MLM
library(lme4)
mod3<-lmer(Response ~ Time  + Gender + (1 + Time | ID), data = long.mydata)
summary(mod3)
#wrong estimator (REML)?
#wrong error covariance structure?

#SEM
library(lavaan)
model2= 	'i =~ 1*Measure1 + 1*Measure2 + 1*Measure3 + 1*Measure4
				s =~ 0*Measure1 + 1*Measure2 + 2*Measure3 + 3*Measure4
				Measure1 ~~ label("Ve")*Measure1
				Measure2 ~~ equal("Ve")*Measure2
				Measure3 ~~ equal("Ve")*Measure3
				Measure4 ~~ equal("Ve")*Measure4
				i ~ 1 + Gender
				s ~ 1 + Gender'
mod4 = growth(model2, data = mydata)
summary(mod4) 
#vs. 
summary(mod3)

				


