getwd()
setwd("/Users/madelinecraft/Desktop/Classes/SEM")
mydata <- read.table ("SEMhomework1.dat.txt")
head(mydata)
summary(mydata)

#3
mean(mydata$jobPerformance)
mean(mydata$Conscientiousness)
mean(mydata$Openness)
mean(mydata$Neuroticism)

var(mydata$jobPerformance)
var(mydata$Conscientiousness)
var(mydata$Openness)
var(mydata$Neuroticism)

dim(mydata)

#4
?cov
cov.m <- round(cov(mydata), digits=2)
cov.m
dim(cov.m)

#5
cor.m <- round(cor(mydata), digits=2)
cor.m

#6
.28/sqrt(1.02*1.31)
cov(mydata)[2,1]/(sd(mydata$jobPerformance)* sd(mydata$Conscientiousness))
cor(mydata$jobPerformance, mydata$Conscientiousness)

#7
mod1 <- lm(jobPerformance ~ Conscientiousness + Openness + Neuroticism, data=mydata)
summary(mod1)

#8
install.packages("lavaan")
library("lavaan")
?lavaan
mod2 <- 'jobPerformance ~ Conscientiousness + Openness + Neuroticism'
fit2 <- sem(mod2, data=mydata)
summary(fit2)

#11
inspect(fit2, "sigma")
inspect(fit2, "sampstat")
#these are the same because it is an exactly identified model

#13
(.009*.996) + (.604*.480) + (.287*-.074)

#14
fit2 <- sem(mod2, data=mydata)
summary(fit2, standardized=TRUE)

#15
?sem
dim(mydata)
fit3 <- sem(mod2, sample.cov=cov(mydata), sample.nobs=50)
summary(fit3)

#16
x <- matrix(c(.7, .7, .7), nrow=3, ncol=1)
t(x)
z <- x%*%t(x)

#17
y <- matrix(c(.51, 0, 0, 0 , .51, 0, 0, 0, .51), nrow=3, ncol=3)
z%*%y

#18
a <- matrix(c(.7, .7, .7, 0, 0, 0, 0, 0, 0,.7, .7, .7), nrow=6, ncol=2)
b <- t(a)
a%*%b

#21
d <- matrix(c(1, .3, .3, 1), nrow=2, ncol=2)
e <- a%*%d
e%*%t(a)

#22
h <- matrix(c(1, 1), nrow=2, ncol=1)
i <- t(h)%*%d
i%*%h