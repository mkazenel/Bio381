# Homework 8 Notes
# 8 March 2017
# MRK


## LINEAR REGRESSION ##
# data
xVar <- 1:10
yVar <- runif(10)
dataFrame <- data.frame(xVar,yVar)

# model
regModel <- lm(yVar~xVar,data=dataFrame)

# model output
print(regModel)
print(summary(regModel))

# plot
plot(y=dataFrame$yVar,x=dataFrame$xVar,pch=21,bg="lightblue",cex=2, main = "Linear Regression", xlab = "Number of Cats", ylab = "Probability of Going Crazy") # pch = parameters related to each point, bg = background for points, cex = size of points
abline(regModel)


## ANOVA ##
# data
xVar <- as.factor(rep(c("Control","Heated","Cooled"),each=5)) # repeating those three treatments, five times each
yVar <- c(rgamma(10,shape=5,scale=5),rgamma(5,shape=5,scale=10)) 
dataFrame <- data.frame(xVar,yVar)

# model
anovaModel <- aov(yVar~xVar,data=dataFrame)

# model output
print(anovaModel)
summary(anovaModel)

# plot
boxplot(yVar~xVar,data=dataFrame,col=c("grey","thistle","orchid"))



## CONTINGENCY TABLE ANALYSIS ##
# data
vec1 <- c(50,66,22) # count data
vec2 <- c(120,22,30)
dataMatrix <- rbind(vec1,vec2)
rownames(dataMatrix) <- c("Cold","Warm")
colnames(dataMatrix) <-c("Aphaenogaster",
                         "Camponotus",
                         "Crematogaster")



# model + model output
print(chisq.test(dataMatrix))

# plot
mosaicplot(x=dataMatrix,
           col=c("goldenrod","grey","black"),
           shade=FALSE)
barplot(height=dataMatrix,
        beside=TRUE,
        col=c("cornflowerblue","tomato"))

# expected data counts
chisq.test(dataMatrix)$expected
# verify that these are actually the expected counts
(sum(dataMatrix[,1])*sum(dataMatrix[1,]))/sum(dataMatrix)

# compare expected vs. observed visually
par(mfrow = c(2,1))
expected <-as.matrix(chisq.test(dataMatrix)$expected)
barplot(height=expected,
        beside=TRUE,
        col=c("cornflowerblue","tomato"), main = "Expected")
barplot(height=dataMatrix,
        beside=TRUE,
        col=c("cornflowerblue","tomato"), main = "Observed")



## LOGISTIC REGRESSION ##
# data
xVar <- rgamma(n=20,shape=5,scale=5)
yVar <- rbinom(n=20,size=1,p=0.5) # gives ones and zeroes
dataFrame <- data.frame(xVar,yVar)

# model
logRegMod <- glm(yVar ~ xVar,
                 data=dataFrame,
                 family=binomial(link="logit"))
# model output
print(logRegMod)
summary(logRegMod)

# plot
par(mfrow=c(1,1))
plot(x=dataFrame$xVar, y=dataFrame$yVar,pch=21,bg="tan",cex=2.5)
curve(predict(logRegMod,data.frame(xVar=x),type="response"),add=TRUE,lwd=2) # predicting based on logistic regression model
