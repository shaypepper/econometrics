##Overall packages to install
library(ggplot2)
library(dplyr)
library(stargazer)
library(AER)


##OLS - LM: Give the expected value of Y given the values of X under restriction that 
##this expected value is a linear function
#model
modelname <- lm(Y ~ X1 + X2 + ....)
#plot
ggplot(data = modelname, aes(x = xaxisname, y = yaxisname))
#confidence interval
confint(modelname, level = 0.95)
#Regression residuals
ehat <- resid(modelname)
summary(ehat)


##Logit/Probit: Y variable is binomial
model_logit <- glm(Y ~ X1 + X2 +......, family = binomial, data = data_use1)
model_probit <- glm(Y ~ X1 + X2 +......, family = binomial (link = 'probit'), data = data_use1)
#model coefficients (odds ratio in logistic regression)
KM <- exp(model_logit$coefficients)
plot(KM)
#regression marginal effect?
coef(model_logit)
coef(model_probit)
# Estimate

val <- b0 + X1 * b1 + X2 * b2 + ...
pred_logit <- 1 / (1 - exp( -(val) ))


val <- b0 + X1 * b1 + X2 * b2 + ...
pred_probit <- pnorm(val)


## Multilevel Modeling
model_multi <- lmer(Y ~ X1 + X2 + ... + as.factor(W1) + (1 | as.factor(W2)), dat_use)
summary(model_multi)


## knn
good.obs <- complete.cases(dat_use)
y.data  <- subset(dat_use, good.obs)

set.seed(17)
NN_obs <- sum(good.obs)
train.obs <- (runif(NN_obs) < 0.8)

train.data <- subset(y.data, train.obs)
test.data  <- subset(y.data, !train.obs)
cl.data    <- y.data$bottom25[train.obs]
true.data  <- y.data$bottom25[!train.obs]

predicted.ent <- knn(train = train.data[-1], 
                     test = test.data[-1], 
                     cl = cl.data,
                     k = 3)
n.correctly.predicted <- sum(predicted.ent == true.data)
correct.rate <- n.correctly.predicted / length(predicted.ent)
print(correct.rate)


## Factor Analysis and Principle component analysis - to suss out explanatory variables that move together
prcomp1 <- prcomp(~ x1 + x2 + x3, data = dat_use)
summary(prcomp1)


##Quantile Regression (runs long if data not cut): Gives expected quartile of Y given X with assumption
## of linearity of the function
#package
library(quantreg)
#Cut down data example
select1 <- (runif(length(dat_acs2015$YEAR)) < 0.1)
dat_acs2015_small <- subset(dat_acs2015,select1)
detach(dat_acs2015)
attach(dat_acs2015_small)
p_tiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
#Test/model
quantreg1 <- rq(AGE ~ EDUC,  tau=p_tiles, data = dat_acs2015_small)


##Non-parametric Regression: Give expected value of Y given X, subject to smoothness constraint
## not linearity but something else
library(np)
#restrict the "main" X
restrict2 <- as.logical(dat_use$edu_bach)
data3 <- subset(dat_use, restrict2)
#restrict the Y
NN <- length(data2$WSAL_VAL)
restrict3 <- as.logical(round(runif(NN, min = 0, max = 0.75)))
data4 <- subset(data3, restrict3)
#run "restricted" model
model_nonparm <- npreg(WSAL ~ Age, regtyp = "11", bwmethod = "civ.aic", 
                       gradients = TRUE, data = data4)
summary(model_nonparm)
npsightest(model_nonparm)
plot(data4$Age, data4$WSAL_VAL, xlab = "age", ylab = "wage", cex = .1)
lines(data4$Age, fitted(model_nonparm), lyt = 1, col = "blue")

##LARS/Lasso: selects variables "important" in predicting, good for small n, 
#package
library(lars)
#select your x's
x_varb <- cbind(Age, I(Age^2), femal, AfAm, Asian, Amindian, race_other,....)
#make a scaling function to scale all x's
stand_Z <- function(X) {
  +     rval <- matrix(data = NA, nrow = nrow(X), ncol = ncol(X))
  +     for(j in 1:ncol(X)) rval[,j] <- (X[,j] - mean(X[,j]))/sd(X[,j])
  +     return(rval)}
#scale your x's using above function, keep names
x_varb_dm <- stand_Z(x_varb)
dimnames(x_varb_dm) <- dimnames(x_varb)
#run model
model_lars <- lars(x_varb_dm, Y)


## Spike and Slab: possibly more explanitory variables than observations, df problem
## runs long when you incorporate lots of interactions like ^2 or other variables
library(spikeslab)
#model
model_spikeslab <- spikeslab(Y ~ (X1 + X2 +...)^2 + (X3 + X4 +....)*X5, data = dat_use)
summary(model_spikeslab)
print(model_spikeslab)


##Trees and Forests: use to help find "features"/explanatory variables to classify an outcome
##by finding the x-variables with most reduces SSE in each stem of the tree, large data
library(rpart)
#single tree model, "class" for binary Y var, "anova" for continuous Y
model_tree <- rpart(Y ~ X1 + X2 + ...., data = data_use, method = "class")
summary(model_tree)
# O(m * n * d * log n).
#will give you a table, variable importance, and specifics on each "node"
plot(model_tree)
text(model_tree)
post(model_tree, file = "tree_1.ps", title = "Classification Tree for Topic")
varImpPlot(model_tree)

#Random Forrest model: binary Y, does multiple trees and compares predicted to actual, 
#good to compare with logit model
library(randomForest)
#  O( v * n log(n) )
model_forest <- randomForest(as.factor(Y) ~ X1 + X2 +...., data = dat_use, 
                             importance = TRUE, proximity = TRUE)
print(model_forest)
round(importance(model_forest), 2)
varImpPlot(model_forest)