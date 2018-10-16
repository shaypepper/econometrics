# cps_1.R
# looking at CPS 2013 data
# uses file from dataferret download of CPS March 2013 supplement, downloaded June 12 2014
# accompanying lecture notes for KFoster class ECO B2000 at CCNY

require(stargazer)
require(AER)

rm(list = ls(all = TRUE))
setwd("C:\\Users\\Kevin\\Documents\\CCNY\\data for classes\\CPS_Mar2013")
load("cps_mar2013.RData")

attach(dat_CPSMar2013)
# use prime-age,fulltime, yearround workers
use_varb <- (Age >= 25) & (Age <= 55) & work_fullt & work_50wks
dat_use <- subset(dat_CPSMar2013,use_varb) # 47,550 out of 202,634 obs

detach(dat_CPSMar2013)

attach(dat_use) # just prime-age,fulltime, yearround workers

# always a good idea to get basic stats of all of the variables in your regression to see if they make sense
summary(WSAL_VAL)
summary(Age)
summary(female)
summary(AfAm)
summary(Asian)
summary(Amindian)
summary(race_oth)
summary(Hispanic)
summary(educ_hs)
summary(educ_smcoll)
summary(educ_as)
summary(educ_bach)
summary(educ_adv)
summary(married)
summary(divwidsep)
summary(union_m)
summary(veteran)
summary(immigrant)
summary(immig2gen)

dat_noZeroWage <- subset(dat_use,(WSAL_VAL > 0))
detach(dat_use)
attach(dat_noZeroWage)

model1 <- lm(WSAL_VAL ~ Age + female + AfAm + Asian + Amindian + race_oth 
             + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
             + married + divwidsep + union_m + veteran + immigrant + immig2gen, data = dat_noZeroWage)

summary(model1)
coeftest(model1)
#sometimes log form is preferred
model1a <- lm(log(WSAL_VAL) ~ Age + female + AfAm + Asian + Amindian + race_oth 
             + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
             + married + divwidsep + union_m + veteran + immigrant + immig2gen, data = dat_noZeroWage)
summary(model1a)
coeftest(model1a)

log(mean(WSAL_VAL))
mean(log(WSAL_VAL))

stargazer(model1,model1a, type = "text")

model1b <- lm(WSAL_VAL ~ Age + female + AfAm + Asian + Amindian + race_oth 
             + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv +
               female:(educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv) 
             + married + divwidsep + union_m + veteran + immigrant + immig2gen, data = dat_noZeroWage)

summary(model1b)
stargazer(model1b, type = "text")

model1c <- lm(WSAL_VAL ~ Age + female + AfAm + Asian + Amindian + race_oth 
              + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
              + married + divwidsep + union_m + veteran + immigrant + immig2gen
              + female:(Age + AfAm + Asian + Amindian + race_oth 
              + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
              + married + divwidsep + union_m + veteran + immigrant + immig2gen), data = dat_noZeroWage)

summary(model1c)
stargazer(model1c, type = "text")


det_ind <- as.factor(A_DTIND)
det_occ <- as.factor(A_DTOCC)

model1d <- lm(WSAL_VAL ~ Age + female + AfAm + Asian + Amindian + race_oth 
             + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
             + married + divwidsep + union_m + veteran + immigrant + immig2gen
             + det_ind + det_occ, data = dat_noZeroWage)

summary(model1d)


detach(dat_noZeroWage)
attach(dat_use)
# ^^ yes there are more elegant ways to do that, avoiding attach/detach - find them!

# for heteroskedasticity consistent errors
require(sandwich)
require(lmtest)

coeftest(model1,vcovHC)

# jam nonlinear into linear regression
model2 <- lm(WSAL_VAL ~ Age + I(Age^2) + I(Age^3)+I(Age^4)
             + female + AfAm + Asian + Amindian + race_oth 
             + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
             + married + divwidsep + union_m + veteran + immigrant + immig2gen)

model3 <- lm(WSAL_VAL ~ Age + I(Age^2) 
             + female + I(female*Age) + I(female*(Age^2)) 
             + AfAm + Asian + Amindian + race_oth 
             + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
             + married + divwidsep + union_m + veteran + immigrant + immig2gen)
# could do this with "update" function instead
summary(model2)
summary(model3)
stargazer(model1,model2,model3,type = "text")

# the ANOVA function is flexible - can compare nested models
anova(model1,model2,model3)

# Applied Econometrics in R suggests also spline and kernel estimators, we might get to that later

# subset in order to plot...
NNobs <- length(WSAL_VAL)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like 4000 obs
dat_graph <-subset(dat_use,graph_obs)  

plot(WSAL_VAL ~ jitter(Age, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.02), data = dat_graph)
# ^^ that looks like crap since Wages are soooooooo skew!  So try to find ylim = c(0, ??)
plot(WSAL_VAL ~ jitter(Age, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.02), ylim = c(0,150000), data = dat_graph)

# to plot the predicted values might want to do something like, lines(fitted.values(model2) ~ Age)
# but that will plot ALLLLL the values, which is 4500 too many and looks awful
# so back to this,
to_be_predicted2 <- data.frame(Age = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, 
                              Hispanic = 1, educ_hs = 0, educ_smcoll = 0, educ_as = 0, educ_bach = 1, educ_adv = 0,
                              married = 0, divwidsep =0, union_m = 0, veteran = 0, immigrant = 0, immig2gen = 1)
to_be_predicted2$yhat <- predict(model2, newdata = to_be_predicted2)

lines(yhat ~ Age, data = to_be_predicted2)

# now compare model3
to_be_predicted3m <- data.frame(Age = 25:55, female = 0, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, 
                               Hispanic = 1, educ_hs = 0, educ_smcoll = 0, educ_as = 0, educ_bach = 1, educ_adv = 0,
                               married = 0, divwidsep =0, union_m = 0, veteran = 0, immigrant = 0, immig2gen = 1)
to_be_predicted3m$yhat <- predict(model3, newdata = to_be_predicted3m)

to_be_predicted3f <- data.frame(Age = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, 
                                Hispanic = 1, educ_hs = 0, educ_smcoll = 0, educ_as = 0, educ_bach = 1, educ_adv = 0,
                                married = 0, divwidsep =0, union_m = 0, veteran = 0, immigrant = 0, immig2gen = 1)
to_be_predicted3f$yhat <- predict(model3, newdata = to_be_predicted3f)

plot(WSAL_VAL ~ jitter(Age, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.02), ylim = c(0,150000), xlab = "Age", data = dat_graph)
lines(yhat ~ Age, data = to_be_predicted3f)
lines(yhat ~ Age, data = to_be_predicted3m, lty = 2)
legend("topleft", c("male", "female"), lty = c(2,1), bty = "n")


# and always remember this part...
detach(dat_use)

# ----------
# with interactions

model1 <- lm(WSAL_VAL ~ Age + female + AfAm + Asian + Amindian + race_oth 
             + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
             + married + divwidsep + union_m + veteran + immigrant + immig2gen, data = dat_noZeroWage)

model1 <- lm(WSAL_VAL ~ Age*as.factor(GESTCEN) )

summary(model1)
