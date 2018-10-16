library(dplyr)
library(ggplot2)

load("data/CEXdata/cex_2012.RData")

clean_df <- mutate(data_cex, 
                   ent_prop = ENTERTPQ / TOTEXPPQ
                   
                   )
lm()

t.val <- qt(.025, lower.tail = FALSE)

male_female_desc <- data_cex %>%
  mutate(ent_prop = ENTERTPQ / TOTEXPPQ) %>%
  select(ent_prop, female) %>%
  group_by(female) %>%
  summarize(
    n = length(ent_prop),
    meanProp = mean(ent_prop),
    sdProp = sd(ent_prop),
    stdError = qt(.025, df = n - 1, lower.tail = FALSE) * sd(ent_prop) / sqrt(n),
    lowerBound = meanProp - stdError,
    upperBound = meanProp + stdError
  )

male_female_desc

ggplot(clean_df, aes(x = ent_prop)) +
  geom_density()

ggplot(clean_df, aes(x = ent_prop)) +
  geom_histogram()

attach(clean_df)
model1 <- lm(
  ent_prop ~
  BLS_URBN + 
  educ_nohs  + 
  educ_hs    + 
  educ_smcoll+ 
  educ_as    + 
  educ_bach  + 
  educ_adv   + 
  female + 
  ALCBEVPQ +
  AGE_REF
  , clean_df)
detach(clean_df)
summary(model1)


ent_prop_25 <- quantile(clean_df$ent_prop, 0.25, na.rm = TRUE)

knn_data <- clean_df %>%
  select(ent_prop, 
           BLS_URBN,
           # educ_nohs,
           # educ_hs,
           # educ_smcoll,
           # educ_as,
           # educ_bach,
           # educ_adv,
           female,
           # ALCBEVPQ,
           AGE_REF
         ) %>%
  mutate(bottom25 = ent_prop < ent_prop_25) %>%
  select( -ent_prop)

good.obs <- complete.cases(knn_data)
knn_data  <- subset(knn_data, good.obs)

y.data <- knn_data

set.seed(14876875)
NN_obs <- sum(good.obs)
train.obs <- (runif(NN_obs) < 0.8)

train.data <- subset(y.data, train.obs)
test.data  <- subset(y.data, !train.obs)
cl.data    <- y.data$bottom25[train.obs]
true.data  <- y.data$bottom25[!train.obs]

summary(cl.data)

predicted.ent <- knn(train = train.data[-1], 
                         test = test.data[-1], 
                         cl = cl.data,
                      k = 3)
n.correctly.predicted <- sum(predicted.ent == true.data)
correct.rate <- n.correctly.predicted / length(predicted.ent)
print(correct.rate)

