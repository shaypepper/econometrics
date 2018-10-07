library(dplyr)

setwd("/Users/shaypepper/Documents/school/econometrics")
load("data/pums/pums_NY.RData")

attach(dat_pums_NY)

norm_varb <- function(X_in) { 
  m <- min(X_in, na.rm = TRUE)
  M <- max(X_in, na.rm = TRUE)
  
  (X_in - m)/abs(M - m)
}

are.statistically.different <- function(p1, p2, n1, n2, pbar, alpha = 0.05){
  crit.z <- -qnorm(alpha/2)
  test.stat <- (p1 - p2)/sqrt(pbar * (1-pbar) * (1/n1 + 1/n2))
  abs(crit.z) < abs(test.stat)
}

bk.norm.age <- norm_varb(Age[in_Brooklyn == 1])
qn.norm.age <- norm_varb(Age[in_Queens == 1])

bk.x.bar <- mean(bk.norm.age)
qn.x.bar <- mean(qn.norm.age)

bk.var <- var(bk.norm.age)
qn.var <- var(qn.norm.age)

bk.n <- length(bk.norm.age)
qn.n <- length(qn.norm.age)

our.df <- min(bk.n, qn.n) - 1

numerator <- bk.x.bar - qn.x.bar
denominator <- sqrt(bk.var/bk.n + qn.var/qn.n)

test.stat <- numerator/denominator

pt(test.stat, our.df)


bk.children <- Age[in_Brooklyn == 1] < 18
qn.children <- Age[in_Queens == 1] < 18

bk.children.prop <- mean(bk.children)
qn.children.prop <- mean(qn.children)
children.pbar <- mean(Age[in_Brooklyn == 1 | in_Queens == 1] < 18)

are.statistically.different(
    bk.children.prop, 
    qn.children.prop, 
    length(bk.children),
    length(qn.children),
    children.pbar
    )

bk.older <- Age[in_Brooklyn == 1] > 64
qn.older <- Age[in_Queens == 1] > 64

bk.older.prop <- mean(bk.older)
qn.older.prop <- mean(qn.older)
older.pbar <- mean(Age[in_Brooklyn == 1 | in_Queens == 1] > 64)


are.statistically.different(
  bk.older.prop, 
  qn.older.prop, 
  length(bk.older),
  length(qn.older),
  older.pbar
)

library(dplyr)
library(ggplot2)
library(tidyr)
library(class)

setwd("/Users/shaypepper/Documents/school/econometrics")
load("data/pums/pums_NY.RData")

borough.factor <- factor(
  1 * in_Bronx + 
    2 * in_Brooklyn + 
    3 * in_Manhattan + 
    4 * in_StatenI + 
    5 * in_Queens + 
    6 * in_Westchester + 
    7 * in_Nassau, 
  levels = 0:7,
  labels = c("Not NYC", 
             "Bronx", 
             "Brooklyn", 
             "Manhattan", 
             "Staten Island", 
             "Queens", 
             "Westchester", 
             "Nassau")
)
commute.factor <- factor(
  Commute_car + 
    Commute_bus * 2 +
    Commute_subway * 3 + 
    Commute_rail * 4 + 
    Commute_other * 5,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Commute_car",
    "Commute_bus",      
    "Commute_subway",    
    "Commute_rail",     
    "Commute_other" ) 
)


pums.compact <- dat_pums_NY %>%
  filter(in_NYC == 1,
         Age >= 18, 
         Age < 66, 
         Commute_rail == 0, 
         Commute_other == 0) %>%
  mutate(income_total = norm_varb(income_total),
         borough = factor(
           1 * in_Bronx + 
             2 * in_Brooklyn + 
             3 * in_Manhattan + 
             4 * in_StatenI + 
             5 * in_Queens + 
             6 * in_Westchester + 
             7 * in_Nassau, 
           levels = 0:7,
           labels = c("Not NYC", 
                      "Bronx", 
                      "Brooklyn", 
                      "Manhattan", 
                      "Staten Island", 
                      "Queens", 
                      "Westchester", 
                      "Nassau")
         ), 
         commute = factor(
           Commute_car + 
             Commute_bus * 2 +
             Commute_subway * 3 + 
             Commute_rail * 4 + 
             Commute_other * 5,
           levels = c(1, 2, 3, 4, 5),
           labels = c(
             "Commute_car",
             "Commute_bus",      
             "Commute_subway",    
             "Commute_rail",     
             "Commute_other" ) 
         ),
         PUMA = factor(PUMA, 
                       levels=c(03701, 03702, 03703, 03704, 03705, 03706, 
                                03707, 03708, 03709, 03710, 03801,
                                03802, 03803, 03804, 03805, 03806, 
                                03807, 03808, 03809, 03810, 03901,
                                03902, 03903, 04001, 04002, 04003, 
                                04004, 04005, 04006, 04007, 04008,
                                04009, 04010, 04011, 04012, 04013,
                                04014, 04015, 04016, 04017, 04018, 
                                04101, 04102, 04103, 04104, 04105, 
                                04106, 04107, 04108, 04109, 04110,
                                04111, 04112, 04113, 04114),
                       labels= c("Riverdale, Fieldston & Kingsbridge",
                                 "Wakefield, Williamsbridge & Woodlawn",
                                 "Co-op City, Pelham Bay & Schuylerville",
                                 "Pelham Parkway, Morris Park & Laconia",
                                 "Belmont, Crotona Park East & East Tremont",
                                 "Bedford Park, Fordham North & Norwood",
                                 "Morris Heights, Fordham South & Mount Hope",
                                 "Concourse, Highbridge & Mount Eden",
                                 "Castle Hill, Clason Point & Parkchester",
                                 "Hunts Point, Longwood & Melrose",
                                 "Washington Heights, Inwood & Marble Hill",
                                 "Hamilton Heights, Manhattanville & West Harlem",
                                 "Central Harlem",
                                 "East Harlem",
                                 "Upper East Side",
                                 "Upper West Side & West Side",
                                 "Chelsea, Clinton & Midtown Business District",
                                 "Murray Hill, Gramercy & Stuyvesant Town",
                                 "Chinatown & Lower East Side",
                                 "Battery Park City, Greenwich Village & Soho",
                                 "Tottenville, Great Kills & Annadale",
                                 "New Springville & South Beach",
                                 "Port Richmond, Stapleton & Mariner's Harbor",
                                 "Greenpoint & Williamsburg",
                                 "Bushwick",
                                 "Bedford-Stuyvesant",
                                 "Brooklyn Heights & Fort Greene",
                                 "Park Slope, Carroll Gardens & Red Hook",
                                 "Crown Heights North & Prospect Heights",
                                 "Brownsville & Ocean Hill",
                                 "East New York & Starrett City",
                                 "Canarsie & Flatlands",
                                 "East Flatbush, Farragut & Rugby",
                                 "Crown Heights South, Prospect Lefferts & Wingate",
                                 "Sunset Park & Windsor Terrace",
                                 "Bay Ridge & Dyker Heights",
                                 "Borough Park, Kensington & Ocean Parkway",
                                 "Flatbush & Midwood",
                                 "Sheepshead Bay, Gerritsen Beach & Homecrest",
                                 "Bensonhurst & Bath Beach",
                                 "Brighton Beach & Coney Island",
                                 "Astoria & Long Island City",
                                 "Jackson Heights & North Corona",
                                 "Flushing, Murray Hill & Whitestone",
                                 "Bayside, Douglaston & Little Neck",
                                 "Queens Village, Cambria Heights & Rosedale",
                                 "Briarwood, Fresh Meadows & Hillcrest",
                                 "Elmhurst & South Corona",
                                 "Forest Hills & Rego Park",
                                 "Sunnyside & Woodside",
                                 "Ridgewood, Glendale & Middle Village",
                                 "Richmond Hill & Woodhaven",
                                 "Jamaica, Hollis & St. Albans",
                                 "Howard Beach & Ozone Park",
                                 "Far Rockaway, Breezy Point & Broad Channel)"
                       ))
         ) %>%
  select(commute, 
         in_Bronx, 
         in_Brooklyn, 
         in_Manhattan, 
         in_Queens, 
         in_StatenI, 
         income_total
          )

good.pums.obs <- complete.cases(pums.compact)
pums.compact  <- subset(pums.compact, good.pums.obs)
y.data <- pums.compact

set.seed(12345)
NN_obs <- sum(good.pums.obs)
train.obs <- (runif(NN_obs) < 0.8)

train.data <- subset(pums.compact, train.obs)
test.data  <- subset(pums.compact, !train.obs)
cl.data    <- y.data$commute[train.obs]
true.data  <- y.data$commute[!train.obs]

summary(cl.data)

predicted.commute <- knn(train = train.data[-1], 
                         test = test.data[-1], 
                         cl = cl.data,
                         k = 9)
n.correctly.predicted <- sum(predicted.commute == true.data)
correct.rate <- n.correctly.predicted / length(predicted.commute)
print(correct.rate)

# -----------------------

pums.NYC <- dat_pums_NY %>%
  filter(in_NYC == 1,
         Age >= 18, 
         Age < 66) %>%
  mutate(borough = factor(
           1 * in_Bronx + 
             2 * in_Brooklyn + 
             3 * in_Manhattan + 
             4 * in_StatenI + 
             5 * in_Queens + 
             6 * in_Westchester + 
             7 * in_Nassau, 
           levels = 0:7,
           labels = c("Not NYC", 
                      "Bronx", 
                      "Brooklyn", 
                      "Manhattan", 
                      "Staten Island", 
                      "Queens", 
                      "Westchester", 
                      "Nassau")
         ), 
         commute = factor(
           Commute_car + 
             Commute_bus * 2 +
             Commute_subway * 3 + 
             Commute_rail * 4 + 
             Commute_other * 5,
           levels = c(1, 2, 3, 4, 5),
           labels = c(
             "car",
             "bus",      
             "subway",    
             "rail",     
             "other" ) 
         )
  ) %>%
  select(commute, borough, PUMA, income_total) %>%
  na.omit()

p.by.puma <- pums.NYC %>%
  group_by(PUMA) %>%
  summarize(
    p.car       = mean(commute == "car"),
    p.bus       = mean(commute == "bus"),
    p.subway    = mean(commute == "subway"),
    p.rail      = mean(commute == "rail"),
    p.other     = mean(commute == "other"),
    mean.income = mean(income_total),
    n           = n()
  ) %>%
  arrange( desc(mean.income))

head(p.by.puma)

puma.counts <- pums.NYC %>%
  group_by(PUMA) %>%
  summarize(puma.n = n())

gathered <- pums.NYC %>%
  group_by(borough, PUMA, commute) %>%
  summarize(
    mean.income = mean(income_total),
    median.income = median(income_total),
    n        = n()
  ) %>%
  arrange( desc(mean.income)) 

data.with.puma.counts <- left_join(gathered, puma.counts) %>%
  mutate(p = n/puma.n) %>%
  filter(PUMA == data.with.puma.counts$PUMA[1])

data.with.puma.counts

highest_income <- data.with.puma.counts$PUMA[1]


# --------------------------------
borough.data <- pums.NYC %>%
  mutate(
    under25k = income_total < 25000,
    middle = income_total >= 25000 & income_total <= 75000,
    over75k = income_total > 75000
  ) %>%
  mutate(
    bracket = factor(under25k + middle * 2 + over75k * 3, 
                     labels = c("<25k", "mid", ">25k"),
                     levels = 1:3)
  )

borough.counts <- borough.data %>%
  group_by(borough, bracket) %>%
  summarize(borough.n = n())

borough.summaries <- borough.data %>%
  group_by(borough, commute, bracket) %>%
  summarize(
    p.car       = mean(commute == "car"),
    p.bus       = mean(commute == "bus"),
    p.subway    = mean(commute == "subway"),
    p.rail      = mean(commute == "rail"),
    p.other     = mean(commute == "other"),
    mean.income = mean(income_total),
    median.income = median(income_total),
    n           = n()
  ) %>%
  arrange( desc(mean.income))


data.with.boro.counts <- left_join(borough.summaries, borough.counts)

ggplot(data.with.boro.counts, aes(x = bracket, 
                                  y = n/borough.n, 
                                  fill = commute)) + 
  geom_col() +
  facet_wrap(~borough) + 
  labs(y = " ", x = "Income Bracket")




