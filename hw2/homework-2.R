library(dplyr)
library(ggplot2)
library(tidyr)

setwd("/Users/shaypepper/Documents/school/econometrics")
load("data/pums/pums_NY.RData")

pums.compact <- dat_pums_NY %>%
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
         edu = ordered(
           1 * educ_nohs + 
             2 * educ_hs + 
             3 * educ_smcoll + 
             4 * educ_as + 
             5 * educ_bach +  
             6 * educ_adv,
           levels = 1:6,
           labels = c("No High School", 
                      "High School", 
                      "Some College", 
                      "Associates", 
                      "Bachelors", 
                      "Advanced Degree")
           ),
        race = factor(
          1 * white + 
          1 * Hispanic + 
          3 * AfAm + 
          4 * Amindian + 
          5 * Asian + 
          6 * race_oth,
          levels = 1:6,
          labels = c("White - non-hispanic", 
                     "White - hispanic",
                     "African American",
                     "American Indian",
                     "Asian",
                     "Other")
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
  select(`borough`, `edu`, `race`, PUMA, income_total, Age) %>%
  filter(borough != "Not NYC", income_total >= 0, Age >= 18) %>%
  arrange(PUMA)
  
pums.income_by_neighborhood <- pums.compact %>%
  group_by(`borough`, `PUMA`) %>%
  summarize(meanIncome = mean(income_total), 
            q75.less.q25 = quantile(income_total, 0.75) - quantile(income_total, 0.25),
            q10 = quantile(income_total, 0.10), 
            q25 = quantile(income_total, 0.25), 
            q75 = quantile(income_total, 0.75), 
            q90 = quantile(income_total, 0.90),
            v = var(income_total)
            )


# richest neighborhood in each borough
pums.highest.income.by.borough <- pums.income_by_neighborhood %>%
  top_n(n=1, meanIncome) %>%
  select(borough, PUMA, meanIncome)

pums.highest.income.by.borough

ggplot(pums.highest.income.by.borough, aes(x = borough, y = meanIncome)) + 
  geom_col()

ggplot(pums.income_by_neighborhood, aes(x = PUMA, y = meanIncome, color = borough)) + 
  geom_col()

# poorest neighborhood in each borough
pums.lowest_income_by_borough <- pums.income_by_neighborhood %>%
  top_n(n=1, -meanIncome) %>%
  select(borough, PUMA, meanIncome)
pums.lowest_income_by_borough

# ways to measure income inequality
ggplot(pums.income_by_neighborhood, aes(x = borough, y = meanIncome)) +
  geom_boxplot()

# What is the most unequal neighborhood in every borough?

pums.most.unequal.by.borough <- pums.income_by_neighborhood %>%
  top_n(n=1, -q75.less.q25) %>%
  select(borough, PUMA, q75.less.q25)

pums.most.unequal.by.borough

pums.most.unequal.by.borough <- pums.income_by_neighborhood %>%
  top_n(n=1, -v) %>%
  select(borough, PUMA, v)

pums.most.unequal.by.borough

# How much do rankings of inequality change for slightly different measures

pums.most.unequal.by.borough <- pums.income_by_neighborhood %>%
  top_n(n=3, -q75.less.q25) %>%
  select(borough, PUMA, q75.less.q25)

pums.most.unequal.by.borough

pums.most.unequal.by.borough <- pums.income_by_neighborhood %>%
  top_n(n=3, -v) %>%
  select(borough, PUMA, v)

pums.most.unequal.by.borough


# Is there a mean/risk tradeoff?

# Consider using different measures of income
