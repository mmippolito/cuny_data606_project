library(tidyverse)
library(zoo)
library(mice)

# pull data - from http://www.systemicpeace.org/inscrdata.html
mepv_full <- read.csv("/Users/ippolito/Box Sync/cuny/606-Stats/project/MEPV2012ex.csv")
regions <- read.csv("/Users/ippolito/Box Sync/cuny/606-Stats/project/regions.csv")

# pop densities - https://datahub.io/world-bank/en.pop.dnst
# Population density is midyear population divided by land area in square kilometers.
popdens <- read.csv("/Users/ippolito/Box Sync/cuny/606-Stats/project/pop_densities.csv")

# number of armed conflict events by number of bordering states
mepv %>%
  select(nBORDER) %>%
  group_by(nBORDER) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# histogram number of armed conflict events by number of bordering states
mepv %>%
  select(nBORDER) %>%
  ggplot(aes(x = nBORDER)) +
  geom_bar()

# magnitude of armed conflict events by number of bordering states
mepv %>%
  select(nBORDER, ACTOTAL) %>%
  group_by(nBORDER) %>%
  summarize(mean_actotal = mean(ACTOTAL)) %>%
  arrange(desc(mean_actotal))

# bar plot magnitude of armed conflict events by number of bordering states
mepv %>%
  select(nBORDER, ACTOTAL) %>%
  group_by(nBORDER) %>%
  summarize(mean_actotal = mean(ACTOTAL)) %>%
  ggplot() +
  geom_bar(aes(x = nBORDER, y = mean_actotal), stat = "identity")

# number of armed conflict events by region
mepv %>%
  left_join(regions, by = c("REGION" = "region_code")) %>%
  select(REGION, region_name) %>%
  group_by(region_name) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# magnitude of armed conflict events by region
mepv %>%
  left_join(regions, by = c("REGION" = "region_code")) %>%
  select(REGION, region_name, ACTOTAL) %>%
  group_by(region_name) %>%
  summarize(mean_actotal = mean(ACTOTAL)) %>%
  arrange(desc(mean_actotal))

# number of armed conflicts by region
mepv %>%
  left_join(regions, by = c("REGION" = "region_code")) %>%
  select(REGION, region_name) %>%
  group_by(region_name) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(region_name, n), y = n)) +
  geom_bar(stat = "identity") + coord_flip()

# magnitude of armed conflicts by region
mepv %>%
  left_join(regions, by = c("REGION" = "region_code")) %>%
  select(REGION, region_name, ACTOTAL) %>%
  group_by(region_name) %>%
  summarize(mean_actotal = mean(ACTOTAL)) %>%
  ggplot(aes(x = reorder(region_name, mean_actotal), y = mean_actotal)) +
  geom_bar(stat = "identity") + coord_flip()

# number of armed conflict events by year
mepv %>%
  select(YEAR, ACTOTAL) %>%
  group_by(YEAR) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# histogram - armed conflicts by year
mepv %>%
  select(YEAR, ACTOTAL) %>%
  ggplot(aes(x = YEAR)) +
  geom_histogram(binwidth = 1)

# load literacy rates
lit_full <- read.csv("/Users/ippolito/Box Sync/cuny/606-Stats/project/cross-country-literacy-rates.csv")
lit_raw <- lit_raw %>% rename(lit_rate = `Literacy.rates..World.Bank..CIA.World.Factbook..and.other.sources.`)
lit <- lit_raw
lit$Year <- as.integer(lit$Year)
lit$lit_rate <- as.numeric(lit$lit_rate)

# Fill in lit rates for missing years
# First, generate a data frame of country codes vs number of observations
lit_countries <- lit %>% group_by(Code) %>% summarize(n = n()) %>% arrange(Code)

# Walk each country codee
for (i in lit_countries$Code) {

  # Skip things like "small states" and anything that doesn't have a country code
  if (str_length(i) == 0) {
    next
  }
  
  # Create a data frame for just the current country's literacy rates
  print(i)
  lit_country <- lit %>% filter(Code == i)

  # Linear regression
  if (1 == 0) {
    b <- 0
    m <- 0
    if (nrow(lit_country) > 1) {
      model <- lm(as.numeric(lit_country$lit_rate) ~ as.integer(lit_country$Year))
      b <- model$coefficients[[1]]
      m <- model$coefficients[[2]]
    } else {
      b <- as.numeric(lit_country$lit_rate)
    }
    for (j in min(mepv$YEAR):max(mepv$YEAR)) {
      tmp_yr <- lit_country %>% filter(Year == j)
      newrate <- b + (m * j)
      if (newrate > 100) { newrate = 100 }
      if (newrate < 0) { newrate = 0 }
      newrow <- c(
        lit_country$Entity[[1]], 
        lit_country$Code[[1]], 
        as.integer(j), 
        as.numeric(newrate)
      )
      #print(newrow)
      if (nrow(tmp_yr) == 0) {
        lit <- rbind(lit, newrow)
      }
    }
  }

  # Geometric
  for (j in min(mepv$YEAR):max(mepv$YEAR)) {
    print(str_c("    ", j))
    
  }
  break
}

print(lit_country$Year)
print(lit_country$lit_rate)
model <- lm(lit_country$lit_rate ~ lit_country$Year)
b <- model$coefficients[[1]]
m <- model$coefficients[[2]]
print(b)
print(m)


x <- c(1979, 2011, 2015)
y <- c(18.15, 31.74, 38.17)
print(x)
print(y)
model <- lm(y ~ x)
summary(model)
model$coefficients
model$coefficients[[1]] + (model$coefficients[[2]] * 2012)

# Data for imputation question
tmp3 <- data.frame(matrix(ncol = 3))
colnames(tmp3) <- c("year", "stat1", "stat2")
tmp1 <- data.frame(year = c(2001, 2002, 2004, 2005, 2006), stat1 = c(2000.1, 2000.2, 2000.4, 2000.5, 2000.6))
tmp2 <- data.frame(year = c(2001, 2003, 2004, 2006, 2007), stat2 = c(2.1, 2.3, 2.4, 2.6, 2.7))
tmp3 <- tmp1 %>% full_join(tmp2, by = "year")
tmp3 %>% arrange(year)
tmp3
tmp3$stat1 <- na.approx(tmp3$stat1, tmp3$year)
tmp3$stat2 <- na.approx(tmp3$stat2, tmp3$year)
tmp3 %>% arrange(year)

na.approx(tmp3$stat1, tmp3$year)
mice::complete(tmp3)

#join
j <- mepv %>%
  left_join(popdens, by = c("YEAR" = "Year", "SCODE" = "Country.Code")) %>%
  left_join(regions, by = c("REGION" = "region_code")) %>%
  select(SCODE, COUNTRY, REGION, region_name, YEAR, ACTOTAL, Value)
j2 <- j %>% 
  group_by(SCODE, COUNTRY, REGION, region_name) %>%
  summarize(actotal = sum(ACTOTAL, na.rm = T), meanpop = mean(Value, na.rm = T))
meanpd <- popdens %>%
  group_by(`Country.Code`, `Country.Name`) %>%
  summarize(meanpop = mean(Value, na.rm = T))
j3 <- j2 %>% full_join(meanpd, by = c("COUNTRY" = "Country.Name"))

# Pre-import tidying done:
# 1. Replaced missing or incorrect country and region codes
# 2. Standardized country codes across datasets

#Research question:
#Is population density a good predictor of the frequency or scale of political violence, either regionally or at a national level?
  
# Rename MEPV columns to something a little more informative
mepv <- mepv_full %>%
  rename(
    "country_code" = "SCODE",
    "country" = "COUNTRY",
    "year" = "YEAR",
    "conflict_score" = "ACTOTAL",
    "region_code" = "REGION"
  ) %>%
  select("country_code", "country", "region_code", "year", "conflict_score")

# Rename population density columns
popdens <- popdens_full %>%
  rename (
    "country_code" = "Country.Code",
    "country" = "Country.Name",
    "year" = "Year",
    "pop_density" = "Value"
  ) %>%
  select("country_code", "country", "year", "pop_density")

j <- mepv %>%
  inner_join(popdens, by = c("country_code", "year")) %>%
  left_join(regions, by = c("region_code"))

#-------------------------------------------------------------

# Load countries by region
# from https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv
regions_full <- read.csv("https://raw.githubusercontent.com/mmippolito/cuny/main/data606/project/country_regions.csv")

# Rename some fields for standardization purposes
regions <- regions_full %>%
  rename(
    "country_numeric_code" = "country.code",
    "country_code" = "alpha.3",
    "subregion" = "sub.region"
  )

# Load govt spending on education data set
# from https://data.worldbank.org/indicator/SE.XPD.TOTL.GD.ZS
edspending_full <- read.csv("https://raw.githubusercontent.com/mmippolito/cuny/main/data606/project/govt_ed_spending.csv")

# Rename fields to something more useful
edspending <- edspending_full %>%
  rename(
    "country" = "Country.Name",
    "country_code" = "Country.Code"
  ) %>%
  select(-`Indicator.Name`, -`Indicator.Code`, -`X`)

# Remove Xs from years in column names
edspending <- rename_with(edspending, function(x) ifelse(substr(x, 1, 1) == "X", substr(x, 2, 5), x))

# Gather columns
edspending <- edspending %>% gather(3:63, key = "year", value = "pct_gdp")

# Convert years to numeric
edspending$year <- as.numeric(edspending$year)

# Load literacy rates
# from https://commons.wikimedia.org/wiki/Data:Cross-country_literacy_rates_-_World_Bank,_CIA_World_Factbook,_and_other_sources_(OWID_2762).tab
lit_full <- read.csv("https://raw.githubusercontent.com/mmippolito/cuny/main/data606/project/cross-country-literacy-rates.csv")

# Rename fields to something more useful
lit <- lit_full %>%
  rename(
    "country" = "Entity",
    "country_code" = "Code",
    "year" = "Year",
    "lit_rate" = "Literacy.rates..World.Bank..CIA.World.Factbook..and.other.sources."
  )

# Calculate mean literacy rate per country
lit_sum <- lit %>% group_by(country, country_code) %>%
  summarize(mean_lit_rate = mean(lit_rate, na.rm = T))

# Calculate mean education spending per country
ed_sum <- edspending %>% group_by(country, country_code) %>%
  summarize(mean_pct_gdp = mean(pct_gdp, na.rm = T))

# Join the summarized tables
j <- ed_sum %>% 
  inner_join(lit_sum, by = c("country_code")) %>%
  left_join(regions, by = c("country_code"))

# Filter out NaNs
j <- j %>%
  filter(!is.na(mean_pct_gdp)) %>%
  filter(!is.na(mean_lit_rate))

# Select only the fields we care about
j <- j %>%
  select(country_code, country = country.x, region, subregion, mean_pct_gdp, mean_lit_rate)

#economic disparity rankings
#https://www.indexmundi.com/facts/indicators/SI.POV.GINI/rankings

#type of govt
#https://en.wikipedia.org/wiki/List_of_countries_by_system_of_government

x = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
y = c(5, 6, 4, 8, 10, 8)
lmxy = lm(y ~ x)

j %>% group_by(subregion) %>% summarize(n())

# forms of govt

library(tidyverse)
library(rvest)

parsed_page <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_system_of_government", encoding = "UTF-8")
parsed_tables <- html_table(parsed_page, fill = TRUE)
govts <- data.frame(parsed_tables[6])
govts <- govts %>%
    rename(
        country = Name, 
        govt = Constitutional.form, 
        head_of_state = Head.of.state, 
        exec_legitimacy = Basis.of.executive.legitimacy)
govts$exec_legitimacy = str_replace_all(govts$exec_legitimacy, '\\[.+?\\]', '')

govts %>% group_by(exec_legitimacy) %>% summarise(n())
