# Load packages
library(tidyverse)
library(ggplot2)
library(scales)
library(purrr)
library(lubridate)
library(stringr)
library(readxl)
library(janitor)

source("R/synthetic_data.R")

# Public Age Ethnicity data set -----------------------------------------------

ladenominators2017 <- read_excel("data-feed/ladenominators2017.xls")

# Female = 2

publicEthnicityEstimates <- ladenominators2017 %>% 
  mutate(sex = case_when(sex == 2 ~ 'Female',
                         sex == 1 ~ 'Male')) %>% 
  filter(lad2014_name %in% local_authority_name,
         #sex == 2,
         age > min(df_synth$age_at_referral) -1,
         age < max(df_synth$age_at_referral) +1) %>% 
  pivot_longer(cols = c('White', 
                        'Asian', 
                        'Black', 
                        'Mixed', 
                        'Other'),
               names_to = 'Ethnicity',
               values_to = 'Count') %>% 
  mutate(age = as.factor(age),
         Ethnicity = as.factor(Ethnicity)) %>% 
  group_by(age, Ethnicity) %>% 
  arrange(age) %>% 
  ungroup()


# Public IMD Ethnicity data -----------------------------------------------

regionbirthsbyethnicityIMD <- read_excel("data-feed/regionbirthsbyethnicityIMD20072013.xls",
                                         skip = 1)

regionIMD <- regionbirthsbyethnicityIMD %>%
  remove_empty(c("rows", "cols")) %>% 
  fill(c(`IMD Quintile`, Sex), .direction = c("down")) %>% 
  filter(Region == 'East Midlands' | is.na(Sex)) %>% 
  select(Sex,
         IMD.Quintile = `IMD Quintile`,
         `2013/14`:...38
  ) %>% 
  replace_na(list(Sex = 'Sex')) %>% 
  replace_na(list(IMD.Quintile = 'IMD.Quintile')) %>% 
  row_to_names(1) %>% 
  pivot_longer(cols = White:`Not Stated`,
               names_to = 'Ethnicity',
               values_to = 'Counts') %>% 
  group_by(IMD.Quintile, Ethnicity) %>% 
  mutate(Counts = as.numeric(Counts),
         Totals = sum(Counts)) %>% 
  ungroup() %>% 
  mutate(Ethnicity = as.factor(Ethnicity),
         IMD.Quintile = as.integer(IMD.Quintile))


# LOOK UP LISTS -------------------------------------------------------------------

# 1.0 FinYrs data ----------------------------------------------------------------

finYears <- df_synth %>%
  arrange(desc(fin_year_name)) %>%
  pull(fin_year_name) %>%
  unique()


# 2.0 Ethnicity Grouped List ----------------------------------------------------------

ethnicGroupedList <- df_synth %>%
  arrange(ethnicity_category) %>%
  group_by(ethnicity_category) %>%
  slice(1) %>%
  pull(ethnicity_category)


# 3.0 Ethnicity Detail list ----------------------------------------------------------

ethnicDetailList <- map(unique(df_synth$ethnicity_category), 
                        function(x) {
                          df_synth %>%
                            filter(ethnicity_category == x) %>%
                            pull(ethnicity_detail)
                        })

names(ethnicDetailList) <- unique(df_synth$ethnicity_category)


# 4.0 Age Estimates Public Ethnicity --------------------------------------------------------

ageEstimates <- publicEthnicityEstimates %>% 
  arrange(Ethnicity) %>% 
  group_by(Ethnicity) %>% 
  slice(1) %>% 
  pull(Ethnicity)

# 5.0 IMD Public Ethnicity --------------------------------------------------------

IMDONSList <- regionIMD %>% 
  arrange(Ethnicity) %>% 
  group_by(Ethnicity) %>% 
  slice(1) %>% 
  pull(Ethnicity)

# 6.0 IMD Decile list ----------------------------------------------------------------

IMDDecileList <- df_synth %>% 
  arrange(imd_decile) %>% 
  pull(imd_decile) %>% 
  unique()


# 7.0 IMD Local Quintile list -------------------------------------------------------

IMDQuintileList <- df_synth %>% 
  arrange(imd_quintile) %>% 
  pull(imd_quintile) %>% 
  unique()

# 8.0 Public IMD Quintile list ------------------------------------------------

IMDPublicQuintileList <- regionIMD %>%
  arrange(IMD.Quintile) %>%
  pull(IMD.Quintile) %>%
  unique()

# 9.0 Local Authority region

LARegion <- df_synth %>% 
  arrange(local_authority_name) %>% 
  pull(local_authority_name) %>% 
  unique()

# 10.0 Public Local Authority region

publicLARegion <- publicEthnicityEstimates %>% 
  arrange(lad2014_name) %>% 
  pull(lad2014_name) %>% 
  unique()

# 11.0 Gender local

genderList <- df_synth %>% 
  pull(gender) %>% 
  unique()

## 13.0 Gender public

genderPublicList <- publicEthnicityEstimates %>% 
  pull(sex) %>% 
  unique()



# Functions ---------------------------------------------------------------

ageGroupFunction <- function(object){
  
  object %>% 
    group_by(age_at_referral) %>%
    count(n = n_distinct(row_id)) %>% 
    ungroup() %>%
    complete(age_at_referral = seq(min(object$age_at_referral), max(object$age_at_referral), 1)) 
}


imdGroupFunction <- function(object, colName, maxLimit){
  
  object %>% 
    group_by(.data[[colName]]) %>%
    count(n = n_distinct(row_id)) %>% 
    ungroup() %>% 
    rename(IMD = 1) %>% 
    complete(IMD = seq(1, maxLimit, 1))
  
}
