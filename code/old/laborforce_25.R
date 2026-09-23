#### OVERVIEW ####
### 2025 childcare factsheets update
## request: lfpr for all and by STATE
# overall labor force levels (16 and older, nilf == 0)
# lfpr for initial mothers with children under 6 (16 and older, nilf == 0, agechild == 5)

library(tidyverse)
library(dplyr)
library(epidatatools)
library(epiextractr)
library(here)
library(openxlsx)

#### CPS PULL ####
cps <- load_basic(2016:2024, year, basicwgt, statefips, female, age, nilf, agechild) %>%
  filter(age >= 16, nilf == 0) %>% 
  mutate(wgt = basicwgt/12,
         gender = female,
         date = year,
         state = as.character(as_factor(statefips)))

#### overall labor force levels ####
overall <- cps %>% 
  group_by(date, state) %>% 
  summarize(overall_lf = sum(wgt)) %>% 
  pivot_wider(id_cols = state, names_from = date, values_from = c(overall_lf))


#### mothers with children under 6 labor force levels ####
mothers <- cps %>% 
  filter(agechild %in%  c(1, 2, 5:9, 11:15),
         gender == 1) %>% 
  group_by(date, state) %>% 
  summarize(mothers_lf = sum(wgt)) %>% 
  pivot_wider(id_cols = state, names_from = date, values_from = c(mothers_lf))

#### create workbook ####
childcare_lf_data <- createWorkbook()

addWorksheet(childcare_lf_data, sheetName = "Overall")
addWorksheet(childcare_lf_data, sheetName = "Mothers")


writeData(childcare_lf_data, overall, sheet = "Overall", startCol = 1, startRow = 1, colNames = TRUE)
writeData(childcare_lf_data, mothers, sheet = "Mothers", startCol = 1, startRow = 1, colNames = TRUE)


saveWorkbook(childcare_lf_data, here("output/childcare_25_lf.xlsx"), overwrite = TRUE)


