#packages
library(dplyr)
library(ipumsr)
library(tidyverse)
library(janitor)
library(realtalk)
library(MetricsWeighted)
library(openxlsx2)
library(realtalk)
library(readr)
library(epidatatools)
library(labelled)

# pulling in chained CPI-U series
cpi_data <- realtalk::c_cpi_u_annual

# set base year to 2025
cpi2025 <- cpi_data$c_cpi_u[cpi_data$year==2025]

# #### IPUMS DATA PULL: ACS ####
# Get & set your API key from https://account.ipums.org/api_keys 
# ipumsr::set_ipums_api_key("YOUR_API_KEY", save = TRUE)

## 2025
#Define extract
#acs2024_extract <- define_extract_micro(
#  collection = 'usa',
#  description = 'ACS extract for childcare factsheets',
#  samples = c('us2024a'),
#  variables = c('YEAR', 'STATEFIP', 'FTOTINC', 'FAMUNIT', 'YNGCH', 'HHTYPE', 'LABFORCE', 'SEX')) %>%
#  submit_extract() %>%
#  wait_for_extract()

# Download extract to input folder
#download_ext <- download_extract(extract = acs2024_extract,
 #                                download_dir = './inputs/', overwrite = TRUE)

# Load downloaded ACS extract
acs2024 <- read_ipums_micro(ddi = 'inputs/usa_00001.xml')

#### ACS DATA ANALYSIS ####

acs_clean <- acs2024 |> 
  clean_names() %>%
  mutate(south = case_when(
          statefip %in% c(1,5,12,13,21,22,28,37,40,45,47,48,51,54) ~ 1,
          TRUE ~ 0), 
          statefip = to_factor(statefip))


## 2024
acs24_medfaminc <- acs_clean %>%  
  mutate(ftotinc = as.numeric(ftotinc)) %>% 
  #select(year, serial, pernum, famunit, ftotinc, perwt, hhwt) %>% 
  # remove duplicate household observations
  distinct(serial, famunit, .keep_all = TRUE) %>% 
  filter(yngch < 6, ftotinc > 0 & ftotinc < 9999998, hhtype %in% c(1:3)) %>% 
  left_join(cpi_data, by='year') |> 
  mutate(real_ftotinc = ftotinc * (cpi2025/c_cpi_u))


state_inc <- acs24_medfaminc |> 
  summarize(med24_faminc = averaged_median(ftotinc, w=perwt, na.rm=TRUE),
            avg24_faminc = weighted.mean(ftotinc, w=perwt, na.rm=TRUE),
            med25_faminc = averaged_median(real_ftotinc, w=perwt, na.rm=TRUE),
            avg25_faminc = weighted.mean(real_ftotinc, w=perwt,na.rm=TRUE),
            n = n(),
            wgt_n = sum(perwt),
            .by = c(year, statefip))

south_inc <- acs24_medfaminc |> 
  summarize(med24_faminc = averaged_median(ftotinc, w=perwt, na.rm=TRUE),
            avg24_faminc = weighted.mean(ftotinc, w=perwt, na.rm=TRUE),
            med25_faminc = averaged_median(real_ftotinc, w=perwt, na.rm=TRUE),
            avg25_faminc = weighted.mean(real_ftotinc, w=perwt,na.rm=TRUE),
            n = n(),
            wgt_n = sum(perwt),
            .by = c(year, south))

overall_lf_state <- acs_clean %>% 
  #clean label names 
  clean_names() %>%
  filter(labforce == 2) %>% 
  summarize(lf_count = n(),
            wgt_lfcount = sum(perwt),
            .by = c(year, statefip))

mothers_lf_state <- acs_clean %>% 
  #clean label names 
  clean_names() %>%
  filter(labforce == 2, yngch < 6, sex == 2) %>% 
  summarize(lf_count = n(),
            wgt_lfcount = sum(perwt),
            .by = c(year, statefip))

overall_lf_south <- acs_clean %>% 
  #clean label names 
  clean_names() %>%
  filter(labforce == 2) %>% 
  summarize(lf_count = n(),
            wgt_lfcount = sum(perwt),
            .by = c(year, south))

mothers_lf_south <- acs_clean %>% 
  #clean label names 
  clean_names() %>%
  filter(labforce == 2, yngch < 6, sex == 2) %>% 
  summarize(lf_count = n(),
            wgt_lfcount = sum(perwt),
            .by = c(year, south))


infant_cost <- read_csv("./inputs/childcare_input2026.csv") %>% 
  mutate(infant_afford = center_infant / .07)

acs24_affordshare <- acs_clean %>%  
  distinct(serial, famunit, .keep_all = TRUE) %>%
  filter(famunit == 1) %>% 
  rename(state_name = statefip) %>% 
  filter(ftotinc > 0 & ftotinc < 9999998, hhtype %in% c(1:3)) %>%
  left_join(infant_cost, by = 'state_name') %>% 
  select(year, perwt, state_name, ftotinc, center_infant, infant_afford) %>% 
  left_join(cpi_data, by='year') |> 
  mutate(real_ftotinc = ftotinc * (cpi2025/c_cpi_u),
         afford_ind = if_else(infant_afford <= real_ftotinc, 1, 0)) %>% 
  group_by(state_name) %>% 
  mutate(afford_share = weighted_mean(afford_ind, w=perwt, na.rm=TRUE)) %>% 
  distinct(state_name, afford_share)

south_affordshare <- acs_clean %>%  
  distinct(serial, famunit, .keep_all = TRUE) %>%
  filter(famunit == 1) %>% 
  rename(state_name = statefip) %>% 
  filter(ftotinc > 0 & ftotinc < 9999998, hhtype %in% c(1:3)) %>%
  left_join(infant_cost, by = 'state_name') %>% 
  select(year, perwt, state_name, ftotinc, center_infant, infant_afford, south) %>% 
  left_join(cpi_data, by='year') |> 
  mutate(real_ftotinc = ftotinc * (cpi2025/c_cpi_u),
         afford_ind = if_else(infant_afford <= real_ftotinc, 1, 0)) %>% 
  group_by(south) %>% 
  mutate(afford_share = weighted_mean(afford_ind, w=perwt, na.rm=TRUE)) %>% 
  distinct(south, afford_share)

ccworkers_affordshare <- read.csv("./inputs/uprooting_childcare.csv") |> 
  summarize(income = averaged_median(wage, w = employment))

#### create workbook ####


# create WB object
wb <- wb_workbook()

wb$
  # add new worksheet
  add_worksheet(sheet = "uprooting_childcare_output")$
  # add family income data to worksheet
  add_data(x = "median fam inc young child")$
  add_data(x = state_inc, start_row = 2)$
  add_data(x = south_inc, start_row = 55)$
  # add new worksheet for mothers labor force participation
  add_worksheet(sheet = "mothers LFP")$
  add_data(x = "mothers lf", start_row = 1)$
  add_data(x = mothers_lf_state, start_row = 2)$
  add_data(x = mothers_lf_south, start_row = 55)$
    # add new worksheet for share that can afford infant care
  add_worksheet(sheet = "infant care afford share")$
  add_data(x = "afford share", start_row = 1)$
  add_data(x = acs24_affordshare, start_row = 2)$
  add_data(x = south_affordshare, start_row = 55)


# save workbook to output folder
wb$
  save("./output/uprooting_childcare_output.xlsx", overwrite = TRUE)


