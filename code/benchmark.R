#### BENCHMARK: overview ####
### CCFS 2025 update 
## benchmark: CPS and ACS 
# benchmark to overall median fam income published tables 
# benchmark to specific median family income children under 6 data from 2018 factsheets

## Benchmark notes
# benchmarked overall median family income in 2023 to within MOE here: https://data.census.gov/table/ACSST1Y2017.S1901 
# checked "nchlt5" with yngch < 5 counts to make sure they were similar
# checked counts for different parameters against previous code

#packages
library(dplyr)
library(ipumsr)
library(tidyverse)
library(here)
library(janitor)
library(realtalk)
library(MetricsWeighted)
library(openxlsx)

#### ACS DATA PULL ####
# Get & set your API key from https://uma.pop.umn.edu/usa/registration
# ipumsr::set_ipums_api_key("YOUR_API_KEY", save = TRUE)

## 2017 for benchmark
#Define extract
acs2017_extract <- define_extract_micro(
  collection = 'usa',
  description = 'ACS extract for childcare factsheets',
  samples = c('us2017a'),
  variables = c('YEAR', 'STATEFIP', 'FTOTINC', 'FAMUNIT', 'YNGCH', 'LABFORCE', 'HHTYPE', 'NCHLT5')) %>%
  submit_extract() %>%
  wait_for_extract()

# Download extract to input folder
download_ext <- download_extract(extract = acs2017_extract,
                                 download_dir = here('inputs/'), overwrite = TRUE,)

# Load downloaded ACS extract
acs2017 <- read_ipums_micro(download_ext)


#### CPS MARCH DATA PULL ####

# ## 2017
# #Define extract
# cps2017_extract <- define_extract_micro(
#   collection = 'cps',
#   description = 'CPS extract for childcare factsheets',
#   samples = c('cps2017_03s'),
#   variables = c('YEAR', 'STATEFIP', 'FTOTVAL', 'YNGCH')) %>%
#   submit_extract() %>%
#   wait_for_extract()
# 
# # Download extract to input folder
# download_ext <- download_extract(extract = cps2017_extract,
#                                  download_dir = here('inputs/'), overwrite = TRUE,)
# 
# # Load downloaded ACS extract
# cps2017 <- read_ipums_micro(download_ext)


#### DATA ANALYSIS ####

## overall
acs17_overall <- acs2017 %>% 
  #clean label names 
  clean_names() %>%
  mutate(ftotinc = as.numeric(ftotinc)) %>% 
  #select(year, serial, pernum, famunit, ftotinc, perwt, hhwt) %>% 
  # remove duplicate household observations
  distinct(serial, famunit, .keep_all = TRUE) %>% 
  #no parameters other than eliminating empty values 
  filter(ftotinc > 0 & ftotinc < 9999998, hhtype %in% c(1:3)) %>% 
  group_by(year) %>% 
  summarize(med17_faminc = weighted_median(ftotinc, w=perwt, na.rm=TRUE),
            avg17_faminc = weighted.mean(ftotinc, w=perwt, na.rm=TRUE),
            n = n(),
            wgt_n = sum(perwt))
  
#ACS
acs17_medfaminc <- acs2017 %>% 
  #clean label names 
  clean_names() %>%
  mutate(ftotinc = as.numeric(ftotinc)) %>% 
  # remove duplicate household observations
  distinct(serial, .keep_all = TRUE) %>% 
  #no parameters other than eliminating empty values 
  filter(yngch < 6, ftotinc > 0, ftotinc < 9999998) %>% 
  group_by(year, statefip) %>% 
  summarize(med17_faminc = weighted_median(ftotinc, w=hhwt, na.rm=TRUE),
            avg17_faminc = weighted.mean(ftotinc, w=hhwt, na.rm=TRUE),
            n = n(),
            wgt_n = sum(hhwt))

#affordability share



