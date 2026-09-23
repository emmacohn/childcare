#packages
library(dplyr)
library(ipumsr)
library(tidyverse)
library(here)
library(janitor)
library(realtalk)
library(MetricsWeighted)
library(openxlsx)

## 2017 for benchmark
#Define extract
acs2017_extract <- define_extract_micro(
  collection = 'usa',
  description = 'ACS extract for childcare factsheets',
  samples = c('us2017a'),
  variables = c('YEAR', 'STATEFIP', 'FTOTINC', 'YNGCH')) %>%
  submit_extract() %>%
  wait_for_extract()

# Download extract to input folder
download_ext <- download_extract(extract = acs2017_extract,
                                 download_dir = here('inputs/'), overwrite = TRUE,)

# Load downloaded ACS extract
acs2017 <- read_ipums_micro(download_ext)


## 2017 for all_data benchmark
acs17_medfaminc <- acs2017 %>% 
  #clean label names 
  clean_names() %>%
  mutate(ftotinc = as.numeric(ftotinc)) %>% 
  # remove duplicate household observations
  distinct(serial, .keep_all = TRUE) %>% 
  #no parameters other than eliminating empty values 
  filter(yngch < 6, ftotinc >= 0, ftotinc < 9999999) %>% 
  group_by(year, statefip) %>% 
  summarize(med17_faminc = weighted_median(ftotinc, w=hhwt, na.rm=TRUE),
            avg17_faminc = weighted.mean(ftotinc, w=hhwt, na.rm=TRUE),
            n = n(),
            wgt_n = sum(hhwt))

