#### OVERVIEW ####
### 2025 childcare factsheets update
## request: median family income for families with young children, by STATE
## CPS: FTOTVAL, YNGCH
## ACS: FTOTINC, YNGCH

#lfpr benchmarked to here for female, 2023: https://data.census.gov/table?q=employment%20by%20sex&g=010XX00US


#packages
library(dplyr)
library(ipumsr)
library(tidyverse)
library(here)
library(janitor)
library(realtalk)
library(MetricsWeighted)
library(openxlsx)


# #### IPUMS DATA PULL: ACS ####
# Get & set your API key from https://uma.pop.umn.edu/usa/registration
# ipumsr::set_ipums_api_key("YOUR_API_KEY", save = TRUE)

## 2024
#Define extract
acs2023_extract <- define_extract_micro(
  collection = 'usa',
  description = 'ACS extract for childcare factsheets',
  samples = c('us2023a'),
  variables = c('YEAR', 'STATEFIP', 'FTOTINC', 'FAMUNIT', 'YNGCH', 'HHTYPE', 'LABFORCE', 'SEX')) %>%
  submit_extract() %>%
  wait_for_extract()

# Download extract to input folder
download_ext <- download_extract(extract = acs2023_extract,
                                 download_dir = here('inputs/'), overwrite = TRUE,)

# Load downloaded ACS extract
acs2023 <- read_ipums_micro(download_ext)

#### ACS DATA ANALYSIS ####

## 2023
acs23_medfaminc <- acs2023 %>% 
  #clean label names 
  clean_names() %>%
  mutate(ftotinc = as.numeric(ftotinc)) %>% 
  #select(year, serial, pernum, famunit, ftotinc, perwt, hhwt) %>% 
  # remove duplicate household observations
  distinct(serial, famunit, .keep_all = TRUE) %>% 
  filter(ftotinc > 0 & ftotinc < 9999998, hhtype %in% c(1:3)) %>% 
  group_by(year) %>% 
  summarize(med23_faminc = weighted_median(ftotinc, w=perwt, na.rm=TRUE),
            avg23_faminc = weighted.mean(ftotinc, w=perwt, na.rm=TRUE),
            n = n(),
            wgt_n = sum(perwt))

acs23_overall_lf <- acs2023 %>% 
  #clean label names 
  clean_names() %>%
  filter(labforce == 2) %>% 
  group_by(year, sex) %>% 
  summarize(lf_count = n(),
            wgt_lfcount = sum(perwt))

acs23_mothers_lf <- acs2023 %>% 
  #clean label names 
  clean_names() %>%
  filter(labforce == 2, yngch < 7, sex == 2) %>% 
  group_by(year, statefip) %>% 
  summarize(lf_count = n(),
            wgt_lfcount = sum(perwt))
            

#### IPUMS DATA PULL: CPS MARCH ####

# ## 2024
# #Define extract
# cps2024_extract <- define_extract_micro(
#   collection = 'cps',
#   description = 'CPS extract for childcare factsheets',
#   samples = c('cps2024_03s'),
#   variables = c('YEAR', 'STATEFIP', 'FTOTVAL', 'YNGCH')) %>%
#   submit_extract() %>%
#   wait_for_extract()
# 
# # Download extract to input folder
# download_ext <- download_extract(extract = cps2024_extract,
#                                  download_dir = here('inputs/'), overwrite = TRUE,)
# 
# # Load downloaded ACS extract
# cps2024 <- read_ipums_micro(download_ext)
# 
# 
# #### CPS DATA ANALYSIS ####
# 
# ## 2024
# cps24_medfaminc <- cps2024 %>% 
#   clean_names() %>% 
#   #need to check these parameters 
#   filter(yngch < 5, ftotval >= 0 & ftotval < 9999999999) %>% 
#   group_by(year, statefip) %>% 
#   mutate(w24_ftotval = asecwth * ftotval) %>% 
#   summarize(w24_faminc = median(w24_ftotval)) %>% 
#   pivot_wider(id_cols = statefip, names_from = year, values_from = w24_faminc)

#### create workbook ####

childcare_faminc_lf_data <- createWorkbook()

addWorksheet(childcare_faminc_lf_data, sheetName = "ACS_faminc")
addWorksheet(childcare_faminc_lf_data, sheetName = "ACS_lf_overall")
addWorksheet(childcare_faminc_lf_data, sheetName = "ACS_lf_mothers")


writeData(childcare_faminc_lf_data, acs23_medfaminc, sheet = "ACS_faminc", startCol = 1, startRow = 1, colNames = TRUE)
writeData(childcare_faminc_lf_data, acs23_overall_lf, sheet = "ACS_lf_overall", startCol = 1, startRow = 1, colNames = TRUE)
writeData(childcare_faminc_lf_data, acs23_mothers_lf, sheet = "ACS_lf_mothers", startCol = 1, startRow = 1, colNames = TRUE)

saveWorkbook(childcare_faminc_lf_data, here("output/cc_faminc_lf_v1.xlsx"), overwrite = TRUE)


