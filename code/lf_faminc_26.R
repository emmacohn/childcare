#### OVERVIEW ####
### 2026 childcare factsheets update
## request: median family income for families with young children, by STATE
## CPS: FTOTVAL, YNGCH
## ACS: FTOTINC, YNGCH

#lfpr benchmarked to here for female, 2023: https://data.census.gov/table?q=employment%20by%20sex&g=010XX00US
#median fam income by state benchmarked(mostly) to here, 2023: https://data.census.gov/table?q=B19113:%20Median%20Family%20Income%20in%20the%20Past%2012%20Months%20(in%202023%20Inflation-Adjusted%20Dollars)&g=010XX00US$0400000 
#overall lfpr benchmarked to here, 2023: https://data.census.gov/table?q=Employment%20and%20Labor%20Force%20Status&g=010XX00US 


#packages
library(dplyr)
library(ipumsr)
library(tidyverse)
library(here)
library(janitor)
library(realtalk)
library(MetricsWeighted)
library(openxlsx)
library(realtalk)
library(readr)

# pulling in chained CPI-U series
cpi_data <- realtalk::c_cpi_u_annual

# set base year to 2025
cpi2025 <- cpi_data$c_cpi_u[cpi_data$year==2025]

# #### IPUMS DATA PULL: ACS ####
# Get & set your API key from https://uma.pop.umn.edu/usa/registration
# ipumsr::set_ipums_api_key("YOUR_API_KEY", save = TRUE)

## 2025
#Define extract
acs2024_extract <- define_extract_micro(
  collection = 'usa',
  description = 'ACS extract for childcare factsheets',
  samples = c('us2024a'),
  variables = c('YEAR', 'STATEFIP', 'FTOTINC', 'FAMUNIT', 'YNGCH', 'HHTYPE', 'LABFORCE', 'SEX')) %>%
  submit_extract() %>%
  wait_for_extract()

# Download extract to input folder
download_ext <- download_extract(extract = acs2024_extract,
                                 download_dir = here('inputs/'), overwrite = TRUE)

# Load downloaded ACS extract
acs2024 <- read_ipums_micro(ddi = 'inputs/usa_00046.xml')

#### ACS DATA ANALYSIS ####

## 2024
acs24_medfaminc <- acs2024 %>% 
  #clean label names 
  clean_names() %>%
  mutate(ftotinc = as.numeric(ftotinc)) %>% 
  #select(year, serial, pernum, famunit, ftotinc, perwt, hhwt) %>% 
  # remove duplicate household observations
  distinct(serial, famunit, .keep_all = TRUE) %>% 
  filter(yngch < 6, ftotinc > 0 & ftotinc < 9999998, hhtype %in% c(1:3)) %>% 
  group_by(year, statefip) %>% 
  left_join(cpi_data, by='year') |> 
  mutate(real_ftotinc = ftotinc * (cpi2025/c_cpi_u)) |> 
  summarize(med24_faminc = weighted_median(ftotinc, w=perwt, na.rm=TRUE),
            avg24_faminc = weighted.mean(ftotinc, w=perwt, na.rm=TRUE),
            med25_faminc = weighted_median(real_ftotinc, w=perwt, na.rm=TRUE),
            avg25_famin = weighted.mean(real_ftotinc, w=perwt,na.rm=TRUE),
            n = n(),
            wgt_n = sum(perwt))


acs24_overall_lf <- acs2024 %>% 
  #clean label names 
  clean_names() %>%
  filter(labforce == 2) %>% 
  group_by(year, statefip) %>% 
  summarize(lf_count = n(),
            wgt_lfcount = sum(perwt))

acs24_mothers_lf <- acs2024 %>% 
  #clean label names 
  clean_names() %>%
  filter(labforce == 2, yngch < 6, sex == 2) %>% 
  group_by(year, statefip) %>% 
  summarize(lf_count = n(),
            wgt_lfcount = sum(perwt))


# infant affordability
state_xwalk <- read_csv("./inputs/state_crosswalk.csv") %>% 
  mutate(state_geo = as.character(state_geo)) 
  

infant_cost <- read_csv("./inputs/childcare_input2026.csv") %>% 
  mutate(infant_afford = center_infant / .07)

acs24_affordshare <- acs2024 %>% 
  clean_names() %>% 
  distinct(serial, famunit, .keep_all = TRUE) %>% 
  filter(famunit == 1) %>% 
  rename(state_geo = statefip) %>% 
  mutate(state_geo = as.character(state_geo)) %>% 
  left_join(state_xwalk) %>% 
  select(-state_geo, -state_name) %>% 
  filter(ftotinc > 0 & ftotinc < 9999998, hhtype %in% c(1:3)) %>%
  left_join(infant_cost) %>% 
  select(year, perwt, statefips, ftotinc, center_infant, infant_afford) %>% 
  left_join(cpi_data, by='year') |> 
  mutate(real_ftotinc = ftotinc * (cpi2025/c_cpi_u),
         afford_ind = if_else(infant_afford <= real_ftotinc, 1, 0)) %>% 
  group_by(statefips) %>% 
  mutate(afford_share = weighted_mean(afford_ind, w=perwt, na.rm=TRUE)) %>% 
  distinct(statefips, afford_share)

#### create workbook ####

childcare_faminc_lf_data <- createWorkbook()

addWorksheet(childcare_faminc_lf_data, sheetName = "med_faminc")
addWorksheet(childcare_faminc_lf_data, sheetName = "lf_overall")
addWorksheet(childcare_faminc_lf_data, sheetName = "lf_mothers")
addWorksheet(childcare_faminc_lf_data, sheetName = "afford_share")


writeData(childcare_faminc_lf_data, acs24_medfaminc, sheet = "med_faminc", startCol = 1, startRow = 1, colNames = TRUE)
writeData(childcare_faminc_lf_data, acs24_overall_lf, sheet = "lf_overall", startCol = 1, startRow = 1, colNames = TRUE)
writeData(childcare_faminc_lf_data, acs24_mothers_lf, sheet = "lf_mothers", startCol = 1, startRow = 1, colNames = TRUE)
writeData(childcare_faminc_lf_data, acs24_affordshare, sheet = "afford_share", startCol = 1, startRow = 1, colNames = TRUE)

saveWorkbook(childcare_faminc_lf_data, here("output/ccfs_acs2024_v2.xlsx"), overwrite = TRUE)


