library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(httr)
library(scales)


#### Get COVID incidence from ONS ####

url <- 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveycumulativeincidenceofthepercentageofpeoplewhohavetestedpositiveforcovid19infectionsengland/current/cumulativeincidence.xlsx'

# Download the file into a temporary location
response <- GET(url, write_disk(temp <- tempfile(fileext = ".xlsx")))

# Read the specific sheet from the downloaded Excel file
incidence_raw <- read_excel(temp, sheet = 3, range = 'A6:Z936')
unlink(temp)

#Process incidence data
incidence <- incidence_raw %>% mutate(Date = date(Date))
#Rename and calculate the diff
names(incidence) = c('Date', 'Variant', 'All', 'All_lc', 'All_uc', '2-11', '2-11_lc', '2-11_uc', '12-16', '12-16_lc', '12-16_uc', '17-24', '17-24_lc', '17-24_uc', '25-34', '25-34_lc', '25-34_uc', '35-49', '35-49_lc', '35-49_uc', '50-69', '50-69_lc', '50-69_uc', '70+', '70+_lc', '70+_uc')
incidence <- incidence %>% mutate(across(-c('Date', 'Variant'), ~c(NA, diff(.))))
#Drop first 4 lines, and then 
incidence <- incidence %>% 
  slice(5:n()) %>% 
  mutate(across(-c(Date, Variant), ~ifelse(. < -0.005, NA, .))) %>%
  fill(everything(), .direction = "downup") %>% 
  slice(2:n())

#Plot incidence
# Pivot data to longer format
df_long <- incidence %>%
  pivot_longer(-c(Date, Variant), names_to="Metric", values_to="Value")

df_long_no_confs <- df_long %>%
  filter(!grepl("(_uc|_lc)$", Metric))

# Create line chart
ggplot(df_long_no_confs, aes(x = Date, y = Value, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) + 
  theme_minimal() +
  labs(title = "COVID infections per day by age group",
       y = "Value (%)",
       x = "Date") +
  theme(legend.title = element_blank())

ons_incidence <- incidence



