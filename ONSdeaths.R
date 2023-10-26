library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(httr)

url <- 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2022/publicationfileweek522022.xlsx'

# Download the file into a temporary location
response <- GET(url, write_disk(temp <- tempfile(fileext = ".xlsx")))

# Read the specific sheet from the downloaded Excel file
# For instance, if you want to read the first sheet, you can do:
deaths2122_raw <- read_excel(temp, sheet = 8, range = 'A7:W111')
unlink(temp)
deaths2122 <- deaths2122_raw %>% mutate(`Week ending` = as.Date(`Week ending`)) %>% mutate(across(-`Week ending`, as.integer))

url2 <- 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2021/publishedweek522021.xlsx'

# Download the file into a temporary location
response <- GET(url2, write_disk(temp <- tempfile(fileext = ".xlsx")))

# Read the specific sheet from the downloaded Excel file
deaths2021_raw <- read_excel(temp, sheet = 8, range = 'B5:DC31')
unlink(temp)

deaths2021 <- deaths2021_raw %>%
  as.matrix() %>%
  t() %>%
  as_tibble()
# Extract the column names from the first row
new_colnames <- deaths2021[1, ] %>% unlist() %>% as.character()
deaths2021 <- deaths2021[-1, ] %>% setNames(new_colnames)
# Set the names of the first 6 columns
colnames(deaths2021)[1:6] <- c('Date', 'Remove1', 'Remove2', 'All ages', 'Remove3', 'Deaths by age group')
# Convert the values in the 'Date' column from Excel integers to R dates
deaths2021$Date <- as.Date(as.numeric(deaths2021$Date), origin = "1899-12-30")
# Remove all three "Remove" columns
deaths2021 <- deaths2021 %>%
  select(-c(Remove1, Remove2, Remove3, `Deaths by age group`))
# Convert all the remaining columns to be integers
deaths2021[] <- lapply(deaths2021, function(x) {
  if (is.character(x)) {
    as.integer(gsub("[^0-9]", "", x))  # Remove non-numeric characters and convert to integer
  } else {
    x
  }
})

deaths2021 <- deaths2021 %>%
  mutate(`Week number` = (row_number() - 1) %% 52 + 1) %>%
  select(`Week number`, everything()) %>%
  rename(`Week ending` = Date)

ons_deaths <- bind_rows(deaths2021, deaths2122) %>%
  arrange(`Week ending`) %>%
  distinct(`Week ending`, .keep_all = TRUE)

#Plot
# Reshape the data to long format
long_data <- ons_deaths %>%
  select(-`Week number`, -`All ages`) %>%
  gather(key = "Age group", value = "Deaths", -`Week ending`)

# Plot using ggplot2
ggplot(long_data, aes(x = `Week ending`, y = Deaths, color = `Age group`)) +
  geom_line() +
  labs(title = "COVID deaths per week (mentioned on certificate) by age Group",
       y = "Number of Deaths",
       x = "Week ending",
       color = "Age Group") +
  theme_minimal()
