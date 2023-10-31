
library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(httr)
library(purrr)

urls <- c('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/02/Covid-Publication-13-01-2022-Supplementary-Data-up-to-210406.xlsx',
          'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/02/Covid-Publication-13-01-2022-Supplementary-Data-210407-210930.xlsx',
          'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Covid-Publication-12-05-2022-Supplementary-Data-211001-220331.xlsx',
          'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/12/Covid-Publication-08-12-2022-Supplementary-Data-220401-220930.xlsx',
          'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/06/Covid-Publication-08-06-2023-Supplementary-Data.xlsx')
cell_ranges <- c("C13:FX26", "D13:FX26", "D13:GC26", "D13:GD26", "D13:IJ26")

temp_files <- map(urls, function(url) {
  temp <- tempfile(fileext = ".xlsx")
  GET(url, write_disk(temp))
  return(temp)
})

# Read the specific range from each temporary Excel file
df_list <- map2(temp_files, cell_ranges, ~read_excel(.x, range = .y))
# Horizontally bind (join) all dataframes
df_combined <- bind_cols(df_list)
# Transpose the dataframe
df_transposed <- as.data.frame(t(df_combined))
# Adjust the column names based on the first row and remove that row
colnames(df_transposed) <- df_transposed[1, ]
df_transposed <- df_transposed[-1, ]
df_transposed$Date <- as.Date(as.integer(rownames(df_transposed)), origin="1899-12-30")

# Set the Date column as the first column and drop the NA column
df_transposed <- df_transposed[, -1] %>%
  select(Date, everything())

new_names <- c('Date', 'All', '0-5', '6-17', '18-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84', '85+', 'Unknown age')
names(df_transposed) <- new_names
# Convert to a tibble
NHS_hospitalisations <- as_tibble(df_transposed)

# Set the desired data types for each column
NHS_hospitalisations <- NHS_hospitalisations %>%
  mutate(Date = as.Date(Date),
         across(-Date, as.numeric))



Plot_age <- "35-44" # Replace this with the age group you want to plot

# Transform the data to long format
long_data <- NHS_hospitalisations %>%
  pivot_longer(cols = -Date, names_to = "Age_Group", values_to = "Count")

# Filter the data for the specified age group
age_data <- long_data %>%
  filter(Age_Group == Plot_age)

# Create the line plot for the specified age group
ggplot(data = age_data, aes(x = Date, y = Count, group = Age_Group, color = Age_Group)) +
  geom_line() +
  theme_minimal() +
  labs(title = paste("NHS Hospitalisations for Age Group", Plot_age),
       x = "Date",
       y = "Hospitalisations") +
  theme(legend.position = "none") # Hide legend since there's only one line
