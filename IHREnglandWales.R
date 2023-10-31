library(tidyverse)
library(lubridate)
library(zoo)
library(bsts)
library(forcats)


#Some useful tables/lookups
incidence_hosp_lookups <- list('0-5' = c('2-11'), 
                   '6-17' = c('2-11', '12-16'),
                   '18-24' = c('17-24'),
                   '25-34' = c('25-34'),
                   '35-44' = c('35-49'),
                   '45-54' = c('35-49', '50-69'),
                   '55-64' = c('50-69'),
                   '65-74' = c('50-69','70+'),
                   '75-84' = c('70+'),
                   '85+' = c('70+'),
                   'All' = c('All'))

population_lookups <- list('0-5' = 3735639, 
                           '6-17' = 8038971,
                                '18-24' = 4697707,
                                '25-34' = 7667912,
                                '35-44' = 7357808,
                                '45-54' = 7510402,
                                '55-64' = 7062354,
                                '65-74' = 5564168,
                                '75-84' = 3464873,
                                '85+' =  1372267,
                                'All' = 59597300)

optimum_lags <- list('0-5' = 15, 
                     '6-17' = 14,
                     '18-24' = 16,
                     '25-34' = 14,
                     '35-44' = 11,
                     '45-54' = 12,
                     '55-64' = 14,
                     '65-74' = 13,
                     '75-84' = 10,
                     '85+' =  10,
                     'All' = 14)

# Function to calculate the average based on death_age
get_average <- function(hosp_age, ons_data, lookup_list) {
  # Identify columns based on hosp_age
  columns <- lookup_list[[hosp_age]]
  # Subset ons_data
  relevant_data <- ons_data[, columns, drop = FALSE]
  # Calculate row-wise average
  avg_values <- rowMeans(relevant_data)
  return(avg_values)
}

#Script starts here: select first date

start_week <- as.Date("2020-10-12")
age_list <- names(incidence_hosp_lookups)
start_num <- 2
dyn_coeff_frame = data.frame()


hosp_age <- age_list[i]
print(hosp_age)
offset_val <- optimum_lags[[hosp_age]]
print(offset_val)

average_incidence <- get_average(hosp_age, ons_incidence, incidence_hosp_lookups)
number_of_infections <- population_lookups[[hosp_age]] * average_incidence 
pre_offset_selected_infections <- tibble(Date = ons_incidence$Date, Incidence = number_of_infections)
offset_selected_infections <- pre_offset_selected_infections %>% 
  mutate(Incidence_lag = lag(Incidence, n = offset_val)) %>%
  mutate(Incidence_lag = rollapply(Incidence_lag, width = 7, FUN = mean, align = 'right', fill = NA))

result <- offset_selected_infections %>%
  inner_join(NHS_hospitalisations, by = c("Date" = "Date")) %>%
  select(Date, Incidence_lag = Incidence, Hosps = hosp_age) %>%
  mutate(Hosps = as.numeric(Hosps)) %>%
  drop_na()


#Loop around all the ages
for (i in 1:length(age_list)){

  hosp_age <- age_list[i]
  print(hosp_age)
  offset_val <- optimum_lags[[hosp_age]]
  print(offset_val)

  average_incidence <- get_average(hosp_age, ons_incidence, incidence_hosp_lookups)
  number_of_infections <- population_lookups[[hosp_age]] * average_incidence 
  pre_offset_selected_infections <- tibble(Date = ons_incidence$Date, Incidence = number_of_infections)
  offset_selected_infections <- pre_offset_selected_infections %>% 
    mutate(Incidence_lag = lag(Incidence, n = offset_val))
  
  result <- offset_selected_infections %>%
    inner_join(NHS_hospitalisations, by = c("Date" = "Date")) %>%
    select(Date, Incidence_lag = Incidence_lag, Incidence = Incidence, Hosps = hosp_age) %>%
    mutate(Hosps = as.numeric(Hosps)) %>%
    drop_na()

  #  Plot
  ggplot(result, aes(x = Date)) +
    geom_line(aes(y = Incidence_lag, color = "Incidence")) +
    geom_line(aes(y = Deaths , color = "Deaths")) + 
    scale_y_continuous(name = "Incidence", labs(title = "Incidence and Deaths over Time", color = "Metric")) +
    theme_minimal()

  iter <- 1000
  model_frame <- drop_na(result)
  ss <- list()
  ss <- AddDynamicRegression(ss, model_frame$Hosps ~ model_frame$Incidence_lag)
  model <- bsts(Hosps ~ Incidence_lag, state.specification = ss, data = model_frame, niter = iter)
  plot(model, "dynamic", burn = 200)

  column_name <- paste0("dyn_coeffs_", hosp_age)

  #Only if this is the first loop - add Date column
  if (i == start_num){
    dyn_coeff_frame <- data.frame(Date = model_frame$Date)
  }

  dyn_coeff_frame <- dyn_coeff_frame %>%
    mutate(!!column_name := apply(model$dynamic.regression.coefficients, c(3), mean))
}

# Reshape the dataframe from wide format to long format
long_data <- dyn_coeff_frame %>%
  gather(key = "Variable", value = "Value", -Date) %>%
  mutate(log_Value = log(Value))


long_data$Variable <- fct_relevel(long_data$Variable, 'dyn_coeffs_6-17')


# Plot using ggplot2
ggplot(data = long_data, aes(x = Date, y = Value, color = Variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Dynamic Coefficients over Time",
       y = "Coefficient Value",
       x = "Date") +
  theme(legend.title = element_blank())


# Plot using ggplot2 - log scale
ggplot(data = long_data, aes(x = Date, y = log_Value, color = Variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Dynamic Coefficients over Time",
       y = "Coefficient Value",
       x = "Date") +
  theme(legend.title = element_blank())


# Plot using ggplot2 with facet_wrap - same scales
ggplot(data = long_data, aes(x = Date, y = Value)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Dynamic Coefficients over Time",
       y = "Coefficient Value",
       x = "Date") +
  theme(legend.title = element_blank()) +
  facet_wrap(~ Variable, ncol = 3)

sig_fig_percent <- function(x) {
  paste0(formatC(x * 100, format = "g", digits = 2), "%")
}

# Plot using ggplot2 with facet_wrap
ggplot(data = long_data, aes(x = Date, y = Value, group = Variable, color = Variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +  # Set size for thicker line, and no legend for the lines
  scale_color_viridis_d(end = 0.85, direction = -1, option = "D", guide = "none") +  # Set colors
  theme_minimal() +
  labs(title = "Dynamic Coefficients - IHR estimates - over Time",
       y = "Coefficient Value (%)",
       x = "Date") +
  theme(legend.title = element_blank()) +
  scale_x_date(breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  scale_y_continuous(labels = sig_fig_percent) +
  facet_wrap(~ Variable, ncol = 4, scales = "free_y")


# Plot using ggplot2 with facet_wrap
ggplot(data = long_data, aes(x = Date, y = Value, group = Variable, color = Variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +  # Set size for thicker line, and no legend for the lines
  scale_color_viridis_d(end = 0.85, direction = -1, option = "D", guide = "none") +  # Set colors
  theme_minimal() +
  labs(title = "Dynamic Coefficients - IHR estimates - over Time",
       y = "Coefficient Value (%)",
       x = "Date") +
  theme(legend.title = element_blank()) +
  scale_x_date(breaks = "1 year", date_labels = "%Y", minor_breaks = "1 month") +
  scale_y_log10(labels = sig_fig_percent) +
  facet_wrap(~ Variable, ncol = 3, scales = "free_y")

