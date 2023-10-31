






height_of_peak <- function(i, hosp_age = age) {
offset_val <- i

average_incidence <- get_average(hosp_age, ons_incidence, incidence_hosp_lookups)
number_of_infections <- population_lookups[[hosp_age]] * average_incidence 
pre_offset_selected_infections <- tibble(Date = ons_incidence$Date, Incidence = number_of_infections)
offset_selected_infections <- pre_offset_selected_infections %>% 
  mutate(Incidence_lag = lag(Incidence, n = offset_val)) %>%
  mutate(Incidence_lag = rollapply(Incidence_lag, width = 7, FUN = mean, align = 'right', fill = NA))

result <- offset_selected_infections %>%
  inner_join(NHS_hospitalisations, by = c("Date" = "Date")) %>%
  select(Date, Incidence_lag = Incidence_lag, Incidence = Incidence, Hosps = hosp_age) %>%
  mutate(Hosps = as.numeric(Hosps)) %>%
  drop_na()

# # Calculate scale ratio for secondary axis
scale_ratio <- max(result$Incidence_lag) / max(result$Hosps)
#Plot
ggplot(result, aes(x = Date)) +
  geom_line(aes(y = Incidence, color = "Incidence")) +
  geom_line(aes(y = Incidence_lag, color = "Incidence_lag")) +
  geom_line(aes(y = Hosps * scale_ratio, color = "Hospitalisations")) + # Scale the Deaths data
  scale_y_continuous(name = "Incidence",
                     sec.axis = sec_axis(~ . / scale_ratio, name = "Deaths")) +
  labs(title = "Incidence and Hosps over Time",
       color = "Metric") +
  theme_minimal()

iter <- 250
model_frame <- drop_na(result)
ss <- list()
ss <- AddDynamicRegression(ss, model_frame$Hosps ~ model_frame$Incidence_lag)
model <- bsts(Hosps ~ Incidence_lag, state.specification = ss, data = model_frame, niter = iter)
#plot(model, "dynamic", burn = 200)
list_of_coeffs <- apply(model$dynamic.regression.coefficients, c(3), mean)
peak <- min(list_of_coeffs)
return(peak)
}

optimum_lags <- list()
for (k in 1:length(age_list)){
  x_values <- 10:25
  y_values <- sapply(x_values, function(x) height_of_peak(x, hosp_age = age_list[k]))
  max_x <- x_values[which.max(y_values)]
  optimum_lags <- append(optimum_lags, max_x)
}




#Using cross-correlation
x_correlation <- function(i, hosp_age = age) {
  offset_val <- i
  
  average_incidence <- get_average(hosp_age, ons_incidence, incidence_hosp_lookups)
  number_of_infections <- population_lookups[[hosp_age]] * average_incidence 
  pre_offset_selected_infections <- tibble(Date = ons_incidence$Date, Incidence = number_of_infections)
  offset_selected_infections <- pre_offset_selected_infections %>% 
    mutate(Incidence_lag = lag(Incidence, n = offset_val)) %>%
    mutate(Incidence_lag = rollapply(Incidence_lag, width = 7, FUN = mean, align = 'right', fill = NA))
  
  result <- offset_selected_infections %>%
    inner_join(NHS_hospitalisations, by = c("Date" = "Date")) %>%
    select(Date, Incidence_lag = Incidence_lag, Incidence = Incidence, Hosps = hosp_age) %>%
    mutate(Hosps = as.numeric(Hosps)) %>%
    drop_na()
  
  # # Calculate scale ratio for secondary axis
  scale_ratio <- max(result$Incidence_lag) / max(result$Hosps)
  #Plot
  ggplot(result, aes(x = Date)) +
    geom_line(aes(y = Incidence, color = "Incidence")) +
    geom_line(aes(y = Incidence_lag, color = "Incidence_lag")) +
    geom_line(aes(y = Hosps * scale_ratio, color = "Hospitalisations")) + # Scale the Deaths data
    scale_y_continuous(name = "Incidence",
                       sec.axis = sec_axis(~ . / scale_ratio, name = "Deaths")) +
    labs(title = "Incidence and Hosps over Time",
         color = "Metric") +
    theme_minimal()
  cc <- ccf(result$Incidence_lag[1:135], result$Hosps[1:135], lag.max = NULL, type = "correlation", plot = FALSE)
  return(cc)
}


hosp_age <- '55-64'
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

scale_ratio <- max(result$Incidence_lag) / max(result$Hosps)

ggplot(result, aes(x = Date)) +
  geom_line(aes(y = Incidence, color = "Incidence")) +
  #geom_line(aes(y = Incidence_lag, color = "Incidence_lag")) +
  geom_line(aes(y = Hosps * scale_ratio, color = "Hospitalisations")) + # Scale the Deaths data
  scale_y_continuous(name = "Incidence",
                     sec.axis = sec_axis(~ . / scale_ratio, name = "Hospitalisations")) +
  labs(title = "Incidence and Hosps over Time",
       color = "Metric") +
  theme_minimal()

offset_val




