


height_of_peak <- function(i, death_age = age) {
offset_val <- i

average_incidence <- get_average(death_age, ons_incidence, incidence_death_lookups)
number_of_infections <- population_lookups[[death_age]] * average_incidence 
pre_offset_selected_infections <- tibble(Date = ons_incidence$Date, Incidence = number_of_infections)
offset_selected_infections <- pre_offset_selected_infections %>% 
  mutate(Incidence_lag = lag(Incidence, n = offset_val))

ons_weekly_incidence <- offset_selected_infections %>%
  filter(Date >= start_week) %>%
  mutate(week_offset = as.numeric(difftime(Date, start_week, units = "days")) %/% 7) %>%
  group_by(week_start = start_week + (week_offset * 7)) %>%
  summarize(value = sum(Incidence_lag, na.rm = TRUE)) %>%
  select(Date = week_start, Incidence = value)


# Joining the tibbles based on the date columns and selecting only the relevant columns
result <- ons_weekly_incidence %>%
  inner_join(ons_deaths, by = c("Date" = "Week ending")) %>%
  select(Date, Incidence_lag = Incidence, Deaths = death_age) %>%
  #mutate(Incidence_lag = lag(Incidence_raw, n = offset_val)) %>%
  drop_na()

# # Calculate scale ratio for secondary axis
# scale_ratio <- max(result$Incidence_raw) / max(result$Deaths)
# # Plot
# ggplot(result, aes(x = Date)) +
#   geom_line(aes(y = Incidence_raw, color = "Incidence")) +
#   geom_line(aes(y = Incidence_lag, color = "Incidence_lag")) +
#   geom_line(aes(y = Deaths * scale_ratio, color = "Deaths")) + # Scale the Deaths data
#   scale_y_continuous(name = "Incidence",
#                      sec.axis = sec_axis(~ . / scale_ratio, name = "Deaths")) +
#   labs(title = "Incidence and Deaths over Time",
#        color = "Metric") +
#   theme_minimal()

iter <- 100
model_frame <- drop_na(result)
ss <- list()
ss <- AddDynamicRegression(ss, model_frame$Deaths ~ model_frame$Incidence_lag)
model <- bsts(Deaths ~ Incidence_lag, state.specification = ss, data = model_frame, niter = iter)
#plot(model, "dynamic", burn = 200)
list_of_coeffs <- apply(model$dynamic.regression.coefficients, c(3), mean)
peak_to_trough <- max(list_of_coeffs[20:40]) - min(list_of_coeffs[1:32])
return(peak_to_trough)
}

optimum_lags <- list()
for (k in 2:length(age_list)){
x_values <- 15:45
y_values <- sapply(x_values, function(x) height_of_peak(x, death_age = age_list[k]))
min_x <- x_values[which.min(y_values)]
optimum_lags <- append(optimum_lags, min_x)
}
