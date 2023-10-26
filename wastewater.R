library(tidyverse)
library(lubridate)
library(zoo)
library(bsts)

#Loading the two series

#Go to web page and select wastewater
wastewater_url <- 'https://scotland.shinyapps.io/phs-respiratory-covid-19/_w_097fa693/session/3571da2e29d016faefbcc17621776bea/download/data_download_output?w=097fa693'
waste_tb_original <- read_csv(wastewater_url)

#Now go and select hospital admissions and redo
hospital_url <- 'https://scotland.shinyapps.io/phs-respiratory-covid-19/_w_097fa693/session/3571da2e29d016faefbcc17621776bea/download/data_download_output?w=097fa693'
hosp_tb_original <- read_csv(hospital_url)

#Copy to make sure that we have the originals (it's fiddly to reload)
waste_tb <- waste_tb_original
hosp_tb <- hosp_tb_original

# Convert date columns to date format
waste_tb$Date <- lubridate::ymd(waste_tb$Date)
hosp_tb$AdmissionDate <- lubridate::ymd(hosp_tb$AdmissionDate)

#Create a sequence of dates
all_dates <- seq(min(c(waste_tb$Date, hosp_tb$AdmissionDate)), max(c(waste_tb$Date, hosp_tb$AdmissionDate)), by = "1 day")

# Fill in missing dates and interpolate values for waste_tb
waste_filled <- waste_tb %>%
  complete(Date = all_dates) %>%
  mutate(WastewaterSevenDayAverageMgc = approx(Date, WastewaterSevenDayAverageMgc, Date)$y)

#plot
ggplot(waste_filled, aes(x = Date, y = WastewaterSevenDayAverageMgc)) +
  geom_line() +
  labs(title = "Wastewater Seven Day Average Mgc over Time",
       x = "Date",
       y = "WastewaterSevenDayAverageMgc") +
  theme_light()

# Fill in missing dates and interpolate values for hosp_tb
hosp_filled <- hosp_tb %>% arrange(AdmissionDate) %>%
  complete(AdmissionDate = all_dates) %>%
  mutate(TotalInfections = approx(AdmissionDate, TotalInfections, AdmissionDate)$y) %>%
  mutate(TotalInfections7DayAvg = zoo::rollmean(TotalInfections, 7, fill = NA, align = "right"))# Replace 'SomeColumn' with the correct column name

#plot
ggplot(hosp_filled, aes(x = AdmissionDate, y = TotalInfections7DayAvg)) +
  geom_line() +
  labs(title = "Hospital Admissions over Time",
       x = "Admission Date",
       y = "Admissions per day") +
  theme_light()

# Join the two tibbles together
combined <- left_join(waste_filled, hosp_filled, by = c("Date" = "AdmissionDate"))
combined <- combined %>% select(Date, WastewaterSevenDayAverageMgc, TotalInfections7DayAvg) %>% rename(Wastewater = WastewaterSevenDayAverageMgc, Admissions = TotalInfections7DayAvg)

ggplot(combined, aes(x = Date)) +
  geom_line(aes(y = Wastewater, color = "Wastewater")) +
  geom_line(aes(y = Admissions, color = "Admissions")) +
  labs(title = "Wastewater vs Admissions over Time",
       x = "Date",
       y = "Value",
       color = "Legend") +
  theme_light()

#Save a copy down before messing around
combined_original <- combined


#Analysis
calculate_dynamic_regression <- function(combined, k = 1, iter = 250) {
  model_frame <- combined %>% mutate(Admissions = lead(Admissions, n = k))
  model_frame <- drop_na(model_frame)
  ss <- list()
  ss <- AddDynamicRegression(ss, model_frame$Admissions ~ model_frame$Wastewater)
  model <- bsts(Admissions ~ Wastewater, state.specification = ss, data = model_frame, niter = iter)
  plot(model, "dynamic", burn = 100)
  result <- apply(model$dynamic.regression.coefficients, c(3), mean)
  return(sd(result[700:1100])/mean(result[700:1100]))}

k_values <- 3:15
results <- sapply(k_values, function(k) calculate_dynamic_regression(combined, k = k))
