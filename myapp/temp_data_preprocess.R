# import packages 
library(tidyverse)
library(janitor)
# load in data 
weather_data <- read_csv(here::here("myapp","data", "table_28.csv"))
# temps by month and year 
temp_month_summary <- weather_data %>% 
  clean_names() %>% 
  mutate(year = format(date, format="%Y"),
         month = format(date, format="%m")) %>% 
  group_by(year, month) %>% 
  summarize(
    mean_temp = round(mean(temperature_average_c), 1),
    max_temp  = max(temperature_high_c),
    min_temp  = min(temperature_low_c),
  ) %>% 
  mutate(month_numeric = as.numeric(month),
         month_name = month.name[month_numeric]) %>% 
  select(year, month_name, mean_temp, max_temp, min_temp)

# be sure to update this code with YOUR file path
saveRDS(temp_month_summary, here::here(".", "myapp", "data", "temp_month_summary.rds"))
