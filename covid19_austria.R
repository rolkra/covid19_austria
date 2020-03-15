# packages
library(tidyverse)

# reported infections (per week)
# source: https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv
infected <- c(2,2,3,3,9,14,
              18,21,29,41,55,79,104,
              131,182,246,302,504)

# prepare dataset
data <- tibble(infected = infected)

data <- data %>% 
  mutate(type = "reported",
         day = row_number()) %>% 
  arrange(day) %>% 
  mutate(new_abs = infected - lag(infected),
         new_pct = new_abs / lag(infected) * 100)

# predict
predict_corona <- function(data, infection_rate, days)  {

  future <- seq(from = max(data$day) + 1, to = max(data$day)+days-1)
  infected_all <- NULL
  infected_act <- max(data$infected)
  infected_predict <- NULL
  for (i in seq_along(future))  {
    infected_act <- infected_act * infection_rate
    infected_all <- c(infected_all, infected_act)
  }
  
  data2 <- tibble(type = paste("growth", infection_rate),
                  day = future, 
                  infected = infected_all)
  data2
  
} # predict_corona

# predict for different growth
predict_days <- 50 #57

data_50 <- data %>% 
  predict_corona(infection_rate = 1.50, days = predict_days)

data_40 <- data %>% 
  predict_corona(infection_rate = 1.40, days = predict_days)

data_33 <- data %>% 
  predict_corona(infection_rate = 1.33, days = predict_days)

data_20 <- data %>% 
  predict_corona(infection_rate = 1.20, days = predict_days)

data_15 <- data %>% 
  predict_corona(infection_rate = 1.15, days = predict_days)

data_10 <- data %>% 
  predict_corona(infection_rate = 1.10, days = predict_days)

# combine dataset
data_plot <- data %>% 
  bind_rows(data_50, data_40, data_33, data_20, data_15, data_10)

# visualise
last_day <- length(infected)
data_plot %>% 
  mutate(infected_M = infected / 1000000) %>% 
  ggplot(aes(day, infected_M, color = type)) + 
  geom_line(size = 1.5) +
  geom_vline(xintercept = c(last_day, last_day + 28), 
             linetype = "dotted") +
  ylim(0,5.5) +
  xlab("Days since outbreak") +
  ylab("Infected in Mio") + 
  ggtitle("Covid-19 outbreak in Austria") +
  theme_minimal()+
  annotate("text", last_day/2, 5, 
           label = "until today", size = 2.5) +
  annotate("text", last_day+14, 5, 
           label = "next 4 weeks", size = 2.5)
