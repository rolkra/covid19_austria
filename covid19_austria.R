# packages
library(tidyverse)
library(patchwork)
library(lubridate)
library(scales)

# read data (https://github.com/CSSEGISandData/COVID-19)
raw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

work <- raw %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "date") %>% 
  mutate(date = substr(date,2,8)) %>% 
  mutate(date = mdy(date))

# select data for Austria
infected <- work %>% 
  filter(Country.Region == "Austria",
         value > 0) %>% 
  pull(value)
  
# prepare dataset
data <- tibble(infected = infected)

data <- data %>% 
  mutate(type = "confirmed",
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
  
  data2 <- tibble(type = paste0("growth ", (infection_rate-1)*100,"%"),
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
  bind_rows(data_33, data_20, data_15, data_10)

# visualise
last_day <- length(infected)
p0 <- data_plot %>% 
  mutate(infected_M = infected / 1000000) %>% 
  ggplot(aes(day, infected_M, color = type)) + 
  geom_line(size = 1.5) +
  geom_vline(xintercept = c(last_day, last_day + 28), 
             linetype = "dotted") +
  ylim(0,5.5) +
  xlab("Days since outbreak") +
  ylab("Infections in Mio") + 
  #ggtitle("Covid-19 outbreak in Austria") +
  theme_minimal()+
  annotate("text", last_day/2, 5, 
           label = "until today", size = 2.5) +
  annotate("text", last_day+14, 5, 
           label = "next 4 weeks", size = 2.5)

#############################################

# data for reference line 33% growth (Austria)
data_line_start <- tibble(
  day = 21,
  infected = 1018
)

# predict 10%/33% growth (days since 50 cases)
data_line10 <- predict_corona(
  data_line_start,
  infection_rate = 1.10,
  days = 15)

data_line33 <- predict_corona(
  data_line_start,
  infection_rate = 1.33,
  days = 15)

# infected
p1 <- data %>% 
  mutate(infected_M = infected / 1000000) %>% 
  ggplot(aes(day, infected)) + 
  geom_line(size = 1.5, color = "red") +
  geom_line(data = data_line10, 
            aes(day,infected), color = "grey", alpha = 0.7) +
  geom_line(data = data_line33, 
            aes(day,infected), color = "grey", alpha = 0.7) +
  xlim(c(1,length(infected)+1)) +
  #ylim(0,max(infected)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE),
                     limits=c(0,max(infected)) ) +
  #  xlab("Days since outbreak") +
  xlab("") +
  ylab("Confirmed infections") + 
  #  ggtitle("Covid-19 outbreak in Vienna") +
  theme_minimal()

# daily growth infected
p2 <- data %>% 
  filter(day >= 5) %>% 
  ggplot(aes(day, new_pct)) +
  geom_col(fill = "grey") +
  geom_text(aes(day, new_pct, 
                label = paste0(format(new_pct, digits=1))),
            size = 2, vjust = "bottom", nudge_y = 1) +
  geom_hline(yintercept = 10, color = "darkgrey", alpha = 0.75) +
  geom_hline(yintercept = 33, color = "darkgrey", alpha = 0.75) +
  xlim(c(1,length(infected)+1)) +
  ylim(c(0,75)) +
  xlab("Days since outbreak") +
  ylab("Daily growth in %") + 
  #  ggtitle("Covid-19 outbreak in Vienna") +
  theme_minimal() +
  annotate("text", 3, 10+1, 
           label = "10% growth",
           size = 2,vjust = "bottom") +
  annotate("text", 3, 33+1, 
           label = "33% growth",
           size = 2, vjust = "bottom") 


# combine plots
p <- ((p1 / p2) | p0) + plot_annotation('Covid-19 outbreak in Austria',
                            caption = "source: https://github.com/CSSEGISandData/COVID-19")

# plot
p

# save plot
p %>% ggsave(filename = "covid-19-austria.png", 
             device = "png",
             width = 7, height = 4)

