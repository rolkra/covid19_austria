# packages
library(tidyverse)
library(patchwork)
library(lubridate)
library(scales)

# read data (https://github.com/CSSEGISandData/COVID-19)
raw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

work <- raw %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "date") %>% 
  mutate(date = substr(date,2,8)) %>% 
  mutate(date = mdy(date))

# recode China/Hubei
work <- work %>% 
  mutate(Country.Region = as.character(Country.Region)) %>% 
  mutate(Province.State = as.character(Province.State)) %>% 
  mutate(Country.Region = ifelse(Country.Region == "China" & Province.State == "Hubei", "China/Hubei", Country.Region)) %>% 
  mutate(Province.State = ifelse(Country.Region == "China/Hubei", "China/Hubei", Province.State))

# define countries
countries <- c("Austria","Italy","Spain","China/Hubei", "Korea, South", "Japan","Taiwan*","Iran")

# filter countries
work <- work %>% 
  filter(Country.Region %in% countries,
         (Province.State =="" | Province.State %in% countries),
         value > 0) %>% 
  mutate(country = Country.Region,
         type = "reported") 

# days
data <- work %>% 
  arrange(country, date) %>% 
  group_by(country) %>% 
  mutate(day = row_number(),
         infected = value)

# new infections
data <- data %>% 
  group_by(country) %>% 
  mutate(new_abs = infected - lag(infected),
         new_pct = new_abs / lag(infected) * 100) %>% 
  ungroup()

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

#############################################
# predict growth
#############################################

predict_days <- 50 #57

predict_data <- data %>% 
  filter(country == "Austria") %>% 
  select(type, day, infected)

data_50 <- predict_data %>% 
  predict_corona(infection_rate = 1.50, days = predict_days)

data_40 <- predict_data %>% 
  predict_corona(infection_rate = 1.40, days = predict_days)

data_33 <- predict_data %>% 
  predict_corona(infection_rate = 1.33, days = predict_days)

data_20 <- predict_data %>% 
  predict_corona(infection_rate = 1.20, days = predict_days)

data_15 <- predict_data %>% 
  predict_corona(infection_rate = 1.15, days = predict_days)

data_10 <- predict_data %>% 
  predict_corona(infection_rate = 1.10, days = predict_days)

# combine dataset
data_plot <- predict_data %>% 
  bind_rows(data_50, data_40, data_33, data_20, data_15, data_10)

# visualise
last_day <- nrow(predict_data)
p0 <- data_plot %>% 
  mutate(infected_M = infected / 1000000) %>% 
  ggplot(aes(day, infected_M, color = type)) + 
  geom_line(size = 1.5) +
  geom_vline(xintercept = c(last_day, last_day + 28), 
             linetype = "dotted") +
  ylim(0,5.5) +
  xlab("Days since outbreak") +
  ylab("Confirmed infections in Mio") + 
  #ggtitle("Covid-19 outbreak in Austria") +
  theme_minimal()+
  annotate("text", last_day/2, 5, 
           label = "until today", size = 2.5) +
  annotate("text", last_day+14, 5, 
           label = "next 4 weeks", size = 2.5)

#############################################
# confirmed infections
#############################################

# data by country (>= 50 confirmed infections)
data_countries <- data %>% 
  filter(infected >= 50) %>% 
  arrange(country, day) %>% 
  group_by(country) %>% 
  mutate(day = row_number()) %>% 
  ungroup() %>% 
  mutate(infected_M = infected / 1000000)

# data for reference line 33% growth (Austria)
data_line_start <- tibble(
  day = 1,
  infected = 55
)

# predict 33% growth (days since 50 cases)
data_line33 <- predict_corona(
  data_line_start,
  infection_rate = 1.33,
  days = 30)

data_line10 <- predict_corona(
  data_line_start,
  infection_rate = 1.10,
  days = 30)

highlight_country <- "Austria"

# infected
p1 <- ggplot(data = data_countries %>% filter(!country %in% highlight_country), 
             aes(day,infected, colour = country)) +
  geom_line(alpha = 0.7, size = 1.1) +
  geom_line(data = data_countries %>% filter(country %in% highlight_country),
            aes(day,infected, colour = country), 
            alpha = 1, size = 1.5) +
  geom_line(data = data_line10, 
            aes(day,infected), color = "grey", alpha = 0.7) +
  geom_line(data = data_line33, 
            aes(day,infected), color = "grey", alpha = 0.7) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_y_log10(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  labs(x = "Days since 50 cases", 
       y = "Confirmed infections") +
  ggtitle("Covid-19 outbreak (logarithmic)") +
  theme_minimal()

# daily growth infected
p2 <- data_countries %>% 
  filter(country == highlight_country) %>% 
  ggplot(aes(day, new_pct)) +
  geom_col(fill = "grey") +
  geom_text(aes(day, new_pct, 
                label = paste0(format(new_pct, digits=1),"%")),
            size = 2) +
  geom_hline(yintercept = 33, linetype = "dotted") +
  ylim(c(0,100)) +
  xlab("Days since 50 cases") +
  ylab("Daily growth in %") + 
  #  ggtitle("Covid-19 outbreak in Vienna") +
  theme_minimal() +
  annotate("text", 2.5, 33, 
           label = "33% growth",
           size = 2,
           vjust = "bottom"
  ) 

# combine plots
p <- ((p1 / p2) | p0) + plot_annotation('Covid-19 outbreak in Austria',
                            caption = "source: https://github.com/CSSEGISandData/COVID-19")

# plot
p

# save plot
p1 %>% ggsave(filename = "covid-19-austria-world.png", 
             device = "png",
             width = 7, height = 4)
