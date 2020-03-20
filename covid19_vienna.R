# packages
library(tidyverse)
library(scales)
library(patchwork)

# reported infections
# source: APA OTS (https://www.ots.at)
# keywords: "kennzahlen corona wien"
infected <- c(
   16, #"2020-03-05"
   23, #"2020-03-06"
   23, #"2020-03-07"
   32, #"2020-03-08"
   33, #"2020-03-09"
   43, #"2020-03-10"
   50, #"2020-03-11"
   66, #"2020-03-12"
   74, #"2020-03-13"
   85, #"2020-03-14"
  122, #"2020-03-15"
  128, #"2020-03-16"
  166, #"2020-03-17"
  185, #"2020-03-18"
  240, #"2020-03-19"
  308  #"2020-03-20"
)

# prepare data
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


# visualise data
# infected
last_day <- length(data$infected)
p0 <- data_plot %>% 
  mutate(infected_M = infected / 1000000) %>%
  ggplot(aes(day, infected_M, color = type)) + 
  geom_line(size = 1.5) +
  geom_vline(xintercept = c(last_day, last_day + 28), 
             linetype = "dotted") +
  ylim(0,1) +
  xlab("Days since outbreak") +
  ylab("Infections in Mio") + 
  #ggtitle("Covid-19 outbreak in Vienna") +
  theme_minimal()+
  annotate("text", last_day/2, 0.9, 
           label = "until today", size = 2.5) +
  annotate("text", last_day+14, 0.9, 
           label = "next 4 weeks", size = 2.5)

#############################################

# data for reference line 33% growth (Austria)
data_line_start <- tibble(
  day = 10,
  infected = 85
)

# predict 33% growth (days since 50 cases)
data_line <- predict_corona(
  data_line_start,
  infection_rate = 1.33,
  days = 7)

# infected
p1 <- data %>% 
  mutate(infected_M = infected / 1000000) %>% 
  ggplot(aes(day, infected)) + 
  geom_line(size = 1.5, color = "red") +
  geom_line(data = data_line, 
            aes(day,infected), color = "grey", alpha = 0.7) +
  xlim(c(1,length(infected)+1)) +
  ylim(0,max(infected)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
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
                label = paste0(format(new_pct, digits=1),"%")),
            size = 2) +
  geom_hline(yintercept = 33, linetype = "dotted") +
  xlim(c(1,length(infected)+1)) +
  ylim(c(0,100)) +
  xlab("Days since outbreak") +
  ylab("Daily growth in %") + 
#  ggtitle("Covid-19 outbreak in Vienna") +
  theme_minimal() +
  annotate("text", 2.5, 33, 
           label = "33% growth",
           size = 2,
           vjust = "bottom"
  ) 

# combine plots
p <- ((p1 / p2) | p0) + plot_annotation('Covid-19 outbreak in Vienna',
                            caption = "source: APA OTS https://www.ots.at ('kennzahlen corona wien')")
# plot
p

# save plot
p %>% ggsave(filename = "covid-19-vienna.png", 
             device = "png",
             width = 7, height = 4)
