# packages
library(tidyverse)
library(scales)
library(patchwork)

# reported infections
# source: APA OTS (https://www.ots.at)
# keywords: "kennzahlen corona wien"
val <- c(
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
  308, #"2020-03-20"
  359, #"2020-03-21"
  413, #"2020-03-22"
  488, #"2020-03-23"
  542, #"2020-03-24"
  666, #"2020-03-25"
  776, #"2020-03-26"
  933, #"2020-03-27"
 1059, #"2020-03-28"
 1107, #"2020-03-29"
 1128, #"2020-03-30"
 1327, #"2020-03-31"
 1456, #"2020-04-01"
 1580, #"2020-04-02"
 1646, #"2020-04-03"
 1701, #"2020-04-04"
 1732, #"2020-04-05" 
 1750, #"2020-04-06"
 1818, #"2020-04-07"
 1869, #"2020-04-08"
 1924, #"2020-04-09"
 1952, #"2020-04-10"
 1989, #"2020-04-11"
 2029, #"2020-04-12"
 2044, #"2020-04-15"
 2121, #"2020-04-16"
 2149, #"2020-04-17"
 2197, #"2020-04-18"
 2228, #"2020-04-19"
 2241, #"2020-04-20"
 2260, #"2020-04-21"
 2278, #"2020-04-22"
 2307, #"2020-04-23"
 2338, #"2020-04-24"
 2366, #"2020-04-25"
 2393
)

# prepare data
data <- tibble(confirmed = val)

data <- data %>% 
  mutate(infected = val) %>% 
  mutate(country = "Vienna") %>% 
  mutate(type = "confirmed",
         day = row_number()) %>% 
  arrange(day) %>% 
  mutate(new_abs = confirmed - lag(confirmed),
         new_pct = new_abs / lag(confirmed) * 100)

p1 <- data %>% covid19_plot_confirmed("Vienna", title = NULL) +
                  theme(legend.position = "none")
p2 <- data %>% filter(confirmed>= 50) %>% covid19_plot_daily_growth("Vienna", title = NULL) + xlab("Days since 50 cases")
p3 <- data %>% covid19_plot_cases(var = new_abs, countries = "Vienna", title = NULL, ylab="New confirmed infections per day") +
                  theme(legend.position = "none")
p <- ((p1 / p2) | p3) + 
  plot_annotation(title = "Covid-19 outbreak in Vienna",
                  caption = "source: https://www.ots.at ('Kennzahlen Wien Corona')")
p %>% covid19_save_plot("covid-19-vienna.png") 

