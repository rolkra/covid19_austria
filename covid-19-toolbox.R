#################################################
## setup
#################################################

# packages
library(tidyverse)
library(patchwork)
library(lubridate)
library(scales)

#################################################
## read infected
#################################################

covid19_read_infected <- function(raw = FALSE, countries = NA)  {

  # read data (https://github.com/CSSEGISandData/COVID-19)
  data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

  # keep raw?
  if (raw) {
    return(data)
  }
    
  data <- data %>% 
    pivot_longer(cols = starts_with("X"),
                 names_to = "date") %>% 
    mutate(date = substr(date,2,8)) %>% 
    mutate(date = mdy(date))
  
  # recode China/Hubei, US
  data <- data %>% 
    mutate(Country.Region = as.character(Country.Region)) %>% 
    mutate(Province.State = as.character(Province.State)) %>% 
    mutate(Country.Region = ifelse(Country.Region == "China" & Province.State == "Hubei", "China/Hubei", Country.Region)) %>% 
    mutate(Province.State = ifelse(Country.Region == "China/Hubei", "China/Hubei", Province.State)) %>% 
    mutate(Country.Region = ifelse(Country.Region == "US" & Province.State == "New York", "US/New York", Country.Region)) %>% 
    mutate(Province.State = ifelse(Country.Region == "US/New York", "US/New York", Province.State))
  
  # filter countries  
  if (!is.na(countries))  {
    data <- data %>% 
      filter(Country.Region %in% countries,
             (Province.State =="" | Province.State %in% countries),
             value > 0)
  } #if
  
  # add country and type
  data <- data %>% 
    mutate(country = Country.Region, 
           type = "confirmed") 

  # add day, infected
  data <- data %>% 
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

  # return data
  data
    
} #covid19_read_infected

#################################################
## predict infections (for one country)
#################################################

covid19_predict_infections <- function(data, country = NA, infection_rate = 1.33, days = 7)  {

  if (!is.na(country))  {
    data <- data %>% filter(country == .env$country)
  }
    
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
  
} # predict_infections

#################################################
## plot infected
#################################################

covid19_plot_infected <- function(data, countries = "Austria", highlight_country = NA, log = FALSE, min_infected = 50, title = NA) {

  # filter countries
  data <- data %>% filter(country %in% countries)

  # default value for highlight_country
  if (is.na(highlight_country))  {
    highlight_country <- countries[1]
  }
  
  # overwrite day
  data <- data %>% 
    filter(infected >= min_infected) %>% 
    arrange(country, day) %>% 
    group_by(country) %>% 
    mutate(day = row_number()) %>% 
    ungroup() %>% 
    mutate(infected_M = infected / 1000000)
  
  # plot
  p <- ggplot(data = data %>% filter(!country %in% highlight_country), 
         aes(day,infected, colour = country)) +
    geom_line(alpha = 0.7, size = 1.1) +
    geom_line(data = data %>% filter(country %in% highlight_country),
              aes(day,infected, colour = country), 
              alpha = 1, size = 1.5) +
    #geom_line(data = data_line, 
    #          aes(day,infected), color = "grey", alpha = 0.7) +
    labs(x = paste("Days since", min_infected, "cases"), 
         y = "Confirmed infections") +
    theme_minimal()

  if (log) {
    p <- p + 
      scale_y_log10(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
      ggtitle("Covid-19 outbreak (logarithmic)")
  } else {
    p <- p + 
      scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
      ggtitle("Covid-19 outbreak")
  } #if

  # overrule title?
  if (!missing(title)) {
    p <- p + ggtitle(title)
  }
    
  # output
  p
  
} #covid19_plot_infected

#################################################
## plot szenarios
#################################################

covid19_plot_szenarios <- function(data, country = "Austria", predict_days = 50, max_mio = 2.5, title = NA)  {

  predict_data <- data %>% 
    filter(country == .env$country) %>% 
    select(type, day, infected)
  
  data_33 <- predict_data %>% 
    covid19_predict_infections(infection_rate = 1.33, days = predict_days)
  
  data_20 <- predict_data %>% 
    covid19_predict_infections(infection_rate = 1.20, days = predict_days)
  
  data_15 <- predict_data %>% 
    covid19_predict_infections(infection_rate = 1.15, days = predict_days)
  
  data_10 <- predict_data %>% 
    covid19_predict_infections(infection_rate = 1.10, days = predict_days)
  
  # combine dataset
  data_plot <- predict_data %>% 
    bind_rows(data_33, data_20, data_15, data_10)
  
  # visualise
  last_day <- nrow(predict_data)
  p <- data_plot %>% 
    mutate(infected_M = infected / 1000000) %>% 
    ggplot(aes(day, infected_M, color = type)) + 
    geom_line(size = 1.5) +
    geom_vline(xintercept = c(last_day, last_day + 28), 
               linetype = "dotted") +
    ylim(0, max_mio) +
    xlab("Days since outbreak") +
    ylab("Confirmed infections in Mio") + 
    #ggtitle("Covid-19 outbreak in Austria") +
    theme_minimal()+
    annotate("text", last_day/2, max_mio*0.9, 
             label = "until\ntoday", size = 2.5) +
    annotate("text", last_day+14, max_mio*0.9, 
             label = "next\n4 weeks", size = 2.5)

    # title
    if (missing(title)) {
      p <- p + ggtitle(paste("Covid-19 outbreak in", country))
    } else {
      p <- p + ggtitle(title)
    } 
  
    # output
    p
    
} #covid19_plot_szenarios

#################################################
## plot daily growth
#################################################

covid19_plot_daily_growth <- function(data, country = "Austria", min_infected = 50, title = NA)  {
  
  # filter countries
  data <- data %>% filter(country == .env$country)

  # overwrite day
  data <- data %>% 
    filter(infected >= 1) %>% 
    arrange(country, day) %>% 
    group_by(country) %>% 
    mutate(day = row_number()) %>% 
    ungroup() %>% 
    mutate(infected_M = infected / 1000000)
  
  # daily growth infected
  p <- data %>% 
    filter(infected >= min_infected) %>% 
    ggplot(aes(day, new_pct)) +
    geom_col(fill = "grey") 
  
  p <- p +
    geom_hline(yintercept = 33, color = "darkgrey", alpha = 0.7) +
    geom_hline(yintercept = 10, color = "darkgrey", alpha = 0.7)
    
  if (nrow(data) <= 30) {
    p <- p + 
      geom_text(aes(day, new_pct, label = 
                      paste0(format(new_pct, digits=0))),
                size = 2, 
                vjust = "bottom", nudge_y = 1)
    
  } #if
  
  p <- p +
    #ylim(c(0,75)) +
    xlab(paste("Days")) +
    ylab("Daily growth in %") + 
    #  ggtitle("Covid-19 outbreak in Vienna") +
    theme_minimal() +
    annotate("text", min(data$day), 33+1, 
             label = "33% growth",
             size = 2, 
             vjust = "bottom", hjust = "left") +
    annotate("text", min(data$day), 10+1, 
             label = "10% growth",
             size = 2, 
             vjust = "bottom", hjust = "left")
  

  # title
  if (missing(title)) {
    p <- p + ggtitle(paste("Covid-19 outbreak in", country))
  } else {
    p <- p + ggtitle(title)
  } 
      
  # output
  p
  
} #covid19_plot_daily_growth
