#################################################
## setup
#################################################

# packages
library(tidyverse)
library(patchwork)
library(lubridate)
library(scales)

#################################################
## read file
#################################################

covid19_read_file <- function(file = NA, type, raw = FALSE, countries = NA)  {

  
  # read data (https://github.com/CSSEGISandData/COVID-19)
  data <- read.csv(file)

  # keep raw?
  if (raw) {
    return(data)
  }
    
  data <- data %>% 
    pivot_longer(cols = starts_with("X"),
                 names_to = "date") %>% 
    mutate(date = substr(date,2,8)) %>% 
    mutate(date = mdy(date))
  
  # convert factors to character
  data <- data %>% 
    mutate(Country.Region = as.character(Country.Region)) %>% 
    mutate(Province.State = as.character(Province.State))

  # add country and type
  data <- data %>% 
    mutate(country = ifelse((Province.State == "" | Country.Region == Province.State), 
                            Country.Region, 
                            paste0(Country.Region,"/",Province.State)), 
           type = type) 

  # add day, infected
  data <- data %>% 
    arrange(country, date) %>% 
    group_by(country) %>% 
    mutate(day = row_number())
  
  # new infections
  data <- data %>% 
    group_by(country) %>% 
    mutate(new_abs = value - lag(value),
           new_pct = new_abs / lag(value) * 100) %>% 
    ungroup()

  # filter countries  
  if (!is.na(countries))  {
    data <- data %>% 
      filter(country %in% countries) %>% 
      filter(value > 0)
  } #if
  
  # return data
  data
    
} #covid19_read_infected

#################################################
## read confirmed
#################################################

covid19_read_confirmed <- function(raw = FALSE, countries = NA)  {

  # define file
  file <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

  
  # read data (https://github.com/CSSEGISandData/COVID-19)
  data <- covid19_read_file(file, type = "confirmed", raw = raw, countries = countries)

  # create new variable
  data <- data %>% 
    mutate(infected = value) %>% 
    mutate(confirmed = value)
  
  # output
  data
  
} #function
  
#################################################
## read recovered
#################################################

covid19_read_recovered <- function(raw = FALSE, countries = NA)  {
  
  # define file
  file <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
  
  # read data (https://github.com/CSSEGISandData/COVID-19)
  data <- covid19_read_file(file, type = "recovered", raw = raw, countries = countries)
  
  # create new variable
  data <- data %>% 
    mutate(recovered = value)

    # output
  data
  
} #function

#################################################
## read deaths
#################################################

covid19_read_deaths <- function(raw = FALSE, countries = NA)  {
  
  # define file
  file <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  
  # read data (https://github.com/CSSEGISandData/COVID-19)
  data <- covid19_read_file(file, type = "deaths", raw = raw, countries = countries)
  
  # create new variable
  data <- data %>% 
    mutate(deaths = value)
  
  # output
  data
  
} #function

#################################################
## combine data
#################################################

covid19_combine_data <- function(confirmed, deaths, recovered) {
  
  # confirmed
  d1 <- confirmed %>% 
    select(country, date, day, type, value)
  
  # deaths
  d2 <- deaths %>% 
    select(country, date, day, type, value)
  
  # recovered
  d3 <- recovered %>% 
    select(country, date, day, type, value)
  
  # combine
  d <- d1 %>% 
    bind_rows(d2) %>% 
    bind_rows(d3) %>% 
    pivot_wider(names_from = type, values_from = value)
  
  # calculations
  d <- d %>% 
    mutate(deaths_pct = ifelse(confirmed > 0, deaths/confirmed*100, 0)) %>% 
    mutate(recovered_pct = ifelse(confirmed > 0, recovered/confirmed*100, 0))
  
  # return
  d
  
} #function

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

covid19_plot_confirmed <- function(data, countries = "Austria", highlight_country = NA, log = FALSE, min_confirmed = 50, title = NA) {

  # filter countries
  data <- data %>% filter(country %in% countries)

  # default value for highlight_country
  if (is.na(highlight_country))  {
    highlight_country <- countries[1]
  }
  
  # overwrite day
  data <- data %>% 
    filter(infected >= min_confirmed) %>% 
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
    labs(x = paste("Days since", min_confirmed, "cases"), 
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
    ylab("Infections in Mio") + 
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

#################################################
## show deaths
#################################################

covid19_show_deaths <- function(data, top_n = 10)  {
  
  # countries by deaths
  data %>% 
    group_by(country) %>% 
    summarise(deaths = max(deaths, na.rm = TRUE)) %>% 
    arrange(-deaths)

} #function  

#################################################
## plot deaths
#################################################

covid19_plot_deaths <- function(data, top_n = 10, title = NULL)  {
  
  # countries by deaths
    top_deaths <- data %>% 
      covid19_show_deaths(top_n)

  # plot  
  p <- top_deaths %>% 
    top_n(top_n, deaths) %>% 
    mutate(country = factor(country)) %>% 
    ggplot(aes(reorder(country, deaths), deaths, label = deaths)) +
    geom_col(fill = "grey") + 
    geom_text(aes(label = format(deaths, big.mark=" ")), 
              hjust = 0, nudge_y = max(top_deaths$deaths, na.rm = TRUE)/200, size = 2.5) +
    #ylim(0, max(top_deaths$deaths)*1.2) +
    scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE),
                       limits=c(0, max(top_deaths$deaths, na.rm = TRUE)*1.2)) +
    xlab("") +
    theme_light() +
    coord_flip()
  p
  
  # overrule title?
  if (!missing(title)) {
    p <- p + ggtitle(title)
  }
  
  # result
  p
  
} #function  

#################################################
## save plot
#################################################

covid19_save_plot <- function(plot, filename)  {
  
  plot %>% ggsave(filename = filename, 
                device = "png",
                width = 7, height = 4)
} #function

#################################################
## overview
#################################################

covid19_plot_overview <- function(data, country = "Austria", log = FALSE, title = NULL)  {
    
  p1 <- data %>% 
    covid19_plot_confirmed(countries = country, 
                           log = log,
                           title = NULL)
  
  p2 <- data %>% 
    covid19_plot_daily_growth(country = country,
                              title = NULL)
  
  p3 <- data %>% 
    covid19_plot_szenarios(country = country,
                           title = NULL)
  
  if (!missing(title)) {
    ((p1 / p2) | p3) + plot_annotation(title,
                                       caption = "source: https://github.com/CSSEGISandData/COVID-19")
  } else {    
    ((p1 / p2) | p3) + plot_annotation(paste('Covid-19 outbreak in', country),
                                       caption = "source: https://github.com/CSSEGISandData/COVID-19")
  } #if
} #function


#################################################
## show death rate
#################################################

covid19_show_death_rate <- function(data, countries, min_confirmed = 100)  {

  if(!missing(countries))  {
    data <- data %>% 
      filter(country %in% countries)
  }
  
  data %>% 
    filter(confirmed >= min_confirmed) %>% 
    arrange(country, -day) %>% 
    group_by(country) %>% 
    top_n(1, day) %>% 
    select(country, deaths_pct, confirmed, deaths) %>% 
    arrange(-deaths_pct)
  
} #function
