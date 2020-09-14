##########################################################
## update <ctr> <shift> s (source)
##########################################################

# use toolbox
suppressWarnings(source("covid-19-toolbox.R"))

# read data
cat("read data...\n")
confirmed <- covid19_read_confirmed()
deaths <- covid19_read_deaths()
recovered <- covid19_read_recovered()
data <- covid19_combine_data(confirmed, deaths, recovered)

# A1 dashboard
cat("generate dashboard...\n")
countries <- c("Austria","Belarus","Slovenia","Croatia","Serbia","North Macedonia", "Bulgaria")
title <- paste0("Covid-19 Infections (",Sys.Date(),")")

p1 <- data %>% 
  covid19_plot_cases(var = confirmed, 
                     countries = countries,
                     min_cases = 0,
                     title = title,
                     ylab="Confirmed infections per day")  +
  labs(subtitle = "We are tired of the virus, but the virus does't care!")



p2 <- data %>% 
    covid19_plot_cases(var = new_abs, 
                       countries = countries,
                       min_cases = 0,
                       title = NULL,
                       ylab="New confirmed infections per day")
  
p <- p1 / p2
p %>% covid19_save_plot("covid-19-dashboard.png",
                        width = 7, heigh = 7)

# deaths
cat("generate plot deaths...\n")
p1 <- deaths %>% 
  mutate(country = ifelse(country == "United Kingdom", "UK", country)) %>% 
  covid19_plot_deaths_ranking(top_n = 20,
                              title = "Covid-19  - deaths")

p2 <- deaths %>% 
  mutate(country = ifelse(country == "United Kingdom", "UK", country)) %>% 
  covid19_plot_deaths(countries = c("Austria","Germany","Italy","US","United Kingdom","UK"), 
                      min_deaths = 50, log = TRUE,
                      title = "Covid-19 - deaths (logarithmic)")


p <- (p1 | p2)

p %>% covid19_save_plot("covid-19-deaths.png")

# infected Asia
cat("generate plots infected...\n")
p1 <- data %>% covid19_plot_infected("China/Hubei", title = "China/Hubei") + theme(legend.position = "none")
p2 <- data %>% covid19_plot_infected("Korea, South", title = "Korea, South")+ theme(legend.position = "none")
p3 <- data %>% covid19_plot_infected("Japan", title = "Japan")+ theme(legend.position = "none")
p4 <- data %>% covid19_plot_infected("Iran", title = "Iran")+ theme(legend.position = "none")
p5 <- data %>% covid19_plot_infected("India", title = "India")+ theme(legend.position = "none")
p6 <- data %>% covid19_plot_infected("Turkey", title = "Turkey")+ theme(legend.position = "none")
p7 <- data %>% covid19_plot_infected("US", title = "US")+ theme(legend.position = "none")
p8 <- data %>% covid19_plot_infected("Brazil", title = "Brazil")+ theme(legend.position = "none")
p9 <- data %>% covid19_plot_infected("Argentina", title = "Argentina")+ theme(legend.position = "none")
p10 <- data %>% covid19_plot_infected("Russia", title = "Russia")+ theme(legend.position = "none")
p11 <- data %>% covid19_plot_infected("Israel", title = "Israel")+ theme(legend.position = "none")
p12 <- data %>% covid19_plot_infected("South Africa", title = "South Africa")+ theme(legend.position = "none")

p <- (p1 | p2 | p3) / (p4 | p5 | p6) / (p7 | p8 | p9) / (p10 | p11 | p12) +
  plot_annotation(title = "Covid-19 outbreak (World)")

p %>% covid19_save_plot("covid-19-infected-world.png", height = 4*2)

# Infected Europe
p1 <- data %>% covid19_plot_infected("Italy", title = "Italy")+ theme(legend.position = "none")
p2 <- data %>% covid19_plot_infected("Spain", title = "Spain")+ theme(legend.position = "none")
p3 <- data %>% covid19_plot_infected("France", title = "France")+ theme(legend.position = "none")
p4 <- data %>% covid19_plot_infected("United Kingdom", title = "UK")+ theme(legend.position = "none")
p5 <- data %>% covid19_plot_infected("Germany", title = "Germany")+ theme(legend.position = "none")
p6 <- data %>% covid19_plot_infected("Netherlands", title = "Netherlands")+ theme(legend.position = "none")

p7 <- data %>% covid19_plot_infected("Sweden", title = "Sweden")+ theme(legend.position = "none")
p8 <- data %>% covid19_plot_infected("Belgium", title = "Belgium")+ theme(legend.position = "none")
p9 <- data %>% covid19_plot_infected("Switzerland", title = "Switzerland")+ theme(legend.position = "none")
p10 <- data %>% covid19_plot_infected("Hungary", title = "Hungary")+ theme(legend.position = "none")
p11 <- data %>% covid19_plot_infected("Poland", title = "Poland")+ theme(legend.position = "none")
p12 <- data %>% covid19_plot_infected("Romania", title = "Romania")+ theme(legend.position = "none")

p13 <- data %>% covid19_plot_infected("Belarus", title = "Belarus")+ theme(legend.position = "none")
p14 <- data %>% covid19_plot_infected("Slovenia", title = "Slovenia")+ theme(legend.position = "none")
p15 <- data %>% covid19_plot_infected("Serbia", title = "Serbia")+ theme(legend.position = "none")
p16 <- data %>% covid19_plot_infected("Croatia", title = "Croatia")+ theme(legend.position = "none")
p17 <- data %>% covid19_plot_infected("Bulgaria", title = "Bulgaria")+ theme(legend.position = "none")
p18 <- data %>% covid19_plot_infected("North Macedonia", title = "North Macedonia")+ theme(legend.position = "none")

p <- (p1 | p2 | p3) / (p4 | p5 | p6) / 
     (p7 | p8 | p9) / (p10 | p11 | p12) /
     (p13 | p14 | p15) / (p16 | p17 | p18) +
  plot_annotation(title = "Covid-19 outbreak in Europe")
                  
p %>% covid19_save_plot("covid-19-infected-europe.png", height = 4*3)

# infected Austria
title <- paste0("Covid-19 in Austria (",Sys.Date(),")")
p <- data %>% covid19_plot_infected("Austria", title = title)
p %>% covid19_save_plot("covid-19-austria-infected.png")

# world
cat("generate plot world...\n")
suppressWarnings(source("covid19_austria_world.R"))

# europe
cat("generate plot europe...\n")
suppressWarnings(source("covid19_austria_europe.R"))

# austria
cat("generate plot austria...\n")
p <- confirmed %>% covid19_plot_overview("Austria")
p %>% covid19_save_plot("covid-19-austria.png")
#suppressWarnings(source("covid19_austria.R"))

# austria/vienna (update confirmed manually!)
cat("generate plot vienna...\n")
suppressWarnings(source("covid19_vienna.R"))

# done
cat("done!\n")