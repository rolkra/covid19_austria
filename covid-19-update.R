# use toolbox
source("covid-19-toolbox.R")

# read data
confirmed <- covid19_read_confirmed()
deaths <- covid19_read_deaths()

# deaths
p <- deaths %>% 
  covid19_plot_deaths(title = "Covid-19 outbreak - deaths")

p %>% covid19_save_plot("covid-19-deaths.png")

# world
countries <- c("Austria","Italy","Spain","China/Hubei", "Korea, South", "Japan","Taiwan*","Iran")

p <- confirmed %>% 
  covid19_plot_confirmed(countries, log = TRUE)

p
 
