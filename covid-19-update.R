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

# deaths
cat("generate plot deaths...\n")
p1 <- deaths %>% 
  covid19_plot_deaths_ranking(top_n = 20,
                              title = "Covid-19  - deaths")

p2 <- deaths %>% 
  covid19_plot_deaths(countries = c("Austria","Germany","Italy","US","United Kingdom"), 
                      min_deaths = 50, log = TRUE,
                      title = "Covid-19 - deaths (logarithmic)")


p <- (p1 | p2)

p %>% covid19_save_plot("covid-19-deaths.png")

# infected World
p1 <- data %>% covid19_plot_infected("China/Hubei", title = "China/Hubei") + theme(legend.position = "none")
p2 <- data %>% covid19_plot_infected("Korea, South", title = "Korea, South")+ theme(legend.position = "none")
p3 <- data %>% covid19_plot_infected("Japan", title = "Japan")+ theme(legend.position = "none")
p4 <- data %>% covid19_plot_infected("US", title = "US")+ theme(legend.position = "none")
p5 <- data %>% covid19_plot_infected("Brazil", title = "Brazil")+ theme(legend.position = "none")
p6 <- data %>% covid19_plot_infected("South Africa", title = "South Africa")+ theme(legend.position = "none")

p <- (p1 | p2 | p3) / (p4 | p5 | p6)
p %>% covid19_save_plot("covid-19-world-infected.png")

# infected Europe
p7 <- data %>% covid19_plot_infected("Italy", title = "Italy")+ theme(legend.position = "none")
p8 <- data %>% covid19_plot_infected("Spain", title = "Spain")+ theme(legend.position = "none")
p9 <- data %>% covid19_plot_infected("United Kingdom", title = "UK")+ theme(legend.position = "none")
p10 <- data %>% covid19_plot_infected("Germany", title = "Germany")+ theme(legend.position = "none")
p11 <- data %>% covid19_plot_infected("Netherlands", title = "Netherlands")+ theme(legend.position = "none")
p12 <- data %>% covid19_plot_infected("Belgium", title = "Belgium")+ theme(legend.position = "none")

p <- (p7 | p8 | p9) / (p10 | p11 | p12)
p %>% covid19_save_plot("covid-19-europe-infected.png")

# infected europe/east
p13 <- data %>% covid19_plot_infected("Belarus", title = "Belarus")+ theme(legend.position = "none")
p14 <- data %>% covid19_plot_infected("Slovenia", title = "Slovenia")+ theme(legend.position = "none")
p15 <- data %>% covid19_plot_infected("Serbia", title = "Serbia")+ theme(legend.position = "none")
p16 <- data %>% covid19_plot_infected("Croatia", title = "Croatia")+ theme(legend.position = "none")
p17 <- data %>% covid19_plot_infected("Bulgaria", title = "Bulgaria")+ theme(legend.position = "none")
p18 <- data %>% covid19_plot_infected("North Macedonia", title = "North Macedonia")+ theme(legend.position = "none")

p <- (p13 | p14 | p15) / (p16 | p17 | p18)
p %>% covid19_save_plot("covid-19-europeeast-infected.png")

# infected Austria
p <- data %>% covid19_plot_infected("Austria")
p %>% covid19_save_plot("covid-19-austria-infected.png")

# world
cat("generate plot world...\n")
suppressWarnings(source("covid19_austria_world.R"))

# europe
cat("generate plot europe...\n")
suppressWarnings(source("covid19_austria_europe.R"))

# austria
cat("generate plot austria...\n")
suppressWarnings(source("covid19_austria.R"))

# austria/vienna (update confirmed manually!)
cat("generate plot vienna...\n")
suppressWarnings(source("covid19_vienna.R"))

# done
cat("done!\n")