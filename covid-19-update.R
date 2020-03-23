# use toolbox
source("covid-19-toolbox.R")

# read data
confirmed <- covid19_read_confirmed()
deaths <- covid19_read_deaths()

# deaths
p <- deaths %>% 
  covid19_plot_deaths(top_n = 20,
                      title = "Covid-19 outbreak - deaths")

p %>% covid19_save_plot("covid-19-deaths.png")

# world
source("covid19_austria_world.R")

# europe
source("covid19_austria_europe.R")

# austria
source("covid19_austria.R")

# austria/vienna (update confirmed manually!)
source("covid19_vienna.R")
