##########################################################
## update <ctr> <shift> s (source)
##########################################################

# use toolbox
suppressWarnings(source("covid-19-toolbox.R"))

# read data
cat("read data...\n")
confirmed <- covid19_read_confirmed()
deaths <- covid19_read_deaths()

# deaths
cat("generate plot deaths...\n")
p <- deaths %>% 
  covid19_plot_deaths(top_n = 20,
                      title = "Covid-19 outbreak - deaths")

p %>% covid19_save_plot("covid-19-deaths.png")

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