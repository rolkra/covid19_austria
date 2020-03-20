# Covid-19 Outbreak in Austria

**This COVID-19 analysis is for educational purpose!**

Data based on *2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins*
CSSE (https://github.com/CSSEGISandData/COVID-19)

<img src="covid-19-austria-world.png" alt="Covid-19 Austria" width="800">

<img src="covid-19-austria-europe.png" alt="Covid-19 Austria" width="800">

<img src="covid-19-austria.png" alt="Covid-19 Austria" width="800">

<img src="covid-19-vienna.png" alt="Covid-19 Vienna" width="800">

# Covid-19 R-Code Toolbox

You can use this toolbox to explore Covid-19 with R-Code:

```R
source("covid-19-toolbox.R")

data <- covid19_read_infected()

data %>% 
  covid19_plot_infected(
    countries = c("Austria","Italy","China/Hubei"),
    log = FALSE,
    min_infected = 100
  )

data %>% 
  covid19_plot_szenarios(country = "Spain")
```

You may need to install the following R-packages:

```R
install.packages(c("tidyverse", "lubridate", "scales")
```

# Additional Covid-19 Links

* https://coronavirus.jhu.edu/map.html
* https://orf.at/corona/daten
* https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_%C3%96sterreich
* https://medium.com/@tomaspueyo/coronavirus-the-hammer-and-the-dance-be9337092b56
* https://towardsdatascience.com/top-5-r-resources-on-covid-19-coronavirus-1d4c8df6d85f

