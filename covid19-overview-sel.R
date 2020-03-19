# packages
library(dplyr)
library(ggplot2)
library(scales)
library("zoo")

# read data
aaa<- read.csv("https://covid.ourworldindata.org/data/full_data.csv")
aaa$location<- as.character(aaa$location)
aaa$date<- as.character.Date(aaa$date)

# prepare
#countries <- c("Austria", "Switzerland", "Denmark", "Italy", "Spain", "France")
countries <- c("Austria", "Slovenia", "Croatia", "Serbia", "Northmazedonia","Bulgaria","Belarus")
highlight_country <- c("Austria")
main <- aaa %>% filter(location %in% countries)
main$death_rate <- main$total_deaths/main$total_cases
main$death_rate[is.na(main$total_deaths)] <- 0
row <- data.frame("a"=1)
for (i in countries) {
  assign(i, main %>% filter(location == i & (total_cases > 0 ) ))
  row$a <- cbind(row$a,nrow(get(i)))
}
counting <-data.frame(counting=1:max(row$a))
for (i in countries) {
  ZZZ<-data.frame(matrix(NA, nrow = max(row$a) -nrow(get(i)) , ncol = 7))
  names(ZZZ)<-names(get(i))
  assign(i, rbind(get(i), ZZZ))
  assign(i, cbind(get(i), counting))
}
Toty<- data.frame(matrix(NA, nrow = 0 , ncol = 8))
names(Toty) <- names(get(i))
for (i in countries) {
  Toty <- rbind(Toty, get(i))
}
Toty$location <-na.locf(Toty$location)
today <-format(Sys.Date(), format="%d_%m")

Toty <- Toty %>% 
  mutate(highlight = ifelse(location %in% highlight_country, 1, 0.7))

# plot
p <- ggplot(Toty %>% filter(!location %in% highlight_country), 
       aes(counting,total_cases, colour = location)) +
  geom_line(alpha = 0.7, size = 1.1) +
  geom_line(data = Toty %>% filter(location %in% highlight_country),
            aes(counting,total_cases, colour = location), 
            alpha = 1, size = 1.5) +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
    labs(x = "Days since first infection", 
         y = "Total Cases",
         title = "Covid-19 Infections in Europe",
         subtitle = "Source: https://covid.ourworldindata.org") +
  theme_minimal()


# plot
p

# save plot
p %>% ggsave(filename = "covid-19-overview-sel.png", 
             device = "png",
             width = 7, height = 4)


