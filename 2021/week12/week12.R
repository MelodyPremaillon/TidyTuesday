## Video Games and Sliced

#The data this week comes from Steam (https://www.kaggle.com/michau96/popularity-of-games-on-steam)
#Credit: [Mélody Prémaillon](Twitter handle or other social media profile)


#Packages
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggalt)
library(patchwork)
library(ggtext)
library(grid)
library(ggridges)
library(plotly)
library(lubridate)

font_add_google("Montserrat", "Montserrat")
font_add_google("Lobster","Lobster")
font_add_google("Rubik", "Rubik")
showtext_auto()

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2021-03-16')
tuesdata <- tidytuesdayR::tt_load(2021, week = 12)
games <- tuesdata$games
rm(tuesdata)
games$avg_peak_perc2 <- as.numeric(str_replace(games$avg_peak_perc, "%",""))
games$date <- as.Date(paste(games$year, match(games$month, month.name), 1, sep="/"))

###Creating a data frame with the games that record more than 150 000 players playing simultenaously the month they where released
releaseddates <- games %>% group_by(gamename) %>% summarise(releasedate = min(date)) #df of release date for each game
games$key <- paste(games$gamename, games$date) #creating key for merging
releaseddates$key <- paste(releaseddates$gamename, releaseddates$releasedate)

releasedata <- merge(games, releaseddates, by= "key") %>% 
  filter(peak > 150000) %>%
  arrange(desc(peak)) #df  


###Plotting the data
myplot <- ggplot(releasedata, aes(date, peak)) + geom_bar(stat="identity",  fill= "#3C7C9C", alpha=0.4) +
  coord_flip() + 
  geom_text(aes(date, label= gamename.x), 
            color= "#C6C5C1", family= "Rubik", 
            size=6,
            hjust=0.2) +
  ###axes
  scale_y_continuous(breaks = seq(100000,900000, by=100000),
                     labels = seq(100,900,by=100),
                     limits = c(0,1005000),
                     name = "SIMULTANEOUS PLAYERS PEAK \n[thousands of players]") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               date_minor_breaks = "1 month",
               name= toupper("Release date")) +
  ##theme
  theme(
    panel.background = element_rect(fill="#183D57"),
    plot.background = element_rect(fill="#183D57"),
    panel.grid.major = element_line(color = "#1B1F26", size=0.8),
    panel.grid.minor.x =  element_blank(),
    panel.grid.minor.y =  element_line(color="#1B1F26",size=0.2),
    axis.title = element_text(size=16),
    axis.text = element_text(size=12,color="#3C7C9C"),
    text = element_text(family = "Rubik",color="#3C7C9C"),
  )

final_plot <- myplot + plot_annotation(title="Entrées les plus fracassantes",
                                       subtitle = "Games that record more than 150 000 players simultaneously <br>
                                       the month they were released",                                  
                                       theme = theme(text = element_text('Rubik',  colour = "#C1C0BB"),
                                                     plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
                                                     plot.subtitle = element_markdown(size = 14, hjust = 0.5),
                                                     plot.background = element_rect(fill ="#183D57", color="#183D57"),
                                                     plot.margin = unit(c(1, 0, 0, 0), "cm")))  
final_plot
