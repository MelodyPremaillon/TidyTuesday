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


font_add_google("Rubik", "Rubik")

# Get the Data5
tuesdata <- tidytuesdayR::tt_load('2021-03-16')
tuesdata <- tidytuesdayR::tt_load(2021, week = 12)
games <- tuesdata$games
rm(tuesdata)
games$date <- as.Date(paste(games$year, match(games$month, month.name), 1, sep="/"))

###Creating a data frame with the games that record more than 150 000 players playing simultenaously the month they where released
releaseddates <- games %>% 
  group_by(gamename) %>% 
  summarise(releasedate = min(date)) #df of release date for each game

games$key <- paste(games$gamename, games$date) #creating key for merging "games" data
releaseddates$key <- paste(releaseddates$gamename, releaseddates$releasedate)

releasedata <- merge(games, releaseddates, by= "key") %>% 
  filter(peak > 200000) %>%
  arrange(desc(peak)) #df of games with more than 200.000 simultaneous player the month they were released and all games data

#releasedatatot <- merge(games, releaseddates, by= "key")  

monthmax <- games %>% group_by(date) %>% summarise(avgplay = max(peak, na.rm = T))


###Plotting the data
bckgdcol <- "#171A21"

#myplot <-
  
myplot <- ggplot(releasedata, aes(date, peak)) + 
  geom_bar(stat="identity",  fill= "#3C7C9C") +
  coord_flip() + 
  geom_line(data=monthmax, aes(date, avgplay), alpha=0.5, color="#F28D35") + 
  geom_text(aes(date, label= toupper(gamename.x)), 
            color= "#C6C5C1", family= "Rubik", 
            size=5,
            hjust=0) +
  
  ###axes
  scale_y_continuous(breaks = seq(0,4E6, by=1E6),
                     labels = seq(0,4,by=1),
                     limits = c(0,4E6),
                     name = "Highest number of players at the same time \n[in millions]") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               date_minor_breaks = "1 month",
               name= "") +
  ##theme
  theme(
    panel.background = element_rect(fill=bckgdcol),
    plot.background = element_rect(fill=bckgdcol, color=bckgdcol),
    panel.grid.major = element_blank(), #element_line(color = "#1B1F26", size=0.8),
    panel.grid.minor.x =  element_blank(),
    panel.grid.minor.y =  element_blank(), #element_line(color="#1B1F26",size=0.2),
    axis.title = element_text(size=16),
    axis.text = element_text(size=12,color="#3C7C9C"),
    text = element_text(family = "Rubik",color="#3C7C9C"),
    axis.ticks.x = element_line(color="#3C7C9C")
  ) +
  annotate("text",x = as.Date("2013/01/01"), y=350000, label="Monthly maximum",
           color="#F28D35", hjust=0, alpha = 0.5, size=4) +
  annotate(geom = "curve", x = as.Date("2017/01/01"), y = 3E6, xend = as.Date("2018/01/01"), yend = 3.27E6, 
           curvature = .3, arrow = arrow(length = unit(2, "mm")), color="#F28D35", alpha=0.8) +
  annotate("text",x=as.Date("2016/08/01"),y=2.9E6, label="PUBG record \n3.2M people playing at the same time",
           color="#F28D35", hjust=0.5, alpha = 0.5, size=3.5)

myplot

final_plot <- myplot + plot_annotation(title="STEAM'S BEST RELEASE",
                                       subtitle = "<br>The 7 over 1258 games with more than 200.000 simultaneous player the month they were released. <br>
                                                          ",                                  
                                       theme = theme(text = element_text('Rubik',  colour = "#F28D35"),
                                                     plot.title = element_text(face = "bold", size = 25, hjust = 0),
                                                     plot.subtitle = element_markdown(size = 12, hjust = 0, color= "#C6C5C1"),
                                                     plot.background = element_rect(fill =bckgdcol, color=bckgdcol)))  
final_plot

ggsave("tidytuestay21_week12.png", width=8,height=7,units = "cm")
