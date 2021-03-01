# Get the Data
library(tidytuesdayR)
library(ggplot2)
library(extrafont)
library(pBrackets)
library(showtext)
library(grid)

## Loading Google fonts (https://fonts.google.com/)
##  font-family: 'Teko', sans-serif;
font_add_google("Teko", "teko")

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

georgia_pop <- tuesdata$georgia_pop

#options(repr.plot.width = 22, repr.plot.height = 28)

ggplot(georgia_pop, aes(Year, Colored)) + 
  geom_line() +
  geom_line(aes(y=White), linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(breaks = seq(0,100,by=5), trans = "reverse",name="") +
  scale_x_continuous(breaks = seq(1790,1890,by=10),name="") +
  ggtitle("COMPARATIVE INCREASE OF WHITE AND COLORED \n POPULATION OF GEORGIA.") +
  theme(
    panel.border = element_rect(colour="black",fill=NA, size= 0.25),
    panel.grid.major = element_line(colour = "#dda58c", size=0.25),
    panel.grid.minor=element_blank(),
    plot.background = element_rect(fill="#EAD3B8"),
    panel.background = element_rect(fill="#EAD3B8"),
    text = element_text(family = "teko"),
    plot.title = element_text(hjust = 0.5,size =25),
    legend.key = element_rect(fill = "#e7ccb0", color = NA),
    plot.margin = margin(2.3,.8,2.3,.8, "cm"),
    aspect.ratio = 0.78
    )

wbottom_y <- 318
grid.brackets(572, bottom_y,   78, bottom_y, lwd=1, col="black")

ggsave("plotweek1test.png",width = 22, height = 28)
