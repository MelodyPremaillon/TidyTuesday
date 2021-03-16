## EMPLOYED STATUS

#The data this week comes from the BLS (https://www.bls.gov/cps/tables.htm#charemp_m)
#This BLS article (https://www.bls.gov/careeroutlook/2018/article/blacks-in-the-labor-force.htm) talks about Employed Status in greater detail.

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

font_add_google("Montserrat", "Montserrat")
font_add_google("Lobster","Lobster")
showtext_auto()

##Loading datas
# tuesdata <- tidytuesdayR::tt_load(2021, week = 9)
# employed <- tuesdata$employed
# earn <- tuesdata$earn

earn <- read.csv("DATA/earn.csv")
employed <- read.csv('DATA/employed.csv')

# employed$industry <- as.factor(employed$industry); employed$major_occupation <- as.factor(employed$major_occupation); employed$minor_occupation <- as.factor(employed$minor_occupation); employed$race_gender <- as.factor(employed$race_gender)
# earn$sex <- as.factor(earn$sex); earn$race <- as.factor(earn$race); earn$ethnic_origin <- as.factor(earn$ethnic_origin);earn$age <- as.factor(earn$age)

medianearn <- earn %>% 
  group_by(year, sex, race) %>%
  summarise(mean = mean(median_weekly_earn), median = median(median_weekly_earn), max=max(median_weekly_earn),min=min(median_weekly_earn)) %>%
  filter(sex != "Both Sexes") %>%
  filter(race != "All Races")
medianearn$race = with(medianearn, reorder(race,median))


dat_text <- data.frame(race = c("Black or African American", "White", "Asian"), sex="woman", y=c(2018,2016,2014))

myplot <- ggplot(medianearn, aes(mean, year,col=sex, shape = sex)) + geom_point(size=1) +
  facet_grid(.~race) + 
  scale_color_manual(values=c("#141C26", "#8C5E54", "#8C5E54"))+
  geom_text(aes(mean, year, label=round(mean)), hjust=-0.25, size=2.5) +
  geom_smooth(alpha=0., se=F)+
  #labels
  scale_x_continuous(breaks = seq(0,2000,by=250),name="Median weekly earn [$]", limits = c(500,1400)) +
  scale_y_continuous(breaks = seq(2010,2020), limits=c(2010,2020)) +
  geom_text(data=dat_text, aes(x = -Inf, y , label = race), hjust= -0.5,vjust   = -1, family = "Lobster", color="black",size=5) +
  #theme
  theme(
    text = element_text(family = "Montserrat"),
    plot.background = element_rect(fill="#F0F2F2"),
    panel.background = element_rect(fill = "#F0F2F2"),
    panel.grid.major.x = element_line(colour = "#FFFFFF"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    
  ) 
myplot  


earn$year <- as.factor(earn$year)


earnmen <- earn  %>% filter(race != "All Races") %>%
  filter(is.na(year) == F) %>%
  filter(sex == "Men")


men <- ggplot(earnmen,aes(median_weekly_earn, y=year, fill= race)) + 
  geom_density_ridges2(alpha=0.5, bandwidth =35) + 
  #geom_point(data= medianearn[which(earn$sex == "Men"),], aes(mean, year, color= race)) + 
  scale_x_continuous(breaks = seq(0,2000,by=200),name="Median weekly earn [$]", limits = c(0,2000)) +
  scale_fill_manual(values=c( "#141C26","#8C5E54", "#5E7B8C")) +
  #theme
  theme(
    text = element_text(family = "Montserrat"),
    plot.background = element_rect(fill="#F0F2F2"),
    panel.background = element_rect(fill = "#F0F2F2"),
    panel.grid.major.x = element_line(colour = "#FFFFFF"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank()
    
  ) +
  geom_text(aes(y=1.5,x=1800, label="Men"), family="Lobster", size=5)
men

earnwomen <- earn  %>% filter(race != "All Races") %>%
  filter(is.na(year) == F) %>%
  filter(sex == "Women")


women <- ggplot(earnwomen,aes(median_weekly_earn, y=year, fill= race)) + 
  geom_density_ridges2(alpha=0.5, bandwidth =35) + 
  scale_x_continuous(breaks = seq(0,2000,by=200),limits=c(2000,0),expand=c(0,0),name="Median weekly earn [$]",trans="reverse") +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual(values=c( "#141C26","#8C5E54", "#5E7B8C")) +
  #theme
  theme(
    text = element_text(family = "Montserrat"),
    plot.background = element_rect(fill="#F0F2F2"),
    panel.background = element_rect(fill = "#F0F2F2"),
    panel.grid.major.x = element_line(colour = "#FFFFFF"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
    
  ) +
  geom_text(aes(y=1.5,x=1800, label="Women"), family="Lobster",size=5) + 
  annotate(geom="text", x= c(740,890,1100), y = c(12.5,12.5,12), label=c("748$", "869$", "1056$"), color=c( "#8C5E54","#5E7B8C", "#141C26" ))
women


gglmyplot <- women + men


final_plot <- myplot + plot_annotation(title="Earnings growth but inegality stays",
                                      subtitle = "Data shown are the median weekly earnings in US dollars ($) <br> 
                          for <span style='color:#141C26;'><strong>Mens</strong></span> and <span style='color:#8C5E54;'><strong>Womans</strong></span> and depending on your race. <br>
                                      ",
                                      theme = theme(text = element_text('Montserrat',  colour = "#535657"),
                                                    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
                                                    plot.subtitle = element_markdown(size = 14, hjust = 0.5),
                                                    plot.background = element_rect(fill ="#9BB5BF", colour = "#C6D6D1"),
                                                    plot.margin = unit(c(1, 0, 0, 0), "cm")))  

final_plot

ggsave('us_earnings.png', final_plot, width = 6, height = 3.5) 
