## UN votes

#The data this week comes from Harvard's Dataverse (https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379)
#Credit: [Melody Premaillon](@Melau_Yellau)


#Packages
library(pacman)
pacman::p_load(tidytuesdayR, cowplot, tidyverse,lubridate,showtext,patchwork, magick, 
               ggrepel, extrafont, showtext, ggtext, gridExtra)

font_add_google("Russo One","Russo One")
font_add_google("Lobster","Lobster")
showtext_auto()


# Get the Data
tuesdata <- tidytuesdayR::tt_load('2021-03-23')
tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

unvotes <- tuesdata$unvotes
roll_calls <- tuesdata$roll_calls
issues <- tuesdata$issues
rm(tuesdata)

#Select votes from USA and USSR during cold war, counting number of yes/no
coldwar <- unvotes %>%
  filter(country %in% c("United States", "Russia")) %>%
  inner_join(roll_calls %>% select(rcid, date)) %>%
  filter(date > "1947/01/01", date < "1991/12/31") 

usyes <- coldwar %>% filter(country == "United States") %>% filter(vote == "yes") %>% pull(rcid)
usyes <- coldwar %>% filter(rcid %in% usyes)
usno <- coldwar %>% filter(country == "United States") %>% filter(vote == "no") %>% pull(rcid)
usno <- coldwar %>% filter(rcid %in% usno)


finalus <- rbind(data.frame(table(usno$country, usno$vote), id="usno"),
                 data.frame(table(usyes$country, usyes$vote), id="usies"))
colnames(finalus) <- c("country","vote","freq","usvote")

#Modifying votes characters style for russia
finalus$vote <- as.character(levels(finalus$vote))[finalus$vote]
finalus$vote[which(finalus$country == "Russia" & finalus$vote == "no")] <- "????"
finalus$vote[which(finalus$country == "Russia" & finalus$vote == "yes")] <- "Y??S"
finalus$vote[which(finalus$country == "Russia" & finalus$vote == "abstain")] <- "??BSTAI??"
finalus$vote <- as.factor(finalus$vote)


#Plot general Theme
mytheme <-  theme(
    #plot.title = element_text(family = "Lobster", size=40),
    text = element_text(family = "Lobster", size=15),
    panel.background = element_rect(fill="#d9c8b4", colour = "#d9c8b4"),
    plot.background = element_rect(fill="#d9c8b4", colour = "#d9c8b4"),
    panel.grid =  element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    axis.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )


  
  
#Russian votes plot  
#American vote plot
plotus <- finalus %>%
  filter(country == "United States", freq > 0) %>%
  ggplot() +
  geom_text_repel(aes(country, freq, label=toupper(vote),  color=vote), family = "Russo One", size=35) +
  facet_grid(usvote~.) + 
   ##scales
  scale_color_manual(values = c( "#022873","#022859","red")) +
  ###theme
  mytheme  +
  theme(
    panel.spacing = unit(2, "lines"),
    plot.title = element_markdown(),
  ) +
  labs(title = 'When <br> <b style="font-size:40pt;font-family:sans;">Americans</b> vote')

plotrussia <- finalus %>%
  filter(country == "Russia") %>%
  ggplot()+
  geom_text_repel(aes(country, freq, label=toupper(vote), size = freq, color=vote),family = "Russo One",segment.color = 'transparent') +
  facet_grid(usvote~.) +
  #title
  labs(title = '<b style="font-size:40pt;font-family:sans;">??ussians</b><br> vote')+
  ##scales
  scale_size(range=c(5,30)) +
  scale_color_manual(values = c( "#400101","#D90404","red")) +
  ###theme
  mytheme +
  theme(
    panel.spacing = unit(1, "lines"),
    plot.title.position = "plot",
    plot.title = element_markdown(hjust=1)
  ) 

#Final plot

final <- plotus + plotrussia +theme(plot.margin = unit(c(1, 0, 0, 0), "cm"))

final <- final + plot_annotation(
                title = '<i style="color:#022859;">USA</i> vs <i style="color:#D90404;">USSR</i>', 
                subtitle = '<b style="font-size:20pt;*">44 years of UN votes during cold war</b> <br>
                Sizes are proportional to number of votes during the 1947-1991 period',
                theme = theme(text = element_text('Russo One'),
                              plot.title = element_markdown(size = 35, hjust = 0.5, color="#400101"),
                              plot.subtitle = element_markdown(size = 12, hjust = 0.5),
                              plot.background = element_rect(fill ="#d9c8b4", colour = "#d9c8b4"),
                              #plot.margin = unit(c(1, 0, 0, 0), "cm")
                              )) 
final

ggsave("rplotweek13.pdf", width = 10, height = 5.5, device = cairo_pdf)
