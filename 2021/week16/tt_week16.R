## US post offices

#The data this week comes from Cameron Blevins and Richard W. Helbock
#Blevins, Cameron; Helbock, Richard W., 2021, "US Post Offices", https://doi.org/10.7910/DVN/NUKCNA, Harvard Dataverse, V1, UNF:6:8ROmiI5/4qA8jHrt62PpyA== [fileUNF]
#Credit: [Melody Premaillon](@Melau_Yellau)



# Get the Data
library(pacman)
pacman::p_load(tidyverse, plotly, cowplot, magick, showtext, extrafont, ggtext, lubridate, ragg, 
               geodist, ggvoronoi, ggrepel, plotly, gganimate, usmap, cairo)

font_add_google("Montserrat","Montserrat")
font_add_google("Lobster", "Lobster")
showtext.auto()

# Either ISO-8601 date or year/week works!
tuesdata <- tidytuesdayR::tt_load(2021, week = 16)

post_offices <- tuesdata$post_offices %>%
  mutate(x = longitude, y = latitude, xy = paste(x,y))

boston <- post_offices %>% 
   filter(established == 1639)


maxfromboston <- data.frame(dist = 0, year =1639, nbpostoff = 1,name = boston$name, x=boston$x, y= boston$y)
for (i in 1640:2000){
   postyear <- post_offices %>% 
     filter(established <= i)
   nbpostoff <- nrow(postyear)
   if (nrow(postyear) == 0) 
     next
   else
     a <- geodist(boston, postyear, measure = "geodesic")
     rowmax <- which.max(a)
     maxa <- max(a, na.rm=T)
     maxfromboston <- rbind(maxfromboston, 
                            data.frame(dist = maxa, year=i, nbpostoff = nbpostoff, 
                                       name = postyear$name[rowmax],
                                       x = postyear$x[rowmax], y = postyear$y[rowmax]))
}

farest <- maxfromboston  %>%
  mutate(name = as.character(levels(name))[name])  %>%
  group_by(name) %>%
  summarise(dist=max(dist), year=min(year), x= min(x), y=min(y)) %>%
  mutate(label = paste(name, ", \n", round(dist/1E3,0), "km", sep="")) 


US <- map_data("world")
backcol <- "#FFF5EE"

map <- ggplot(farest, aes(x,y)) + 
  geom_polygon(data=US,aes(long,lat,group=group),color="#806450", fill="#B3A194") +
  geom_point(aes(size=dist)) +
  geom_text_repel(aes(label=label), family = "Montserrat") +
  geom_point(data=boston, aes(x, y), color="#B73837", size = 3) + 
  scale_x_continuous(limits = c(-180,-50)) +
  scale_y_continuous(limits = c(10,75)) +
  theme(
    panel.background = element_rect(fill = backcol, color = backcol),
    plot.background = element_rect(fill = backcol, color = backcol),
    legend.position = "none" ,
    axis.text = element_blank() ,
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(family = "Montserrat"),
    panel.grid = element_blank()
    #plot.subtitle.position = "plot"
  ) 
map

leplot <- ggplot(maxfromboston, aes(year,dist)) + 
  geom_point(color="#806450") +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "Distance to Boston [thousands of km]", breaks = seq(0,8E6, by=2E6), labels = seq(0,8,by=2) ) +
  theme(
    plot.background = element_rect(fill = backcol, color = backcol),
    panel.background = element_rect(fill = backcol, color = backcol),
    text = element_text(family = "Montserrat")
        ) +
  annotate(geom = "curve", yend = 4.157E6, xend = 1847, y = 3.7E6, x = 1870, 
           curvature = .3, arrow = arrow(length = unit(2, "mm")), color="black") +
  annotate("text", x= 1870, y= 3.7E6, label="Reaching west coast \nAstoria", hjust= 0) +
  annotate(geom="curve", x=1800, y=5.5E6,xend=1867, yend=4.71E6,
           curvature = .1, arrow = arrow(length = unit(2, "mm")), color="black") +
  annotate("text", x=1800, y=5.5E6, label = "Sitka \nFirst post office in Alaska", hjust = 1) +
  annotate("text", x = 1900, y= 8.3E6, label = "Jumping overseas, Kekaha \nFarest post Office from Boston since 1900", hjust=1) +
  annotate("text", x= 1642, y = 0, label = "Boston, first post office established in 1639", hjust = 0)

#leplot

title <- ggdraw() + 
  draw_text("What is the farest post office from the first established one?", 
             family = "Montserrat", x = 0,  hjust= 0, size = 20) + 
  theme(plot.background = element_rect(fill=backcol, color = backcol))

subtitle <- ggdraw() +
  draw_text("The very first post office in colonial America was established in 1639 in the Boston home, 
which was also a tavern that sold “stronge water” of a man named Richard Fairbanks.
Ever since the number of post office keeps growing, but how far are they from the first one?
Established in 2000, Kekaha (Hawai) is the farest post office from Boston with 8 292 thousands kilometers.
This chart shows the evolution of the farest post offices from Boston from 1639 to 2000",
             family = "Montserrat", x = 0, hjust= 0, size = 12, vjust=0.5) + 
  theme(plot.background = element_rect(fill=backcol, color = backcol))

caption <- ggdraw() + 
  draw_text("viz Mélody Prémaillon | 2021, tidy tuesday 15 | data : Blevins, Cameron; Helbock, Richard W., 2021", 
            family = "Montserrat", x = 1,  hjust= 1, size = 9) + 
  theme(plot.background = element_rect(fill=backcol, color = backcol))

plotss <- plot_grid( map, leplot, ncol = 2, rel_widths = c(1.6,1)) 

plot_grid(title, subtitle,plotss, caption, nrow=4, rel_heights = c(0.3,0.8,3,0.1))

  
  

file <- here::here("2021/week16", "week_16.pdf")

ggsave(file,width=14,height = 8, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = file, 
  filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
  format = "png", dpi = 200
)


