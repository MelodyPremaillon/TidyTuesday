## Makeups shades

#The data this week comes from the pudding (https://github.com/the-pudding/data/tree/master/foundation-names)
#This article (https://pudding.cool/2021/03/foundation-names/) talks about makeups shades and their bias in greater detail.
#Credit: [Mélody Prémaillon](@Melau_Yellau)


###Packages

library(pacman)
pacman::p_load(tidytuesdayR, cowplot, tidyverse,lubridate,showtext,patchwork, magick, 
               ggrepel, extrafont, showtext, ggtext, gridExtra, factoextra, plotwidgets, ggridges, jpeg, grid)

showtext_auto()
font_add_google("Rubik","Rubik")
font_add_google("Dancing Script","Dancing Script")
font_add_google("Cinzel","Cinzel")

###Datas
tuesdata <- tidytuesdayR::tt_load(2021, week = 14)
allShades <- tuesdata$allShades


###Code

#convert hex to rgb
allShades <- allShades %>%
  mutate(R = col2rgb(hex)[1,], G = col2rgb(hex)[2,], B = col2rgb(hex)[3,])


kClusters <- 10
kMeans <- kmeans(allShades[, 14:16], centers = kClusters)

# fviz_cluster(kMeans, data = allShades[, 14:16],
#              #palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
#              geom = "point",
#              ellipse.type = "convex", 
#              ggtheme = theme_bw()
# )

x <-  kMeans$centers #matrix of rgb centers
rgb2hex <- function(R,G,B) rgb(R,G,B, maxColorValue = 255)
hexcenters <- rgb2hex(x[,1],x[,2],x[,3])

clusteredShades <- enframe(hexcenters, "rank", "hex")
clusteredShades$size <- kMeans$size
clusteredShades$lightness <-  col2hsl(clusteredShades$hex)[3,]

hexcols <- clusteredShades %>% 
  arrange(lightness) %>%
  pull(hex)
  

clusteredShades <- clusteredShades %>%
  arrange(desc(lightness)) %>% 
  mutate(rank = rep(seq(1:5),2), palettepos=rep(c(2,1),each=5))
  
##Plot
makeupplot <- ggplot(clusteredShades, aes(rank, y= palettepos)) + 
  #geom_point(aes(size=size+20), color="white") +
  geom_point(aes(size=size, color=hex)) +
  geom_text(aes(y=palettepos-0.5,label=size), color="white", family="Cinzel") +
  #scales
  scale_color_manual(values=hexcols) +
  scale_size_continuous(range=c(5,40)) +
  scale_y_continuous(limits=c(0.5,2.5)) +
  scale_x_continuous(limits=c(0.5,5.5)) +
  #theme
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(color="black", fill="black"),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 1.5, -0.2, 1.5), "cm")
  )

img <- readJPEG("couvercle2.jpg")
img <- rasterGrob(img, interpolate=TRUE)



x <- allShades$lightness
y <- density(x, n = 2^12)

lighnessridge <- ggplot(data.frame(x = y$x, y = y$y), aes(x, y)) + geom_line() + 
  geom_segment(aes(xend = x, yend = 0, colour = x)) + 
  scale_color_gradient(low = "#623D2C", high = "#F7E5D7") +
  scale_x_continuous(breaks=seq(0,1,by=0.2), trans = "reverse", name="",position="top") +
  scale_y_continuous(trans="reverse") +
  theme(
    panel.background = element_rect(color="white", fill="white"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = unit(c(0, 1, 1, 1), "cm")
  ) +
  annotate("text",x=0.8,y=0.9, label="foundation lightness \ndistribution",size=4, hjust=0, family = "Cinzel")


#Title
ggdraw() +
  draw_text("The foundation shade palette",
            size = 30, x = 0.5, y = 0.5, hjust = 0.5,
            color = "black",family="Cinzel") -> title

#Subtitle
ggdraw() +
  draw_text("
    From hundreds of fondation shade, we can look at their colors and lightness.
    Light shades are way more common than darker ones. 
    Here, fundation shades were clustered in 10 categories.
    They are plotted with the color of the centroïd value,
    Patch size is proportional to cluster ones, the value is written in white",
            size = 12, x = 0.03, y = 0.7, hjust = 0,
            color = "#885437", family="Rubik") -> subtitle


#caption
ggdraw() +
  draw_text("Visualization: Melody Premaillon | Data: The Pudding, 'The Naked Truth'",
            size = 10, x = 0.98, y = 0.2, hjust = 1,
            color = "#D6A680") -> caption

finalplot <- plot_grid(title,
                  img,
                  makeupplot,
                  lighnessridge,
                  subtitle,
                  caption, 
                  ncol=1,rel_heights = c(0.8, 1,2.4,1.5,0.8,0.4))
finalplot

#ggsave("finalplot.pdf",height=8, width=10)
ggsave("finalplot.png",height=8, width=10, dpi=72)
