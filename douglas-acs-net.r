library(tidyverse)
library(urbnmapr)

# Add data
net <- read.csv("douglas-acs-net.csv", header=TRUE, sep=",", quote="\'", colClass=c("character", "character", "numeric", "numeric"))
# Join with urbnmapr
net.data <- left_join(net, counties, by = "county_fips")

#################################################
# Pretty Breaks
pretty.breaks.net <- c(-50,-25,0,25,50)
# find min and max values of pop growth
minVal.net <- min(net.data$net_migration, na.rm = T)
maxVal.net <- max(net.data$net_migration, na.rm = T)
# compute pop growth labels
labels.net <- c()
brks.net <- c(minVal.net, pretty.breaks.net, maxVal.net)
# round the labels (actually, only the extremes)
labels <- c()
for(idx in 1:length(brks.net)){
  labels.net <- c(labels.net, paste0(round(brks.net[idx], 2),
  " – ",
  round(brks.net[idx + 1], 2)))
}
# Minus one label to remove the odd ending one
labels.net <- labels.net[1:length(labels.net)-1]

# Create new variable for fill
net.data$brks.net <- cut(net.data$net_migration,
  breaks = brks.net,
  labels = labels.net,
  include.lowest = T)

#################################################
# City Labels
annot <- read.table(text=
  "lat|long|just|city
  41.257160|-95.995102|0|Omaha
  41.881832|-87.623177|1|Chicago",
  sep="|", header=TRUE, stringsAsFactors=FALSE)

#################################################
# Net Migration Map
p <- ggplot() +
  # County Map
  geom_polygon(data = net.data, mapping = aes(x = long, y = lat, group = group,
    fill = net.data$brks.net), color = alpha("white", 1 / 2), size = 0.2) +
  # State Map
  geom_polygon(data = urbnmapr::states, mapping = aes(long, lat, group = group),
    fill = NA, color = "#7f7f7f", size = 0.25, alpha=0.5) +
  # Projection
  coord_map(projection = "polyconic") +
  scale_fill_brewer(
    type = "div",
    palette = "PRGn",
    name = "ACS Net Migration, 2012-2016",
    #discrete = T,
    direction = -1,
    #begin=0.1,
    #end=0.9,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = F
    )) +
  # City labels -- experimental
  #geom_label(data=annot, aes(x=long, y=lat, label=city, hjust=just),
  #  family="Open Sans Condensed Light", lineheight=0.95,
  #  size=3.5, label.size=0, color="#2b2b2b", fill = "transparent") +
  # Theming
  theme_minimal(base_family = "Open Sans Condensed Light") +
  theme(
    legend.position = "bottom",
    legend.text.align = 0,
    legend.title.align = 0.5,
    plot.margin = unit(c(.5,.5,.2,.5), "cm")) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    ) +
  theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15))) +
  theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic")) +
  theme(plot.margin=unit(rep(0.5, 4), "cm")) +
  labs(x = "",
       y = "",
       title = "Douglas County loses the most population to warm places",
       subtitle = "Net Migration for Douglas County, Nebraska. 2012-2016",
       caption = "Author: Chris Goodman (@cbgoodman), Data: U.S. Census Bureau American Community Survey")

  ggsave(plot=p, "douglas-acs-net.png", width=10, height=8, units="in", dpi="retina")
