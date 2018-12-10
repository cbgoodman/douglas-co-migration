library(tidyverse)
library(viridis)
library(urbnmapr)

# Add data
out <- read.csv("douglas-acs-out.csv", header=TRUE, sep=",", quote="\'", colClass=c("character", "character", "numeric", "numeric"))
# Join with urbnmapr
out.data <- left_join(out, counties, by = "county_fips")

#################################################
# Pretty Breaks
pretty.breaks.out <- c(25,50,75,100)
# find min and max values of pop growth
minVal.out <- min(out.data$out_migration, na.rm = T)
maxVal.out <- max(out.data$out_migration, na.rm = T)
# compute pop growth labels
labels.out <- c()
brks.out <- c(minVal.out, pretty.breaks.out, maxVal.out)
# round the labels (actually, only the extremes)
labels <- c()
for(idx in 1:length(brks.out)){
  labels.out <- c(labels.out, paste0(round(brks.out[idx], 2),
  " – ",
  round(brks.out[idx + 1], 2)))
}
# Minus one label to remove the odd ending one
labels.out <- labels.out[1:length(labels.out)-1]

# Create new variable for fill
out.data$brks.out <- cut(out.data$out_migration,
  breaks = brks.out,
  labels = labels.out,
  include.lowest = T)

#################################################
# Out Migration Map
p <- ggplot() +
  # County Map
  geom_polygon(data = out.data, mapping = aes(x = long, y = lat, group = group,
    fill = out.data$brks.out), color = alpha("white", 1 / 2), size = 0.2) +
  # State Map
  geom_polygon(data = urbnmapr::states, mapping = aes(long, lat, group = group),
    fill = NA, color = "#7f7f7f", size = 0.25, alpha=0.5) +
  # Projection
  coord_map(projection = "polyconic") +
  scale_fill_viridis(
    option = "viridis",
    name = "ACS Out Migration, 2012-2016",
    discrete = T,
    direction = -1,
    begin=0.1,
    #end=0.9,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = F
  )) +
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
       title = "Douglas County loses population locally and to other metro areas",
       subtitle = "Out Migration for Douglas County, Nebraska. 2012-2016",
       caption = "Author: Chris Goodman (@cbgoodman), Data: U.S. Census Bureau American Community Survey")

ggsave(plot=p, "douglas-acs-out.png", width=(5*2), height=(4*2), units="in", dpi="retina")
