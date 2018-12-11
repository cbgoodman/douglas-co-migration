library(tidyverse)
library(urbnmapr)

# Add data
migrate.in <- read.csv("douglas-acs-in.csv", header=TRUE, sep=",", quote="\'", colClass=c("character", "character", "numeric", "numeric"))
# Join with urbnmapr
migrate.in.data <- left_join(migrate.in, counties, by = "county_fips")

#################################################
# Pretty Breaks
pretty.breaks.in <- c(25,50,75,100)
# find min and max values of pop growth
minVal.in <- min(migrate.in.data$in_migration, na.rm = T)
maxVal.in <- max(migrate.in.data$in_migration, na.rm = T)
# compute pop growth labels
labels.in <- c()
brks.in <- c(minVal.in, pretty.breaks.in, maxVal.in)
# round the labels (actually, only the extremes)
labels <- c()
for(idx in 1:length(brks.in)){
  labels.in <- c(labels.in, paste0(round(brks.in[idx], 2),
  " – ",
  round(brks.in[idx + 1], 2)))
}
# Minus one label to remove the odd ending one
labels.in <- labels.in[1:length(labels.in)-1]

# Create new variable for fill
migrate.in.data$brks.in <- cut(migrate.in.data$in_migration,
  breaks = brks.in,
  labels = labels.in,
  include.lowest = T)

#################################################
# In Migration Map
p <- ggplot() +
  # County Map
  geom_polygon(data = migrate.in.data,
    mapping = aes(x = long, y = lat, group = group,
    fill = migrate.in.data$brks.in),
    color = alpha("white", 1 / 2),
    size = 0.2) +
  # State Map
  geom_polygon(data = urbnmapr::states, mapping = aes(long, lat, group = group),
    fill = NA, color = "#7f7f7f", size = 0.25, alpha=0.5) +
  # Projection
  coord_map(projection = "polyconic") +
  scale_fill_viridis(
    option = "viridis",
    name = "ACS In Migration, 2012-2016",
    discrete = T,
    direction = 1,
    begin=0.1,
    #end=0.9,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      #reverse = F
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
       title = "Douglas County gains population locally and from the Southwest and West",
       subtitle = "In Migration for Douglas County, Nebraska. 2012-2016",
       caption = "Author: Chris Goodman (@cbgoodman), Data: U.S. Census Bureau American Community Survey")

  ggsave(plot=p, "douglas-acs-in.png", width=10, height=8, units="in", dpi="retina")
