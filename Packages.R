x = c("tidyverse", "ggpubr", "sf", "simecol","reshape2",
      "sp","rgeos","scico","khroma","GGally","ggrepel","maps",
      "tsibble","raster","geodata","car","latex2exp","jtools",
      "ggpubr","bibliometrix","igraph","sbm","RColorBrewer")
# to install the packages: lapply(x, install.packages, character.only = TRUE)
lapply(x, require, character.only = TRUE)
the_theme2 = theme_classic() + theme(
  legend.position = "bottom",
  panel.border = element_rect(colour = "black", fill=NA),
  strip.background = element_rect(fill = "transparent",color="transparent"),
  strip.text.y = element_text(size = 10, angle = -90),
  strip.text.x = element_text(size = 10),title = element_text(size=8),
  axis.title.y=element_text(size = 10),
  axis.title.x=element_text(size = 10),
  #legend.box="vertical",
  legend.text = element_text(size = 10), text = element_text(family = "NewCenturySchoolbook")
)
