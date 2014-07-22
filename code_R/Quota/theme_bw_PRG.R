# First go at creating a custom ggplot2 theme from scratch (i.e. 
#   completely defining a new theme function rather than tweaking an
#   existing theme using the %replace% syntax). Code is sucessful, but
#   resulting theme is pretty terrible. Can easily be tweaked though.

#  !!! This ended up being a time-waster/learning experience :) as  
#    theme_classic() or theme_minimal() will do !!!

#  Adapted from theme_complete_bw() published online at 
#        http://sape.inf.usi.ch/quick-reference/ggplot2/themes
#  NOTE: ggplot had a syntax change since this code was published.
#        I'm planning on updating it with the new syntax.
#
#     Used syntax from theme_black() published on to get it done
#        http://docs.ggplot2.org/dev/vignettes/themes.html



library(grid)

theme_bw_PRG <- function(base_size = 12, base_family = "Serif") {
  theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                                      lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = base_family, face = "plain",
                                      colour = "black", size = base_size,
                                      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text =          element_text(size = rel(0.8), colour = "white"),
    strip.text =         element_text(size = rel(0.8), colour = "white"),
    
    axis.line =          element_blank(),
    axis.text.x =        element_text(size = base_size * 0.8 , lineheight = 0.9, colour = "black", vjust = 1),
    axis.text.y =        element_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1),
    axis.ticks =         element_line(colour = "black"),
    axis.title =         element_text(colour = "white"),
    axis.title.x =       element_text(size = base_size, vjust = 0.5),
    axis.title.y =       element_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),
    
    legend.background =  element_rect(colour=NA), 
    legend.margin =      unit(0.2, "cm"),
    legend.key =         element_rect(fill = NA, colour = "black", size = 0.25),
    legend.key.size =    unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(size = rel(0.8), face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   "vertical",
    legend.justification = "center",
    legend.box =         NULL,
    
    panel.background =   element_rect(fill = NA, colour = "black", size = 0.25), 
    panel.border =       element_blank(),
    panel.grid.major =   element_line(colour = "black", size = 0.05),
    panel.grid.minor =   element_line(colour = "black", size = 0.05),
    panel.margin =       unit(0.25, "lines"),
    
    strip.background =   element_rect(fill = NA, colour = NA), 
    strip.text.x =       element_text(colour = "black", size = base_size * 0.8),
    strip.text.y =       element_text(colour = "black", size = base_size * 0.8, angle = -90),
     
    plot.background =    element_rect(colour = NA, fill = "white"),
    plot.title =         element_text(size = base_size * 1.2),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),
    
    complete = TRUE
  )
}
