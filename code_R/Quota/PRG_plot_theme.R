# Custom theme for plots. Simple as possible, black and white base.
#  Adapted from theme_complete_bw() published online at 
#        http://sape.inf.usi.ch/quick-reference/ggplot2/themes
#  NOTE: ggplot had a syntax change since this code was published.
#        I'm planning on updatint it with the new syntax.

theme_complete_bw <- function(base_size = 12) {
  structure(list(
    axis.line =         element_blank(),
    axis.text.x =       element_text(size = base_size * 0.8 , lineheight = 0.9, colour = "black", vjust = 1),
    axis.text.y =       element_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1),
    axis.ticks =        element_segment(colour = "black"),
    axis.title.x =      element_text(size = base_size, vjust = 0.5),
    axis.title.y =      element_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),
    
    legend.background = element_rect(colour=NA), 
    legend.key =        element_rect(fill = NA, colour = "black", size = 0.25),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       element_text(size = base_size * 0.8),
    legend.title =      element_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",
    
    panel.background =  element_rect(fill = NA, colour = "black", size = 0.25), 
    panel.border =      element_blank(),
    panel.grid.major =  element_line(colour = "black", size = 0.05),
    panel.grid.minor =  element_line(colour = "black", size = 0.05),
    panel.margin =      unit(0.25, "lines"),
    
    strip.background =  element_rect(fill = NA, colour = NA), 
    strip.text.x =      element_text(colour = "black", size = base_size * 0.8),
    strip.text.y =      element_text(colour = "black", size = base_size * 0.8, angle = -90),
    
    plot.background =   element_rect(colour = NA, fill = "white"),
    plot.title =        element_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}