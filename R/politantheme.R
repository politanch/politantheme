##' Politan Color Theme
##' @export
##' 

  

theme_politan<- function(base_size = 12, base_family = "Helvetica") {
  
  
  
  library("grid")
  theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
        rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 0),
        #text = element_text(family = base_family, face = "plain",
        #                    colour = "black", 
        #                    size = base_size,
        #                    hjust = 0.5, 
        #                    vjust = 0.5, 
        #                    angle = 0, 
        #                    lineheight = 1),
        
        axis.text =          element_text(size = rel(0.8), colour = "black"),
        strip.text =         element_text(size = rel(0.8)),
        
        axis.line =          element_line(),
        axis.line.y =        element_blank(),
        axis.line.x =        element_line(colour = "gray48", linetype=1, size=0.7),
        axis.text.x =        element_text(vjust = 1),
        axis.text.y =        element_text(hjust = 1),
        axis.ticks.x =       element_line(colour = "gray48"),
        axis.ticks.y =       element_blank(),
        axis.title.x =       element_text(vjust=0),
        axis.title.y =       element_text(angle = 90, vjust=1),
        axis.ticks.length =  unit(0.15, "cm"),
        #axis.ticks.margin =  unit(0.1, "cm"),
        
        legend.background =  element_blank(),
        legend.margin =      unit(0.2, "cm"),
        legend.key =         element_rect(fill = NA, colour = NA),
        legend.key.size =    unit(1.2, "lines"),
        legend.key.height =  NULL,
        legend.key.width =   NULL,
        legend.text =        element_text(size = rel(0.8)),
        legend.text.align =  NULL,
        legend.title =       element_text(size = rel(0.8), face = "bold", hjust = 0),
        legend.title.align = NULL,
        legend.position =    "right",
        legend.direction =   NULL,
        legend.justification = "center",
        legend.box =         NULL,
        
        panel.background =  element_rect(fill = "aliceblue", colour = "black"),
        panel.border =      element_rect(fill = NA, colour = NA),
        panel.grid.major =   element_line(colour = "grey20", size = 0.2, linetype="dotted"),
        panel.grid.minor =   element_line(colour = NA, size = 0.1),
        panel.margin =       unit(0.25, "lines"),
        
        plot.background  = element_rect(fill = "aliceblue",
                                        colour = NA,
                                        size = 1,
                                        linetype = "solid"),
        plot.title =         element_text(family = base_family,
                                          size = rel(1.5),
                                          hjust = 0,
                                          #vjust = -1,
                                          face="bold"),
        
        #plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),
        #plot.margin = unit(c(6, 5, 6, 5) * 2, "points"),
        plot.margin = unit(c(2, 1, 1, 2), "lines"),
        #panel.margin.x =     NULL,
        #panel.margin.y =     NULL,
        
        #strip.background =   element_rect(fill = "grey30", colour = "grey10"),
        strip.text.x =       element_text(),
        strip.text.y =       element_text(angle = -90, hjust=0.5),
        
        complete = TRUE
        
        
        
  )
}

#Error in (function (el, elname)  : 
#           "panel.margin.x" is not a valid theme element name.


