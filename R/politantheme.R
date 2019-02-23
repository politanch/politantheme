##' Politan Color Theme
##' @export
##' 
##' Politan Color Theme
##' @export
##' 

# generating new theme
theme_politan <- function(base_size = 12, 
                      base_family = "Helvetica",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170){
  ggplot2::theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    ggplot2::theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        face = "bold",
        hjust = 0),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.7)),
      panel.grid.major = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted"),   
      panel.grid.minor = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted", 
        size = rel(4)), 
      panel.background =  element_rect(
        fill = "aliceblue", colour = "aliceblue"),
      plot.background  = element_rect(fill = "aliceblue",
                                      colour = NA,
                                      size = 1,
                                      linetype = "solid"),
      
      
      
      complete = TRUE
    )
}

