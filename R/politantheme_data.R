##' Color Palette data for politantheme package
##'
##' List with the data used by the palettes in the politan package.
##'
##' @export
politantheme_data <- {
  ## x to hold value of list as I create it
  x <- list()
  x$politan <- list()
  
  ## colors imported from here: http://www.javascripter.net/faq/rgbtohex.htm
  ## Colors picked
  x$politan$bg <-
    c(brown4 = rgb(139, 35, 35, max=255), # brown
      coral3 = rgb(205, 91, 69, max=255), # kind of pinkbrown
      coral = rgb(255, 127, 80, max=255),,
      darkslategray3 = rgb(121, 205, 205, max=255), # gray bg for graphics blog
      darkslategray4 = rgb(82, 139, 139, max=255),
      darkslategray = rgb(47, 79, 79, max=255)      
    ) 
  ## Return
  x
  
}

