##' Color Palette data for politantheme package
##'
##' List with the data used by the palettes in the politan package.
##'
##' @export
#politantheme_data <- {
  ## x to hold value of list as I create it
  #x <- list()
  #x$politan <- list()
  
  ## colors imported from here: http://www.javascripter.net/faq/rgbtohex.htm
  ## Colors picked
 # x$politan$bg <-
   # c(brown4 = rgb(139, 35, 35, max=255), # brown
      #coral3 = rgb(205, 91, 69, max=255), # kind of pinkbrown
    #  coral = rgb(255, 127, 80, max=255),,
    #  darkslategray3 = rgb(121, 205, 205, max=255), # gray bg for graphics blog
    #  darkslategray4 = rgb(82, 139, 139, max=255),
    #  darkslategray = rgb(47, 79, 79, max=255)      
   # ) 
  ## Return
  ##x
  
#}

politan_col <- function(){
  cat("\nmaps_col() returns the colors for yes and no shares\n\nparty_col() returns a data.frame from the party colors, it takes the following arguments:\n\n     'party' = c('SVP', 'FDP', 'CVP', 'SP', 'EVP', 'GPS', 'BDP', 'glp')\n     'scheme' = c('light', 'normal')")

  }

maps_col <- function(x){
  colls <- c(
  brown4 = rgb(139, 35, 35, max=255), # brown
  coral3 = rgb(205, 91, 69, max=255), # kind of pinkbrown
  coral = rgb(255, 127, 80, max=255),
  darkslategray3 = rgb(121, 205, 205, max=255), # gray bg for graphics blog
  darkslategray4 = rgb(82, 139, 139, max=255),
  darkslategray = rgb(47, 79, 79, max=255)) 
  
  print(colls)
  
}


party_col <- function(party=NULL, scheme=NULL){
  
  partycol <- data.frame(Partei=c("SVP", "FDP", "CVP", "SP", "EVP", "GPS", "BDP", "glp"),
                         light=c("#2E9E64", "#6D86B1", "#F8B062", "#EC5962", "#FAE55E", "#A8CE83", "#E2C855", "#D5DB5A"),
                         normal=c("#008640", "#48689D", "#F69B43", "#E82E3B", "#F8DE4F", "#92C266", "#DABA41", "#CAD249"))
  
  if(!is.null(party)) partycol <- subset(partycol, Partei==party)

  if(!is.null(scheme)){
    assign(se, scheme)
    partycol <- as.character(partycol[,get(se)])
  }
 
  return(partycol)

}

