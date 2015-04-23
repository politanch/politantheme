##' Make any data.frame numerical
##'
##' 
##'
##' @export





nummerisch <- function(data.frame){
  for(i in 1:length(data.frame)){
    data.frame[,i] <- as.numeric(data.frame[,i])
  }
}

