##' delete leading and trailing whitespace
##'
##' 
##'
##' @export

trim <- function (x) gsub("^\\s+|\\s+$", "", x)