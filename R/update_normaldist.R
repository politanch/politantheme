##' Update mean and variance from a normal prior
##'
##' 
##'
##' @export


mean_updater <- function(variance_prior, mean_prior, variance_data, mean_data, n){
  meanpost <- ((1/variance_prior)*mean_prior + (n/variance_data)*mean_data) / ((1/variance_prior)+(n/variance_data))
  return(meanpost)
}

var_updater <- function(variance_prior, variance_data, n){
  varpred <- variance_data*((variance_prior/variance_data)+ 1/n)
  return(varpred)
}