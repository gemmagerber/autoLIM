#' Title
#'
#' @param x the something to be defined
#' @param zero.na Change NA's to zero's
#'
#' @return

#' @importFrom network get.vertex.attribute
#'
as_extended <- function(x, zero.na = TRUE) {
  #Check for network class object
  if (class(x) != "network") {
    warning('x is not a network class object')
  }

  "%v%"<-function(x,attrname){
    network::get.vertex.attribute(x,attrname=attrname)
  }

  #unpack the data from the network object
  flow <- as.matrix(x, attrname = "flow")
  input <- x %v% 'input'
  respiration <- x %v% 'respiration'
  export <- x %v% 'export'
  #recombine into the extended format
  import <- c(input, 0, 0, 0)
  x <- cbind(flow, export, respiration, rep(0, nrow(flow)))
  x <- rbind(x, rep(0, length(import)), rep(0, length(import)), import)
  #make NA values zero if zero.na == TRUE
  if (zero.na) {
    x[is.na(x)] = 0
  }
  return(x)
}
