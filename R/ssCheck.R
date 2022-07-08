#' ssCheck
#' A vrief description goes here
#'
#' @param x not sure
#' @param tol not sure
#' @param more not sure
#' @param zero.na convert NA to zero
#'

#' @importFrom network network.size
#'
ssCheck <- function(x,
                    tol = 5,
                    more = FALSE,
                    zero.na = TRUE) {
  #Check for network class object
  if (class(x) != 'network') {
    warning('x is not a network class object')
  }
  T. <- as_extended(x) #convert to extended format
  if (zero.na) {
    T.[is.na(T.)] <- 0
  }
  n <- network::network.size(x) #get the number of nodes
  Tin <- apply(T.[, 1:n], 2, sum) #in throughflow
  Tout <- apply(T.[1:n, ], 1, sum) #out throughflow
  d <- abs(Tin - Tout) # SSerror difference
  pe <- (d / Tout) * 100 # SSerror as percent of total throughflow

  if (more == FALSE) {
    return(all(pe < tol)) #returns a logical indicating that all node differences are less than tolerance (==TRUE)
  } else {
    return(list(
      "ss" = all(pe < tol),
      "Tin" = Tin,
      "Tout" = Tout,
      "perror" = pe
    ))
  }
}
