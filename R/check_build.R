#' multinets::check_build()
#' Function to check if the limfile is included or not
#' @param file the name of the R script containing the LIM declaration file ".R" extension
#'
#' @export
#'
#' @importFrom LIM Setup
check_build <- function (file) {
  if (is.null(file)) {
    stop("No limfile provided")
  }
  if (!is.null(file)) {
    full_limfile <- LIM::Setup(LIM::Read(file))
  }

}
