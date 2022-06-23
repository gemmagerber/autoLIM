#' Primary producers True
#' If primary producers are included in main autoLIMR function, this function changes Q to GPP, and P to NPP
#' @param x network input data matrix
#' @param primary_producer Primary producers defined in main autoLIMR function
#' @export
#'

pp_true <- function(x, primary_producer) {
  ppq <- grep(as.vector(x),
              pattern = paste0(paste0(primary_producer, "_Q"),
                               collapse = "|"))

  ppnpp <- grep(as.vector(x),
                pattern = paste0(paste0(primary_producer, "_P"), collapse = "|"))


  if (length(ppq) >= 1) {
    gsub(
      as.vector(x),
      pattern = paste0(paste0(primary_producer, "_Q"), collapse = "|"),
      replacement = paste0(primary_producer, "_GPP")
    )

  } else {
    if (length(ppq == 0)) {
      x
    } else {
      if (length(ppnpp) >= 1) {
        gsub(
          as.vector(x),
          pattern = paste0(paste0(primary_producer, "_P"), collapse = "|"),
          replacement = paste0(primary_producer, "_NPP")
        )
      }
      else {
        if (length(ppnpp) == 0) {
          x
        }
      }
    }
  }
}
