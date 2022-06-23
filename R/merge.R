# Function:
#' Merge
#' Merge lists together, name sections
#' @param type type of section
#' @param ... other arguments
#'
#' @export
#' @importFrom stats setNames
merge <- function(type = NULL, ...) {
  l <- list(...)
  keys <- unique(unlist(lapply(l, names)))
  x <-
    setNames(do.call(mapply, c(FUN = c, lapply(l, `[`, keys))), keys)
  if (!is.null(type)) {
    x <- lapply(x,
                function(x)
                  c(
                    paste0("### ", toupper(type)),
                    "",
                    x ,
                    "",
                    paste0("### END ", toupper(type)),
                    ""
                  ))
  }
  return(x)
}
