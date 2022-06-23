
#' Defines the first part of the metadata from network input data workbook sheets
#'
#' @param x network input data matrix
#' @param author author name. Defined in the main autoLIMR function. Defaults to system user
#' @param date date. Defined in the main autoLIMR function. Defaults to system date
#' @param respiration If respiration = TRUE in main autoLIMR argument
#' @param NLNode the defined NLNodes from the main autoLIMR argument
#' @param weighted whether to return weighted LIM declaration files. Default to TRUE
#' @param primary_producer Primary producers defined in main autoLIMR function
#' @param respiration_element if Respiration = TRUE, the respiration element to be defined. Default to "CO2"

#' @export
#'
meta1 <- function (x,
                   author = NULL,
                   date = NULL,
                   respiration,
                   respiration_element,
                   NLNode,
                   weighted,
                   primary_producer) {
  if (weighted == TRUE) {
    head1 <- "! Weighted Network"
  } else {
    head1 <- "! Unweighted Network"
  }

  heading2 <- paste0("! ",
                     names(x),
                     "Network LIM Declaration File")
  reference2 <-
    "! Composed with autoLIM::autoLIMR (Gerber et al., in prep)"

  author2 <- if (is.null(author)) {
    paste0("! Author: ", Sys.getenv("USERNAME"))
  } else {
    paste0("! Author: ", author)
  }

  date <- if (is.null(date)) {
    paste0("! Date: ", Sys.Date())
  } else {
    paste0("! Date: ", date)
  }

  living <-
    paste0("! Living compartments: ", length(net_data_node(x, node.type = "living")))

  NLN <- net_data_node(x, node.type = "nonliving")
  nonliving <- paste0("! Non-living compartments: ", length(NLN))

  resp <-
    paste0("! respiration included: ",
           ifelse(respiration == TRUE, "Yes", "No"))
  uflows <-
    paste0("! U included: ", ifelse(length(NLNode) > 0, "Yes", "No"))

  externals <- net_data_external_list(x,
                                      respiration = respiration,
                                      respiration_element = respiration_element)

  externals2 <-
    paste0("! External compartments: ", length(externals))

  countx <- if (respiration == TRUE) {
    countx <- paste0(length(externals) - 1 +
                       length(primary_producer) +
                       length(net_data_node(x, node.type = "living")))


  } else {
    countx <- paste0(length(externals))
  }

  boundary <- paste0("! Boundary flows: ", countx)

  metadata1 <- c(
    head1,
    heading2,
    reference2,
    author2,
    date,
    "",
    resp,
    uflows,
    "",
    living,
    nonliving,
    externals2,
    boundary
  )

  return(metadata1)
}
