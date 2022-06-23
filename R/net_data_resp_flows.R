# Function:
#' Title respiration flow definition
#' Defines repiration flows
#' @param x network data input matrix
#' @param respiration  If respiration = TRUE in main autoLIMR argument
#' @param respiration_element if Respiration = TRUE, the respiration element to be defined. Default to "CO2"
#' @param primary_producer Primary producers defined in main autoLIMR function
#'
#' @export
#'
net_data_resp_flows <- function(x,
                                respiration,
                                respiration_element,
                                primary_producer) {
  if (respiration == TRUE) {
    LN <- net_data_node(x, node.type = "living")

    if (!is.null(primary_producer)) {
      gpp <- grep(
        as.vector(rownames(x)),
        pattern = paste0(primary_producer, collapse = "|"),
        value = TRUE,
        invert = FALSE,
        ignore.case = FALSE
      )

      if (is.null(respiration_element)) {
        gppF <- paste(gpp, "_GPP: ", "CO2", " -> ", gpp, sep = "")
        RF <- paste(LN, "_R: ", LN, " -> ", "CO2", sep = "")

      } else {
        gppF <-
          paste(gpp,
                "_GPP: ",
                toupper(respiration_element),
                " -> ",
                gpp,
                sep = "")
        RF <-
          paste(LN,
                "_R: ",
                LN,
                " -> ",
                toupper(respiration_element),
                sep = "")

      }
      toreturn <-
        c("! GPP flows",
          "",
          gppF,
          "",
          "! respiration flows",
          "",
          RF,
          "")
      return(toreturn)

    } else {
      if (is.null(respiration_element)) {
        RF <- paste(LN, "_R: ", LN, " -> ", "CO2", sep = "")

      } else {
        RF <-
          paste(LN,
                "_R: ",
                LN,
                " -> ",
                toupper(respiration_element),
                sep = "")

      }
      toreturn <- c("! respiration flows", "", RF, "")
      return(toreturn)


    }
  }

}
