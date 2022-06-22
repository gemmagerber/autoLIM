# To do
# Add custom inequalities
# Add custom variables
# Add parameters section

library(readxl)
library(rlang)

autoLIMR <- function (net_data_input,
                      adj_mat_input,
                      NLNode = NULL,
                      respiration = NULL,
                      respiration_element = "CO2",
                      primary_producer = NULL,
                      author = Sys.getenv("USERNAME"),
                      date = Sys.Date(),
                      weighted = TRUE) {


  # Function: read all excel sheets, define as list, with sheet names as list element names
  read_all_sheets <- function(filename, tibble = FALSE) {
    options('scipen' = 99999)
    sheets <- readxl::excel_sheets(filename)
    x <-
      lapply(sheets, function(X)
        readxl::read_excel(
          filename,
          sheet = X,
          col_types = "text",
          .name_repair = "unique"
        ))
    if (!tibble)
      x <- lapply(x, as.data.frame)
    names(x) <- sheets
    return(x)
  }

  # Function: turn OFF scientific notation
  sci_notation_off <- function(x) {
    x <- as.matrix(x)
    poss_num <- suppressWarnings(as.numeric(x))
    isna <- is.na(poss_num)
    x[!isna] <- format(poss_num[!isna], scientific = FALSE)
    x <- gsub(x, pattern = "1.0000", replace = "1")
    x
  }

  # Function: Append NLNode to non-living nodes
  # In both rows and columns
  NLNode_mat <- function(x, NLNode) {
    if (length(NLNode) > 0) {
      colnames(x) <- ifelse(colnames(x) %in% NLNode,
                            paste0(NLNode[match(colnames(x), NLNode)],
                                   "NLNode"),
                            colnames(x))
      rownames(x) <- ifelse(rownames(x) %in% NLNode,
                            paste0(NLNode[match(rownames(x), NLNode)],
                                   "NLNode"),
                            rownames(x))

      return(x)
    }
  }

  # Function: search and return columns that match type (as character vector)
  # Inputs, Exports, Assimilation Efficiency,

  search_cols <- function (x, col.match) {

    if (col.match == "Input") {
      x <- grep(
        as.vector(colnames(x)),
        pattern = paste0(
          c("Import", "Imports", "Input", "^+In+$", "^+IN+$"),
          collapse = "|"
        ),
        value = TRUE,
        invert = FALSE,
        ignore.case = FALSE
      )
      return(x)
    }

    if (col.match == "Export") {
      x <- grep(
        colnames(x),
        pattern = paste0(c("Export", "Exports", "Ex", "EX"),
                         collapse = "|"),
        value = TRUE,
        invert = FALSE,
        ignore.case = TRUE
      )

      return(x)

    }

    if (col.match == "AE") {
      x <- grep(
        colnames(x),
        pattern = paste0(
          c("AE", "Assimilation", "efficiency", "AssEm"),
          collapse = "|"
        ),
        value = TRUE,
        invert = FALSE,
        ignore.case = TRUE
      )
      return(x)
    }

    if (col.match == "Custom") {
      x <- grep(
        colnames(x),
        pattern = paste0(c("Custom"),
                         collapse = "|"),
        value = TRUE,
        invert = FALSE,
        ignore.case = TRUE
      )
      return(x)
    }

    if (col.match == "Parameters") {
      x <- grep(
        colnames(x),
        pattern = paste0(c("Parameters|Parameters"),
                         collapse = "|"),
        value = TRUE,
        invert = FALSE,
        ignore.case = TRUE
      )
      return(x)
    }
  }

  # Function: Return matrix based on inputs/exports/whatever
  matrix_def <- function (x, mat.type) {
    if (mat.type == "Input") {
      col.j.in <- search_cols(x, col.match = "Input")
      in.mat <-
        x[c(rownames(x)), c(col.j.in)] # create input matrix based on exports only
      input.matrix <-
        in.mat[rowSums(is.na(in.mat)) != ncol(in.mat), ] # Drop rows where all are NA
      return(input.matrix)
    }

    if (mat.type == "Export") {
      col.j.ex <- search_cols(x, col.match = "Export")
      x <-
        x[c(rownames(x)), c(col.j.ex)] # create export matrix based on exports only
      x <-
        x[rowSums(is.na(x)) != ncol(x), ] # Drop rows where all are NA
      return(x)

    }

  }

  ### Function: Tidy up net_data_sheet_list
  net_data_tidy <- function (x, NLNode) {
    x <- as.matrix(x)
    rownames(x) <- x[, 1] # Make Compartment Name the Row Name
    x <- x[, -1] # Make Compartment Name the Row Name
    x <- NLNode_mat(x, NLNode = NLNode)

    x <-
      apply(
        X = x,
        MARGIN = 2,
        FUN = gsub,
        pattern = ",",
        replace = "."
      )   # Substitute commas for periods
    x <-
      x[!x[, "Biomass"] == "0" |
          !x[, "Biomass"] == 0 , ] # Drop any rows that have biomass of zero or NA
    x <-
      x[!is.na(x[, "Biomass"]),]

    x <- sci_notation_off(x)
    x <-
      apply(
        X = x,
        MARGIN = 2,
        FUN = gsub,
        pattern = " ",
        replace = ""
      ) # Substitute spaces for nothing
    x
  }


  # Function: Tidy up adj mat sheets
  adj_mat_tidy <- function (x, NLNode) {
    x <- as.matrix(x)
    rownames(x) <- x[, 1] # Make Compartment Name the Row Name
    x <- x[,-1] # Make Compartment Name the Row Name
    rownames(x) <- gsub(rownames(x), pattern = " ", replace = ".")
    x <- sci_notation_off(x)
    x <-
      apply(
        X = x,
        MARGIN = 2,
        FUN = gsub,
        pattern = " ",
        replace = ""
      ) # Substitute spaces for nothing
    x <- NLNode_mat(x, NLNode = NLNode)
    return(x)
  }


  # Function: define list of living/NL nodes/input/export
  net_data_node <- function (x, node.type) {
    if (node.type == "living") {
      LN <- grep(
        as.vector(rownames(x)),
        pattern = paste0(NLNode, "NLNode", collapse = "|"),
        value = TRUE,
        invert = TRUE,
        ignore.case = FALSE
      )
      return(LN)
    }
    if (node.type == "nonliving") {
      NLN <- grep(
        as.vector(rownames(x)),
        pattern = paste0(NLNode, "NLNode", collapse = "|"),
        value = TRUE,
        invert = FALSE,
        ignore.case = FALSE
      )
      return(NLN)
    }

    if (node.type == "Input") {
      input.matrix <- matrix_def(x, mat.type = "Input")
      in.mat3 <- as.vector(paste0(rownames(input.matrix), "Input"))
      return(in.mat3)
    }

    if (node.type == "Export") {
      export.matrix <- matrix_def(x, mat.type = "Export")
      export.nodes <-
        as.vector(paste0(rownames(export.matrix), "Export"))
      return(export.nodes)

    }

  }

  # Function: define compartment list
  net_data_node_list <- function(x) {
    x2 <- as.matrix(x[, "Biomass"])
    x2 <- as.vector(paste(rownames(x2), "=", x2))
    LN <- grep(x2,
               pattern = "NLNode",
               invert = TRUE,
               value = TRUE) # grab living nodes
    NLN <- grep(x2,
                pattern = "NLNode",
                invert = FALSE,
                value = TRUE) # grab nonliving nodes
    combined <-
      c(sort(LN),
        sort(NLN)) # paste together so that NLN are at bottom of list
    return(combined)

  }



  # Function: define externals list
  net_data_external_list <- function (x, respiration,
                                      respiration_element) {
    exports <- net_data_node(x, node.type = "Export")
    inputs <- net_data_node(x, node.type = "Input")

    if (respiration == TRUE) {
      if (!is.null(respiration_element)) {
        resp.vec <- as.vector(paste0(toupper(respiration_element)))
      } else {
        resp.vec <- as.vector(paste0("CO2"))
      }
    } else {
      resp.vec <- as.vector(paste0(""))
    }

    returnme <-
      c(resp.vec,
        sort(inputs),
        sort(exports))
    return(returnme)

  }



  # Function
  pvar_wo_ex <- function(x, respiration, NLNode) {
    if (length(NLNode) > 0) {
      if (!is.null(respiration) | respiration == FALSE) {
        wo.ex_pvar <- paste0(x, "_P = ", "Flowfrom(", x, ") - ",
                             x, "_U")
      }
      if (respiration == TRUE) {
        wo.ex_pvar <- paste0(x,
                             "_P = ",
                             "Flowfrom(",
                             x,
                             ") - ",
                             x,
                             "_R - ",
                             x,
                             "_U")
      }
    } else {
      if (!is.null(respiration) | respiration == FALSE) {
        wo.ex_pvar <- paste0(x, "_P = ", "Flowfrom(", x, ")")
      }
      if (respiration == TRUE) {
        wo.ex_pvar <- paste0(x,
                             "_P = ",
                             "Flowfrom(",
                             x,
                             ") - ",
                             x,
                             "_R")
      }
    }
    return(wo.ex_pvar)
  }
  # Function
  pvar_w_ex <- function (x, respiration, NLNode) {
    if (!is.null(respiration) | respiration == FALSE) {
      w.ex_pvar <-
        paste0(x,
               "_P = ",
               "Flowfrom(",
               x,
               ") - ",
               x,
               "_U - ",
               x,
               "_EX")
    }
    if (respiration == TRUE) {
      w.ex_pvar <-
        paste0(x,
               "_P = ",
               "Flowfrom(",
               x,
               ") - ",
               x,
               "_R - ",
               x,
               "_U - ",
               x,
               "_EX")
    }
    return(w.ex_pvar)
  }
  # Function
  uvar_wo_ex <- function(x) {
    if (!is.null(respiration) | respiration == FALSE) {
      wo.ex_uvar <- paste0(x,
                           "_U = ",
                           "Flowto(",
                           x,
                           ") - ",
                           x,
                           "_P")
    }

    if (respiration == TRUE) {
      wo.ex_uvar <- paste0(x,
                           "_U = ",
                           "Flowto(",
                           x,
                           ") - ",
                           x,
                           "_P - ",
                           x,
                           "_R")

    }
    return(wo.ex_uvar)
  }
  # Function
  uvar_w_ex <- function (x) {
    if (!is.null(respiration) | respiration == FALSE) {
      w.ex_uvar <- paste0(x,
                          "_U = ",
                          "Flowto(",
                          x,
                          ") - ",
                          x,
                          "_P - ",
                          x,
                          "_EX")
    }

    if (respiration == TRUE) {
      w.ex_uvar <- paste0(x,
                          "_U = ",
                          "Flowto(",
                          x,
                          ") - ",
                          x,
                          "_P - ",
                          x,
                          "_R - ",
                          x,
                          "_EX")

    }
    return(w.ex_uvar)
  }
  # Function
  pp_true <- function(x) {
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

  ## FUNCTION: Variables Definition
  variable_def <-
    function(x,
             NLNode,
             primary_producer,
             respiration) {
      ## Q Variables
      # Living nodes only, depends on Inputs
      qvar <- function(x) {
        inmat <- x[, grep("Import", colnames(x)), drop = T]
        in.mat <-
          inmat[grep("NLNode", rownames(inmat), invert = T), , drop = F]
        wo.in <-
          names(which(rowSums(is.na(in.mat)) == ncol(in.mat)))
        w.in <- names(which(rowSums(is.na(in.mat)) != ncol(in.mat)))

        if (length(wo.in) > 0 & length(w.in) > 0) {
          wo.in_qvar <- paste0(wo.in, "_Q = ", "Flowto(", wo.in, ")")
          w.in_qvar <-
            paste0(w.in, "_Q = ", "Flowto(", w.in, ") - ", w.in, "_IN")
          var <- c(wo.in_qvar, w.in_qvar)
        }

        if (identical(wo.in, character(0)) & length(w.in) > 0) {
          var <- paste0(w.in, "_Q = ", "Flowfrom(", w.in, ") - ", w.in, "_IN")

        }

        if (length(wo.in) > 0 & identical(w.in, character(0))) {
          var <- paste0(wo.in, "_Q = ", "Flowfrom(", wo.in, ")")

        }

        qvar <-
          c("! Consumption (Q) / Gross Primary Production (GPP) Variables",
            "",
            sort(var),
            "")
        return(qvar)
      }

      # Production variable definitions
      pvar <- function(x, respiration, NLNode) {
        exmat <- x[, grep("Export", colnames(x)), drop = T]
        ex.mat <-
          exmat[grep("NLNode", rownames(exmat), invert = T), , drop = F]
        wo.ex <-
          names(which(rowSums(is.na(ex.mat)) == ncol(ex.mat)))
        w.ex <- names(which(rowSums(is.na(ex.mat)) != ncol(ex.mat)))

        if (length(wo.ex) > 0 & length(w.ex) > 0) {
          wo.ex_pvar <-
            pvar_wo_ex(wo.ex, respiration = respiration, NLNode = NLNode)
          w.ex_pvar <-
            pvar_w_ex(w.ex, respiration = respiration, NLNode = NLNode)
          var <- c(wo.ex_pvar, w.ex_pvar)
        }

        if (identical(wo.ex, character(0)) & length(w.ex) > 0) {
          var <- pvar_w_ex(w.ex, respiration = respiration, NLNode = NLNode)
        }

        if (length(wo.ex) > 0 & identical(w.ex, character(0))) {
          var <- pvar_wo_ex(wo.ex, respiration = respiration, NLNode = NLNode)
        }

        if (length(var) > 0) {
          pvar <- c("! Production (P/NPP) Variables", "", sort(var), "")
        } else {
          pvar <- c("", "! No Production Variables (U) defined", "")
        }
      }

      if (length(NLNode) > 0) {
        uvar <- function (x) {
          exmat <- x[, grep("Export", colnames(x)), drop = T]
          ex.mat <-
            exmat[grep("NLNode", rownames(exmat), invert = T), , drop = F]
          wo.ex <-
            names(which(rowSums(is.na(ex.mat)) == ncol(ex.mat)))
          w.ex <-
            names(which(rowSums(is.na(ex.mat)) != ncol(ex.mat)))

          if (length(wo.ex) > 0 & length(w.ex) > 0) {
            wo.ex_uvar <- uvar_wo_ex(wo.ex)
            w.ex_uvar <- uvar_w_ex(w.ex)
            var <- c(wo.ex_uvar, w.ex_uvar)

          }

          if (identical(wo.ex, character(0)) & length(w.ex) > 0) {
            var <- uvar_w_ex(w.ex)

          }

          if (length(wo.ex) > 0 & identical(w.ex, character(0))) {
            var <- uvar_wo_ex(wo.ex)

          }

          if (length(var) > 0) {
            uvar <-
              c("! Unused Energy/Material (U) Variables",
                "",
                sort(var),
                "")
          } else {
            uvar <-
              c("",
                "! No Unused Energy/Material (U) Variables defined",
                "")
          }

        }
      } else {
        uvar <- c("! No Unused Energy/Material Variables defined", "")
      }

      # AE Variable definition
      aevar <- function (x) {
        ## Assimilation efficiencies

        ae.search <- search_cols(x, col.match = "AE")
        aemat <- x[, c(ae.search), drop = T]
        ae.mat <-
          aemat[grep("NLNode", rownames(aemat), invert = T), , drop = F]
        w.ae <- names(which(rowSums(is.na(ae.mat)) != ncol(ae.mat)))
        if (length(w.ae) > 0) {
          # Function: Define AE variables only for nodes that have AE inequalities
          aevar_w_ae <- function (x, respiration) {
            w.ae_uvar <- paste0(x,
                                "_AE = ",
                                x,
                                "_P")

            if (respiration == TRUE) {
              w.ae_uvar <- paste0(x,
                                  "_AE = ",
                                  x,
                                  "_P - ",
                                  x,
                                  "_R")

            }
            return(w.ae_uvar)
          }

          aevar <- aevar_w_ae(w.ae, respiration = respiration)
          aevar <-
            c("! Assimilation Efficiency (AE) Variables",
              "",
              sort(aevar))
        } else {
          aevar <- c("! No Assimilation Efficiency (AE) Variables defined")
        }
      }

      qvar <- qvar(x)
      pvar <- pvar(x, NLNode = NLNode, respiration = respiration)
      uvar <- uvar(x)
      aevar <- aevar(x)

      if (!is.null(primary_producer)) {
        qvar <- pp_true(qvar)
        pvar <- pp_true(pvar)
        uvar <- pp_true(uvar)
        aevar <- pp_true(aevar)

      }

      toreturn <- c(qvar,
                    pvar,
                    uvar,
                    aevar)

      return(toreturn)

    }



  # Function: respiration flow definition
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



  # Function : Input and Export flows
  net_data_inex_flows <-  function (x) {
    ex.mat2 <- matrix_def(x, mat.type = "Export")
    in.mat2 <- matrix_def(x, mat.type = "Input")

    x <- c(
      "! Input flows",
      "",
      paste0(
        rownames(in.mat2),
        "_IN: ",
        rownames(in.mat2),
        "Input",
        " -> ",
        rownames(in.mat2)
      ),
      "",
      "! Export flows",
      "",
      paste0(
        rownames(ex.mat2),
        "_EX: ",
        rownames(ex.mat2),
        " -> ",
        rownames(ex.mat2),
        "Export"
      ),
      ""
    )
    return(x)
  }


  # Function: Adjacency Matrix flow definition
  adj_mat_flows <- function (x) {
    x <- na.omit(as.data.frame(as.table(x)))
    x$flowtype <-
      ifelse(
        grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
        "_to_",
        ifelse(
          grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
          "_Q_",
          ifelse(
            !grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
            "_U_",
            ifelse(
              !grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
              "_Q_",
              "test"
            )
          )
        )
      )

    x$flownames <- paste0(x$Var1, x$flowtype, x$Var2)
    x$flows <- paste0(x$flownames, ": ", x$Var1, " -> ", x$Var2)
    flows <- as.vector(x$flows)

    # Add headings
    flows2 <- c("! Adjacency Matrix flows", "", sort(flows))
    return(flows2)
  }


  # # Rearrange Q, U, to
  # flows2 <- c(
  #   "! Adjacency Matrix Consumption (Q) flows",
  #   "",
  #   grep("_Q_", flows, value = TRUE),
  #   "",
  #   "! Adjacency Matrix Unused Energy/Material (U) flows",
  #   "",
  #   grep("_U_", flows, value = TRUE),
  #   "",
  #   "! Adjacency Matrix Other (to) flows",
  #   "",
  #   grep("_to_", flows, value = TRUE),
  #   ""
  # )
  # return(flows2)

  ### Function: Inequalities (with headings)
  net_data_ineq <- function (x) {
    x <- na.omit(as.data.frame(as.table(x)))
    x$Variable <- ifelse(
      grepl("Consumption|GPP", x$Var2) &
        grepl(paste0(primary_producer, collapse = "|"), x$Var1),
      paste0(x$Var1, "_GPP"),
      ifelse(
        grepl("Consumption|GPP", x$Var2) &
          !grepl(paste0(primary_producer, collapse = "|"), x$Var1),
        paste0(x$Var1, "_Q"),
        ifelse(
          grepl("Production|Net_Primary_Production|NPP|P", x$Var2) &
            grepl(paste0(primary_producer, collapse = "|"), x$Var1),
          paste0(x$Var1, "_NPP"),
          ifelse(
            grepl("Production|Net_Primary_Production|NPP|P", x$Var2) &
              !grepl(paste0(primary_producer, collapse = "|"), x$Var1),
            paste0(x$Var1, "_P"),
            ifelse(
              grepl("respiration|Resp|R", x$Var2),
              paste0(x$Var1, "_R"),
              ifelse(
                grepl("Unused energy|Unused", x$Var2),
                paste0(x$Var1, "_U"),
                ifelse(
                  grepl("Export|EX|Ex|Exports", x$Var2),
                  paste0(x$Var1, "_EX"),
                  ifelse(
                    grepl("Import|Imports|Input|IN|In", x$Var2),
                    paste0(x$Var1, "_IN"),
                    ifelse(
                      grepl("Assimilation|Efficiency|AE", x$Var2),
                      paste0(x$Var1, "_AE"),
                      "none"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


    x$Freq <- as.character(x$Freq)

    x$Inequality <- ifelse(grepl("^1$|^0$", x$Freq),
                           "none",
                           ifelse(
                             grepl("lower|low|min|minimum", x$Var2, ignore.case = TRUE),
                             paste0(x$Variable, " > ", x$Freq),
                             ifelse(
                               grepl("upper|up|max|maximum", x$Var2, ignore.case = TRUE),
                               paste0(x$Variable, " < ", x$Freq),
                               "none"
                             )
                           ))

    x <- x[!grepl("none", x$Inequality),]
    #x <- with(x, x[order(x$Var2, x$Var1) , ]) # Sort

    toreturn <- c("! Network Data Input Inequalities",
                  "",
                  as.vector(x$Inequality),
                  "")

    # toreturn <- c(
    #   "! Network Data Input Inequalities",
    #   "",
    #   as.vector(x[grepl("_GPP", x$Inequality),]$Inequality),
    #   "",
    #   as.vector(x[grepl("_Q", x$Inequality),]$Inequality),
    #   "",
    #   as.vector(x[grepl("_NPP", x$Inequality),]$Inequality),
    #   "",
    #   as.vector(x[grepl("_P", x$Inequality),]$Inequality),
    #   "",
    #   as.vector(x[grepl("_R", x$Inequality),]$Inequality),
    #   "",
    #   as.vector(x[grepl("_U", x$Inequality),]$Inequality),
    #   "",
    #   as.vector(x[grepl("_IN", x$Inequality),]$Inequality),
    #   "",
    #   as.vector(x[grepl("_EX", x$Inequality),]$Inequality),
    #   "",
    #   as.vector(x[grepl("_AE", x$Inequality),]$Inequality),
    #   ""
    # )

    return(toreturn)

  }

  # Function: Matrix inequalities
  adj_mat_ineq <- function (x) {
    x <- na.omit(as.data.frame(as.table(x)))
    x$flowtype <-
      ifelse(
        grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
        "_to_",
        ifelse(
          grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
          "_Q_",
          ifelse(
            !grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
            "_U_",
            ifelse(
              !grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
              "_Q_",
              "test"
            )
          )
        )
      )

    x$flownames <- paste0(x$Var1, x$flowtype, x$Var2)

    x$Variable <-
      ifelse(
        grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
        "_to",
        ifelse(
          grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
          "_Q",
          ifelse(
            !grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
            "_U",
            ifelse(
              !grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
              "_Q",
              "test"
            )
          )
        )
      )

    x$Freq <- as.character(x$Freq)

    # x$low_ineq <-
    #   ifelse(
    #     grepl("1|1.0", x$Freq) & !grepl(",", x$Freq),
    #     "none",
    #     ifelse(
    #       grepl(",", x$Freq),
    #       paste0(
    #         x$flownames,
    #         " > ",
    #         x$Var2,
    #         x$Variable,
    #         " * ",
    #         gsub(",.+$", "", x$Freq)
    #       ),
    #       "none"
    #     )
    #   )

    # x$low_ineq <-
    #   ifelse(
    #     grepl("1|1.0", x$Freq) & !grepl(",", x$Freq),
    #     "none",
    #     ifelse(
    #       grepl(",", x$Freq),
    #       paste0(
    #         x$flownames,
    #         " = ",
    #         x$Var2,
    #         x$Variable,
    #         " * ",
    #         "[",
    #         x$Freq,
    #         "]"
    #       ),
    #       "none"
    #     )
    #   )
    #
    # x$up_ineq <-
    #   ifelse(
    #     !grepl("1|1.0|,", x$Freq),
    #       paste0(
    #         x$flownames,
    #         " < ",
    #         x$Var2,
    #         x$Variable,
    #         " * ",
    #         x$Freq)
    #       ,
    #         "none"
    #       )
    #
    x$ineq <-
      ifelse(
        grepl("1|1.0", x$Freq) & !grepl(",", x$Freq),
        "none",
        ifelse(
          grepl(",", x$Freq) & grepl("NLNode", x$Var2),
          paste0(x$flownames,
                 " = ",
                 x$Var1,
                 x$Variable,
                 " * ",
                 "[",
                 x$Freq,
                 "]"),
          ifelse(
            grepl(",", x$Freq) & !grepl("NLNode", x$Var2),
            paste0(x$flownames,
                   " = ",
                   x$Var2,
                   x$Variable,
                   " * ",
                   "[",
                   x$Freq,
                   "]"),
            ifelse(
              !grepl(",", x$Freq) & grepl("NLNode", x$Var2),
              paste0(x$flownames,
                     " < ",
                     x$Var1,
                     x$Variable,
                     " * ",
                     x$Freq)
              ,
              ifelse(
                !grepl(",", x$Freq) & !grepl("NLNode", x$Var2),
                paste0(x$flownames,
                       " < ",
                       x$Var2,
                       x$Variable,
                       " * ",
                       x$Freq)
                ,
                "none"
              )
            )
          )
        )
      )

    # x$up_ineq <-
    #   ifelse(
    #     grepl("1|1.0", x$Freq) & !grepl(",", x$Freq),
    #     "none",
    #     ifelse(
    #       grepl(",", x$Freq),
    #       paste0(
    #         x$flownames,
    #         " < ",
    #         x$Var2,
    #         x$Variable,
    #         " * ",
    #         gsub("^.+,", "", x$Freq)
    #       ),
    #       ifelse(
    #         !grepl(",", x$Freq),
    #         paste0(
    #           x$flownames,
    #           " < ",
    #           x$Var2,
    #           x$Variable,
    #           " * ",
    #           gsub("^.+,", "", x$Freq)
    #         ),
    #         "none"
    #       )
    #     )
    #   )
    #

    na.remove <- x[!grepl("none", x$ineq),]
    ineq.vec <- as.vector(na.remove$ineq)


    # lower.na.remove <- x[!grepl("none", x$low_ineq),]
    # lower.inequality <- as.vector(lower.na.remove$low_ineq)
    #
    # upper.na.remove <- x[!grepl("none", x$up_ineq),]
    # upper.inequality <- as.vector(upper.na.remove$up_ineq)
    #


    toreturn <- c("! Adjacency Matrix Inequalities",
                  "",
                  ineq.vec)

    return(toreturn)
  }

  # New function: meta1 (without ad mats info or abbreviations)
  meta1 <- function (x,
                     author = NULL,
                     date = NULL,
                     respiration,
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

  # New function: metadata2 (ad mats) with abbreviations

  meta2 <- function(x) {
    internals <- paste0("! Internal flows: ", sum(!is.na(x)))
    metadata2 <- c(
      internals,
      "",
      "! Abbreviations",
      "! GPP = Gross Primary Production (autotrophs only)",
      "! Q = Consumption",
      "! NPP = Net Primary Production (autotrophs only)",
      "! P = Production",
      "! R = respiration",
      "! U = Passive flows to non-living compartments/Unassimilated material",
      "! AE = Assimilation Efficiency",
      "! IN = Import flow",
      "! EX = Export Flow",
      "! NLNode = Non-living compartment",
      ""
    )
    return(metadata2)
  }

  # Function: Merge lists together, name sections
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

  # Execution: Print Errors for undefined sheets
  error_print(net_data_input, adj_mat_input)

  # Execution: Read in net data sheets
  net_data_sheet_list <- read_all_sheets(filename = net_data_input)

  # Execution: Read in Fmats
  adj_matrix_sheet_list <- read_all_sheets(filename = adj_mat_input)

  # Execution: Tidy up net_data_sheet_list
  net_data_sheets <-
    lapply(X = net_data_sheet_list, net_data_tidy, NLNode = NLNode)

  # Execution: Tidy up adj mat sheets
  adj_matrix_sheets <-
    lapply(X = adj_matrix_sheet_list, adj_mat_tidy, NLNode = NLNode)

  # Execution: define compartment list
  comp.list <- lapply(X = net_data_sheets, FUN = net_data_node_list)

  # Execution: define externals list
  externals.list <-
    lapply(X = net_data_sheets,
           FUN = net_data_external_list,
           respiration,
           respiration_element)

  #x <- net_data_sheets[["Winter"]]
  #x2 <- net_data_external_list(x, respiration = respiration, respiration_element = respiration_element)


  # Execution: define QPU variables, change based on primary producers
  vars <- lapply(
    X = net_data_sheets,
    FUN = variable_def,
    NLNode = NLNode,
    primary_producer = primary_producer,
    respiration = respiration
  )

  # Execution: respiration flow definition
  resp_flows <- lapply(
    X = net_data_sheets,
    FUN = net_data_resp_flows,
    respiration = TRUE,
    respiration_element = respiration_element,
    primary_producer = primary_producer
  )

  # Execution: Input and Export flows
  inex.flow.list <-
    lapply(X = net_data_sheets, FUN = net_data_inex_flows)

  # Execution: Define matrix flows only
  adj.mats.flow.list <-
    lapply(X = adj_matrix_sheets, adj_mat_flows)

  # Execution: Get inequalities from matrices
  net_data_ineq_list <-
    lapply(X = net_data_sheets, net_data_ineq)
  adj_mat_ineq_list <-
    lapply(X = adj_matrix_sheets, adj_mat_ineq)

  # Execution: Get metadata table1 for weighted file
  meta_w <- lapply(
    X = net_data_sheets,
    FUN = meta1,
    primary_producer = primary_producer,
    respiration = respiration,
    NLNode = NLNode,
    weighted = TRUE
  )

  # Execution: Get metadata table1 for unweighted file
  meta_uw <- lapply(
    X = net_data_sheets,
    FUN = meta1,
    primary_producer = primary_producer,
    respiration = respiration,
    NLNode = NLNode,
    weighted = FALSE
  )

  # Execution: get metadata table2
  meta_2 <- lapply(X = adj_matrix_sheets,
                   FUN = meta2)

  # Execution: Merge compartments, give name
  comp.lim <- merge(comp.list, type = "Compartments")

  # Execution: Merge compartments, give name
  externals.lim <- merge(externals.list, type = "Externals")

  # Execution: Merge variable lists, name sections
  var.lim <- merge(vars, type = "Variables")

  # Execution: Merge flow lists, add section headings
  flow.lim <-
    merge(resp_flows, inex.flow.list, adj.mats.flow.list,
          type = "Flows")
  # Execution: Merge inequalities lists, add section headings
  ineq.lim <-
    merge(net_data_ineq_list, adj_mat_ineq_list, type = "Inequalities")

  # Execution: merge all sections into full lim files
  Weighted <-
    merge(meta_w,
          meta_2,
          comp.lim,
          externals.lim,
          var.lim,
          flow.lim,
          ineq.lim,
          type = NULL)
  Unweighted <-
    merge(meta_uw,
          meta_2,
          comp.lim,
          externals.lim,
          var.lim,
          flow.lim,
          type = NULL)

  # Function: write limfiles to subfolders in working directory
  testdir2 <- paste(getwd(),
                    "autoLIMR Weighted Network LIMfiles",
                    collapse = "/",
                    sep = "/")

  if (dir.exists(paste(
    getwd(),
    "autoLIMR Weighted Network LIMfiles",
    collapse = "/",
    sep = "/"
  )) == FALSE) {
    path <- paste(
      getwd(),
      "autoLIMR Weighted Network LIMfiles",
      collapse = "/",
      sep = "/"
    )
    dir.create(path)
  }

  for (i in names(Weighted)) {
    write(Weighted[[i]],
          paste0(testdir2,
                 "////",
                 i,
                 "_Weighted Network LIMfile.R"))
  }


  testdir3 <- paste(
    getwd(),
    "autoLIMR Unweighted Network LIMfiles",
    collapse = "/",
    sep = "/"
  )
  if (dir.exists(paste(
    getwd(),
    "autoLIMR Unweighted Network LIMfiles",
    collapse = "/",
    sep = "/"
  )) == FALSE) {
    path <- paste(
      getwd(),
      "autoLIMR Unweighted Network LIMfiles",
      collapse = "/",
      sep = "/"
    )
    dir.create(path)
  }

  for (i in names(Unweighted)) {
    write(Unweighted[[i]],
          paste0(testdir3,
                 "////",
                 i,
                 "_Unweighted Network LIMfile.R"))
  }

}
