#' autoLIMR Automatically defines LIM weighted and unweighted declaration files
#' from two input workbooks
#' @inheritParams error_print
#' @inheritParams read_all_sheets
#' @inheritParams net_data_tidy
#' @inheritParams adj_mat_tidy

#' @param NLNode the defined NLNodes from the main autoLIMR argument
#' @param respiration If respiration = TRUE in main autoLIMR argument
#' @param respiration_element if Respiration = TRUE, the respiration element to
#'   be defined. Default to "CO2"
#' @param primary_producer Primary producers defined in main autoLIMR function
#' @param author author name. Defined in the main autoLIMR function. Defaults to
#'   system user
#' @param date date. Defined in the main autoLIMR function. Defaults to system
#'   date
#' @param weighted whether to return weighted LIM declaration files. Default to
#'   TRUE
#' @return Two folders containing weighted, and unweighted network LIM
#'   declaration files respectively. For use with R package LIM
#' @export
#' @examples \dontrun{autoLIMR(net_data_input = "your_network_data_workbook.xlsx",
#' adj_mat_input = "your_adjacency_matrix_data_workbook.xlsx",
#' NLNode = NULL,
#' respiration = NULL,
#' respiration_element = "CO2",
#' primary_producer = NULL,
#' author = "<your name>",
#' date = "<the date>",
#' weighted = TRUE)}


autoLIMR <- function (net_data_input,
                      adj_mat_input,
                      NLNode = NULL,
                      respiration = NULL,
                      respiration_element = "CO2",
                      primary_producer = NULL,
                      author = Sys.getenv("USERNAME"),
                      date = Sys.Date(),
                      weighted = TRUE) {

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
