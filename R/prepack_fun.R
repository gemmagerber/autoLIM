#' prepack()
#' Function to prepack netwprks for analysis with enaR (compatibility function)
#'
#' @param x an object?
#' @param limfile the built limfile
#' @importFrom network network set.edge.attribute set.vertex.attribute

prepack_fun <- function(x, limfile) {

  # List objects together
  listed <- list(
    flow = as.matrix(fmat_fun(x)),
    input = input_fun(x),
    export = export_fun(x),
    respiration = resp_fun(x),
    storage = as.vector(limfile[["Components"]][["val"]]),
    living = living_fun(x)
  )

  # enaR::pack function bits
  y <- network::network(listed[[1]], directed = TRUE, loops = TRUE)
  network::set.edge.attribute(y, names(listed)[1], as.numeric(listed[[1]]))
  flow <- as.matrix(fmat_fun(x))
  rownames(flow) <- colnames(flow)
  network::set.edge.attribute(y, "flow", flow[flow > 0])
  network::set.vertex.attribute(y, "input", input_fun(x))
  network::set.vertex.attribute(y, "export", export_fun(x))
  network::set.vertex.attribute(y, "respiration", resp_fun(x))
  network::set.vertex.attribute(y, "output", output_fun(x))
  network::set.vertex.attribute(y, "storage", as.vector(limfile[["Components"]][["val"]]))
  network::set.vertex.attribute(y, "living", living_fun(x))
  network::set.vertex.attribute(y, "vertex.names", rownames(flow))



}
