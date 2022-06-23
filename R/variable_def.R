## FUNCTION:
#' Variables Definition
#' Defines whole variables sections from various other functions
#' @inheritParams qvar
#' @inheritParams pvar
#' @inheritParams uvar
#' @inheritParams aevar
#' @inheritParams pp_true
#' @export
#'
variable_def <-
  function(x,
           NLNode,
           primary_producer,
           respiration) {

    if (length(NLNode) > 0) {
      u_var <- uvar(x)
    } else {
      u_var <- c("! No Unused Energy/Material Variables defined", "")
    }

    q_var <- qvar(x)
    p_var <- pvar(x, NLNode = NLNode, respiration = respiration)
    ae_var <- aevar(x)

    if (!is.null(primary_producer)) {
      q_var <- pp_true(qvar)
      p_var <- pp_true(pvar)
      u_var <- pp_true(uvar)
      ae_var <- pp_true(aevar)

    }

    toreturn <- c(q_var,
                  p_var,
                  u_var,
                  ae_var)

    return(toreturn)

  }
