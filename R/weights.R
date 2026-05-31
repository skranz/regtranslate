#' Translate regression weight configurations into R syntax.
#'
#' Evaluates the `weights` and `flags` fields from a `reg` parcel and
#' constructs two snippets of code:
#' 1. `data_code`: A string setting up `.weight` if the weight is a mathematical expression.
#' 2. `weight_arg`: The exact string to inject into the `weights = ...` argument of the regression call.
#'
#' @param reg The single-row dataframe of the regression metadata parcel.
#' @param template A format string denoting how the final weight should be injected.
#'                 Use `"~ %s"` for fixest/formulas, or `"dat[['%s']]"` for lm/rq/tobit vectors.
#'
#' @return A list with `$data_code` and `$weight_arg`
r_weight_code = function(reg, template = "~ %s") {
  res = list(data_code = "", weight_arg = "")

  if (!"weights" %in% names(reg)) return(res)

  w_str = reg$weights
  if (is.na(w_str) || !nzchar(w_str)) return(res)

  flags = if (!is.null(reg$flags) && !is.na(reg$flags)) {
    strsplit(reg$flags, ",\\s*")[[1]]
  } else {
    character(0)
  }

  if ("weights_non_parseable" %in% flags) {
    res$weight_arg = "# Weights skipped: non-parseable expression"
    return(res)
  }

  if ("weights_expr" %in% flags) {
    res$data_code = paste0("dat[['.weight']] = with(dat, ", w_str, ")")
    res$weight_arg = paste0("weights = ", sprintf(template, ".weight"))
  } else {
    res$weight_arg = paste0("weights = ", sprintf(template, w_str))
  }

  return(res)
}
