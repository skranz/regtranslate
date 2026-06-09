#' Drop perfect predictors for binary outcome models
#'
#' Emulates Stata's behavior of dropping variables and observations
#' that perfectly predict the outcome in logit/probit models.
#'
#' @param dat data.frame
#' @param depvar character, name of dependent variable
#' @param indepvars character vector of independent variables
#' @param verbose logical, if TRUE prints Stata-like notes
#' @return A list with `dat` (filtered data) and `indepvars` (variables kept).
#' @export
stata_drop_perfect_predictors = function(dat, depvar, indepvars, verbose = TRUE) {
  if (length(depvar) != 1 || !depvar %in% colnames(dat)) return(list(dat=dat, indepvars=indepvars))

  y = dat[[depvar]]
  if (is.logical(y)) y = as.integer(y)
  y = as.numeric(y)
  # Stata treats exactly 0 as failure, and non-zero as success
  y = ifelse(y == 0, 0, 1)

  uni_y = unique(na.omit(y))
  if (length(uni_y) < 2) {
    return(list(dat=dat, indepvars=indepvars))
  }

  kept_vars = indepvars
  keep_rows = !is.na(y)

  changed = TRUE

  while(changed) {
    changed = FALSE
    cur_y = y[keep_rows]

    for (var in kept_vars) {
      if (!var %in% colnames(dat)) next
      x = dat[[var]]

      if (is.character(x) || is.factor(x)) next

      cur_x = x[keep_rows]
      if (all(is.na(cur_x))) next

      # CRITICAL FIX: Stata only applies the "!= 0" heuristic to dummy variables.
      # If we don't check this, continuous variables with a single 0 observation
      # will falsely appear as perfect predictors and drop the observation.
      uni_x = unique(na.omit(cur_x))
      if (length(uni_x) > 2) next

      idx_neq_0 = which(!is.na(cur_x) & cur_x != 0)
      idx_eq_0 = which(!is.na(cur_x) & cur_x == 0)

      if (length(idx_neq_0) > 0 && length(idx_eq_0) > 0) {

        y_neq = cur_y[idx_neq_0]
        if (min(y_neq) == max(y_neq)) {
          if (verbose) {
             cat(sprintf("note: %s != 0 predicts %s perfectly;\n      %s omitted and %d obs not used.\n",
                         var, ifelse(y_neq[1]==1, "success", "failure"), var, length(idx_neq_0)))
          }
          keep_rows[which(keep_rows)[idx_neq_0]] = FALSE
          kept_vars = setdiff(kept_vars, var)
          changed = TRUE
          break
        }

        y_eq = cur_y[idx_eq_0]
        if (min(y_eq) == max(y_eq)) {
          if (verbose) {
             cat(sprintf("note: %s == 0 predicts %s perfectly;\n      %s omitted and %d obs not used.\n",
                         var, ifelse(y_eq[1]==1, "success", "failure"), var, length(idx_eq_0)))
          }
          keep_rows[which(keep_rows)[idx_eq_0]] = FALSE
          kept_vars = setdiff(kept_vars, var)
          changed = TRUE
          break
        }
      }
    }
  }

  if (!all(keep_rows)) {
    dat = dat[keep_rows, , drop=FALSE]
  }

  list(dat = dat, indepvars = kept_vars)
}
