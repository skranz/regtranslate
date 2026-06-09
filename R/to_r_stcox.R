stata_to_r_code_stcox = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_stcox")

  timevar = reg$timevar[1]
  failvar = reg$panelvar[1]

  if (is.na(timevar) || !nzchar(timevar)) {
    stop("Cannot translate stcox: timevar missing (stset not found or not parsed)")
  }

  if (!is.na(failvar) && nzchar(failvar)) {
    surv_expr = paste0("survival::Surv(`", timevar, "`, `", failvar, "`)")
  } else {
    surv_expr = paste0("survival::Surv(`", timevar, "`)")
  }

  # stcox doesn't have a LHS variable in varlist. cmdparts_of_stata_reg treats the first one as dep.
  # We convert it to exo to prevent it from going to LHS.
  regvar$role[regvar$role == "dep"] = "exo"
  if (!is.null(regxvar) && nrow(regxvar) > 0) {
    regxvar$role[regxvar$role == "dep"] = "exo"
  }

  formula_rhs = regvar_to_formula_fixest(regvar, regxvar, cmdpart, reg = reg)
  formula = paste0(surv_expr, formula_rhs)

  library_code = "library(survival)"
  rcmd_code = 'rcmd = "coxph"'

  arg_str = c(
    "formula = formula",
    "data = dat",
    'ties = "breslow"'
  )

  # Handle se
  if (reg$se_category == "robust") {
    arg_str = c(arg_str, "robust = TRUE")
  } else if (reg$se_category == "cluster") {
    clustervar = extract_clustervar_from_se_args(reg$se_args)
    if (length(clustervar) > 0) {
      arg_str = c(arg_str, paste0('cluster = dat[["', clustervar[1], '"]]'))
    }
  }

  data_code = r_listwise_deletion_code(regvar)

  # Apply dynamic weights via centralized helper
  wt = r_weight_code(reg, template = "dat[['%s']]")
  if (nzchar(wt$data_code)) {
    data_code = if (nzchar(data_code)) paste0(data_code, "\n", wt$data_code) else wt$data_code
  }
  if (nzchar(wt$weight_arg)) {
    arg_str = c(arg_str, wt$weight_arg)
  }

  formula_code = paste0('formula = ', formula)
  reg_code = paste0('reg = coxph(', paste0(arg_str, collapse=","),')')

  code_df = tibble(part = c("library", "rcmd","data","formula","reg"), code = c(library_code, rcmd_code, data_code, formula_code, reg_code))

  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=FALSE, use_conf_int=TRUE)
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}
