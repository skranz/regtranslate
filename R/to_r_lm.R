# TO DO: VCOV
# TO DO: VCOV
stata_to_r_code_lm = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_lm")

  org_depvars = regvar$cterm[regvar$role=="dep"]
  mod_depvars = replace_cterm_special_symbols(org_depvars)

  formula = regvar_to_formula_fixest(regvar, regxvar, cmdpart, reg = reg)

  command = "lm"
  arg_str = c(
    paste0("formula = formula"),
    paste0('data = dat')
  )

  rcmd_code = paste0('rcmd = "',command,'"')
  # We use the default ssc arguments since they are closest to the
  # Stata defaults
  if (all(org_depvars==mod_depvars)) {
    data_code = ""
  } else {
    data_code = paste0(
      'dat[["', mod_depvars,'"]] = dat[["', org_depvars,'"]]',
      collapse="\n"
    )
  }

  # Apply dynamic weights via centralized helper
  wt = r_weight_code(reg, template = "dat[['%s']]")
  if (nzchar(wt$data_code)) {
    data_code = if (nzchar(data_code)) paste0(data_code, "\n", wt$data_code) else wt$data_code
  }
  if (nzchar(wt$weight_arg)) {
    arg_str = c(arg_str, wt$weight_arg)
  }

  formula_code = paste0('formula = ', formula)
  reg_code = paste0('reg = ', command, "(", paste0(arg_str, collapse=","),")")

  code_df = tibble(part = c("rcmd","data","formula", "reg"), code = c(rcmd_code,data_code, formula_code, reg_code))


  use_summary=FALSE
  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=use_summary, use_conf_int=TRUE)
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}

regvar_to_formula_lm = function(regvar, regxvar, cmdpart, reg = NULL) {
  regvar_to_formula_fixest(regvar, regxvar, cmdpart, reg = reg)
}
