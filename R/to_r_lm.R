# TO DO: VCOV
stata_to_r_code_lm = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_lm")

  org_depvars = regvar$cterm[regvar$role=="dep"]
  mod_depvars = replace_cterm_special_symbols(org_depvars)

  formula = regvar_to_formula_fixest(regvar, regxvar, cmdpart)

  command = "lm"
  arg_str = c(
    paste0("formula = formula"),
    paste0('data = dat')
  )

  weight_var = regvar$cterm[regvar$role == "weight"]
  if (length(weight_var)>0) {
    arg_str = c(arg_str, paste0("weights = dat$", paste0(weight_var, collapse="+")))
  }

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
  formula_code = paste0('formula = ', formula)
  reg_code = paste0('reg = ', command, "(", paste0(arg_str, collapse=","),")")

  code_df = tibble(part = c("rcmd","data","formula", "reg"), code = c(rcmd_code,data_code, formula_code, reg_code))


  # vcov_type = fixest_vcov_type_from_regdb(reg$se_type, reg$se_args)
  # use_sandwich = TRUE
  # use_summary = use_sandwich | opts$prefer_summary
  #
  # if (use_sandwich) {
  #   reg_vcov = "iid"
  #   vcov = regdb_se_to_sandwich(reg$se_category, reg$se_type, reg$se_args)
  # }
  #
  #
  # if (use_summary) {
  #   sum_vcov_code = paste0('sum_vcov = ', quote_arg(vcov))
  #   sum_code = 'sum = summary(reg, vcov = sum_vcov)'
  #   code_df = bind_rows(code_df,
  #     tibble(part = c("sum_vcov","sum"), code = c(sum_vcov_code, sum_code))
  #   )
  # }
  use_summary=FALSE
  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=use_summary, use_conf_int=TRUE)
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}

regvar_to_formula_lm = function(regvar, regxvar, cmdpart) {
  regvar_to_formula_fixest(regvar, regxvar, cmdpart)
}
