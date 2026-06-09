example = function() {

  dat = tibble(y=sample(0:1, size=100, replace=TRUE), x1 = rnorm(100), x2 = rnorm(100))
  reg = glm(y~x1+x2,family = binomial(link = "probit"), data=dat)

  library(mfx)
  mfx=probitmfx(y~x1+x2, data=dat)
  coeftest(mfx,vcov. = sandwich::vcovHAC(mfx))

  broom::tidy(mfx)

}


stata_to_r_code_mfx = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_mfx")

  # Ignore dropped regvars (if they are nor part of an interaction)
  #regvar = filter(regvar, !is_dropped | ia_cterm != cterm)

  # Currently we just use the fixest formula
  formula = regvar_to_formula_fixest(regvar,regxvar, cmdpart, reg = reg)

  cmd = reg$cmd
  if (cmd=="dprobit") {
    rcmd = "probitmfx"
  } else {
    stop("Cannot yet translate Stata command ", cmd)
  }

  # The exclude='select' arguments avoids overwriting
  # of dplyr's select function
  library_code = "library(MASS, exclude='select')\nlibrary(mfx)\n  "
  rcmd_code = paste0('rcmd = "',rcmd,'"')
  # We use the default ssc arguments since they are closest to the
  # Stata defaults
  formula_code = paste0('formula = ', formula)

  data_code = r_listwise_deletion_code(regvar)

  is_binary = reg$cmd %in% c("logit", "xtlogit", "probit", "xtprobit", "dprobit", "clogit", "logistic", "exlogistic")
  if (is_binary && isTRUE(opts$drop_perfect_predictors)) {
    mod_depvars = regvar$cterm[regvar$role=="dep"]
    pred_cols = unique(regxvar$cterm)
    pred_cols = setdiff(pred_cols, c("(Intercept)", ""))
    if (length(pred_cols) > 0) {
      pred_str = paste0('c(', paste0('"', pred_cols, '"', collapse=", "), ')')
      dp_code = paste0(
        'dp_cols = intersect(', pred_str, ', colnames(dat))\n',
        'dp_res = regtranslate::stata_drop_perfect_predictors(dat, "', mod_depvars[1], '", dp_cols, verbose = TRUE)\n',
        'dat = dp_res$dat'
      )
      data_code = paste0(data_code, "\n", dp_code)
    }
  }

  # mfx
  arg_str = NULL
  if (reg$se_category == "robust") {
    arg_str = "robust = true"
  } else if (reg$se_category == "cluster") {
    clustervar = extract_clustervar_from_se_args(reg$se_args)
    arg_str = paste0('clustervar1 = "', clustervar[1],'"')
    if (reg$se_type == "twoway") {
      arg_str = c(arg_str, paste0('clustervar2 = "', clustervar[2],'"'))
    }
  }
  arg_str = c(
    paste0("formula = formula"),
    paste0('data = dat'),
    arg_str
  )

  reg_code = paste0('reg = ', rcmd,'(', paste0(arg_str, collapse=","),")")
  code_df = tibble(part = c("library", "rcmd","data","formula","reg"), code = c(library_code, rcmd_code,data_code,formula_code,reg_code))
  code_df = code_df[code_df$code != "", ]

  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=FALSE, use_conf_int=TRUE)
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}
