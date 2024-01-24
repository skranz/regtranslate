
stata_to_r_code_quantreg = function(reg, regvar,regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_quantreg")

  # Ignore dropped regvars (if they are nor part of an interaction)
  #regvar = filter(regvar, !is_dropped | ia_cterm != cterm)


  # Currently we just use the fixest formula
  formula = regvar_to_formula_fixest(regvar, regxvar, cmdpart)

  rcmd = "rq"

  library_code = paste0("library(quantreg)")
  rcmd_code = paste0('rcmd = "',rcmd,'"')
  # We use the default ssc arguments since they are closest to the
  # Stata defaults
  formula_code = paste0('formula = ', formula)

  arg_str = NULL
  if (reg$se_category != "iid") {
    stop("Currently stata_to_r_code_quantreg is only implemented for iid standard errors. ")
  }
  arg_str = c(
    paste0("formula = formula"),
    paste0('data = dat'),
    arg_str
  )

  weight_var = regvar$cterm[regvar$role == "weight"]
  if (length(weight_var)==1) {
    arg_str = c(arg_str, paste0('weights = dat[["',weight_var,'"]]'))
  } else if (length(weight_var)>1) {
    stop("Cannot deal with multiple weight variables.")
  }

  opts_df = cmdpart_to_opts_df(cmdpart)
  opt_row = which(opts_df$opt=="quantile")
  if (length(opt_row)>0) {
    arg_str = c(arg_str, paste0("tau = ", opts_df$opt_arg[opt_row]))
  }


  reg_code = paste0('reg = suppressWarnings(', rcmd,'(', paste0(arg_str, collapse=","),"))")

  code_df = tibble(part = c("library", "rcmd","formula","reg"), code = c(library_code, rcmd_code,formula_code,reg_code))

  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=FALSE, use_conf_int=TRUE)
    code_df = bind_rows(code_df, tibble(part="ct_mod",code='
ct = mutate(ct, std.error=NA_real_, statistic= NA_real_,  p.value = NA_real_)
if ("logLik" %in% names(glance)) {
  glance$logLik = as.numeric(glance$logLik)
}
'))
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}

tidy.tobit <- function(x, ...) {
  class(x) <- "survreg"
  tidy(x, ...)
}
