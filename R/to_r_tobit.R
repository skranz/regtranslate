
stata_to_r_code_tobit = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_mfx")

  # Ignore dropped regvars (if they are nor part of an interaction)
  #regvar = filter(regvar, !is_dropped | ia_cterm != cterm)

  # Currently we just use the fixest formula
  formula = regvar_to_formula_fixest(regvar, regxvar, cmdpart)

  rcmd = "tobit"

  library_code = paste0("library(AER)")
  rcmd_code = paste0('rcmd = "',rcmd,'"')
  # We use the default ssc arguments since they are closest to the
  # Stata defaults
  formula_code = paste0('formula = ', formula)

  arg_str = NULL
  if (reg$se_category == "robust") {
    arg_str = "robust = true"
  } else if (reg$se_category == "cluster") {
    clustervar = extract_clustervar_from_se_args(reg$se_args)
    arg_str = paste0('cluster = "', clustervar[1],'"')
    if (reg$se_type == "twoway") {
      stop("Two way clusters not yet implemented for tobit in R")
    }
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
    stop("Cannot deal with mutliple weight variables.")
  }


  reg_code = paste0('reg = ', rcmd,'(', paste0(arg_str, collapse=","),")
class(reg) = 'survreg'")

  code_df = tibble(part = c("library", "rcmd","formula","reg"), code = c(library_code, rcmd_code,formula_code,reg_code))

  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=FALSE, use_conf_int=TRUE)
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
