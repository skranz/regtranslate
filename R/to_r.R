# Translate to R code
example = function() {
  project.dir = "C:/libraries/repbox/projects_reg/testsupp"

  core = readRDS(file.path(project.dir, "regdb","reg_core.Rds"))
  regs = core$reg
  varinfo = readRDS(file.path(project.dir, "regdb","base_varinfo.Rds"))
  regvars =  varinfo$regvar

  step = 3
  reg = regs[regs$step==step,]
  regvar = regvars[regvars$step==step,]

  code_df = reg_stata_to_r_code(reg, regvar, opts = code_options(add_function=TRUE, add_restorepoint = TRUE))
}


code_options = function(prefer_sandwich=FALSE, prefer_summary=FALSE, add_broom=TRUE, add_function=FALSE, add_restorepoint=FALSE, drop_perfect_predictors=TRUE, match_stata_singletons=TRUE) {
  list(
    prefer_sandwich = prefer_sandwich,
    prefer_summary = prefer_summary,
    add_broom = add_broom,
    add_function = add_function,
    add_restorepoint = add_restorepoint,
    drop_perfect_predictors = drop_perfect_predictors,
    match_stata_singletons = match_stata_singletons
  )
}

reg_stata_to_r_code = function(reg, regvar, regxvar, cmdpart, prefer="fixest", opts=code_options()) {
  restore.point("reg_stata_to_r_code")

  r_cmd = get_stata_to_r_cmd(reg$cmd, prefer, reg = reg)
  if (isTRUE(r_cmd == "no_trans") | is.na(r_cmd)) {
    cat(paste0("The Stata command ", reg$cmd, " is not yet implemented for translation in regtranslate."))
    return(NULL)
  }

  args = list(reg=reg, regvar=regvar,regxvar, cmdpart=cmdpart, opts=opts)
  fun = paste0("stata_to_r_code_",r_cmd)
  res = do.call(fun, args)

  # Centralized injection of Date to numeric conversion code
  if (!is.null(res) && "data" %in% res$part) {
    date_code = r_date_to_numeric_code(regvar, runid = reg$runid)
    if (nzchar(date_code)) {
      data_idx = which(res$part == "data")[1]
      if (nzchar(res$code[data_idx])) {
        res$code[data_idx] = paste0(date_code, "\n", res$code[data_idx])
      } else {
        res$code[data_idx] = date_code
      }
    }
  }

  res
}

reg_stata_to_r_formula = function(reg, regvar, regxvar, cmdpart, prefer="fixest", opts=code_options()) {
  restore.point("reg_stata_to_r_formula")

  r_cmd = get_stata_to_r_cmd(reg$cmd, prefer, reg = reg)
  if (isTRUE(r_cmd == "no_trans")) {
    return(NULL)
  } else if (is.na(r_cmd)) {
    stop(paste0("The Stata command ", reg$cmd, " is neither implemented for translation nor specified in stata_cmds_without_r_translation()"))
  }

  args = list(regvar=regvar,regxvar=regxvar, cmdpart=cmdpart, reg=reg)
  fun = paste0("regvar_to_formula_",r_cmd)
  res = do.call(fun, args)
  as.formula(res)
}

get_stata_to_r_cmd = function(cmd, prefer = NULL, reg = NULL) {
  restore.point("get_stata_to_r_cmd")
  df = stata_to_r_cmds_df()
  rows = df$stata_cmd == cmd
  if (length(rows)==0) return(NA)
  r_cmds = df$r_cmd[rows]

  if (!is.null(reg) && cmd %in% c("xtreg", "xtivreg")) {
    flags = if (!is.null(reg$flags) && !is.na(reg$flags)) strsplit(reg$flags, ",\\s*")[[1]] else character(0)
    if ("re" %in% flags || "be" %in% flags || "mle" %in% flags || "pa" %in% flags || "fd" %in% flags) {
      if ("plm" %in% r_cmds) return("plm")
      return("plm")
    }
  }

  if (length(rows)==1 | length(prefer)==0) return(r_cmds[1])
  ma = match(r_cmds, prefer)
  best = which.min(ma)
  if (length(best)==0) return(r_cmds[1])
  r_cmds[best[1]]
}


stata_to_r_cmds_df = function(cmd) {
  li = list(
    # If you add commands here make to sure that you update the to_r_fixest.R code
    fixest.. = c("regress","reg", "cgmreg", "ivregress","ivreg","ivreg2","xtivreg", "xtivreg2", "reghdfe","ivreghdfe", "xtreg","areg","ppmlhdfe","logit","xtlogit","probit","xtprobit","dprobit", "newey", "nbreg", "gnbreg", "poisson", "xtpoisson", "clogit"),
    plm.. = c("xtreg", "xtivreg"),
    lm.. = c("regress","reg", "newey", "newey2"),
    ivreg.. = c("ivregress","ivreg","ivreg2", "xtivreg", "xtivreg2"),
    quantreg.. = c("qreg"),
    # Note that parmest does return coefficients not marginal
    # effects for dprobit. So we don't use mfx
    #mfx... = c("dprobit"),
    tobit.. = c("tobit"),
    stcox.. = c("stcox")
  )
  stata_cmd = unlist(li)
  r_cmd = str.left.of(names(stata_cmd),"..")
  data.frame(stata_cmd=stata_cmd,r_cmd=r_cmd,row.names = NULL)
}

add_reg_function_code = function(code_df,add_results=TRUE, add_restore_point=isTRUE(opts$add_restore_point), opts=NULL) {
  restore.point("add_reg_function_code")
  fun_header = paste0("function(dat,...){")
  rp_code = paste0('restore.point("regfun_',sample.int(1e10,1),'")')

  if (add_results) {
    results = paste0("results = list(reg=reg")
    if ("rcmd" %in% code_df$part) {
      results = paste0(results, ",rcmd=rcmd")
    }

    if ("sum" %in% code_df$part) {
      results = paste0(results, ",sum=sum")
    }
    if ("tidy" %in% code_df$part) {
      results = paste0(results, ",ct=ct")
    }
    if ("glance" %in% code_df$part) {
      results = paste0(results, ",glance=glance")
    }
    results = paste0(results,")")
    results_df = tibble(part="results", code=results)
  } else {
    results_df = NULL
  }
  fun_footer = paste0("return(results)\n}")

  if (add_restore_point) {
    rp_code = paste0('restore.point("regfun_',sample.int(1e10,1),'")')
    head_df = tibble(part=c("fun_header","restorepoint"),code = c(fun_header, rp_code))
  } else {
    head_df = tibble(part=c("fun_header"), code = c(fun_header))
  }
  foot_df = tibble(part=c("fun_footer"), code = c(fun_footer))

  bind_rows(
    head_df,
    code_df,
    results_df,
    foot_df
  )
}

add_reg_broom_code = function(code_df, use_summary=any(code_df$part=="sum"), use_conf_int=TRUE) {
  broom_x = if (use_summary) "sum" else "reg"
  tidy_code = paste0('ct = broom::tidy(',broom_x,",conf.int=TRUE)")
  glance_code = paste0('glance = broom::glance(',broom_x,')')

  tidy_df = tibble(
    part = c("tidy","glance"),
    code = c(tidy_code, glance_code)
  )
  bind_rows(code_df, tidy_df)
}

# update: don't quote one-sided formulas
quote_arg = function(arg) {
  quote = suppressWarnings(!has.substr(arg,"(") & !startsWith(trimws(arg), "~") & is.na(as.numeric(arg)) & is.na(as.logical(arg)))
  ifelse(quote, paste0('"', arg,'"'), arg)
}

# quote_arg = function(arg) {
#   quote = suppressWarnings(!has.substr(arg,"(") & is.na(as.numeric(arg)) & is.na(as.logical(arg)))
#   ifelse(quote, paste0('"', arg,'"'), arg)
# }

replace_regvar_prefix_sep = function(regvar, from="@", to=".") {
  regvar$cterm = gsub(from, to, regvar$cterm, fixed=TRUE)
  regvar$ia_cterm = gsub(from, to, regvar$ia_cterm, fixed=TRUE)
  regvar
}


extract_clustervar_from_se_args = function(se_args) {
  args = regdb_parse_se_args(se_args)
  clustervar = args[startsWith(names(args),"cluster")]
  clustervar
}

#' Generate R code to emulate Stata's listwise deletion (e(sample))
#'
#' Stata drops missing values for all variables in the original varlist
#' *before* omitting collinear terms. Since our R translation explicitly
#' drops collinear terms from the formula, we need to manually drop NAs
#' across all original variables to match Stata's e(sample).
r_listwise_deletion_code = function(regvar) {
  all_base_cterms = unique(regvar$cterm)
  all_base_cterms = setdiff(all_base_cterms, c("(Intercept)", ""))

  if (length(all_base_cterms) == 0) return("")

  paste0(
    "cc_cols = c(", paste0('"', all_base_cterms, '"', collapse=", "), ")\n",
    "dat = stata_drop_missing(dat, cc_cols)"
  )
}



#' Generate R code to convert Date/Datetime variables to numeric
#'
#' Fixest and other packages complain if Date variables are used directly as numeric variables.
#' We also emit a repbox_problem since effect sizes of Dates are hard to interpret.
r_date_to_numeric_code = function(regvar, runid = NULL) {
  restore.point("r_date_to_numeric_code")
  if (!"var_org_type" %in% colnames(regvar)) return("")

  rows = which(regvar$var_org_type  %in% c("Date", "POSIXct", "POSIXt", "difftime") & regvar$var_reg_type == "numeric")

  if (length(rows)==0) return("")


  date_vars = unique(regvar$cterm[rows])
  date_vars = setdiff(date_vars, c("(Intercept)", "", NA))

  if (!is.null(runid)) {
    msg = paste0("Regression uses Date/Datetime variables as numeric: ", paste(date_vars, collapse=", "), ". Effect sizes might be hard to interpret.")
    repboxUtils::repbox_problem(msg, type = "date_as_numeric", runid = runid, fail_action = "msg")
  }

  paste0(
    'dat = stata_datetime_cols_to_numeric(dat, c(', paste0('"', date_vars, '"', collapse=", "), '))'
  )
}
stata_datetime_cols_to_numeric = function(data, cols = NULL) {
  stata_date_offset_days = as.numeric(as.Date("1970-01-01") - as.Date("1960-01-01"))
  stata_datetime_offset_ms = stata_date_offset_days * 24 * 60 * 60 * 1000

  if (is.null(cols)) {
    is_dt = vapply(data, function(v) {
      inherits(v, "Date") || inherits(v, "POSIXct")
    }, logical(1))
    cols = names(data)[is_dt]
  }

  if (length(cols) == 0) return(data)

  for (col in cols) {
    v = data[[col]]

    if (inherits(v, "Date")) {
      data[[col]] = as.numeric(v) + stata_date_offset_days
    } else if (inherits(v, "POSIXct")) {
      data[[col]] = as.numeric(v) * 1000 + stata_datetime_offset_ms
    } else {
      warning(sprintf("Column '%s' is not Date or POSIXct; leaving unchanged.", col))
    }
  }

  data
}

