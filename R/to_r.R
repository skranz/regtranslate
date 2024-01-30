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


code_options = function(prefer_sandwich=FALSE, prefer_summary=FALSE, add_broom=TRUE, add_function=FALSE, add_restorepoint=FALSE) {
  list(prefer_sandwich=prefer_sandwich, prefer_summary=prefer_summary, add_broom=add_broom, add_function=add_function, add_restorepoint=add_restorepoint)
}

reg_stata_to_r_code = function(reg, regvar, regxvar, cmdpart, prefer="fixest", opts=code_options()) {
  restore.point("reg_stata_to_r_code")

  r_cmd = get_stata_to_r_cmd(reg$cmd, prefer)
  if (isTRUE(r_cmd == "no_trans")) {
    return(NULL)
  } else if (is.na(r_cmd)) {
    stop(paste0("The Stata command ", reg$cmd, " is neither implemented for translation nor specified in stata_cmds_without_r_translation()"))
  }

  args = list(reg=reg, regvar=regvar,regxvar, cmdpart=cmdpart, opts=opts)
  fun = paste0("stata_to_r_code_",r_cmd)
  res = do.call(fun, args)
  res
}

reg_stata_to_r_formula = function(reg, regvar, regxvar, cmdpart, prefer="fixest", opts=code_options()) {
  restore.point("reg_stata_to_r_formula")

  r_cmd = get_stata_to_r_cmd(reg$cmd, prefer)
  if (isTRUE(r_cmd == "no_trans")) {
    return(NULL)
  } else if (is.na(r_cmd)) {
    stop(paste0("The Stata command ", reg$cmd, " is neither implemented for translation nor specified in stata_cmds_without_r_translation()"))
  }

  args = list(regvar=regvar,regxvar, cmdpart=cmdpart)
  fun = paste0("regvar_to_formula_",r_cmd)
  res = do.call(fun, args)
  as.formula(res)
}

get_stata_to_r_cmd = function(cmd, prefer = NULL) {
  df = stata_to_r_cmds_df()
  rows = df$stata_cmd == cmd
  if (length(rows)==0) return(NA)
  r_cmds = df$r_cmd[rows]
  if (length(rows)==1 | length(prefer)==0) return(r_cmds[1])
  ma = match(r_cmds, prefer)
  best = which.min(ma)
  if (length(best)==0) return(r_cmds[1])
  r_cmds[best[1]]
}


stata_to_r_cmds_df = function(cmd) {
  li = list(
    # If you add commands here make to sure that you update the to_r_fixest.R code
    fixest.. = c("regress","reg", "ivregress","ivreg","ivreg2","reghdfe","xtreg","areg","ppmlhdfe","logit","xtlogit","probit","xtprobit","dprobit"),
    lm.. = c("regress","reg"),
    ivreg.. = c("ivregress","ivreg","ivreg2"),
    quantreg.. = c("qreg"),
    # Note that parmest does return coefficients not marginal
    # effects for dprobit. So we don't use mfx
    #mfx... = c("dprobit"),
    tobit.. = c("tobit")
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


quote_arg = function(arg) {
  quote = suppressWarnings(!has.substr(arg,"(") & is.na(as.numeric(arg)) & is.na(as.logical(arg)))
  ifelse(quote, paste0('"', arg,'"'), arg)
}

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
