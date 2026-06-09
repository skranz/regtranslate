
# Replace stata_to_r_code_fixest and fixest_vcov_code_from_regdb
# Replace stata_to_r_code_fixest and fixest_vcov_code_from_regdb
# Replace stata_to_r_code_fixest and fixest_vcov_code_from_regdb
stata_to_r_code_fixest = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_fixest")

  org_depvars = regvar$cterm[regvar$role=="dep"]
  mod_depvars = replace_cterm_special_symbols(org_depvars)

  formula = regvar_to_formula_fixest(regvar, regxvar, cmdpart, reg = reg)

  vcov_type = fixest_vcov_type_from_regdb(reg$se_type, reg$se_args)
  ssc_expr = fixest_ssc_code_from_reg(reg, vcov_type = vcov_type)
  use_ssc = !is.null(ssc_expr)

  use_sandwich = (vcov_type == "sandwich") | opts$prefer_sandwich
  use_summary = use_sandwich | opts$prefer_summary

  if (use_sandwich) {
    reg_vcov = "iid"
    vcov = regdb_se_to_sandwich(reg$se_category, reg$se_type, reg$se_args)
  } else {
    reg_vcov = fixest_vcov_code_from_regdb(reg$se_type, reg$se_args, vcov_type, quote=FALSE, reg=reg)
    if (use_summary) {
      vcov = reg_vcov
    }
  }

  command = "feols"
  arg_str = NULL
  if (reg$cmd %in% c("ppmlhdfe", "poisson", "xtpoisson")) {
    command = "fepois"
  } else if (reg$cmd %in% c("nbreg", "gnbreg")) {
    command = "fenegbin"
  } else if (reg$cmd %in% c("logit","xtlogit", "clogit")) {
    command = "feglm"
    arg_str = "family=binomial()"
  } else if (reg$cmd %in% c("probit","xtprobit","dprobit")) {
    command = "feglm"
    arg_str = 'family=binomial(link = "probit")'
  }

  arg_str = c(
    paste0("fml = formula"),
    paste0("data = dat"),
    paste0("vcov = reg_vcov"),
    arg_str
  )

  # Pass ssc to fixest natively when relevant.
  if (use_ssc) {
    arg_str = c(arg_str, "ssc = ssc")
  }

  library_code = "library(fixest)"
  rcmd_code = paste0('rcmd = "',command,'"')
  if (all(org_depvars==mod_depvars)) {
    data_code = ""
  } else {
    data_code = paste0(
      'dat[["', mod_depvars,'"]] = dat[["', org_depvars,'"]]',
      collapse="\n"
    )
  }

  # Apply explicit listwise deletion to emulate Stata's e(sample)
  lw_code = r_listwise_deletion_code(regvar)
  if (nzchar(lw_code)) {
    data_code = if (nzchar(data_code)) paste0(data_code, "\n", lw_code) else lw_code
  }

  is_binary = reg$cmd %in% c("logit", "xtlogit", "probit", "xtprobit", "dprobit", "clogit", "logistic", "exlogistic", "blogit", "glogit", "binreg")

  if (is_binary && isTRUE(opts$drop_perfect_predictors)) {
    # Check all possible expanded predictors before filtering omitted formulas
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

  # Apply dynamic weights via centralized helper
  wt = r_weight_code(reg, template = "~ `%s`")
  if (nzchar(wt$data_code)) {
    data_code = if (nzchar(data_code)) paste0(data_code, "\n", wt$data_code) else wt$data_code
  }
  if (nzchar(wt$weight_arg)) {
    arg_str = c(arg_str, wt$weight_arg)
  }

  ssc_code = if (use_ssc) paste0("ssc = ", ssc_expr) else NULL
  formula_code = paste0("formula = ", formula)
  reg_vcov_code = paste0("reg_vcov = ", quote_arg(reg_vcov))
  reg_code = paste0("reg = ", command, "(", paste0(arg_str, collapse=","), ")")

  code_df = tibble(
    part = c("library", "rcmd", "data", "formula", if (use_ssc) "ssc", "reg_vcov", "reg"),
    code = c(library_code, rcmd_code, data_code, formula_code, if (use_ssc) ssc_code, reg_vcov_code, reg_code)
  )

  if (use_summary) {
    sum_vcov_code = paste0("sum_vcov = ", quote_arg(vcov))
    sum_code = "sum = summary(reg, vcov = sum_vcov)"
    code_df = bind_rows(
      code_df,
      tibble(part = c("sum_vcov","sum"), code = c(sum_vcov_code, sum_code))
    )
  }
  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=use_summary, use_conf_int=TRUE)
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}



fixest_vcov_code_from_regdb = function(se_type, se_args, vcov_type=fixest_vcov_type_from_regdb(se_type,se_args), quote=TRUE, reg=NULL) {
  restore.point("fixest_vcov_code_from_regdb")

  if (vcov_type %in% c("cluster","twoway")) {
    clustervar = extract_clustervar_from_se_args(se_args)
    # Return as a formula (~ var1 + var2) natively supported by fixest
    code = paste0("~ ", paste0("`", clustervar, "`", collapse = " + "))
    return(code)
  }
  if (vcov_type %in% c("DK","NW")) {
    lag = NA_character_
    if (!is.na(se_args) && nzchar(se_args)) {
      args = regdb_parse_se_args(se_args)
      if ("lag" %in% names(args)) lag = args["lag"]
    }

    timevar = if (!is.null(reg) && !is.na(reg$timevar[1]) && nzchar(reg$timevar[1])) reg$timevar[1] else ""
    panelvar = if (!is.null(reg) && !is.na(reg$panelvar[1]) && nzchar(reg$panelvar[1])) reg$panelvar[1] else ""

    p_and_t = ""
    if (panelvar != "" && timevar != "") {
      p_and_t = paste0("`", panelvar, "` + `", timevar, "`")
    } else if (timevar != "") {
      p_and_t = paste0("`", timevar, "`")
    } else if (panelvar != "") {
      p_and_t = paste0("`", panelvar, "`")
    }

    lag_str = ""
    if (!is.na(lag) && lag != "") {
      lag_str = paste0("(", lag, ")")
    }

    if (p_and_t != "") {
      return(paste0(vcov_type, lag_str, " ~ ", p_and_t))
    } else {
      if (lag_str == "") {
        if (quote) return(paste0('"', vcov_type, '"'))
        return(vcov_type)
      } else {
        return(paste0(vcov_type, lag_str))
      }
    }
  }
  if (quote) return(paste0('"',vcov_type,'"'))
  return(vcov_type)
}




fixest_vcov_type_from_regdb = function(se_type, se_args) {
  restore.point("se_type_to_fixest_vcov")
  if (se_type == "hc1") return("hetero")
  if (se_type %in%  c("cluster")) return(se_type)
  if (se_type %in%  c("iid","cluster","twoway", "conley")) return(se_type)
  if (se_type %in% c("nw", "dk")) return(toupper(se_type))
  return("sandwich")
}

# Choose default fixest::ssc() settings for translated Stata commands.
# This centralizes command-specific small sample correction choices.
# Choose default fixest::ssc() settings for translated Stata commands.
# This centralizes command-specific small sample correction choices.
fixest_ssc_code_from_reg = function(reg, vcov_type = fixest_vcov_type_from_regdb(reg$se_type, reg$se_args)) {
  restore.point("fixest_ssc_code_from_reg")

  is_ml = reg$cmd %in% c("logit", "xtlogit", "probit", "xtprobit", "dprobit", "poisson", "xtpoisson", "nbreg", "gnbreg", "clogit")

  if (vcov_type %in% c("cluster", "twoway", "DK", "NW")) {
    if (reg$cmd == "areg") {
      return('fixest::ssc(K.adj = TRUE, K.fixef = "full", G.adj = TRUE)')
    }
    if (is_ml) {
      return("fixest::ssc(K.adj = TRUE, G.adj = TRUE)")
      #return('fixest::ssc(adj = FALSE, cluster.adj = TRUE)')
    }
    return('fixest::ssc()')
  }

  if (is_ml) {
    return('fixest::ssc(K.adj = TRUE, G.adj = TRUE)')
  }

  return('fixest::ssc()')
}



# Ideally this is independent of the original language from
# which regvar was generated. E.g. it would be create
# if we could translate both a stata command and an R command to
# fixest
# Ideally this is independent of the original language from
# which regvar was generated. E.g. it would be create
# if we could translate both a stata command and an R command to
# fixest
# Ideally this is independent of the original language from
# which regvar was generated. E.g. it would be create
# if we could translate both a stata command and an R command to
# fixest
regvar_to_formula_fixest = function(regvar, regxvar, cmdpart, reg = NULL) {
  restore.point("regvar_to_formula_fixest")

  add_main_effects = TRUE

  rv = regvar %>% filter(role == "dep" | absorbed_fe)
  # Update: for variables starting with _I (typically generated by xi)
  # in_regcoef does not always work proper.
  # Example:   artid = "aejmac_3_3_5"; steps = 3
  # So we assume they are part of the formula
  # possibly a problem arises if they are dropped...
  # at some point I need to think on more robust handling
  rxv = regxvar %>% filter(role !="exo" | in_regcoef | startsWith(cterm,"_I"))

  rv = rv %>% mutate(
    prefix = str.left.of(cterm,"@", not.found=rep("", length(cterm))) %>% tolower()
  )

  rxv = rxv %>% mutate(
    prefix = str.left.of(cterm,"@", not.found=rep("", length(cterm))) %>% tolower()
  )

  depvars = rv$cterm[rv$role=="dep"]
  depvars = replace_cterm_special_symbols(depvars)
  form = paste0(paste0("`",depvars,"`", collapse=" + "), " ~ ")

  omit_constant = FALSE
  if (!is.null(reg) && "flags" %in% names(reg)) {
    omit_constant = stringi::stri_detect_fixed(reg$flags, "noconst")
  } else if (!is.null(cmdpart)) {
    omit_constant = any(cmdpart$part=="opt" & startsWith(tolower(cmdpart$content), "nocon"))
  }

  rhs_terms = character()
  if (omit_constant) {
    rhs_terms = c(rhs_terms, "0")
  }

  # Exogeneous x that are no FE
  rows = which(rxv$role == "exo")
  if (sum(rows)>0) {
    rhs_terms = c(rhs_terms, paste0("`",rxv$cterm[rows],"`"))
  } else {
    if (!omit_constant) {
      rhs_terms = c(rhs_terms, "1")
    }
  }

  form = paste0(form, paste0(rhs_terms, collapse= " + "))

  # Exogeneous x as FE
  rows = which(rv$role == "exo" & rv$absorbed_fe)
  if (sum(rows)>0) {
    fe_terms = rv[rows, ] %>%
      group_by(ia_cterm) %>%
      arrange(ia_pos) %>%
      summarize(
        fe_expr = {
          if (first(ia_reg_type) == "factor_numeric" && n() == 2) {
            f_idx = which(var_reg_type %in% c("factor", "fe"))[1]
            n_idx = which(!var_reg_type %in% c("factor", "fe"))[1]
            if (is.na(f_idx)) f_idx = 1
            if (is.na(n_idx)) n_idx = 2
            paste0("`", cterm[f_idx], "`[`", cterm[n_idx], "`]")
          } else {
            paste0("`", cterm, "`", collapse = "^")
          }
        }
      ) %>%
      pull(fe_expr)
    form = paste0(form, " | ", paste0(fe_terms, collapse = " + "))
  }

  # Endogeneous x and instruments (never FE)
  rows = which(rxv$role == "endo")
  if (sum(rows)>0) {
    form = paste0(form, " | ",paste0("`",rxv$cterm[rows],"`", collapse= " + "))
  }
  rows = which(rxv$role == "instr")
  if (sum(rows)>0) {
    form = paste0(form, " ~ ",paste0("`",rxv$cterm[rows],"`", collapse= " + "))
  }
  form
}



# Ideally this is independent of the original language from
# which regvar was generated. E.g. it would be create
# if we could translate both a stata command and an R command to
# fixest
# Ideally this is independent of the original language from
# which regvar was generated. E.g. it would be create
# if we could translate both a stata command and an R command to
# fixest
regvar_to_formula_fixest_noregxvar = function(regvar, regxvar, cmdpart, reg = NULL) {
  restore.point("regvar_to_formula_fixest")

  add_main_effects = TRUE

  rv = regvar
  rv = rv %>% mutate(
    prefix = str.left.of(cterm,"@", not.found=rep("", length(cterm))) %>% tolower(),
    is_ia = ia_num > 1
  )

  # We replace prefix @ with .
  #rv = replace_regvar_prefix_sep(rv, "@",".")

  depvars = rv$cterm[rv$role=="dep"]
  form = paste0(paste0(depvars, collapse=" + "), " ~ ")

  omit_constant = FALSE
  if (!is.null(reg) && "flags" %in% names(reg)) {
    omit_constant = stringi::stri_detect_fixed(reg$flags, "noconst")
  } else if (!is.null(cmdpart)) {
    omit_constant = any(cmdpart$part=="opt" & startsWith(tolower(cmdpart$content), "nocon"))
  }

  rhs_terms = character()
  if (omit_constant) {
    rhs_terms = c(rhs_terms, "0")
  }

  # In stata x variables starting with o. like o.var will be omitted
  rv = rv %>% filter(prefix!="o")

  # TO DO: Specify whether in interaction A*B also A and B
  #        should be included or not.
  if (sum(rv$is_ia) >0 ) {
    ia = rv %>%
      filter(is_ia) %>%
      group_by(is_ia, ia_cterm, role, ia_num, main_pos) %>%
      arrange(desc(var_reg_type=="factor"), desc(var_reg_type=="dummy"), ia_pos) %>%
      summarize(
        absorbed_fe = first(absorbed_fe),
        ia_type = case_when(
          all(var_reg_type=="dummy") ~ "dummies",
          all(var_reg_type=="numeric") ~ "numeric",
          all(var_reg_type %in% c("dummy","factor")) ~ "factor",
          ia_num == 2 & var_reg_type[1] == "dummy" & (var_reg_type[2]=="numeric") ~ "dummy_numeric",
          ia_num == 2 & var_reg_type[1] %in% c("factor", "fe") & var_reg_type[2] == "numeric" ~ "factor_numeric",
          TRUE ~ "unknown"
        )[1],
        fe_expr = case_when(
          ia_type == "factor_numeric" ~ {
            f_idx = which(var_reg_type %in% c("factor", "fe"))[1]
            n_idx = which(!var_reg_type %in% c("factor", "fe"))[1]
            if (is.na(f_idx)) f_idx = 1
            if (is.na(n_idx)) n_idx = 2
            paste0("`", cterm[f_idx], "`[`", cterm[n_idx], "`]")
          },
          TRUE ~ paste0("`", cterm, "`", collapse="^")
        )[1],
        x_expr = paste0(
          ifelse(!var_reg_type %in% c("factor", "fe"), paste0("`", cterm, "`"), paste0("factor(`",cterm,"`)")),
          collapse= if (isTRUE(first(add_main_effects))) "*" else ":"
        )[1]
      ) %>%
      ungroup()
  } else {
    ia = NULL
  }

  no_ia = rv %>%
    filter(!is_ia) %>%
    mutate(
      fe_expr = paste0("`", cterm, "`"),
      x_expr = case_when(
        var_reg_type %in% c("factor", "fe") ~ paste0("factor(`", cterm,"`)"),
        TRUE ~ paste0("`", cterm, "`")
      )
    )

  terms = bind_rows(ia, no_ia) %>%
    arrange(main_pos)

  # Exogeneous x that are no FE
  rows = which(terms$role == "exo" & !terms$absorbed_fe)
  if (sum(rows)>0) {
    rhs_terms = c(rhs_terms, terms$x_expr[rows])
  } else {
    if (!omit_constant) {
      rhs_terms = c(rhs_terms, "1")
    }
  }

  form = paste0(form, paste0(rhs_terms, collapse= " + "))

  # Exogeneous x as FE
  rows = which(terms$role == "exo" & terms$absorbed_fe)
  if (sum(rows)>0) {
    form = paste0(form, " | ",paste0(terms$fe_expr[rows], collapse= " + "))
  }

  # Endogeneous x and instruments (never FE)
  rows = which(terms$role == "endo")
  if (sum(rows)>0) {
    form = paste0(form, " | ",paste0(terms$x_expr[rows], collapse= " + "))
  }
  rows = which(terms$role == "instr")
  if (sum(rows)>0) {
    form = paste0(form, " ~ ",paste0(terms$x_expr[rows], collapse= " + "))
  }
  form
}

stata_to_r_code_fixest_fit.old = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_fixest_fit")

  depvars = regvar$cterm[regvar$role=="dep"]
  if (length(depvars)==1) {
    ycode = paste0('y=dat["', depvars,'"]')
  } else {
    ycode = paste0('y=cbind(', paste0('dat["', depvars,'"]', collapse=","),')')
  }

}

