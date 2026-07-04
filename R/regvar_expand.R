example = function() {
  library(repboxReg)
  project.dir = "/home/rstudio/repbox/projects_reg/aer_105_5_55"
  step = 5

  dat=haven::read_dta("/home/rstudio/repbox/projects_reg/aer_105_5_55/metareg/dap/stata/cache/step_1.dta")

  dap = get.project.dap(project.dir)
  mr = dap
  internal_reg = mr_get_reg_info(mr, step,dat)
  org_dat = dat
  dat = mr_adapt_data_for_reg(project.dir, step, internal_reg, dat)

  regvar = load_parcel(project.dir, "base","regvar")$regvar %>% filter(step==5)
  regcoef = load_parcel(project.dir, "base","regcoef")$regcoef %>% filter(step==5)

  res = expand_reg_dat_cols(regvar, dat)
  erv = specify_used_reg_vars(res$eregvar, regcoef)
}

specify_used_reg_vars = function(eregvar, regcoef) {
  restore.point("specify_used_reg_vars")
  eregvar$has_coef = eregvar$cterm %in% regcoef$cterm

  if (any(!regcoef$cterm %in% c("(Intercept)",eregvar$cterm))) {
    restore.point("not all regcoef cterm")
    extra = setdiff(regcoef$cterm, c("(Intercept)",eregvar$cterm))
    stop("There are regcof cterm that are not part of the expanded regvars. Maybe the order of interaction terms differs. Please check and correct the code.")
  }
  eregvar
}




# Expands interaction effects and factors. Similar to model matrix but uses regvar information
expand_reg_dat_cols = function(regvar, dat) {
  restore.point("expand_reg_dat_cols")
  regvar = regvar[regvar$role %in% c("exo","endo","instr"),]

  rows = regvar$var_reg_type == "factor"
  factor_vars = unique(regvar$cterm[rows])

  factor_levels = lapply(factor_vars, function(var) {
    unique(dat[[var]]) %>% as.character()
  })
  names(factor_levels) = factor_vars

  ia_cterms = unique(regvar$ia_cterm)


  res_li = lapply(ia_cterms, function(ia_term) {
    rows = which(regvar$ia_cterm == ia_term)
    if (length(rows)==1) {
      if (!regvar$var_reg_type[[rows]]=="factor") return(NULL)
      cterm = regvar$cterm[rows]
      res = make_factor_level_vars(cterm,dat, level_li = factor_levels)
    } else if (length(rows)==2) {
      res = make_ia2_vars(regvar[rows,], dat,  level_li = factor_levels)
    } else if (length(rows)==3) {
      res = make_ia3_vars(regvar[rows,], dat,  level_li = factor_levels)
    } else {
      stop("Cannot yet deal with tripple interactions or more.")
    }
    res
  })

  ia_subterms = lapply(seq_along(ia_cterms), function(i) {
    ia_cterm = ia_cterms[i]
    union(regvar$cterm[regvar$ia_cterm==ia_cterm & regvar$var_reg_type!="factor"], names(res_li[[i]]))
  })
  eregvar = tibble(ia_cterm=ia_cterms, cterm = ia_subterms) %>%
    unnest(cterm) %>%
    left_join(regvar %>% select(ia_cterm, role), by="ia_cterm") %>%
    unique()

  new_dat = bind_cols(dat, bind_cols(res_li))
  dupl = duplicated(colnames(new_dat))
  new_dat = new_dat[,!dupl]
  new_dat

  list(eregvar=eregvar, dat=new_dat)
}

make_ia2_vars = function(rv, dat, level_li) {
  restore.point("make_ia2_vars")

  factor_rows = which(rv$var_reg_type == "factor")
  if (length(factor_rows)>0) {
    fdat = lapply(rv$cterm[factor_rows],make_factor_level_vars, dat=dat, level_li=level_li) %>%
      bind_cols()
  } else {
    fdat = NULL
  }

  cterm1 = rv$cterm[1]
  cterm2 = rv$cterm[2]

  is_factor1 = rv$var_reg_type[1]=="factor"
  is_factor2 = rv$var_reg_type[2]=="factor"

  if (is_factor1) {
    vars1 = paste0(cterm1,"=", level_li[[cterm1]])
  } else {
    vars1 = cterm1
  }

  if (is_factor2) {
    vars2 = paste0(cterm2,"=", level_li[[cterm2]])
  } else {
    vars2 = cterm2
  }

  grid = expand.grid(var1=vars1, var2=vars2,stringsAsFactors = FALSE) %>%
    mutate(var12 = sort2_chr(var1, var2, sep="#"))

  dat12_li = lapply(seq_len(nrow(grid)), function(i) {
    if (is_factor1) {
      val1 = fdat[[grid$var1[i]]]
    } else {
      val1 = dat[[grid$var1[i]]]
    }
    if (is_factor2) {
      val2 = fdat[[grid$var2[i]]]
    } else {
      val2 = dat[[grid$var2[i]]]
    }
    val1*val2
  })
  names(dat12_li) = grid$var12
  dat12 = as_tibble(dat12_li)

  if (is.null(rv$add_main_effects) || isTRUE(rv$add_main_effects[1])) {
    return(bind_cols(fdat, dat12))
  } else {
    return(dat12)
  }
}

# FILE: regtranslate/regvar_expand.R
make_ia3_vars = function(rv, dat, level_li) {
  restore.point("make_ia3_vars")

  factor_rows = which(rv$var_reg_type == "factor")
  if (length(factor_rows)>0) {
    fdat = lapply(rv$cterm[factor_rows], make_factor_level_vars, dat=dat, level_li=level_li) %>%
      bind_cols()
  } else {
    fdat = NULL
  }

  is_f1 = rv$var_reg_type[1]=="factor"
  is_f2 = rv$var_reg_type[2]=="factor"
  is_f3 = rv$var_reg_type[3]=="factor"

  vars1 = if (is_f1) paste0(rv$cterm[1],"=", level_li[[rv$cterm[1]]]) else rv$cterm[1]
  vars2 = if (is_f2) paste0(rv$cterm[2],"=", level_li[[rv$cterm[2]]]) else rv$cterm[2]
  vars3 = if (is_f3) paste0(rv$cterm[3],"=", level_li[[rv$cterm[3]]]) else rv$cterm[3]

  # 2-way grids
  g12 = expand.grid(a=vars1, b=vars2, stringsAsFactors=FALSE) %>% mutate(res = sort2_chr(a, b, sep="#"))
  g13 = expand.grid(a=vars1, b=vars3, stringsAsFactors=FALSE) %>% mutate(res = sort2_chr(a, b, sep="#"))
  g23 = expand.grid(a=vars2, b=vars3, stringsAsFactors=FALSE) %>% mutate(res = sort2_chr(a, b, sep="#"))

  # 3-way grid
  g123 = expand.grid(a=vars1, b=vars2, c=vars3, stringsAsFactors=FALSE) %>%
    mutate(res = split_and_sort(paste0(a, "#", b, "#", c), split="#", k=3L))

  # Compute values
  get_val = function(is_f, grid_col, i) if (is_f) fdat[[grid_col[i]]] else dat[[grid_col[i]]]

  dat12 = as_tibble(setNames(lapply(seq_len(nrow(g12)), function(i) get_val(is_f1, g12$a, i) * get_val(is_f2, g12$b, i)), g12$res))
  dat13 = as_tibble(setNames(lapply(seq_len(nrow(g13)), function(i) get_val(is_f1, g13$a, i) * get_val(is_f3, g13$b, i)), g13$res))
  dat23 = as_tibble(setNames(lapply(seq_len(nrow(g23)), function(i) get_val(is_f2, g23$a, i) * get_val(is_f3, g23$b, i)), g23$res))
  dat123 = as_tibble(setNames(lapply(seq_len(nrow(g123)), function(i) get_val(is_f1, g123$a, i) * get_val(is_f2, g123$b, i) * get_val(is_f3, g123$c, i)), g123$res))

  if (is.null(rv$add_main_effects) || isTRUE(rv$add_main_effects[1])) {
    return(bind_cols(fdat, dat12, dat13, dat23, dat123))
  } else {
    return(dat123)
  }
}

make_factor_level_vars = function(var,dat, levels = level_li[[var]],level_li) {
  restore.point("make_factor_level_vars")
  cterms = paste0(var, "=", levels)
  vals = lapply(seq_along(levels), function(i) {
    1L*(dat[[var ]]==levels[i])
  })
  names(vals) = cterms
  as_tibble(vals)
  #list(info=tibble(cterm=cterm), dat=vals)
}
