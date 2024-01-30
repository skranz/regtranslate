s2r_example = function() {
  library(regtranslate)
  library(repboxReg)
  #project.dir = file.path("C:/libraries/repbox/projects_reg",project)

  project = "testsupp"

  project.dir = file.path("~/repbox/projects_reg/",project)

  mr = mr_s2r_study(project.dir)

  parcels = mr$saved_parcels

  rstudioapi::filesPaneNavigate(paste0(project.dir))

  dat = readRDS("C:/libraries/repbox/projects_reg/testsupp/metareg/s2r/s2r_agg.Rds")$regcoef
}


mr_s2r_study = function(project.dir, variant = "s2r") {
  library(repboxReg)
  restore.point("mr_s2r_study")
  project = basename(project.dir)

  opts = mr_opts(stop.on.error = TRUE,create.regxvar.cols = TRUE)

  mr = mr_init_study(project.dir,metaid="s2r",step_run_fun = s2r_step_run_fun, study_agg_fun = s2r_study_agg_fun, opts = opts, version = 0)
  mr$variant = variant

  # If there is no used regression
  # just save the default stuff for an empty study and return
  if (mr_uses_no_reg(mr)) {
    return(mr_finish(mr))
  }

  # Load tables from regdb beyond "reg" that will be used
  mr = mr_load_parcels(mr, c("reg_core", "cmdpart","regcoef","regvar","regxvar"))

  # Run the study
  mr = mr_run(mr)

  mr_finish(mr)
}


# In R we will perform a regression using fixest (lm might take too long)
s2r_step_run_fun =  function(mr, step, reg, dat, org.dat, infeasible_filter, ...) {
  restore.point("s2r_step_run_fun")

  regxvar = mr_get_base_table(mr, "regxvar", steps=step)
  regvar = mr_get_base_table(mr, "regvar", steps=step)
  cmdpart = mr_get_base_table(mr, "cmdpart", steps=step)

  # All columns relevant for the regression
  reg$infeasible_filter = infeasible_filter

  #if (step==4) stop()
  code_df = reg_stata_to_r_code(reg, regvar, regxvar, cmdpart)

  code_df = code_df %>% add_reg_function_code(add_restore_point = TRUE)

  code = paste0(code_df$code, collapse="\n")
  #cat(code)
  reg$cmdline = paste0("reg_fun = ", code,"\nresults=reg_fun(dat)")
  reg_fun = eval(parse(text=code))
  #debug(reg_fun)
  #dat = make_regxvar_cols(dat, regxvar)
  results = reg_fun(dat)

  ct = results$ct
  ct$cterm = cterm_of_r_coefs(ct$term,regvar, dot_to_at = TRUE)
  co_df = ct_to_regcoef(ct, lang="r",variant = mr$variant,artid = mr$artid)
  co_df$step = step

  reg$rcmd = results$rcmd
  reg$regcoef = list(co_df)

  reg$glance = results["glance"]

  mr = mr_set_step_result(mr, step, reg=reg)

  #ct$msg = msg
  #outfile = paste0(mr$step.dir, "/rreg_", step, ".Rds")
  #saveRDS(reg, outfile)
  mr
}



s2r_study_agg_fun = function(mr, ...) {
  restore.point("s2r_study_agg_fun")
  project = basename(mr$project.dir)

  # R results from metareg run
  r_regs = mr_get_steps_result(mr, "reg")

  #r_regs = mr_agg_df_rds(mr, "rreg_*.Rds", file_col="file")
  #r_regs$step = as.integer(str.between(r_regs$file, "_", "."))

  r_co = bind_rows(r_regs$regcoef)

  stata_co = mr_get_base_table(mr, "regcoef", steps = r_regs$step)

  diff_r_tab = coef_diff_table(r_co,stata_co)
  diff_r_sum = coef_diff_summary(diff_r_tab, c("all","coef"))

  inf_filter_steps = r_regs$step[r_regs$infeasible_filter]
  if (length(inf_filter_steps)>0) {
    diff_r_sum$problem[diff_r_sum$step %in% inf_filter_steps] = "stata_filter_not_translated"
  }

  glance_df = bind_rows_with_parent_fields(r_regs, "glance",c("step"))




  res = regdb_stats_to_regscalar_regstring(glance_df, variant = mr$variant, artid=mr$artid)
  regscalar = res$regscalar; regstring = res$regstring

  reg_stats = regdb_glance_to_reg_stats(glance_df)
  r_regs=left_join_overwrite(r_regs, reg_stats, by="step")
  r_regs$variant = mr$variant
  #r_regs$cmdline = ""
  r_regs$cmd = r_regs$rcmd
  r_regs$lang = "r"


  s_regs = mr_get_base_table(mr, "reg")
  regcheck = mr_get_regcheck_after_run(mr,tolerable_deviation = 1e-4, variant=mr$variant)
  regcheck = left_join_overwrite(regcheck, select(s_regs,step, cmd, se_category, se_type), by=c("step"))
  # For some commands we allow larger tolerabl deviation since
  # se are not calculated exactly the same way in R and Stata

  #rows = which(regcheck$cmd %in% c("logit","xtlogit","probit","xtprobit","dprobit"))

  #regcheck$tolerable_deviation[rows] = 0.01


  diff = diff_r_sum %>% filter(compare_what=="all")
  regcheck = left_join_overwrite(regcheck, transmute(diff,step=step,
    # Geometric mean of rel_diff and minimum of rel_diff and abs_diff
    deviation= exp(0.5+log(max_rel_diff)+0.5*log(max_deviation))),
    by="step")

  # For probit & co standard errors will not match
  # we will only use the coefs to compare
  coef_diff = diff_r_sum %>% filter(compare_what=="coef")
  regcheck = left_join_overwrite(regcheck,transmute(coef_diff,step=step,
     coef_deviation= exp(0.5+log(max_rel_diff)+0.5*log(max_deviation))), by="step")

  rows = rows = which(regcheck$cmd %in% c("logit","xtlogit","probit","xtprobit","dprobit") & regcheck$se_category == "iid")
  regcheck$deviation[rows] = regcheck$coef_deviation[rows]
  regcheck$comment[rows] = "deviation ignores se"

  mr = mr_set_header(mr, regcheck)

  s2r_details = list(
    reg = r_regs,
    regscalar = regscalar,
    regstring = regstring,
    regcoef = r_co,
    coeff_diff = diff_r_sum
  )

  s2r_core = list(
    header = mr_get_header(mr),
    regcheck = regcheck
  )

  mr = mr_save_parcels(mr, parcels = list(s2r_core = s2r_core, s2r_details=s2r_details))

  mr
}
