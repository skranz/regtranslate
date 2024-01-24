# These functions provide more direct interfaces usable in connection with
# repbox projects

repbox_load_regtranslate_parcels = function(project_dir, parcels = list()) {
  library(repboxDB)
  parcels = repdb_load_parcels(project_dir,c("base_core","base_regvar","base_regxvar","base_cmdpart"), parcels=parcels)

}

mr_reg_stata_to_r_formula = function(mr, step, prefer="fixest", opts=code_options()) {
  repbox_reg_stata_to_r_code(mr$project_dir, step, mr$parcels, prefer=prefer, opts=opts, just_formula = TRUE)
}

mr_reg_stata_to_r_code = function(mr, step, prefer="fixest", opts=code_options()) {
  repbox_reg_stata_to_r_code(mr$project_dir, step, mr$parcels, prefer=prefer, opts=opts)
}

repbox_reg_stata_to_r_code = function(project_dir, steps=NULL, parcels, prefer="fixest", opts=code_options(), just_formula=FALSE) {
  reg = parcels$base_core$reg
  regvar = parcels$base_regvar$regvar
  regxvar = parcels$base_regxvar$regxvar
  cmdpart = parcels$base_cmdpart$cmdpart

  if (is.null(steps)) steps = reg$step


  reg = reg %>% filter(step %in% steps)
  regvar = regvar %>% filter(step %in% steps)
  regxvar = regxvar %>% filter(step %in% steps)
  cmdpart = regxvar %>% filter(step %in% steps)

  if (just_formula) {
    return(reg_stata_to_r_formula(reg, regvar, regxvar, cmdpart, prefer=prefer, opts=opts))
  }
  reg_stata_to_r_code(reg, regvar, regxvar, cmdpart, prefer=prefer, opts=opts)

}


