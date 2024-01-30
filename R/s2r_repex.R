# Helper functions for investigation

example = function() {
  library(repboxReg)
  library(regtranslate)
  project.dir = "~/repbox/projects_reg/testsupp"
  project.dir = "/home/rstudio/repbox/projects_reg/jpe_125_1_9"

  reg_core = load_parcel(project.dir,"base","reg_core")

  core = load_parcel(project.dir,"s2r","s2r_core")

  regcheck = core$regcheck %>% left_join(reg_core$reg %>% select(cmd,step),by="step") %>% arrange(desc(deviation))
  head(regcheck)
  s2r_repex(project.dir, step=10)
  navigate_to_repex(project.dir)
}

#' Generates a folder with a reproducible example that shall help
#' detect why an R translation yields different results than the Stata version.
s2r_repex = function(project.dir, step,
                     repex.dir = file.path(project.dir,"metareg","s2r","repex", step),
                     parcels=load_parcels(project.dir, c("base","s2r"),just_glob = c("detail","core","regcoef", "source","regvar","regxvar"), ignore_glob = "org_"),
                     dap=get.project.dap(project.dir, make.if.missing=FALSE)
) {
  restore.point("s2r_reprex")

  if (!dir.exists(repex.dir)) dir.create(repex.dir,recursive = TRUE)

  sparcels = filter_parcels_by_step(parcels, step)

  scode = stata_code_reg_data_preparation(project.dir,step, dap,add_reg_code = TRUE)


  rcode_prep = r_code_reg_data_preparation(project.dir,step, dap)
  reg_df = parcels$s2r_details$reg
  rreg = reg_df[reg_df$step==step,]
  rcode_reg = rreg$cmdline

  # Remove function stuff
  rcode_reg = sep.lines(rcode_reg)
  rcode_reg = rcode_reg[-c(1:2, length(rcode_reg)-(0:3))]
  rcode_reg = merge.lines(rcode_reg)
  #cat(rcode_reg)


  rcode = paste0(
    "# R code to replicate analysis in step ", step,"\n",
    'setwd("',repex.dir,'")',
    rcode_prep,
    '
sp = readRDS("step_parcels.Rds")
regxvar = sp$regxvar$regxvar
regvar = sp$regvar$regvar

dat = create_cterm_cols(dat, unique(regvar$cterm))
dat = make_regxvar_cols(dat, regxvar)
\n',
    rcode_reg,
    "\n\nct")

  # Add further R analysis code

  saveRDS(sparcels,file.path(repex.dir,"step_parcels.Rds"))

  rcode = paste0(rcode, '
regvar = sp$regvar$regvar
co = ct_to_regcoef(ct, "r", regvar=regvar)

r_co = sp$s2r_details$regcoef
s_co = sp$regcoef$regcoef

coef_diff = nice_coef_diff(r_co, s_co, labs=c("r","s")) %>%
  left_join(select(co, coef, cterm), by="cterm")
coef_diff
co

navigate_to_repex("',project.dir,'")
')
  si = sparcels$regsource$regsource
  source.file = file.path(project.dir,"mod",si$script_path)
  source_code = paste0('
# Source: Line ', si$code_line_start, ' in ', source.file,'
# rstudioapi::navigateToFile("', source.file,'")
')
  rcode = paste0(rcode, source_code)

  # Write files
  if (!dir.exists(repex.dir)) dir.create(repex.dir, recursive = TRUE)

  writeLines(rcode, file.path(repex.dir,paste0("rcode.R")))
  writeLines(scode, file.path(repex.dir,"stata_code.do"))


  rp_code = make_repex_project_code(project.dir, repex.dir)
  rp_code = paste0(rp_code, "\n  mr = mr_s2r_study(project.dir)")
  writeLines(rp_code, file.path(repex.dir,"repex_project.R"))


  cat("\nReproducible example written to ",repex.dir,"\n")
  invisible()
}

