# Helper functions for investigation

example = function() {
  library(repboxReg)
  project.dir = "~/repbox/projects_reg/aejapp_3_2_2/"

  core = readRDS("~/repbox/projects_reg/aejapp_3_2_2/metareg/s2r/regdb/s2r_core.Rds")
  details = readRDS("~/repbox/projects_reg/aejapp_3_2_2/metareg/s2r/regdb/s2r_details.Rds")

  s2r_reprex(project.dir, step=52)
}

#' Generates a folder with a reproducible example that shall help
#' detect why an R translation yields different results than the Stata version.
s2r_reprex = function(project.dir, step, out.dir = file.path(project.dir,"metareg","s2r","reprex", step), details = load_parcel(project.dir,"s2r","s2r_details"), dap=get.project.dap(project.dir, make.if.missing=FALSE)) {
  restore.point("s2r_reprex")

  dat_file = repboxReg::mr_get_cache_file(project.dir, step)

  s2r_regdb_dir = file.path(project.dir,"metareg","s2r","regdb")

  core = readRDS(file.path(s2r_regdb_dir,"s2r_core.Rds"))
  details = readRDS(file.path(s2r_regdb_dir,"s2r_details.Rds"))

  rreg = details$reg[details$reg$step==step,]

  r_code = rreg$cmdline
}



# Put the following functions back to repboxReg

stata_code_reg_data_preparation = function(project.dir, step, dap=get.project.dap(project.dir, make.if.missing=FALSE)) {
  restore.point("stata_code_step_data_preparation")
  astep = step
  txt = "\n\nset more off\n"
  path = dap$path.df[dap$path.df$astep == astep,]
  # 1. Get data file
  step = path$step[1]
  file = mr_get_cache_file(mr$project.dir, step)
  txt = paste0(txt,'\nuse "', file,'", clear\n')

  # 2. code for all other stepsdata modification steps
  steps = path$step[-1]
  txt = paste0(txt,"\n", paste0(mr$step.df$stata_code[steps], collapse = "\n"))

}

r_code_reg_data_preparation = function(project.dir, step, dap=get.project.dap(project.dir, make.if.missing=FALSE)) {
  restore.point("stata_code_step_data_preparation")
  astep = step
  txt = "\n\nset more off\n"
  path = dap$path.df[dap$path.df$astep == astep,]
  # 1. Get data file
  step = path$step[1]
  file = mr_get_cache_file(mr$project.dir, step)
  txt = paste0(txt,'\nuse "', file,'", clear\n')

  # 2. code for all other stepsdata modification steps
  steps = path$step[-1]
  txt = paste0(txt,"\n", paste0(mr$step.df$stata_code[steps], collapse = "\n"))

}



mr_write_path_stata_code = function(mr, astep=first(mr$path.df$astep), code.file=paste0(mr$project.dir,"/metareg/step_",astep,".do"), add.line.info = !is.null(run.df), run.df=mr$run.df,...) {
  restore.point("mr_write_path_stata_code")

  txt = "\n\nset more off\n"
  path = dap$path.df[dap$path.df$astep == astep,]
  # 1. Get data file
  step = path$step[1]
  file = mr_get_cache_file(mr$project.dir, step)
  txt = paste0(txt,'\nuse "', file,'", clear\n')

  # 2. code for all other stepsdata modification steps
  steps = path$step[-1]
  txt = paste0(txt,"\n", paste0(mr$step.df$stata_code[steps], collapse = "\n"))

  writeLines(txt, code.file)
  invisible(txt)
}


load_parcel = function(project.dir, metaid, parcel) {
  parcel.file=file.path(project.dir,metaid,"regdb", paste0(parcel,".Rds"))
  if (!file.exists(parcel.file)) return(NULL)
  readRDS(parcel.file)
}
