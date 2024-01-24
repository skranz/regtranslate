regdb_se_to_sandwich = function(se_category, se_type, se_args_str="", what = c("code","list")[1], regvar = "reg") {
  restore.point("regdb_se_to_sandwich")
  if (se_type == "nw") {
    fun = "NeweyWest"
    args = get_se_args(se_args_str)
  } else if (se_category == "robust") {
    fun = "vcovHC"
    args = list(type = toupper(se_type))
  } else if (se_category == "cluster") {
    fun = "vcovCL"
    arg_li = get_se_args(se_args_str)
    args = list(cluster = unlist(arg_li[startsWith(names(arg_li("cluster")))]))
  } else if (se_category == "iid") {
    fun = "vcovHC"
    args = list(type="const")
  } else {
    stop("This case is not yet implemented in se_type_to_sandwich")
  }
  if (what == "list") {
    return(list(fun=fun, args=args))
  }

  code = paste0("sandwich::", fun, "(",regvar,",", args_to_str(args),")")
  return(code)
}


example = function() {
  li = se_args_str_to_li("method=new;lag=1;prewhite=TRUE;adjust=FALSE")
  li
  arg_li_to_str(li)
}

args_to_str = function(li) {
  if (length(li)==0) return("")
  quote = ifelse(sapply(li, is.character),'"','')

  paste0(names(li),"=",quote,li,quote, collapse=", " )
}

get_se_args = function(se_args_str, ...) {
  defaults = list(...)
  li = se_args_str_to_li(se_args_str)
  def_cols = setdif(names(default), names(li))
  if (length(def_cols)>0) {
    li[def_cols] = defaults[def_cols]
  }
  li
}

se_args_str_to_li = function(se_args_str) {
  if (se_args_str == "") return(NULL)
  str = strsplit(se_args_str,";")[[1]]
  eq_pos = stringi::stri_locate_first_fixed(str, "=")[,1]
  names = substring(str,1, eq_pos-1)
  val = substring(str,eq_pos+1)

  log_rows = which(val %in% c("TRUE","FALSE"))
  num_vals = suppressWarnings(as.numeric(val))
  num_rows = setdiff(which(!is.na(num_vals)), log_rows)

  li = as.list(val)
  if (length(log_rows)>0) {
    li[log_rows] = as.list(as.logical(val[log_rows]))
  }
  if (length(num_rows)>0) {
    li[num_rows] = as.list(as.numeric(val[num_rows]))
  }
  names(li) = names
  li
}
