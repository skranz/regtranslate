
# Old code: possible can be replaced sometime using
# cmdpart. But not yet done.
regdb_parse_se_args = function(se_args, as_df=FALSE) {
  restore.point("regdb_parse_se_args")
  #se_args = c("cluster1=i1;cluster2=i2","cluster2=i2")[1]
  if (length(se_args)>1) {
    stop("regdb_se_args_to_list is not yet vectorized.")
  }

  str = strsplit(se_args,";", fixed=TRUE)[[1]]
  eq_pos = stri_locate_first_fixed(str,"=")[,1]
  var = substring(str,1,eq_pos-1)
  val = substring(str,eq_pos+1)
  if (!as_df) {
    names(val) = var
    return(val)
  }
  tibble(arg_name=var, arg_val=val)
}
