
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


regdb_se_to_sandwich = function(se_category, se_type, se_args,reg_info=NULL) {

  restore.point("regdb_se_to_sandwich")

  se_category = tolower(trimws(as.character(se_category[1])))
  se_type = tolower(trimws(as.character(se_type[1])))

  if (
    length(se_args) == 0 ||
    is.na(se_args[1]) ||
    !nzchar(se_args[1])
  ) {
    se_args = ""
    args = character(0)
  } else {
    se_args = as.character(se_args[1])

    if (stri_detect_fixed(se_args, "=")) {
      args = repdb_parse_se_args(se_args)
    } else {
      args = setNames(se_args, "raw")
    }
  }

  get_arg = function(name, default=NA_character_) {
    if (!name %in% names(args)) {
      return(default)
    }

    val = as.character(args[name][1])

    if (is.na(val) || !nzchar(val)) {
      return(default)
    }

    val
  }

  is_true_arg = function(name, default=FALSE) {
    val = get_arg(name)

    if (is.na(val)) {
      return(default)
    }

    tolower(val) %in% c("true", "t", "yes", "y", "1")
  }

  reg_field = function(name) {
    if (
      is.null(reg_info) ||
      !name %in% names(reg_info) ||
      length(reg_info[[name]]) == 0
    ) {
      return("")
    }

    val = as.character(reg_info[[name]][1])

    if (is.na(val)) {
      return("")
    }

    trimws(val)
  }

  formula_code = function(vars) {
    vars = as.character(vars)
    vars = vars[!is.na(vars) & nzchar(vars)]

    if (length(vars) == 0) {
      return("")
    }

    vars = paste0("`", vars, "`")

    paste0(
      "~ ",
      stri_flatten(vars, collapse=" + ")
    )
  }

  cluster_vars = function() {
    if (length(args) == 0 || is.null(names(args))) {
      return(character(0))
    }

    arg_names = names(args)
    rows = which(stri_startswith_fixed(arg_names, "cluster"))

    if (length(rows) == 0) {
      return(character(0))
    }

    cluster_names = arg_names[rows]
    cluster_num = suppressWarnings(as.integer(
      stri_extract_first_regex(cluster_names, "[0-9]+$")
    ))

    rows = rows[order(cluster_num, na.last=TRUE)]
    vars = as.character(args[rows])

    vars[!is.na(vars) & nzchar(vars)]
  }

  unsupported_code = function(msg) {
    repbox_problem(
      msg,
      "sandwich_se_not_implemented",
      fail_action="msg"
    )

    paste0(
      "stop(",
      encodeString(msg, quote='"'),
      ")"
    )
  }

  finite_sample_factor = function(code) {
    paste0(
      "(",
      code,
      ") * stats::nobs(reg) / ",
      "(stats::nobs(reg) - sum(!is.na(stats::coef(reg))))"
    )
  }

  small = is_true_arg("small")

  # Conventional, OIM, and other model-based covariance estimators.
  #
  # reg is estimated with vcov = "iid" before this expression is evaluated,
  # so stats::vcov(reg) returns the model-based covariance matrix.
  if (
    se_category == "iid" ||
    se_type %in% c(
      "iid", "unadjusted", "conventional", "ols", "oim"
    )
  ) {
    return("stats::vcov(reg)")
  }

  # The OPG covariance is the inverse outer product of the score vectors.
  if (se_type == "opg") {
    return(paste0(
      "sandwich::vcovOPG(reg, adjust = ",
      if (small) "TRUE" else "FALSE",
      ")"
    ))
  }

  clustervar = cluster_vars()
  is_cluster = se_category == "cluster" ||
    se_type %in% c("cluster", "twoway", "multiway")

  # Cluster-robust covariance estimators.
  if (is_cluster) {
    if (length(clustervar) == 0) {
      return(unsupported_code(
        paste0(
          "Cannot construct cluster-robust covariance matrix: ",
          "no clustering variable is stored in se_args."
        )
      ))
    }

    cluster_fml = formula_code(clustervar)

    # sandwich::vcovCL supports HC0-HC3 corrections. HC2 and HC3
    # with a cluster variable correspond to CR2-style and CR3-style
    # cluster leverage corrections.
    if (se_type %in% c("hc0", "hc1", "hc2", "hc3")) {
      hc_type = toupper(se_type)
    } else {
      # HC1 plus cadjust gives the usual observation and cluster
      # finite-sample corrections used by standard Stata cluster VCEs.
      hc_type = "HC1"
    }

    if (se_type %in% c("hc4", "hc5")) {
      return(unsupported_code(
        paste0(
          "sandwich::vcovCL does not implement clustered ",
          toupper(se_type),
          " covariance matrices."
        )
      ))
    }

    if (is_true_arg("hansen")) {
      repbox_problem(
        paste0(
          "The Stata Hansen correction for clustered HC3 is not ",
          "implemented by sandwich::vcovCL. Use the standard HC3 ",
          "cluster correction."
        ),
        "sandwich_hansen_not_implemented",
        fail_action="msg"
      )
    }

    return(paste0(
      "sandwich::vcovCL(",
      "reg, ",
      "cluster = ", cluster_fml, ", ",
      'type = "', hc_type, '", ',
      "cadjust = TRUE",
      ")"
    ))
  }

  # Observation-level heteroskedasticity-consistent covariance estimators.
  if (se_type %in% c(
    "hc0", "hc1", "hc2", "hc3", "hc4", "hc5"
  )) {
    return(paste0(
      "sandwich::vcovHC(",
      "reg, ",
      'type = "', toupper(se_type), '"',
      ")"
    ))
  }

  # A generic robust type does not identify a particular HC correction.
  # sandwich::sandwich() is the unadjusted empirical score sandwich.
  if (
    se_category == "robust" &&
    se_type %in% c("robust", "")
  ) {
    code = "sandwich::sandwich(reg)"

    if (small) {
      code = finite_sample_factor(code)
    }

    return(code)
  }

  # Stata newey uses a Bartlett-kernel Newey-West covariance estimator
  # without prewhitening. NeweyWest(..., adjust=TRUE) applies the
  # regression-style n/(n-k) finite-sample correction.
  if (se_type %in% c("nw", "neweywest", "newey-west")) {
    lag = get_arg("lag")

    if (is.na(lag) || !stri_detect_regex(lag, "^[0-9]+$")) {
      return(unsupported_code(
        paste0(
          "Cannot construct Newey-West covariance matrix: ",
          "a nonnegative integer lag is required in se_args."
        )
      ))
    }

    timevar = reg_field("timevar")
    order_code = ""

    if (nzchar(timevar)) {
      order_code = paste0(
        ", order.by = ",
        formula_code(timevar)
      )
    }

    return(paste0(
      "sandwich::NeweyWest(",
      "reg, ",
      "lag = ", lag, ", ",
      "prewhite = FALSE, ",
      "adjust = TRUE",
      order_code,
      ")"
    ))
  }

  # A generic HAC label does not contain enough information to reproduce
  # a particular Stata kernel and bandwidth. vcovHAC provides a generic
  # HAC estimator, but a stored raw hacspec is reported as an approximation.
  if (se_type == "hac") {
    hacspec = get_arg("hacspec")

    if (!is.na(hacspec)) {
      repbox_problem(
        paste0(
          "The stored HAC specification '", hacspec,
          "' is not translated exactly. Use sandwich::vcovHAC ",
          "with its default HAC weighting."
        ),
        "sandwich_hac_approximation",
        fail_action="msg"
      )
    }

    timevar = reg_field("timevar")
    order_code = ""

    if (nzchar(timevar)) {
      order_code = paste0(
        ", order.by = ",
        formula_code(timevar)
      )
    }

    return(paste0(
      "sandwich::vcovHAC(",
      "reg, ",
      "prewhite = FALSE, ",
      "adjust = TRUE",
      order_code,
      ")"
    ))
  }

  # sandwich::vcovPL with aggregate=TRUE computes Driscoll-Kraay:
  # score contributions are aggregated by time before applying the
  # HAC correction, allowing cross-sectional and serial dependence.
  if (se_type %in% c("dk", "dkraay", "driscoll-kraay")) {
    panelvar = reg_field("panelvar")
    timevar = reg_field("timevar")

    if (!nzchar(panelvar) || !nzchar(timevar)) {
      return(unsupported_code(
        paste0(
          "Cannot construct Driscoll-Kraay covariance matrix: ",
          "panelvar and timevar are required."
        )
      ))
    }

    lag = get_arg("lag")
    lag_code = ""

    if (!is.na(lag)) {
      if (stri_detect_regex(lag, "^[0-9]+$")) {
        lag_code = paste0(", lag = ", lag)
      } else if (lag %in% c("max", "NW1987", "NW1994")) {
        lag_code = paste0(
          ", lag = ",
          encodeString(lag, quote='"')
        )
      } else {
        return(unsupported_code(
          paste0(
            "Unsupported Driscoll-Kraay lag specification: ",
            lag
          )
        ))
      }
    }

    return(paste0(
      "sandwich::vcovPL(",
      "reg, ",
      "cluster = ",
      formula_code(c(panelvar, timevar)),
      lag_code, ", ",
      "aggregate = TRUE, ",
      "adjust = TRUE",
      ")"
    ))
  }

  # Basic case-resampling covariance estimator. Recognized arguments are
  # reps and cluster1, cluster2, etc. Other Stata bootstrap options are
  # not reproduced here.
  if (se_type == "bootstrap") {
    reps = get_arg("reps")
    cluster_code = ""
    reps_code = ""

    if (length(clustervar) > 0) {
      cluster_code = paste0(
        ", cluster = ",
        formula_code(clustervar)
      )
    }

    if (!is.na(reps) && stri_detect_regex(reps, "^[0-9]+$")) {
      reps_code = paste0(", R = ", reps)
    }

    return(paste0(
      "sandwich::vcovBS(",
      "reg",
      cluster_code,
      reps_code,
      ")"
    ))
  }

  # The sandwich jackknife drops one observation, or one cluster when a
  # clustering variable is supplied, and recomputes the estimates.
  if (se_type == "jackknife") {
    cluster_code = ""

    if (length(clustervar) > 0) {
      cluster_code = paste0(
        ", cluster = ",
        formula_code(clustervar)
      )
    }

    return(paste0(
      "sandwich::vcovJK(",
      "reg",
      cluster_code,
      ")"
    ))
  }

  unsupported_code(paste0(
    "No sandwich covariance translation is implemented for ",
    "se_category = '", se_category,
    "' and se_type = '", se_type, "'."
  ))
}
