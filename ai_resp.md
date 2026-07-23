Yes, the Stata command `newey` is already supported! It translates to R's `feols` using the `fixest` package (or `lm` if preferred). 

We can easily add support for `newey2` by including it in the translation mapping function `stata_to_r_cmds_df`. Since `newey2` functions similarly to `newey` and `reg` in terms of formula construction and estimation, adding it to the list will automatically route it to `feols` with Newey-West standard errors.

Here is the modification to add `newey2` support:

!MODIFICATION stata_to_r_cmds_df to_r.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r.R"
function_name = "stata_to_r_cmds_df"
description = "Add support for the newey2 command alongside newey."
---
```r
stata_to_r_cmds_df = function(cmd) {
  li = list(
    # If you add commands here make to sure that you update the to_r_fixest.R code
    fixest.. = c("regress","reg", "cgmreg", "ivregress","ivreg","ivreg2","xtivreg", "xtivreg2", "reghdfe","ivreghdfe", "xtreg","areg","ppmlhdfe","logit","xtlogit","probit","xtprobit","dprobit", "newey", "newey2", "nbreg", "gnbreg", "poisson", "xtpoisson", "clogit"),
    plm.. = c("xtreg", "xtivreg"),
    lm.. = c("regress","reg", "newey", "newey2"),
    ivreg.. = c("ivregress","ivreg","ivreg2", "xtivreg", "xtivreg2"),
    quantreg.. = c("qreg"),
    # Note that parmest does return coefficients not marginal
    # effects for dprobit. So we don't use mfx
    #mfx... = c("dprobit"),
    tobit.. = c("tobit"),
    stcox.. = c("stcox")
  )
  stata_cmd = unlist(li)
  r_cmd = str.left.of(names(stata_cmd),"..")
  data.frame(stata_cmd=stata_cmd,r_cmd=r_cmd,row.names = NULL)
}
```
!END_MODIFICATION stata_to_r_cmds_df to_r.R
