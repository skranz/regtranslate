replace_cterm_special_symbols = function(cterms) {
  cterms = gsub("@",".__.", cterms, fixed=TRUE)
  cterms = gsub("#","._..", cterms, fixed=TRUE)
  cterms = gsub("=",".._.", cterms, fixed=TRUE)
  cterms
}

unreplace_cterm_special_symbols = function(cterms) {
  cterms = gsub(".__.","@", cterms, fixed=TRUE)
  cterms = gsub("._..","#", cterms, fixed=TRUE)
  cterms = gsub(".._.","=", cterms, fixed=TRUE)
  cterms
}


# We might put these functions back to repboxReg
cterm_of_r_coefs = function(terms, regvar, rcmd=NULL, dot_to_at=from_stata, from_stata=TRUE) {
  restore.point("cterm_of_r_coefs")

  #rcmd = rep(rcmd, length.out = length(terms))

  #cterms = terms

  # Currently we can use the fixest version for all regressions
  # but that might change
  cterms = cterm_of_r_coefs_fixest(terms, regvar, dot_to_at=dot_to_at)

  cterms
}

cterm_of_r_coefs_fixest = function(terms, regvar, dot_to_at=FALSE) {
  restore.point("r_coefs_cterm_fixest")

  terms = gsub("`","", terms, fixed=TRUE)
  terms = gsub("::","=", terms, fixed=TRUE)

  factor.rx = "factor\\(([a-zA-Z0-9_.]*)\\)"
  terms = gsub(factor.rx,"\\1=",terms,fixed=FALSE)

  if (dot_to_at) {
    terms = gsub(".","@", terms, fixed=TRUE)
  }

  rows = which(startsWith(terms,"fit_"))
  if (length(rows)>0) {
    terms.no.fit = substring(terms[rows], 5)
    change = (!terms[rows] %in% regvar$ia_cterm) & (terms.no.fit %in% regvar$ia_cterm)
    terms[rows[change]] = terms.no.fit[change]
  }

  ia_rows = which(has.substr(terms, "#"))
  if (length(ia_rows) > 0) {
    terms[ia_rows] = split_and_sort(terms[ia_rows], split = "#")
  }

  terms
}

# cterm_of_r_coefs_fixest = function(terms, regvar, dot_to_at=FALSE) {
#   restore.point("r_coefs_cterm_fixest")
#
#   terms = gsub("`","", terms, fixed=TRUE)
#   # factor in form: farmass_q::2
#   # to farmass_q=2
#   terms = gsub("::","=", terms, fixed=TRUE)
#   #terms = gsub(" ","", terms, fixed=TRUE)
#
#   # factor in form
#   # factor(i1)4:factor(d1)1
#   # to i1=4:d1=1
#   factor.rx = "factor\\(([a-zA-Z0-9_.]*)\\)"
#   terms = gsub(factor.rx,"\\1=",terms,fixed=FALSE)
#
#   # Replace . by @ if . was a Stata prefix
#   if (dot_to_at) {
#     terms = gsub(".","@", terms, fixed=TRUE)
#   }
#
#   # In an IV regression feols adds "fit_" to the result
#   # variable. We want to remove that
#   rows = which(startsWith(terms,"fit_"))
#   if (length(rows)>0) {
#     terms.no.fit = substring(terms[rows], 5)
#     change = (!terms[rows] %in% regvar$ia_cterm) & (terms.no.fit %in% regvar$ia_cterm)
#     terms[rows[change]] = terms.no.fit[change]
#   }
#
#   terms
# }
