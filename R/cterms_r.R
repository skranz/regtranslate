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


replace_cterms_dot = function(cterms) {
  cterms = as.character(cterms)

  if (length(cterms)==0) {
    return(cterms)
  }

  # 1. Expand compact chained TS prefixes, e.g.
  # LD.x1 -> L.D.x1
  # L2D3S4.x1 -> L2.D3.S4.x1
  repeat {
    new_cterms = gsub(
      "(^|#|@|\\.)([LlFfDdSsOo][0-9]*)([LlFfDdSsOo][0-9]*)(?=[@\\.])",
      "\\1\\2.\\3",
      cterms,
      perl = TRUE
    )
    if (identical(new_cterms, cterms)) {
      break
    }
    cterms = new_cterms
  }

  # 2. Canonicalize TS operators to upper case and drop explicit 1
  # d.  -> D.
  # d@  -> D@
  # l1. -> L.
  # l1@ -> L@
  ops = c("l", "f", "d", "s", "o")
  for (op in ops) {
    OP = toupper(op)

    cterms = gsub(
      paste0("(^|#|@|\\.)([", op, OP, "])1(?=[@\\.])"),
      paste0("\\1", OP),
      cterms,
      perl = TRUE
    )

    cterms = gsub(
      paste0("(^|#|@|\\.)([", op, OP, "])([0-9]+)(?=[@\\.])"),
      paste0("\\1", OP, "\\3"),
      cterms,
      perl = TRUE
    )

    cterms = gsub(
      paste0("(^|#|@|\\.)([", op, OP, "])(?=[@\\.])"),
      paste0("\\1", OP),
      cterms,
      perl = TRUE
    )
  }

  # 3. Convert only dots that separate actual TS prefixes from the next token.
  # Do not touch decimal points in factor levels such as x1=-1.874.
  repeat {
    new_cterms = gsub(
      "(^|#|@)([LFDSO][0-9]*)\\.",
      "\\1\\2@",
      cterms,
      perl = TRUE
    )
    if (identical(new_cterms, cterms)) {
      break
    }
    cterms = new_cterms
  }

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
    terms = replace_cterms_dot(terms)
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
