replace_cterms_dot = function(cterms) {
  cterms = as.character(cterms)

  if (!length(cterms)) {
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

  # 2. Canonicalize TS operator case everywhere in a TS prefix chain,
  # whether the next separator is . or @
  # d.  -> D.
  # d@  -> D@
  # l2. -> L2.
  # l2@ -> L2@
  repeat {
    new_cterms = gsub(
      "(^|#|@|\\.)([LlFfDdSsOo])1?([@\\.])",
      "\\1\\U\\2\\E\\3",
      cterms,
      perl = TRUE
    )
    new_cterms = gsub(
      "(^|#|@|\\.)([LlFfDdSsOo])([2-9]|[1-9][0-9]+)([@\\.])",
      "\\1\\U\\2\\E\\3\\4",
      new_cterms,
      perl = TRUE
    )
    if (identical(new_cterms, cterms)) {
      break
    }
    cterms = new_cterms
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
