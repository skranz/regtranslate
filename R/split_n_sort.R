sort2_chr = function(a, b, ..., sep=NULL) {
  swap = stringi::stri_cmp_gt(a, b, ...)
  lo = ifelse(swap, b, a)
  hi = ifelse(swap, a, b)
  if (!is.null(sep)) return(paste0(a,sep,b))
  list(lo = lo, hi = hi)
}

sort3_chr = function(a, b, c, ..., sep=NULL) {
  s = sort2_chr(a, b, ...)
  a = s$lo
  b = s$hi

  s = sort2_chr(b, c, ...)
  b = s$lo
  c = s$hi

  s = sort2_chr(a, b, ...)
  a = s$lo
  b = s$hi

  if (!is.null(sep)) return(paste0(a,sep,b,sep,c))

  list(a = a, b = b, c = c)
}

split_and_sort = function(terms, split = "#", ..., k = NULL) {
  if (!length(terms)) {
    return(character())
  }

  if (is.null(k)) {
    n_tok = stringi::stri_count_fixed(terms, split) + 1L
    same_k = length(unique(n_tok)) == 1L
    if (same_k) {
      k = n_tok[1]
    } else {
      k = NA_integer_
    }
  }

  if (identical(k, 2L)) {
    m = stringi::stri_split_fixed(terms, split, simplify = TRUE)
    s = sort2_chr(m[, 1], m[, 2], ..., sep=split)
    return(s)
    #return(stringi::stri_join(s$lo, s$hi, sep = split))
  }

  if (identical(k, 3L)) {
    m = stringi::stri_split_fixed(terms, split, simplify = TRUE)
    s = sort3_chr(m[, 1], m[, 2], m[, 3], ..., sep=split)
    return(s)
    return(stringi::stri_join(s$a, s$b, s$c, sep = split))
  }

  x = stringi::stri_split_fixed(terms, split, simplify = FALSE)
  vapply(
    x,
    function(tok) stringi::stri_flatten(stringi::stri_sort(tok, ...), collapse = split),
    character(1)
  )
}
