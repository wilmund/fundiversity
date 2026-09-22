# A wrapper around geometry::convhulln() to properly handle errors and specific
# cases
#' @importFrom geometry convhulln
fd_chull <- function(traits) {

  traits <- traits[!duplicated(traits),, drop = FALSE]

  # Empty species set (e.g. site without species): volume is undefined.
  # Checked before the single-trait branch where max()/min() would return -Inf
  if (nrow(traits) == 0L) {
    return(list(
      "hull" = integer(0),
      "area" = NA_real_,
      "vol" = NA_real_,
      p = traits
    ))
  }

  if (ncol(traits) == 1L) {
   return(list(
     "hull" = c(which.min(traits), which.max(traits)),
     "area" = max(traits) - min(traits),
     "vol" = max(traits) - min(traits),
     p = traits
   ))
  }

  if (nrow(traits) <= ncol(traits)) {
    return(list(
      "hull" = seq_len(nrow(traits)),
      "area" = NA_real_,
      "vol" = NA_real_,
      p = traits
    ))
  }

  return(convhulln(traits, "FA"))

}
