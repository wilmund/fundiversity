# A wrapper around geometry::intersectn() to properly handle errors and specific
# cases
#' @importFrom geometry intersectn
fd_chull_intersect <- function(traits1, traits2) {

  traits1 <- traits1[!duplicated(traits1),, drop = FALSE]
  traits2 <- traits2[!duplicated(traits2),, drop = FALSE]

  # Empty species set (e.g. site without species): intersection is undefined.
  # Checked before the single-trait branch where max()/min() would return -Inf
  if (nrow(traits1) == 0L || nrow(traits2) == 0L) {
    return(
      list(
        "hull" = integer(0),
        "area" = NA_real_,
        "vol" = NA_real_
      )
    )
  }

  if (ncol(traits1) == 1L && ncol(traits2) == 1L) {

    # Range of the overlap: from the larger of the two range minima
    # to the smaller of the two range maxima, 0 when the ranges are disjoint
    r3 <- c(max(min(traits1), min(traits2)),
            min(max(traits1), max(traits2)))

    r_overlap <- max(r3[2] - r3[1], 0)

    return(
      list(
        "hull" = r3,
        "area" = r_overlap,
        "vol" = r_overlap
      )
    )
  }

  if (nrow(traits1) <= ncol(traits1) || nrow(traits2) <= ncol(traits2)) {
    return(
      list(
        "hull" = seq_len(nrow(traits1) + nrow(traits2)),
        "area" = NA_real_,
        "vol" = NA_real_
      )
    )
  }

  return(intersectn(traits1, traits2, options = "FA")[["ch"]])
}
