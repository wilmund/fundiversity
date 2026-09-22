test_that("fd_chull_intersect() gives good value", {

  box_trait <- matrix(
    c(-0.5, 0.5, -0.5, 0.5, -0.5, -0.5, 0.5, 0.5), ncol = 2,
    dimnames = list(
      species = paste0("sp", 1:4), traits = paste0("trait", 1:2)
    )
  )

  expect_equal(fd_chull_intersect(box_trait, box_trait)$vol,  1)
  expect_equal(fd_chull_intersect(box_trait, box_trait)$area, 4)

})

test_that("fd_chull_intersect() computes range overlap with a single trait", {

  seg <- function(vals) {
    sp_names <- if (length(vals) > 0) paste0("sp", seq_along(vals))
    matrix(vals, nrow = length(vals), ncol = 1,
           dimnames = list(sp_names, "trait1"))
  }

  # Disjoint ranges
  expect_equal(fd_chull_intersect(seg(c(0, 1)), seg(c(5, 6)))$vol, 0)
  # Nested ranges
  expect_equal(fd_chull_intersect(seg(c(0, 6)), seg(c(2, 3)))$vol, 1)
  # Partial overlap
  expect_equal(fd_chull_intersect(seg(c(0, 4)), seg(c(2, 6)))$vol, 2)
  # Identical ranges
  expect_equal(fd_chull_intersect(seg(c(1, 3)), seg(c(1, 3)))$vol, 2)
  # Empty species set
  expect_identical(
    fd_chull_intersect(seg(numeric(0)), seg(c(1, 3)))$vol, NA_real_
  )
})
