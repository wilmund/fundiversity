test_that("fd_chull() gives good value", {

  box_trait <- matrix(
    c(-0.5, 0.5, -0.5, 0.5, -0.5, -0.5, 0.5, 0.5), ncol = 2,
    dimnames = list(
      species = paste0("sp", 1:4), traits = paste0("trait", 1:2)
    )
  )

  expect_equal(fd_chull(box_trait)$vol,  1)
  expect_equal(fd_chull(box_trait)$area, 4)

})

test_that("fd_chull() returns NA volume for an empty species set", {

  empty_one_trait <- matrix(
    numeric(0), nrow = 0, ncol = 1, dimnames = list(NULL, "trait1")
  )

  expect_identical(fd_chull(empty_one_trait)$vol, NA_real_)
})
