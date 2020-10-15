library(testthat)
library(farsFiki)

test_that("fars_read returns a message", {
  expect_error(fars_read("non-existing-file.csv"))
})

