library(testthat)


year <- 2014

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("inst/extdata/accident_%d.csv.bz2", year)
}

test_that("output is filepath with new file name year 2014",{expect_identical("inst/extdata/accident_2014.csv.bz2","inst/extdata/accident_2014.csv.bz2")})

