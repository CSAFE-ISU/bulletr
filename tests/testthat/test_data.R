context("data")

test_that("read_dat works as expected", {
  testfile <- "sample.dat"
  
  expect_error(read_dat("file"))
  
  system("touch file.txt")
  expect_error(read_dat("file.txt"), "grepl")
  
  # Create sample data
  z <- expand.grid(x = (1:10) * .045, y = (1:10)*.06) 
  z$value <- rep(1:10, each = 10)
  readr::write_delim(z, "sample.dat", col_names = F)
  
  # No downsampling, profiley = T
  tmp <- read_dat("sample.dat")
  expect_equal(tmp$surface.matrix, matrix(0:9, 10, 10, byrow = T))
  expect_equal(tmp$header.info$sizeX, 10)
  expect_equal(tmp$header.info$sizeY, 10)
  expect_equal(tmp$header.info$incrementX, 0.045)
  expect_equal(tmp$header.info$incrementY, 0.06)
  
  # profiley = F
  tmp <- read_dat("sample.dat", profiley = F)
  expect_equal(tmp$surface.matrix, matrix(rev(0:9), 10, 10, byrow = F))
  expect_equal(tmp$header.info$sizeX, 10)
  expect_equal(tmp$header.info$sizeY, 10)
  expect_equal(tmp$header.info$incrementX, 0.06)
  expect_equal(tmp$header.info$incrementY, 0.045)
  
  # Downsampling
  tmp <- read_dat("sample.dat", sample = 2)
  expect_equal(tmp$surface.matrix, matrix((0:4)*2, 5, 5, byrow = T))
  expect_equal(tmp$header.info$sizeX, 5)
  expect_equal(tmp$header.info$sizeY, 5)
  expect_equal(tmp$header.info$incrementX, 0.045*2)
  expect_equal(tmp$header.info$incrementY, 0.06*2)
  
  # Clean up
  file.remove("file.txt")
  file.remove("sample.dat")
})
