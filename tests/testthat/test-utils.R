test_that("clear_cache() runs without error", {
  op <- options(obr.cache_dir = tempfile("obr_test_cache_"))
  on.exit(options(op))
  dir.create(getOption("obr.cache_dir"), recursive = TRUE)
  expect_invisible(clear_cache())
})

test_that("obr_cache_dir() creates and returns a directory", {
  op <- options(obr.cache_dir = tempfile("obr_test_dir_"))
  on.exit(options(op))
  d <- obr_cache_dir()
  expect_type(d, "character")
  expect_true(dir.exists(d))
})
