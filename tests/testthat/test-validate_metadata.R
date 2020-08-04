context("validate_metadata")

test_that("validate_metadata validates valid metadata", {

  # standard metadata
  metadata <- data.table(region_id = 1:10, file = "Monitor20.txt", start_datetime = "2020-07-05 08:00:00", stop_datetime = "2020-07-08")
  expect_true(validate_metadata(metadata))

  metadata <- data.table::fread(system.file(package = "fsldamr", "extdata", "metadata.csv"))
  expect_true(validate_metadata(metadata))

})

test_that("validate_metadata rejects invalid metadata", {
   # TODO

})
