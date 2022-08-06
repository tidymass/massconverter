test_that("massconverter version", {
  version <-
    get_massconverter_version()
  expect_match(object = version, "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2}")
})
