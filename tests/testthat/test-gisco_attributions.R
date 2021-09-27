
test_that("Testing attributions", {
  expect_message(gisco_attributions(copyright = TRUE))
  expect_silent(gisco_attributions())
  expect_identical(class(gisco_attributions()), "character")
  expect_identical(gisco_attributions("eN"), gisco_attributions("xxx"))
  expect_identical(class(gisco_attributions("da")), "character")
  expect_identical(class(gisco_attributions("de")), "character")
  expect_identical(class(gisco_attributions("es")), "character")
  expect_identical(class(gisco_attributions("FR")), "character")
  expect_identical(class(gisco_attributions("fi")), "character")
  expect_identical(class(gisco_attributions("no")), "character")
  expect_identical(class(gisco_attributions("sv")), "character")
})