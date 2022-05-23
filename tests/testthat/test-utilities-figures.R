## context("utilities-figures")

test_that("Correct usage of .isPoint", {
  expect_equal(.isPoint(""), FALSE)
  expect_equal(.isPoint("l"), FALSE)
  expect_equal(.isPoint("p"), TRUE)
  expect_equal(.isPoint("lp"), TRUE)
  expect_equal(.isPoint("pl"), TRUE)
  expect_equal(.isPoint("ps"), TRUE)
  expect_equal(.isPoint("b"), TRUE)
})


test_that("Correct usage of .isLine", {
  expect_equal(.isLine(""), FALSE)
  expect_equal(.isLine("p"), FALSE)
  expect_equal(.isLine("l"), TRUE)
  expect_equal(.isLine("lp"), TRUE)
  expect_equal(.isLine("pl"), TRUE)
  expect_equal(.isLine("sl"), TRUE)
  expect_equal(.isLine("b"), TRUE)
})
