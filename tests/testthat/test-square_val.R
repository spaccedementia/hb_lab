test_that("square_val actually squares", {
  # Test that square_val(2) returns 4
  expect_equal(square_val(2), 4)

  # Test that square_val("cat") throws an error with a specific message
  expect_error(
    square_val("cat"),
    "non-numeric argument to binary operator"
  )
})
