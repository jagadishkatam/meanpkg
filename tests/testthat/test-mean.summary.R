

object_df <- data.frame(cyl=c(4,6,8),
                        avg=c(26.7,19.7,15.1)
                        )


expected <- mean.summary(mtcars, rlang::exprs(cyl), mpg) |> as.data.frame()


test_that("summary", {
  expect_equal(object_df, expected)
  expect_s3_class(expected, 'data.frame')
  expect_named(expected, c('cyl','avg'))
  expect_equal(expected[1,2], 26.7)

})
