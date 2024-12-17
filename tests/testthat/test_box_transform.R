library(koo)
library(testthat)
x<-c(-1,4,3,7,14)
y<-c(1,2,3,4,9,8)
transformed_y<-box_transform(y,2)
test_that("box_transform returns box-transformed data",{
  expect_error(box_transform(x,2))
   expect_equal(round(transformed_y, 2), c(0.0, 1.5, 4.0, 7.5, 40.0, 31.5))
})
