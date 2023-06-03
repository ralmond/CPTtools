
test_that("isIncreasing", {
  expect_true(is.na(isIncreasing(1)))
  expect_true(isIncreasing(1:3))
  expect_false(isIncreasing(3:1))
  expect_false(isIncreasing(c(1,7,3)))
  expect_false(isIncreasing(c(1,1,2)))
})

test_that("isDecreasing", {
  expect_true(is.na(isDecreasing(1)))
  expect_false(isDecreasing(1:3))
  expect_true(isDecreasing(3:1))
  expect_false(isDecreasing(c(3,7,1)))
  expect_false(isDecreasing(c(2,1,1)))
  
})

test_that("isNondecreasing", {
  expect_true(is.na(isNondecreasing(1)))
  expect_true(isNondecreasing(1:3))
  expect_false(isNondecreasing(3:1))
  expect_false(isNondecreasing(c(1,7,3)))
  expect_true(isNondecreasing(c(1,1,2)))  
})

test_that("isNonincreasing", {
  expect_true(is.na(isNonincreasing(1)))
  expect_false(isNonincreasing(1:3))
  expect_true(isNonincreasing(3:1))
  expect_false(isNonincreasing(c(3,7,1)))
  expect_true(isNonincreasing(c(2,1,1))) 
})

test_that("isMonotonic", {
  expect_true(is.na(isMonotonic(1)))
  expect_true(isMonotonic(1:3))
  expect_true(isMonotonic(3:1))
  expect_false(isMonotonic(c(1,7,3)))
  expect_false(isMonotonic(c(1,1,2))) 
  expect_false(isMonotonic(c(3,7,1)))
  expect_false(isMonotonic(c(2,1,1))) 
  expect_false(isMonotonic(c(1,7,3),strict=FALSE))
  expect_true(isMonotonic(c(1,1,2),strict=FALSE)) 
  expect_false(isMonotonic(c(3,7,1),strict=FALSE))
  expect_true(isMonotonic(c(2,1,1),strict=FALSE)) 
})
