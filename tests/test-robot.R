setwd("..")
source("R/toy-robot.R")
context('Toy Robot')

test_that('Can create new Robot', {
  subject <- new("Robot")

  expect_equal(subject@x, 0)
  expect_equal(subject@y, 0)
  expect_equal(subject@facing, "NORTH")
})

test_that('Robot report', {
  subject <- new("Robot")
  expect_output(report(subject), "0, 0, NORTH")
})

test_that('Robot can turn left', {
  subject <- new("Robot")
  new_subject <- left(subject)

  expect_equal(new_subject@x, 0)
  expect_equal(new_subject@y, 0)
  expect_equal(new_subject@facing, "WEST")
})

test_that('Robot can turn right', {
  subject <- new("Robot")
  new_subject <- right(subject)

  expect_equal(new_subject@x, 0)
  expect_equal(new_subject@y, 0)
  expect_equal(new_subject@facing, "EAST")
})
