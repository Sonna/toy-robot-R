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

test_that('Robot can move', {
  subject <- new("Robot")
  new_subject <- move(subject)

  expect_equal(new_subject@x, 0)
  expect_equal(new_subject@y, 1)
  expect_equal(new_subject@facing, "NORTH")
})

test_that('Robot can move EAST', {
  subject <- new("Robot", facing="EAST")
  new_subject <- move(subject)

  expect_equal(new_subject@x, 1)
  expect_equal(new_subject@y, 0)
  expect_equal(new_subject@facing, "EAST")
})

test_that('Robot cannot fall off 0,0,SOUTH', {
  subject <- new("Robot", x=0, y=0, facing="SOUTH")
  new_subject <- move(subject)

  expect_equal(new_subject@x, 0)
  expect_equal(new_subject@y, 0)
  expect_equal(new_subject@facing, "SOUTH")
})

test_that('Robot cannot fall off 0,0,WEST', {
  subject <- new("Robot", x=0, y=0, facing="WEST")
  new_subject <- move(subject)

  expect_equal(new_subject@x, 0)
  expect_equal(new_subject@y, 0)
  expect_equal(new_subject@facing, "WEST")
})

test_that('Robot cannot fall off 0,4,WEST', {
  subject <- new("Robot", x=0, y=4, facing="WEST")
  new_subject <- move(subject)

  expect_equal(new_subject@x, 0)
  expect_equal(new_subject@y, 4)
  expect_equal(new_subject@facing, "WEST")
})

test_that('Robot cannot fall off 0,4,NORTH', {
  subject <- new("Robot", x=0, y=4, facing="NORTH")
  new_subject <- move(subject)

  expect_equal(new_subject@x, 0)
  expect_equal(new_subject@y, 4)
  expect_equal(new_subject@facing, "NORTH")
})

test_that('Robot cannot fall off 4,4,NORTH', {
  subject <- new("Robot", x=4, y=4, facing="NORTH")
  new_subject <- move(subject)

  expect_equal(new_subject@x, 4)
  expect_equal(new_subject@y, 4)
  expect_equal(new_subject@facing, "NORTH")
})

test_that('Robot cannot fall off 4,4,EAST', {
  subject <- new("Robot", x=4, y=4, facing="EAST")
  new_subject <- move(subject)

  expect_equal(new_subject@x, 4)
  expect_equal(new_subject@y, 4)
  expect_equal(new_subject@facing, "EAST")
})

test_that('Robot cannot fall off 4,0,EAST', {
  subject <- new("Robot", x=4, y=0, facing="EAST")
  new_subject <- move(subject)

  expect_equal(new_subject@x, 4)
  expect_equal(new_subject@y, 0)
  expect_equal(new_subject@facing, "EAST")
})

test_that('Robot cannot fall off 4,0,SOUTH', {
  subject <- new("Robot", x=4, y=0, facing="SOUTH")
  new_subject <- move(subject)

  expect_equal(new_subject@x, 4)
  expect_equal(new_subject@y, 0)
  expect_equal(new_subject@facing, "SOUTH")
})

test_that('Robot can place', {
  subject <- new("Robot")
  new_subject <- place(subject, "2,3,EAST")

  expect_equal(new_subject@x, 2)
  expect_equal(new_subject@y, 3)
  expect_equal(new_subject@facing, "EAST")
})

## Test the above previously tested methods via `exec`
test_that('Robot can exec turn report', {
  subject <- new("Robot")
  expect_output(exec(subject, "REPORT"), "0, 0, NORTH")
})

test_that('Robot can exec turn left', {
  subject <- new("Robot")
  new_subject <- exec(subject, "LEFT")

  expect_equal(new_subject@x, 0)
  expect_equal(new_subject@y, 0)
  expect_equal(new_subject@facing, "WEST")
})

test_that('Robot can exec turn right', {
  subject <- new("Robot")
  new_subject <- exec(subject, "RIGHT")

  expect_equal(new_subject@x, 0)
  expect_equal(new_subject@y, 0)
  expect_equal(new_subject@facing, "EAST")
})

test_that('Robot can exec move', {
  subject <- new("Robot")
  new_subject <- exec(subject, "MOVE")

  expect_equal(new_subject@x, 0)
  expect_equal(new_subject@y, 1)
  expect_equal(new_subject@facing, "NORTH")
})

test_that('Robot can return subject on exec unknown', {
  subject <- new("Robot", x=1, y=2, facing="SOUTH")
  new_subject <- exec(subject, "UNKNOWN")

  expect_equal(new_subject@x, 1)
  expect_equal(new_subject@y, 2)
  expect_equal(new_subject@facing, "SOUTH")
})


test_that('Robot can exec place', {
  subject <- new("Robot")
  new_subject <- place(subject, "4,2,WEST")

  expect_equal(new_subject@x, 4)
  expect_equal(new_subject@y, 2)
  expect_equal(new_subject@facing, "WEST")
})
