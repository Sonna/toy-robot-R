setwd("..")
source("R/toy-robot.R")
context('Toy Robot')

test_that('Can create new ToyRobot', {
  subject <- new("ToyRobot")

  f <- file()
  options(toyrobotpkg.connection = f)
  ans <- paste(c("REPORT", "EXIT"), collapse = "\n")
  write(ans, f)

  expect_output(run(subject), "0, 0, NORTH")

  # reset connection
  options(toyrobotpkg.connection = stdin())
  # close the file
  close(f)
})

test_that('Can create new ToyRobot', {
  subject <- new("ToyRobot")

  f <- file()
  options(toyrobotpkg.connection = f)
  ans <- paste(
    c("MOVE", "REPORT", "RIGHT", "MOVE", "MOVE", "REPORT", "EXIT"),
    collapse = "\n"
  )
  write(ans, f)

  expect_output(run(subject), "0, 1, NORTH\n2, 1, EAST")

  # reset connection
  options(toyrobotpkg.connection = stdin())
  # close the file
  close(f)
})

test_that('Can run ToyRobot example a', {
  subject <- new("ToyRobot")
  expect_output(run(subject, "data/example_a.txt"), "0, 0, NORTH")
})

test_that('Can run ToyRobot example b', {
  subject <- new("ToyRobot")
  expect_output(run(subject, "data/example_b.txt"), "0, 1, NORTH\n2, 1, EAST")
})
