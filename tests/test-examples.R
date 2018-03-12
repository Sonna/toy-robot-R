context('Examples')

test_that('Can run an example', {
  subject <- "hello"
  expect_equal(subject, "hello")
})

test_that('Pinrt "Hello world"', {
  expect_output(print("hello world"), "hello world")
})
