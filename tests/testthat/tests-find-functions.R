test_that("find_functions: Empty directory", {
  dir.create(tempdir(), showWarnings = FALSE)
  empty_dir <- file.path(tempdir(), "empty_dir")
  dir.create(empty_dir, showWarnings = FALSE)
  expect_message(find_functions(empty_dir))
  expect_equal(find_functions(empty_dir), NULL)
})

test_that("find_dependencies: side effects", {
  dir.create(file.path(tempdir(), "find_functions"), showWarnings = FALSE)
  code <- "add <- function(x, y) {
    sum <- x + y
    assign('sum', sum)
    sum
  }
  x <- 1
  y <- 1"
  write(code, file.path(tempdir(), "find_functions", "side_effects.R"))
  envir <- new.env()
  functions <- find_functions(file.path(tempdir(), "find_functions"), envir = envir)
  expect_equal(ls(envir), "add")
})

test_that("is_function", {
  expect_equal(is_function(quote(f <- function(x) { x })), TRUE)
  expect_equal(is_function(quote(assign("f", function(x) { x }))), TRUE)
  expect_equal(is_function(quote(f <- x)), FALSE)
  expect_equal(is_function(quote(f <- x())), FALSE)
})

test_that("get_function_name", {
  expect_equal(get_function_name(quote(f <- function(x) { x })), "f")
  expect_equal(get_function_name(quote(f <- function(x) { x })), "f")
})

test_that("is_assign", {
  dir.create(tempdir(), showWarnings = FALSE)
  code <- "add <- 1"
  write(code, file.path(tempdir(), "is_assign.R"))
  expect_equal(is_assign(parse(file.path(tempdir(), "is_assign.R"))[[1]]), TRUE)

  code <- "add = 1"
  write(code, file.path(tempdir(), "is_assign.R"))
  expect_equal(is_assign(parse(file.path(tempdir(), "is_assign.R"))[[1]]), TRUE)

  code <- "assign('add', 1)"
  write(code, file.path(tempdir(), "is_assign.R"))
  expect_equal(is_assign(parse(file.path(tempdir(), "is_assign.R"))[[1]]), TRUE)

})
