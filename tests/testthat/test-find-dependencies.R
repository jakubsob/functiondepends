test_that("1 dependency", {
  dir.create(file.path(tempdir(), "find_dependencies"), showWarnings = FALSE)
  code <- "add <- function(x, y) {
    x + y
  }
  add_two = function(x) {
    x <- add(x, 1)
    add(x, 1)
  }"
  write(code, file.path(tempdir(), "find_dependencies", "code1.R"))
  envir <- new.env()
  source(file.path(tempdir(), "find_dependencies", "code1.R"), local = envir)
  expect_equal(
    find_dependencies("add_two", envir = envir),
    tibble::tibble(
      Source = "add",
      SourceRep = 2,
      SourceNamespace = "user-defined",
      Target = "add_two",
      TargetInDegree = 1
    )
  )
})

test_that("0 dependencies", {
  dir.create(file.path(tempdir(), "find_dependencies"), showWarnings = FALSE)
  code <- "
  add <- function(x, y) {
    x + y
  }
  "
  write(code, file.path(tempdir(), "find_dependencies", "code2.R"))
  envir <- new.env()
  source(file.path(tempdir(), "find_dependencies", "code2.R"), local = envir)
  expect_equal(
    find_dependencies("add", envir = envir),
    tibble::tibble(
      Source = NA,
      SourceRep = 0,
      SourceNamespace = "user-defined",
      Target = "add",
      TargetInDegree = 0
    )
  )
})

test_that("no function", {
  expect_message(find_dependencies("x", envir = new.env()), "No x in given environment")
  expect_equal(find_dependencies("x", envir = new.env()), tibble::tibble())
})
