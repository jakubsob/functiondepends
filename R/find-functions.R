#' Is function
#'
#' @param expr Expression
#'
#' @return Logical, indicates whether \code{expr} is a function
#'
#' @noRd
#' @examples
#' is_function(quote(f <- function(x) { x }))
is_function <- function(expr) {
  if (!is_assign(expr)) return(FALSE)
  value <- expr[[3]]
  is.call(value) && as.character(value[[1]]) == "function"
}

#' Function name
#'
#' @param expr Expression
#'
#' @return Character, name of function from \code{expr}
#'
#' @noRd
#' @examples
#' get_function_name(quote(f <- function(x) { x }))
get_function_name <- function(expr) {
  as.character(expr[[2]])
}

#' Is assign
#'
#' @param expr Expression
#'
#' @return Logical, indicates whether expression is an assignment.
#'
#' @noRd
#' @examples
#' is_assign(quote(f <- function(x) { x }))
#' is_assign(quote(function(x) { x }))
is_assign <- function(expr) {
  is.call(expr) && as.character(expr[[1]]) %in% c("=", "<-", "assign")
}

#' Functions in path
#'
#' Parses files in given path. It searches for functions and loads them. Is safe for use with scripts
#' as it doesn't source the whole file, just functions. There are no side-effects to sourcing .R files.
#'
#' @param path Character, path to folder
#' @param envir Environment to source loaded functions into
#' @param recursive Logical, whether to search files recursively
#'
#' @export
#' @return A tibble with character columns indicating path to source files and names of functions defined
#'     in them.
#'
#' @examples
#' \donttest{
#' dir.create(file.path(tempdir(), "find_functions"), showWarnings = FALSE)
#' code <- "
#' add <- function(x, y) {
#'   x + y
#' }
#' add_one = function(x) {
#'   add(x, 1)
#' }
#' assign('add_two', function(x) {
#'   add(x, 2)
#' })
#' "
#' write(code, file.path(tempdir(), "find_functions", "code.R"))
#' find_functions(file.path(tempdir(), "find_functions"))
#' }
#'
#' @importFrom magrittr %>%
find_functions <- function(path, envir = .GlobalEnv, recursive = TRUE) {
  sourceFiles <- list.files(path, full.names = TRUE, recursive = recursive, pattern = ".R$")

  warn <- options()$warn
  options(warn = -1)
  on.exit(options(warn = warn))

  if (length(sourceFiles) == 0) {
    message("No .R files in directory")
    return(NULL)
  }

  df <- purrr::map_dfr(sourceFiles, ~ {
    fileParsed <- parse(.x)
    funcs <- Filter(is_function, fileParsed)
    funcsNames <- unlist(Map(get_function_name, funcs))
    if (length(funcsNames) == 0) return(NULL)
    purrr::map(funcs, eval, envir = envir)
    tibble::tibble(Path = .x, Function = funcsNames)
  })

  df$Path <- stringr::str_remove(df$Path, "^\\./|^/|^\\\\|^\\.")
  paths <- stringr::str_split(df$Path, pattern = "/|\\|\\\\")
  maxDepth <- max(vapply(paths, length, integer(1)))

  tidyr::separate(
    df,
    "Path",
    into = paste0("Level", 1:maxDepth),
    fill = "left",
    sep = "[/]|[\\]|[\\\\]"
  )
}
