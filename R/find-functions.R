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
#' @param separate_path Logical, whether to split path into hierarchy of directories. Produces
#'     multiple character columns with 'Level' prefix.
#'
#' @export
#' @return A tibble with character columns indicating path to source files and names of functions
#'     defined in them.
#'
#' @examples
#' \donttest{
#' path <- file.path(tempdir(), "find_functions_example")
#' dir.create(path, showWarnings = FALSE)
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
#' write(code, file.path(path, "code.R"))
#' find_functions(path)
#' }
#' @importFrom magrittr %>%
find_functions <- function(path, envir = new.env(), recursive = TRUE, separate_path = FALSE) {

  if (!dir.exists(path)) {
    message("Directory does not exist")
    return(invisible(NULL))
  }

  source_files <- list.files(
    path,
    full.names = TRUE,
    recursive = recursive,
    pattern = ".R$"
  )

  if (length(source_files) == 0) {
    message("No R files in directory")
    return(invisible(NULL))
  }

  df <- purrr::map_dfr(source_files, function(file) {
    file_parsed <- parse(file)
    funcs <- Filter(is_function, file_parsed)
    funcs_names <- purrr::map_chr(funcs, get_function_name)
    if (length(funcs_names) == 0) return(NULL)
    purrr::map(funcs, eval, envir = envir)
    tibble::tibble(Path = file, Function = funcs_names)
  })

  source_name <- basename(df$Path)
  Path <- Source <- Function <- NULL

  df <- df %>%
    dplyr::mutate(
      Path = stringr::str_remove(Path, "^\\./|^/|^\\\\|^\\."),
      Path = stringr::str_remove(Path, source_name),
      Path = stringr::str_remove(Path, "/$|\\\\$")
    )

  if (separate_path) {
    paths <- stringr::str_split(df$Path, "/|\\\\")
    max_depth <- max(purrr::map_int(paths, length))
    df <- tidyr::separate(
      df,
      "Path",
      into = paste0("Level", 1:(max_depth)),
      fill = "right",
      sep = "[/]|[\\]|[\\\\]"
    ) %>%
      dplyr::select(tidyselect::starts_with("Level"), Function)
  }

  df %>%
    dplyr::mutate(SourceFile = source_name)
}
