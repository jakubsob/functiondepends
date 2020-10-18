#' Find dependencies
#'
#' This function finds function calls inside a function with given name. Be aware that
#' any variable that has a name that overwrites a function name will be registered as
#' a function call. Use with caution.
#'
#' @param function_name Character, name of function
#' @param envir Environment in which to search for function. Deafult is \code{.GlobalEnv}
#' @param in_envir Logical, whether to return only functions that are loaded into \code{envir}
#'
#' @export
#' @return A tibble with columns:
#'     - Source: character, name of function called inside `Target`
#'     - SourceRep: integer, number of times `Source` is called
#'     - Namespace: character, name of namespace from which the function comes, if
#'         a function is defined in multiple namespaces then it is a vector. If function
#'         is user defined `Namespace` is NA.
#'     - Target: character, name of inspected function
#'     - TargetInDegree: integer, number of function calls inside of function body
#'
#' @importFrom magrittr %>%
find_dependencies <- function(function_name, envir = .GlobalEnv, in_envir = TRUE) {

  purrr::map_dfr(function_name, ~ {
    f_body <- deparse(body(get(.x, envir = envir)))

    calls <- unlist(stringr::str_extract_all(f_body, "[[:alnum:]\\.\\_]+\\(|[[:alnum:]\\.\\_]+::[[:alnum:]\\.\\_]+\\("))
    calls <- stringr::str_remove_all(calls, "\\(")

    arguments <- unlist(stringr::str_extract_all(f_body, "\\((.*?)\\)"))
    arguments <- unlist(stringr::str_remove_all(arguments, "^\\(|\\)$"))
    arguments <- unlist(stringr::str_extract_all(arguments, "[[:alnum:]\\.\\_]+|[[:alnum:]\\.\\_]+::[[:alnum:]\\.\\_]+"))

    functions <- tibble::tibble(
      Source = c(calls, arguments)
    ) %>%
      dplyr::group_by(Source) %>%
      dplyr::tally(name = "SourceRep")

    is_fun <- vapply(functions$Source, function(x) {
      tryCatch(
        {
          f_name <- stringr::str_split(x, "::")[[1]]
          func <- if (length(f_name) == 2) {
            getExportedValue(f_name[1], f_name[2])
          } else {
            get(x, envir = envir)
          }
          is.function(func)
        },
        error = function(e) FALSE
      )},
      logical(1)
    )
    functions <- dplyr::filter(functions, is_fun)
    functions <- functions %>%
      dplyr::bind_cols(tibble::as_tibble(stringr::str_locate(functions$Source, "::"))) %>%
      dplyr::mutate(
        Namespace = ifelse(
          is.na(start),
          Vectorize(find, "what")(Source, mode = "function"),
          stringr::str_sub(Source, 1, start - 1)
        ),
        Namespace = gsub("package:", "", Namespace),
        Namespace = ifelse(Namespace == "character(0)", NA, Namespace),
        Source = ifelse(
          is.na(start),
          Source,
          stringr::str_sub(Source, end + 1)
        )
      ) %>%
      dplyr::select(-c(start, end))

    if (in_envir) {
      functions <- dplyr::filter(functions, Source %in% ls(envir))
    }

    if (nrow(functions) == 0) {
      functions <- tibble::tibble(
        Source = NA,
        SourceRep = 0,
        Namespace = NA,
        Target = .x,
        TargetInDegree = 0
      )
    }

    functions %>%
      dplyr::mutate(
        Target = .x,
        TargetInDegree = nrow(.)
      )
  })
}
