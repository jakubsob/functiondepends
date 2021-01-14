#' Find dependencies
#'
#' This function finds function calls inside a function with given name. Be aware that
#' any variable that has a name that overwrites a function name will be recognised as
#' a function call.
#'
#' @param function_name Character, name of function
#' @param envir Environment in which to search for function. Deafult is \code{.GlobalEnv}
#' @param in_envir Logical, whether to return only functions that are loaded into \code{envir}
#' @param add_info Logical, whether to add list column with line numbers of given function call
#'     in function body and a list column with context of said calls. Default is \code{FALSE}.
#'
#' @export
#' @return A tibble with columns:
#'     - Source: character, name of function called inside `Target`
#'     - SourceRep: integer, number of times `Source` is called
#'     - SourceNamespace: character, name of namespace from which the function comes, if
#'         a function is defined in multiple namespaces then it is a vector. If function
#'         is user defined `Namespace` is NA.
#'     - SourcePosition: optional, integer list with positions of `Source` calls in body
#'     - SouceContext: optional, character list with lines of code with calls of `Source`
#'     - Target: character, name of inspected function
#'     - TargetInDegree: integer, number of function calls inside of function body
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract_all str_remove_all str_split str_locate str_sub
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr group_by tally filter bind_cols mutate select rowwise ungroup
find_dependencies <- function(function_name, envir = .GlobalEnv, in_envir = TRUE, add_info = FALSE) {

  map_dfr(function_name, ~ {
    func <- tryCatch(
      get(.x, envir = envir),
      error = function(e) NULL
    )
    if (is.null(func)) {
      message("No ", .x, " in given environment")
      return(NULL)
    }
    f_body <- deparse(body(func), width.cutoff = 500L)

    calls <- unlist(stringr::str_extract_all(
      f_body,
      "[[:alnum:]\\.\\_]+\\(|[[:alnum:]\\.\\_]+::[[:alnum:]\\.\\_]+\\("
    ))
    calls <- stringr::str_remove_all(calls, "\\(")

    arguments <- unlist(stringr::str_extract_all(f_body, "\\((.*?)\\)"))
    arguments <- unlist(stringr::str_remove_all(arguments, "^\\(|\\)$"))
    arguments <- unlist(stringr::str_extract_all(
      arguments,
      "[[:alnum:]\\.\\_]+|[[:alnum:]\\.\\_]+::[[:alnum:]\\.\\_]+"
    ))

    functions <- tibble::tibble(
      Source = c(calls, arguments)
    ) %>%
      dplyr::group_by(Source) %>%
      dplyr::tally(name = "SourceRep")

    if (in_envir) {
      functions <- dplyr::filter(functions, Source %in% ls(envir))
    }

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

    functions <- dplyr::filter(functions, is_fun[names(is_fun) %in% functions$Source])
    functions <- functions %>%
      dplyr::bind_cols(tibble::as_tibble(stringr::str_locate(functions$Source, "::"))) %>%
      dplyr::mutate(
        SourceNamespace = ifelse(
          is.na(start),
          Vectorize(find, "what")(Source, mode = "function"),
          stringr::str_sub(Source, 1, start - 1)
        ),
        SourceNamespace = gsub("package:", "", SourceNamespace),
        SourceNamespace = ifelse(SourceNamespace == "character(0)", NA, SourceNamespace),
        SourceNamespace = ifelse(Source %in% ls(envir), "user-defined", SourceNamespace),
        Source = ifelse(
          is.na(start),
          Source,
          stringr::str_sub(Source, end + 1)
        )
      ) %>%
      dplyr::select(-c(start, end))

    if (nrow(functions) == 0) {
      functions <- tibble::tibble(
        Source = NA,
        SourceRep = 0,
        SourceNamespace = "user-defined",
        Target = .x,
        TargetInDegree = 0
      )
    }

    if (add_info) {
      functions <- functions %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          SourcePosition = list(grep(paste0("\\b", Source, "\\b"), f_body) - 1),
          SourceContext = list(f_body[SourcePosition + 1])
        ) %>%
        dplyr::ungroup()
    }

    functions %>%
      dplyr::mutate(
        Target = .x,
        TargetInDegree = ifelse(is.na(Source), 0, nrow(.))
      )
  })
}
