# functiondepends 0.2.3

* Replaced `dplyr::filter(is.na())` calls with `na.omit` to avoid breaking with `dplyr` 1.0.8

# functiondepends 0.2.2

* Changed `find_functions` usage to return non-separated path of source file by default.

* Changed column in tibble returned by `find_functions` from Source to SourceFile

# functiondepends 0.2.1

* CRAN description fixes.

# functiondepends 0.2.0

* Added a `NEWS.md` file to track changes to the package.

* Added vignette.

* Added new functionality to `find_dependencies` function: optional columns with info about source function.

# functiondepends 0.1.1

* Additional tests. 
