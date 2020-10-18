
<!-- README.md is generated from README.Rmd. Please edit that file -->

# functiondepends

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/WelcomeToMyVirtualHome/functiondepends.svg?branch=master)](https://travis-ci.com/WelcomeToMyVirtualHome/functiondepends)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/WelcomeToMyVirtualHome/functiondepends?branch=master&svg=true)](https://ci.appveyor.com/project/WelcomeToMyVirtualHome/functiondepends)
[![codecov](https://codecov.io/gh/WelcomeToMyVirtualHome/functiondepends/branch/master/graph/badge.svg)](https://codecov.io/gh/WelcomeToMyVirtualHome/functiondepends)
<!-- badges: end -->

The goal of functiondepends is to allow for tidy exploration of
unstructured codebase without evaluation of code.

## Installation

One can install `functiondepends` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("WelcomeToMyVirtualHome/functiondepends")
```

``` r
library(functiondepends)
```

## Examples

``` r
# Create environment for loaded functions 
envir <- new.env()
# Seach recursively current directory
functions <- find_functions(".", envir = envir, recursive = TRUE)
functions
#> # A tibble: 5 x 3
#>   Level1 Level2              Function         
#>   <chr>  <chr>               <chr>            
#> 1 R      find-dependencies.R find_dependencies
#> 2 R      find-functions.R    is_function      
#> 3 R      find-functions.R    get_function_name
#> 4 R      find-functions.R    is_assign        
#> 5 R      find-functions.R    find_functions
```

Search for dependencies of function `functions_in_path` within parsed
functions:

``` r
dependency <- find_dependencies("find_functions", envir = envir, in_envir = TRUE)
dependency
#> # A tibble: 2 x 5
#>   Source            SourceRep Namespace Target         TargetInDegree
#>   <chr>                 <int> <chr>     <chr>                   <int>
#> 1 get_function_name         1 <NA>      find_functions              2
#> 2 is_function               1 <NA>      find_functions              2
```

Search for all dependencies of function:

``` r
library(ggplot2)
library(dplyr)

dependency <- find_dependencies("find_dependencies", envir = envir, in_envir = FALSE)

dependency %>% 
  ggplot(aes(x = reorder(Source, SourceRep), y = SourceRep, fill = Namespace)) +
  geom_col() +
  coord_flip() +
  labs(title = "functions_in_path", x = "Source")
```

<img src="man/figures/README-functions_in_path-1.png" width="100%" />

Limit results to functions only parsed from source files (user defined):

``` r
library(ggplot2)
library(dplyr)

dependency <- find_dependencies(unique(functions$Function), envir = envir, in_envir = FALSE)
dependency %>% 
  distinct(Target, TargetInDegree) %>%
  mutate(Target = reorder(Target, TargetInDegree)) %>%
  ggplot(aes(x = Target, y = TargetInDegree)) +
  geom_col() +
  coord_flip() 
```

<img src="man/figures/README-target_degree-1.png" width="100%" />

``` r
library(igraph)
library(dplyr)

edges <- dependency %>% 
  select(Source, Target) %>% 
  filter(!is.na(.))
vertices <- unique(c(edges$Source, edges$Target))

g <- graph_from_data_frame(
  d = edges, 
  vertices = vertices,
  directed = TRUE
)
plot(
  g,
  vertex.color = "grey",
  edge.color = "grey", 
  vertex.size = 20
)
```

<img src="man/figures/README-network-1.png" width="100%" />
