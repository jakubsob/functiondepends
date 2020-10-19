
<!-- README.md is generated from README.Rmd. Please edit that file -->

# functiondepends

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
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

Search for dependencies of function `find_functions` within parsed
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
  slice_max(SourceRep, n = 10) %>% 
  mutate(Source = reorder(Source, SourceRep)) %>% 
  ggplot(aes(x = Source, y = SourceRep, fill = Namespace)) +
  geom_col() +
  coord_flip() +
  labs(caption = "Top 10 most repeated function calls in 'find_dependencies' function.")
```

<img src="man/figures/README-functions_in_path-1.png" width="100%" />

``` r
library(ggplot2)
library(dplyr)

dependency <- find_dependencies(unique(functions$Function), envir = envir, in_envir = FALSE)
dependency %>% 
  distinct(Target, TargetInDegree) %>%
  mutate(Target = reorder(Target, TargetInDegree)) %>%
  ggplot(aes(x = Target, y = TargetInDegree)) +
  geom_col() +
  coord_flip() + 
  labs(caption = "Functions with most function calls.")
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
deg <- degree(g, mode = "in")
V(g)$size <- deg
V(g)$label.cex <- (degree(g, mode = "in", normalized = TRUE) + 1) / 1.8

plot(
  g,
  vertex.color = "grey",
  edge.color = "grey",
  edge.arrow.size = .4,
  main = "Functions dependency graph"
)
```

<img src="man/figures/README-network-1.png" width="100%" />
