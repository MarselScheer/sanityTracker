
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/MarselScheer/sanityTracker.svg?branch=master)](https://travis-ci.org/MarselScheer/sanityTracker)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN\_Status\_Badge\_version\_ago](https://www.r-pkg.org/badges/version-ago/sanityTracker)](https://cran.r-project.org/package=sanityTracker)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/sanityTracker)](https://cran.r-project.org/package=sanityTracker)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

# sanityTracker

An R-Package that keeps track of all performed sanity checks.

During the preparation of data set(s) one usually performs some sanity
checks. The idea is that irrespective of where the checks are performed,
they are centralized by this package in order to list all at once with
examples if a check failed.

## Example

Assume you process a data set and you have different functions for
certain aspects. Within those functions you can make some checks,
document what you did, what the outcome of the check was and store some
examples where the check failed. At the end you can summarize the
performed checks (that might be scattered all over our source code) and
their outcomes.

For illustration we consider a very simple data set:

``` r
raw_data
#>   id      start        end height_m
#> 1  1 2020-04-12 2020-03-13     1.77
#> 2  2 2010-01-20 2020-01-26   144.00
#> 3  3 2020-02-20 2020-03-01     1.89
#> 4  4 2020-01-23 2020-01-26     1.74
```

We have two simple data-preparation function for our raw-data-set:

``` r
correct_height <- function(raw_data) {
  ret <- raw_data
  idx <- 100 < ret$height_m
  sanityTracker::add_sanity_check(
    fail_vec = idx,
    description = "Persons are smaller than 100m",
    counter_meas = "Divide by 100. Assume height is given in cm",
    data = ret
  )
  ret$height_m[idx] <- ret$height_m[idx] / 100
  
  sanityTracker::add_sanity_check(
    fail_vec = 2.5 < ret$height_m,
    description = "Persons are smaller than 2.5m",
    counter_meas = "None",
    data = ret
  )  
  return(ret)
}

prep <- function(raw_data) {

  sanityTracker::add_sanity_check(
    fail_vec = duplicated(raw_data$id),
    description = "No duplicated ids",
    counter_meas = "None",
    data = raw_data
  )
  
  raw_data$start <- as.Date(raw_data$start)
  raw_data$end <- as.Date(raw_data$end)
  sanityTracker::add_sanity_check(
    fail_vec = raw_data$end < raw_data$start,
    description = "start-date <= end-date",
    counter_meas = "None",
    data = raw_data
  )

  ret <- correct_height(raw_data = raw_data)  
  return(ret)
}
```

After applying the prep-function we can summarize the sanity checks

``` r
wrangled_data <- prep(raw_data = raw_data)
sanity_checks <- sanityTracker::get_sanity_checks()
sanity_checks
#>                      description n n_fail n_na
#> 1:             No duplicated ids 4      0    0
#> 2:        start-date <= end-date 4      1    0
#> 3: Persons are smaller than 100m 4      1    0
#> 4: Persons are smaller than 2.5m 4      0    0
#>                                   counter_meas
#> 1:                                        None
#> 2:                                        None
#> 3: Divide by 100. Assume height is given in cm
#> 4:                                        None
#>                                   call      example
#> 1:           prep(raw_data = raw_data)             
#> 2:           prep(raw_data = raw_data) <data.frame>
#> 3: correct_height(raw_data = raw_data) <data.frame>
#> 4: correct_height(raw_data = raw_data)
```

This directly gives an overview of what was performed which check failed
how often what counter measure was done and in case of a fail also
random rows (by default at most 3) of the data set where the check
failed.

``` r
sanity_checks[2, ]
#>               description n n_fail n_na counter_meas
#> 1: start-date <= end-date 4      1    0         None
#>                         call      example
#> 1: prep(raw_data = raw_data) <data.frame>
sanity_checks[2, ]$example
#> [[1]]
#>   id      start        end height_m
#> 1  1 2020-04-12 2020-03-13     1.77
```

## Installation

You can install it from github with:

``` r
devtools::install_github("MarselScheer/sanityTracker")
```

# sessionInfo

``` r
sessionInfo()
#> R version 3.6.1 (2019-07-05)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: Debian GNU/Linux 9 (stretch)
#> 
#> Matrix products: default
#> BLAS/LAPACK: /usr/lib/libopenblasp-r0.2.19.so
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=C             
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] badgecreatr_0.2.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_3.6.1           magrittr_1.5            
#>  [3] tools_3.6.1              htmltools_0.3.6         
#>  [5] yaml_2.2.0               Rcpp_1.0.2              
#>  [7] stringi_1.4.3            rmarkdown_1.15          
#>  [9] data.table_1.12.2        sanityTracker_0.0.0.9000
#> [11] knitr_1.25               git2r_0.26.1            
#> [13] stringr_1.4.0            xfun_0.9                
#> [15] digest_0.6.21            evaluate_0.14
```
