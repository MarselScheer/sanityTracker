
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

We have two simple data-preparation functions for our raw-data-set:

``` r
correct_height <- function(raw_data) {
  ret <- raw_data
  # functions starting with sc_ are convenience functions the package
  # offers for ease of use
  sc <- sanityTracker::sc_cols_bounded_above(
    object = ret,
    cols = "height_m",
    upper_bound = 100,
    description = "Persons are smaller than 100m",
    counter_meas = "Divide by 100. Assume height is given in cm",
  )
  
  if (sc[["height_m"]][["fail"]]) {
    fail_vec <- sc[["height_m"]][["fail_vec"]]
    ret$height_m[fail_vec] <- ret$height_m[fail_vec] / 100
  }
  
  
  sanityTracker::sc_cols_bounded(
    object = ret, 
    cols = "height_m",
    rule = "[0.8, 2.5]",
    description = "Persons are between 0.8m and 2.5m"
  )  
  return(ret)
}

prep <- function(raw_data) {

  sanityTracker::sc_cols_unique(
    object = raw_data,
    cols = "id",
    description = "No duplicated ids"
  )
  
  raw_data$start <- as.Date(raw_data$start)
  raw_data$end <- as.Date(raw_data$end)
  # sanity checks can be recoreded as long a
  # logical vector exists with add_sanity_check()
  sanityTracker::add_sanity_check(
    fail_vec = raw_data$end < raw_data$start,
    description = "start-date <= end-date",
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
#>                          description
#> 1:                 No duplicated ids
#> 2:            start-date <= end-date
#> 3:     Persons are smaller than 100m
#> 4: Persons are between 0.8m and 2.5m
#>                                     additional_desc data_name n n_fail n_na
#> 1:                The combination of 'id' is unique  raw_data 4      0    0
#> 2:                                                -  raw_data 4      1    0
#> 3: Elements in 'height_m' should be in (-Inf, 100].       ret 4      1    0
#> 4:  Elements in 'height_m' should be in [0.8, 2.5].       ret 4      0    0
#>                                   counter_meas
#> 1:                                           -
#> 2:                                           -
#> 3: Divide by 100. Assume height is given in cm
#> 4:                                           -
#>                                                                 fail_vec_str
#> 1:                                                        dt$.n_col_cmb != 1
#> 2:                                             raw_data$end < raw_data$start
#> 3: sapply(object[[col]], function(x) !checkmate::qtest(x = x, rules = rule))
#> 4: sapply(object[[col]], function(x) !checkmate::qtest(x = x, rules = rule))
#>    param_name                                call      example
#> 1:       'id'           prep(raw_data = raw_data)             
#> 2:          -           prep(raw_data = raw_data) <data.frame>
#> 3:   height_m correct_height(raw_data = raw_data) <data.frame>
#> 4:   height_m correct_height(raw_data = raw_data)
```

This directly gives an overview of what was performed, which check
failed how often, what counter measure was applied and in case of a fail
also random rows (by default at most 3) of the data set where the check
failed.

``` r
sanity_checks[2, ]
#>               description additional_desc data_name n n_fail n_na counter_meas
#> 1: start-date <= end-date               -  raw_data 4      1    0            -
#>                     fail_vec_str param_name                      call
#> 1: raw_data$end < raw_data$start          - prep(raw_data = raw_data)
#>         example
#> 1: <data.frame>
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
#> R version 3.6.2 (2019-12-12)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: Debian GNU/Linux 10 (buster)
#> 
#> Matrix products: default
#> BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.3.5.so
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
#>  [1] Rcpp_1.0.3          digest_0.6.23       backports_1.1.5    
#>  [4] git2r_0.26.1        magrittr_1.5        evaluate_0.14      
#>  [7] rlang_0.4.2         stringi_1.4.3       data.table_1.12.8  
#> [10] checkmate_2.0.0     sanityTracker_0.1.0 rmarkdown_2.1      
#> [13] tools_3.6.2         stringr_1.4.0       xfun_0.11          
#> [16] yaml_2.2.0          compiler_3.6.2      htmltools_0.4.0    
#> [19] knitr_1.26
```
