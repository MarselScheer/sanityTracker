
<!-- README.md is generated from README.Rmd. Please edit that file -->

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

We have a simple data-preparation function for our raw-data-set:

``` r
prep <- function(raw_data) {
  
  sanityTracker::add_sanity_check(
    fail_vec = duplicated(raw_data$id),
    description = "prep(): No duplicated ids",
    counter_meas = "None",
    data = raw_data
  )
  
  raw_data$start <- as.Date(raw_data$start)
  raw_data$end <- as.Date(raw_data$end)
  sanityTracker::add_sanity_check(
    fail_vec = raw_data$end < raw_data$start,
    description = "prep(): start-date <= end-date",
    counter_meas = "None",
    data = raw_data
  )

  idx <- 100 < raw_data$height_m
  sanityTracker::add_sanity_check(
    fail_vec = 100 < raw_data$height_m,
    description = "prep(): Persons are smaller than 100m",
    counter_meas = "Divide by 100. Assume height is given in cm",
    data = raw_data
  )
  raw_data$height_m[idx] <- raw_data$height_m[idx] / 100
  
  sanityTracker::add_sanity_check(
    fail_vec = 2.5 < raw_data$height_m,
    description = "prep(): Persons are smaller than 2.5m",
    counter_meas = "None",
    data = raw_data
  )
}
```

After applying the prep-function we can summarize the sanity checks

``` r
wrangled_data <- prep(raw_data = raw_data)
sanity_checks <- sanityTracker::get_sanity_checks()
sanity_checks
#>                              description n n_fail n_na
#> 1:             prep(): No duplicated ids 4      0    0
#> 2:        prep(): start-date <= end-date 4      1    0
#> 3: prep(): Persons are smaller than 100m 4      1    0
#> 4: prep(): Persons are smaller than 2.5m 4      0    0
#>                                   counter_meas      example
#> 1:                                        None             
#> 2:                                        None <data.frame>
#> 3: Divide by 100. Assume height is given in cm <data.frame>
#> 4:                                        None
```

This directly gives an overview of what was performed which check failed
how often what counter measure was done and in case of a fail also
random rows (by default at most 3) of the data set where the check
failed.

``` r
sanity_checks[2, ]
#>                       description n n_fail n_na counter_meas      example
#> 1: prep(): start-date <= end-date 4      1    0         None <data.frame>
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
