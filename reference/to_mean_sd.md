# Convert summary statistics to mean and SD

Means with SE or 95% CI are converted exactly; medians with IQR or range
are converted with the methods of Wan et al. (2014, BMC Med Res Methodol
14:135). Other statistic types give `NA`.

## Usage

``` r
to_mean_sd(statistic_type, value1, value2, value3, n)
```

## Arguments

- statistic_type:

  Character vector (see the schema's enum).

- value1, value2, value3:

  Numeric vectors as stored in `data.csv`.

- n:

  Sample sizes.

## Value

A data frame with `mean`, `sd` and `conversion` (`"reported"`,
`"SE to SD"`, `"CI to SD"`, `"Wan 2014 (IQR)"`, `"Wan 2014 (range)"` or
`NA`).

## Examples

``` r
to_mean_sd(c("mean_sd", "median_iqr"), c(5, 4), c(1, 3), c(NA, 6), c(20, 30))
#>       mean       sd     conversion
#> 1 5.000000 1.000000       reported
#> 2 4.333333 2.335261 Wan 2014 (IQR)
```
