# Build the worked example project

Creates a small but complete metawoRld project on maternal interleukin-6
(IL-6) in preeclampsia compared with normotensive pregnancy, using
values from four open-access studies shipped with the package
(`system.file("extdata", "il6-preeclampsia", package = "metawoRld")`;
see the README there for the sources). It runs the whole workflow:
people, a search log, records, screening by two people with a consensus
decision, submissions, acceptance, dual extraction of one study with
reconciliation, and some verified values. Use it to try the package, the
review app and the dashboard.

## Usage

``` r
example_project(path, website = TRUE)
```

## Arguments

- path:

  Folder to create. Must not exist yet.

- website:

  Add the Quarto website templates? Defaults to `TRUE`.

## Value

The project path, invisibly.

## Details

This is a documentation example, not a systematic review. The people are
example personas.

## Examples

``` r
p <- example_project(file.path(tempdir(), "il6-example"))
list_studies(p)
#> [1] "afshari2005"   "dewi2026"      "gencheva2022"  "vitoratos2010"
prisma_counts(p)
#> # A tibble: 10 × 2
#>    stage                                      n
#>    <chr>                                  <int>
#>  1 "Records identified"                       6
#>  2 "Duplicates removed"                       0
#>  3 "Records screened (title/abstract)"        6
#>  4 "Records excluded (title/abstract)"        1
#>  5 "Awaiting decision (title/abstract)"       0
#>  6 "Reports assessed (full text)"             5
#>  7 "Reports excluded (full text)"             1
#>  8 "  Excluded: Group sizes not reported"     1
#>  9 "Studies included"                         4
#> 10 "Reports of included studies"              4
if (FALSE) { # \dontrun{
review_values(p)
generate_website(p)
} # }
```
