# Add a GitHub Actions workflow that publishes the website

Writes `.github/workflows/metawoRld-website.yml`. On every push to the
default branch (and on manual dispatch) it installs metawoRld, renders
the site and deploys `_site/` to GitHub Pages. In the repository
settings, set *Pages \> Source* to *GitHub Actions*.

## Usage

``` r
use_github_action_quarto(path = ".", overwrite = FALSE)
```

## Arguments

- path:

  Project root.

- overwrite:

  Replace an existing workflow file? Defaults to `FALSE`.

## Value

The workflow path, invisibly.
