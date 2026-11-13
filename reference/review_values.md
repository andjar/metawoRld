# Review extracted values against the papers

Opens a Shiny app for verifying data points one at a time. For each
value it shows a plain-language reading of the numbers (e.g. "median
(IQR) = 4.2 (3.1 to 5.5) pg/mL"), the group and method it belongs to,
flags from
[`check_values()`](https://andjar.github.io/metawoRld/reference/check_values.md),
where it was reported (`source`, `source_page`, `source_quote`) and, if
available, the full text opened at that page.

## Usage

``` r
review_values(
  path = ".",
  fulltext_dir = NULL,
  reviewer = NULL,
  launch.browser = TRUE
)

review_values_app(path = ".", fulltext_dir = NULL, reviewer = NULL)
```

## Arguments

- path:

  Project root.

- fulltext_dir:

  Optional folder with full texts named `<study_id>.pdf` or
  `<sanitize_id(doi)>.pdf` (or .html/.txt). Defaults to
  `<path>/fulltext` if it exists.

- reviewer:

  Reviewer name or initials (can also be set in the app).

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Runs the app. `review_values_app()` returns the app object.

## Details

Actions (with keyboard shortcuts):

- **Verify** (`V`): the value matches the paper. If you edited any
  field, the button saves the correction first and records what changed.

- **Reject** (`R`): the value is wrong or should not be used; it is left
  out of
  [`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md)
  by default.

- **Next / previous** (`N` / `P`, or the arrow keys).

- **Map variable**: add an unknown variable name as a synonym in
  `_rules.yml`, or define it as a new variable.

Decisions are stored in `data/<study>/verification.csv` with reviewer,
time and a fingerprint of the values; if the values change later (e.g.
re-extraction), the data point is marked `"changed"` and comes back into
the queue.
