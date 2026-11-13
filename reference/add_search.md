# Record a literature search

Appends the search to `searches.yml`. A re-run of the same query is a
new search with its own id.

## Usage

``` r
add_search(
  path = ".",
  id,
  database,
  query,
  results = NA,
  person,
  date = Sys.Date()
)
```

## Arguments

- path:

  Project root.

- id:

  Search id, e.g. `"pubmed-2026-11"`.

- database:

  Database name.

- query:

  The exact query.

- results:

  Number of hits.

- person:

  Person or tool id who ran it.

- date:

  Date the search was run. Defaults to today.

## Value

The search entry, invisibly.
