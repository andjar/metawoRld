# The record registry

`records.csv` holds one row per unique record (search hit); see
`SPEC.md` section 6.

## Usage

``` r
read_records(path = ".")

add_records(path = ".", records, search = "manual")

link_record(path = ".", record_id, study_id)
```

## Arguments

- path:

  Project root.

- records:

  Data frame with at least `title`, and ideally `doi`, `pmid`, `year`,
  `journal`, `authors` (a character string, `; `-separated).

- search:

  Search id the records came from (see
  [`add_search()`](https://andjar.github.io/metawoRld/reference/add_search.md)),
  or `"manual"`.

- record_id:

  Record id(s).

- study_id:

  Study id to link the record(s) to.

## Value

`read_records()`: a tibble with the registry columns.

`add_records()`: a tibble with `record_id` and `new` (whether the record
was not in the registry before), invisibly. Records already present get
the search id added to their `searches`.
