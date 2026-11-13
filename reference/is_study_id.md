# Check or suggest study ids

Study ids are lowercase letters, digits and hyphens, 3-40 characters,
usually first author + year (`hansen2019`, `hansen2019b`).

## Usage

``` r
is_study_id(study_id)

check_study_id(study_id)

suggest_study_id(authors, year, path = ".")
```

## Arguments

- study_id:

  Character vector of candidate ids.

- authors:

  Author names (first author first), e.g. `"Hansen A"`.

- year:

  Publication year.

- path:

  Project root, used to avoid ids that are already taken.

## Value

`is_study_id()`: logical vector. `check_study_id()`: the id, invisibly,
or an error.

`suggest_study_id()`: a free id such as `"hansen2019"` or
`"hansen2019b"`.
