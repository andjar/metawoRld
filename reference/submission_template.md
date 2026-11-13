# Write blank files for a submission

Writes `metadata.yml` (a placeholder for every form field) and a
header-only `data.csv` to `dir`, for filling in with any text editor or
spreadsheet program that saves CSV. Submit them with
`submit_study(metadata = "<dir>/metadata.yml", data = "<dir>/data.csv")`.

## Usage

``` r
submission_template(path = ".", dir)
```

## Arguments

- path:

  Project root.

- dir:

  Folder to write to. Must not contain the files already.

## Value

`dir`, invisibly.
