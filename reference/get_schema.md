# Read a form from a metawoRld project

Forms live in `forms/<name>.yml` and use the node format described in
`SPEC.md` (section 7). `forms/extraction.yml` is required; others (e.g.
`screening`, `rob2`) are optional.

## Usage

``` r
get_schema(path = ".", form = "extraction")

read_form(path = ".", form = "extraction")
```

## Arguments

- path:

  Project root. Defaults to the working directory.

- form:

  Form name, e.g. `"extraction"` (default) or `"screening"`.
  `"assessment"` is accepted as an alias of `"screening"`.

## Value

The form as a nested list, including its `_form` header.
