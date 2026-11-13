# People and tools of a project

`people.yml` lists the people (with roles) and tools that may appear in
provenance, verification and screening records.

## Usage

``` r
read_people(path = ".")

add_person(
  path = ".",
  id,
  name,
  orcid = NULL,
  roles = c("extractor", "verifier"),
  tool = FALSE
)
```

## Arguments

- path:

  Project root.

- id:

  Short id (lowercase letters, digits, hyphens; 2-20 characters).

- name:

  Full name (people) or tool name.

- orcid:

  Optional ORCID iD.

- roles:

  Roles: any of `admin`, `screener`, `extractor`, `verifier`, `arbiter`.

- tool:

  Add a tool (e.g. DataFindR) instead of a person?

## Value

`read_people()`: a tibble with `id`, `name`, `orcid`, `roles`
(`; `-separated) and `type` (`"person"` or `"tool"`).

`add_person()`: the updated tibble, invisibly. Re-adding an existing id
updates it.
