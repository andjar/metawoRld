# metawoRld: structured, validated data for living systematic reviews

A metawoRld project is a folder (usually a Git repository) holding the
review's settings, a schema describing what is extracted from each
study, and one folder per study with `metadata.yml` and `data.csv`.

## Details

Main functions:

- [`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md)
  creates a project with generic, editable templates.

- [`add_study_data()`](https://andjar.github.io/metawoRld/reference/add_study_data.md)
  validates and stores one study;
  [`add_study_template()`](https://andjar.github.io/metawoRld/reference/add_study_template.md)
  creates files for manual entry;
  [`validate_study()`](https://andjar.github.io/metawoRld/reference/validate_study.md)
  /
  [`validate_world()`](https://andjar.github.io/metawoRld/reference/validate_world.md)
  check edited files.

- [`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md)
  loads everything for analysis.

- [`generate_website()`](https://andjar.github.io/metawoRld/reference/generate_website.md)
  builds a Quarto website;
  [`use_github_action_quarto()`](https://andjar.github.io/metawoRld/reference/use_github_action_quarto.md)
  publishes it automatically.

The companion package DataFindR uses LLMs to screen studies and extract
data into a metawoRld project.

## See also

Useful links:

- <https://github.com/andjar/metawoRld>

- Report bugs at <https://github.com/andjar/metawoRld/issues>

## Author

**Maintainer**: Anders Hagen Jarmund <anders.h.jarmund@ntnu.no>
([ORCID](https://orcid.org/0000-0002-3923-1637))
