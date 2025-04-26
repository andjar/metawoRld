# metawoRld: A Framework for Structured Living Review Data

<!-- badges: start -->
[![Conceptual Overview](https://img.shields.io/badge/Overview-Conceptual%20Pipeline-blue)](articles/conceptual_overview.html) <!-- Adjust link if site structure differs -->
<!-- badges: end -->

The `metawoRld` package provides the foundational structure for creating and managing data for living systematic reviews. It helps ensure data is consistently structured, validated, stored reproducibly (using YAML/CSV), and can be easily loaded for analysis or presentation.

It is designed to work in tandem with the `DataFindR` package, which uses AI to help populate the `metawoRld` project with data extracted from scientific literature.

**Please see the [Conceptual Overview vignette](articles/conceptual_overview.html) for a detailed explanation of the entire living review pipeline and how `metawoRld` and `DataFindR` work together.**

## Installation

```r
# Assuming metawoRld is on GitHub (replace 'andjar/metawoRld')
# install.packages("remotes")
remotes::install_github("andjar/metawoRld")

# You will likely also need DataFindR
# remotes::install_github("andjar/DataFindR")
```

## Example: Creating a Project

```r
library(metawoRld)

proj_path <- file.path(tempdir(), "my_living_review")

# Clean up previous run if needed
if(dir.exists(proj_path)) unlink(proj_path, recursive = TRUE)

create_metawoRld(
  path = proj_path,
  project_name = "Cytokines in Pregnancy",
  project_description = "A test living review project.",
  # Define criteria and schema details here or rely on defaults
  inclusion_criteria = c("Human", "Pregnancy", "Serum/Plasma", "Cytokine"),
  exclusion_criteria = c("Animal", "Review", "Non-English")
)

list.files(proj_path)

# Clean up
# unlink(proj_path, recursive = TRUE)
```
