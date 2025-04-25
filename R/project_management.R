#' @title Create a New metawoRld Project
#'
#' @description
#' Initializes the directory structure, configuration file, and Git repository
#' for a new metawoRld living review project.
#'
#' @param path Character string. The path where the new project directory should be created.
#'             The directory itself will be created and should not already exist.
#' @param project_name Character string. A short, descriptive name for the project.
#' @param project_description Character string. A longer description of the project's scope.
#' @param schema List. A list defining the structure for metadata and data files.
#'               If NULL (default), a standard default schema is used. See details.
#' @param git_init Logical. Initialize a Git repository in the project directory? Defaults to TRUE.
#' @param ... Additional key-value pairs to add to the root `_metawoRld.yml` file.
#'
#' @details
#' The function creates the main project directory at `path` and sets up the following:
#' \itemize{
#'   \item{`_metawoRld.yml`: The main configuration file containing project metadata
#'         and the data schema.}
#'   \item{`data/`: An empty directory to store study-specific subdirectories.}
#'   \item{`.git/`: If `git_init = TRUE`, initializes a Git repository.}
#'   \item{`.gitignore`: A standard R/.git/.quarto ignore file.}
#'   \item{`README.md`: A basic README file.}
#' }
#'
#' The default schema defines expected fields for `metadata.yml` and `data.csv` files
#' within each study folder. You can provide a custom schema using the `schema` argument.
#'
#' @return Invisibly returns the normalized path to the created project directory.
#' @export
#'
#' @importFrom fs dir_create file_exists path_norm dir_exists
#' @importFrom yaml write_yaml
#' @importFrom git2r init is_git_repository discover_repository
#' @importFrom rlang list2 check_installed %||% inform abort
#' @importFrom utils packageVersion
#'
#' @examples
#' \dontrun{
#' # Create a project in a temporary directory
#' proj_path <- file.path(tempdir(), "my_cytokine_review")
#'
#' create_metawoRld(
#'   path = proj_path,
#'   project_name = "Serum Cytokines in Pre-eclampsia",
#'   project_description = "A living review of serum cytokine levels associated with PE.",
#'   inclusion_criteria = c("Human studies", "Serum samples", "Preeclampsia outcome"),
#'   search_keywords = list(mandatory = c("cytokine", "serum", "preeclampsia"),
#'                          optional = c("pregnancy", "biomarker"))
#' )
#'
#' # List files in the new project
#' list.files(proj_path, recursive = TRUE)
#'
#' # Clean up
#' unlink(proj_path, recursive = TRUE)
#' }
create_metawoRld <- function(path,
                             project_name,
                             project_description,
                             schema = NULL,
                             git_init = TRUE,
                             ...) {

  # --- Input Validation ---
  if (missing(path) || !is.character(path) || length(path) != 1 || path == "") {
    rlang::abort("`path` must be a non-empty character string.")
  }
  if (missing(project_name) || !is.character(project_name) || length(project_name) != 1 || project_name == "") {
    rlang::abort("`project_name` must be a non-empty character string.")
  }
  if (missing(project_description) || !is.character(project_description) || length(project_description) != 1 || project_description == "") {
    rlang::abort("`project_description` must be a non-empty character string.")
  }
  if (!is.null(schema) && !is.list(schema)) {
    rlang::abort("`schema` must be NULL or a list.")
  }
  if (!is.logical(git_init) || length(git_init) != 1) {
    rlang::abort("`git_init` must be TRUE or FALSE.")
  }

  # Normalize path and check if directory exists
  proj_path <- fs::path_norm(path)
  if (fs::dir_exists(proj_path) || fs::file_exists(proj_path)) {
    rlang::abort(paste0("Project path '", proj_path, "' already exists."))
  }

  # --- Create Directories ---
  fs::dir_create(proj_path)
  fs::dir_create(fs::path(proj_path, "data"))
  rlang::inform(paste("Created project directory:", proj_path))
  rlang::inform(paste("Created data subdirectory:", fs::path(proj_path, "data")))

  # --- Define Default Schema (if none provided) ---
  default_schema <- list(
    metadata_fields = list(
      required = c("study_id", "title", "authors", "year", "journal", "study_design",
                   "country", "sample_type", "outcome_groups", "measurement_methods"),
      optional = c("doi", "abstract", "keywords_paper", "funding_source",
                   "ethics_approval", "inclusion_summary", "exclusion_summary",
                   "datafindr_assessment")
    ),
    data_fields = list(
      required = c("measurement_id", "method_ref_id", "cytokine_name", "group_label",
                   "gestational_age_timing", "n", "statistic_type", "value1"),
      # value2 is not strictly required if statistic_type is e.g. 'count' or 'mean' only
      optional = c("value2", "unit", # Unit often comes from method_ref now, but can override
                   "gestational_age_weeks_mean", "gestational_age_weeks_sd",
                   "comparison_group_label", "comparison_p_value", "notes")
    ),
    # Suggest structure for complex fields within metadata.yml
    complex_field_structures = list(
      outcome_groups = "Should be a list/map where keys are group IDs (e.g., 'grp1', 'grp2') and values are lists containing 'name', 'definition', etc.",
      measurement_methods = "Should be a list/map where keys are method reference IDs (e.g., 'elisa_il6') and values are lists containing 'analysis_type', 'target_cytokine', 'unit', 'kit_manufacturer', etc.",
      datafindr_assessment = "Should be a list/map containing fields like 'relevance_score', 'rationale', 'model_used', 'extraction_date'."
    )
  )

  active_schema <- schema %||% default_schema # Use provided schema or default

  # --- Create _metawoRld.yml ---
  config_list <- rlang::list2(
    project_name = project_name,
    project_description = project_description,
    creation_date = as.character(Sys.Date()),
    metawoRld_version = as.character(utils::packageVersion("metawoRld")), # Add package name dynamically if possible
    schema = active_schema,
    ... # Include any additional arguments passed
  )

  config_path <- fs::path(proj_path, "_metawoRld.yml")
  yaml::write_yaml(config_list, file = config_path)
  rlang::inform(paste("Created configuration file:", config_path))

  # --- Initialize Git ---
  if (git_init) {
    rlang::check_installed("git2r", reason = "for initializing a Git repository.")
    tryCatch({
      # Check if it's already inside a git repo - avoid nested repos by default?
      # For simplicity now, we just init if requested. User can manage nesting.
      repo <- git2r::init(proj_path)
      rlang::inform(paste("Initialized Git repository in:", proj_path))

      # --- Create .gitignore ---
      # Basic gitignore for R projects
      gitignore_content <- c(
        "# R specific files",
        ".Rproj.user/",
        ".Rhistory",
        ".RData",
        ".Ruserdata",
        "",
        "# Package development files",
        "/*.Rproj", # Usually want to keep the main .Rproj
        "man/",
        "NAMESPACE",
        "src/*.o",
        "src/*.so",
        "src/*.dll",
        "vignettes/*.html",
        "vignettes/*.pdf",
        "vignettes/.Rbuildignore",
        "",
        "# Output files",
        "docs/",
        "_site/",
        "_book/",
        "*.html",
        "*.pdf",
        "",
        "# Temporary files",
        "*~",
        ".DS_Store"
      )
      gitignore_path <- fs::path(proj_path, ".gitignore")
      writeLines(gitignore_content, con = gitignore_path)
      rlang::inform(paste("Created .gitignore file:", gitignore_path))

      # Optional: Initial commit (consider if this is too presumptive)
      # git2r::add(repo, c("_metawoRld.yml", ".gitignore", "data/")) # data/ might not stage empty dir
      # git2r::commit(repo, message = "Initial commit: Create metawoRld project structure")
      # inform("Created initial Git commit.")

    }, error = function(e) {
      rlang::warn(paste("Failed to initialize Git repository:", e$message))
    })
  }

  # --- Create README.md ---
  readme_content <- c(
    paste("#", project_name),
    "",
    project_description,
    "",
    paste("This project was created using the `metawoRld` R package on", Sys.Date(), "."),
    "",
    "The `data/` directory contains subdirectories for each study included in this review.",
    "Each study directory should contain:",
    "* `metadata.yml`: Study design, groups, methods, etc.",
    "* `data.csv`: Quantitative cytokine measurements.",
    "",
    "The overall project configuration and data schema are defined in `_metawoRld.yml`."
  )
  readme_path <- fs::path(proj_path, "README.md")
  writeLines(readme_content, con = readme_path)
  rlang::inform(paste("Created README.md file:", readme_path))

  # --- Add Quarto Website Template ---
  tryCatch({
    .add_quarto_website_template(
      proj_path = proj_path,
      project_name = project_name,
      project_description = project_description
    )
  }, error = function(e) {
    # Don't abort project creation if Quarto files fail, just warn loudly.
    rlang::warn(glue::glue(
      "Failed to create Quarto website template files. ",
      "Project structure created, but website files are missing. ",
      "Error: {e$message}"
    ))
  })

  # --- Return Path ---
  invisible(proj_path)
}

#' @title Get the Schema from a metawoRld Project
#'
#' @description Reads the `_metawoRld.yml` configuration file from a project
#' directory and returns the defined schema.
#'
#' @param path Character string. The path to the root directory of the
#'   metawoRld project. Defaults to the current working directory (`.`).
#'
#' @return A list representing the schema defined in the project's
#'   `_metawoRld.yml` file. This typically includes definitions for
#'   `metadata_fields` and `data_fields`. Returns `NULL` invisibly and issues
#'   a warning if the schema cannot be retrieved.
#'
#' @export
#'
#' @importFrom fs path dir_exists file_exists
#' @importFrom yaml read_yaml yaml.load_file
#' @importFrom rlang warn abort is_list
#'
#' @examples
#' \dontrun{
#' # --- Setup: Create a temporary project ---
#' proj_path <- file.path(tempdir(), "get_schema_test")
#' create_metawoRld(
#'   path = proj_path,
#'   project_name = "Schema Test",
#'   project_description = "Testing get_schema()"
#' )
#'
#' # --- Example Usage ---
#' # Get the schema from the created project
#' project_schema <- get_schema(proj_path)
#' print(project_schema)
#'
#' # Check specific parts of the schema
#' print(project_schema$metadata_fields$required)
#'
#' # --- Clean up ---
#' unlink(proj_path, recursive = TRUE)
#' }
get_schema <- function(path = ".") {

  # --- Input Validation and Path Handling ---
  if (!is.character(path) || length(path) != 1 || path == "") {
    rlang::abort("`path` must be a non-empty character string.")
  }
  proj_path <- fs::path_norm(path)
  if (!fs::dir_exists(proj_path)) {
    rlang::abort(paste("Project path does not exist or is not a directory:", proj_path))
  }

  config_path <- fs::path(proj_path, "_metawoRld.yml")
  if (!fs::file_exists(config_path)) {
    rlang::abort(paste("Configuration file not found:", config_path))
  }

  # --- Read YAML and Extract Schema ---
  config_list <- tryCatch({
    # Use yaml.load_file for potentially better error messages on malformed YAML
    yaml::yaml.load_file(config_path)
  }, error = function(e) {
    rlang::abort(paste("Failed to read or parse YAML file:", config_path, "\nOriginal error:", e$message))
  })

  if (!rlang::is_list(config_list)) {
    rlang::abort(paste("Content of", config_path, "is not a valid YAML list/map."))
  }

  if (!"schema" %in% names(config_list)) {
    rlang::warn(paste("'_metawoRld.yml' does not contain a 'schema' key in:", proj_path))
    return(invisible(NULL))
  }

  schema_content <- config_list$schema

  if (!rlang::is_list(schema_content)) {
    rlang::warn(paste("'schema' key in '_metawoRld.yml' does not contain a list structure in:", proj_path))
    return(invisible(NULL))
  }

  # --- Return Schema ---
  return(schema_content)
}

#' @title Add Quarto Website Template Files
#'
#' @description
#' Creates the basic files (`_quarto.yml`, `index.qmd`, `studies.qmd`) for a
#' simple Quarto website structure within the project directory.
#'
#' @param proj_path Path to the project root directory.
#' @param project_name The name of the project.
#' @param project_description The description of the project.
#'
#' @return NULL (invisibly). Called for side effects (file creation).
#' @noRd
#' @keywords internal
#'
#' @importFrom fs path file_exists
#' @importFrom rlang inform warn abort
#' @importFrom glue glue
.add_quarto_website_template <- function(proj_path, project_name, project_description) {

  # --- Define File Paths ---
  quarto_yml_path <- fs::path(proj_path, "_quarto.yml")
  index_qmd_path <- fs::path(proj_path, "index.qmd")
  studies_qmd_path <- fs::path(proj_path, "studies.qmd")

  # --- Content for _quarto.yml ---
  quarto_yml_content <- glue::glue(
'project:
  type: website
  output-dir: docs # Standard output directory

website:
  title: "{project_name}"
  navbar:
    left:
      - href: index.qmd
        text: Home
      - href: studies.qmd
        text: Studies
  page-footer:
    right: "Built with Quarto and metawoRld"
    left: "&copy; Copyright {format(Sys.Date(), \'%Y\')}" # Add year dynamically

format:
  html:
    theme: cosmo # A clean default theme
    css: styles.css
    toc: true

# Optional: Add knitr options if needed globally
# execute:
#   freeze: auto # Re-render only when code changes
', .trim = FALSE) # Prevent glue from trimming trailing newlines needed by YAML

  # --- Content for index.qmd ---
  index_qmd_content <- glue::glue(
'---
title: "Welcome"
---

## {project_name}

{project_description}

This Quarto website provides a browsable interface to the data collected for this living review.

*   Navigate to the **Studies** page to see a list of included studies and their key details.
*   The data is stored in a structured format within the `data/` directory of this project.
*   This website can be updated by rendering the Quarto project after adding new studies.

*To render the website, open this project in RStudio and click the "Render Website" button, or run `quarto::quarto_render(".")` in the R console from the project root directory.*
', .trim = FALSE)

  # --- Content for studies.qmd ---
  # This uses Quarto markdown and an R code chunk
studies_qmd_content <- '---
title: "Included Studies"
---

This page lists the studies currently included in the review. Click on a study ID to expand its details.

```{r load-and-display-studies, echo=FALSE, warning=FALSE, message=FALSE, results=\'asis\'}
# Load the metawoRld package (assuming it\'s installed)
# If running during development, use devtools::load_all() beforehand in console
if (!requireNamespace("metawoRld", quietly = TRUE)) {
  warning("metawoRld package not found. Please install it.")
} else if (!requireNamespace("dplyr", quietly = TRUE)) {
   warning("dplyr package not found. Please install it.")
} else if (!requireNamespace("purrr", quietly = TRUE)) {
   warning("purrr package not found. Please install it.")
} else {

  # Load project data
  # The path "." assumes rendering from the project root directory
  project_data <- metawoRld::load_metawoRld(".", verbose = FALSE)

  if (is.null(project_data) || length(project_data$studies_metadata) == 0) {
    cat("No studies have been loaded into the project yet.")
  } else {
    metadata_list <- project_data$studies_metadata
    study_ids <- names(metadata_list)

    cat("\\n") # Add newline before accordion

    # Start accordion group
    cat("::: {.accordion}\n\\n")

    # Loop through each study
    for (s_id in study_ids) {
      meta <- metadata_list[[s_id]]

      # Use helper %||% NA_character_ for safe access
      na_if_null <- function(x) if(is.null(x) || length(x) == 0) NA_character_ else x
      `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

      title <- meta$title %||% "N/A"
      authors <- paste(unlist(meta$authors %||% "N/A"), collapse = ", ")
      year <- meta$year %||% "N/A"
      journal <- meta$journal %||% "N/A"
      design <- meta$study_design %||% "N/A"
      country <- meta$country %||% "N/A"
      outcome_groups <- names(meta$outcome_groups %||% list(key="N/A")) # Get keys/names
      outcome_group_names <- purrr::map_chr(meta$outcome_groups %||% list(), ~.x$name %||% "N/A")

      # Start accordion panel for this study
      cat(paste0("## Study ID: ", s_id, "\\n"))
      cat(":::{.panel-body}\\n") # Add panel-body for potential styling

      # Print details using markdown
      cat(paste0("**Title:** ", title, "\\n"))
      cat(paste0("**Authors:** ", authors, "\\n"))
      cat(paste0("**Year:** ", year, "\\n"))
      cat(paste0("**Journal:** ", journal, "\\n"))
      cat(paste0("**Study Design:** ", design, "\\n"))
      cat(paste0("**Country:** ", country, "\\n"))
      cat(paste0("**Outcome Groups:**\\n\\n"))
      for (i in seq_along(outcome_groups)) {
        cat(paste0("* `", outcome_groups[i], "`: ", outcome_group_names[i], "\\n"))
      }
      # Add more fields as needed here...

      cat(":::\\n") # Close panel-body
      cat("\\n") # Add space before next accordion item

    } # End loop through studies

    # End accordion group
    cat(":::\\n")

  } # End else (data loaded)
} # End else (packages loaded)
```' # End of studies_qmd_content string

  # --- Content for styles.css (optional, basic styling) ---
  css_content <- '/* Add custom CSS styles here if needed */

    .accordion-button:not(.collapsed) {
      /* Style for the open accordion header */
        color: #0d6efd; /* Example: Bootstrap primary blue */
        background-color: #e7f1ff; /* Lighter blue background */
    }

  .accordion-body .panel-body {
    /* Style for the content area within the accordion */
      padding-top: 10px;
  }

  /* Increase spacing between list items */
    .accordion-body ul li {
      margin-bottom: 5px;
    }
  '
  css_path <- fs::path(proj_path, "styles.css")


  # --- Write Files ---
  tryCatch({
    writeLines(quarto_yml_content, quarto_yml_path)
    rlang::inform(glue::glue("Created Quarto configuration: {basename(quarto_yml_path)}"))

    writeLines(index_qmd_content, index_qmd_path)
    rlang::inform(glue::glue("Created Quarto index page: {basename(index_qmd_path)}"))

    writeLines(studies_qmd_content, studies_qmd_path)
    rlang::inform(glue::glue("Created Quarto studies page: {basename(studies_qmd_path)}"))

    writeLines(css_content, css_path)
     rlang::inform(glue::glue("Created basic CSS file: {basename(css_path)}"))

  }, error = function(e) {
    # Clean up partially created files? Maybe not necessary.
    rlang::abort(glue::glue("Failed to write Quarto template files: {e$message}"))
  })

  invisible(NULL)
}
