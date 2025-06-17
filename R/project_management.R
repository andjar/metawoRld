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
#' @seealso
#' \\code{\\link{add_study_data}} for adding data to the project,
#' \\code{\\link{load_metawoRld}} for loading data from the project,
#' \\code{\\link{get_schema}} for retrieving project schemas,
#' \\code{\\link{generate_study_webpage}} for creating a project website.
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

  # --- Copy DataFindR files ---
  DataFindR::copy_datafindr_files(
    path = proj_path,
    overwrite = FALSE
  )

  # --- Create _metawoRld.yml ---
  config_list <- rlang::list2(
    project_name = project_name,
    project_description = project_description,
    creation_date = as.character(Sys.Date()),
    metawoRld_version = as.character(utils::packageVersion("metawoRld")), # Add package name dynamically if possible
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
    .generate_webpage(
      proj_path = proj_path
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
#' @description Reads the specified schema configuration file (either `_extraction_schema.yml` or `_assessment_schema.yml`) from a project directory and returns its content.
#'
#' @param path Character string. The path to the root directory of the
#'   metawoRld project. Defaults to the current working directory (`.`).
#' @param schema Character string. Either `extraction` (default) or `assessment`.
#'
#' @return A list representing the schema defined in the specified project schema file (e.g., `_extraction_schema.yml` or `_assessment_schema.yml`). Returns `NULL` invisibly and issues a warning if the schema cannot be retrieved.
#'
#' @seealso
#' \\code{\\link{create_metawoRld}} for setting up a project which includes schema files.
#' @export
#'
#' @importFrom fs path dir_exists file_exists
#' @importFrom yaml read_yaml yaml.load_file
#' @importFrom rlang warn abort is_list
get_schema <- function(path = ".", schema = c("extraction", "assessment")) {

  schema <- match.arg(schema)

  # --- Input Validation and Path Handling ---
  if (!is.character(path) || length(path) != 1 || path == "") {
    rlang::abort("`path` must be a non-empty character string.")
  }
  proj_path <- fs::path_norm(path)
  if (!fs::dir_exists(proj_path)) {
    rlang::abort(paste("Project path does not exist or is not a directory:", proj_path))
  }

  schema_paths = list("extraction" = "_extraction_schema.yml",
                      "assessment" = "_assessment_schema.yml"
  )

  config_path <- fs::path(proj_path, schema_paths[[schema]])
  if (!fs::file_exists(config_path)) {
    rlang::abort(paste("Configuration file not found:", config_path))
  }

  # --- Read YAML and Extract Schema ---
  schema_content <- tryCatch({
    # Use yaml.load_file for potentially better error messages on malformed YAML
    yaml::yaml.load_file(config_path)
  }, error = function(e) {
    rlang::abort(paste("Failed to read or parse YAML file:", config_path, "\nOriginal error:", e$message))
  })

  if (!rlang::is_list(schema_content)) {
    rlang::abort(paste("Content of", config_path, "is not a valid YAML list/map."))
  }

  # --- Return Schema ---
  return(schema_content)
}
