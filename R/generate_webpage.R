#' Generate Base Project Webpage Files from Template
#'
#' @description
#' Internal helper function to copy and process base Quarto template files
#' (like `_quarto.yml`, `index.qmd`) into a project directory.
#' It uses `glue` to inject project-specific variables into the templates.
#'
#' @importFrom glue glue
#' @importFrom fs dir_create path file_exists dir_copy file_copy
#' @importFrom readr read_file write_file
#' @importFrom utils browseURL
#'
#' @param proj_path Path to the metawoRld project.
#' @param output_dir Directory where the processed template files are written.
#'   Defaults to `proj_path`.
#' @param template_folder Name of the base template folder within `inst/`
#'   (e.g., "quarto_base_templates").
#' @param render Logical. If TRUE, attempts to render the Quarto site/document.
#' @param open Logical. If TRUE and `render` is TRUE, attempts to open the
#'   rendered output.
#' @param overwrite Logical. If TRUE, existing files in `output_dir` are
#'   overwritten. If FALSE, existing files are skipped.
#' @param ... Additional arguments passed to `glue::glue()` for template filling.
#'
#' @return Invisibly returns the render target path or the `output_dir`.
#' @keywords internal
#' @noRd
.generate_webpage <- function(proj_path = ".",
                             output_dir = NA,
                             template_folder = "quarto_base_templates",
                             render = FALSE,
                             open = render,
                             overwrite = FALSE,
                             ...) {

  if (is.na(output_dir)) {
    output_dir <- proj_path
  }

  # --- Input Validation ---
  # (Add checks for inputs like data, output_dir existence, template_name validity)
  stopifnot(is.character(output_dir), length(output_dir) == 1)

  config_path <- fs::path(proj_path, "_metawoRld.yml")
  config_data <- yaml::read_yaml(config_path)

  # --- Find Template Directory ---
  template_dir <- system.file(
    fs::path(template_folder),
    package = "metawoRld"
  )

  if (template_dir == "" || !fs::dir_exists(template_dir)) {
    stop("`template_dir` not found!")
  }

  # --- Prepare Output Directory ---
  if (!fs::dir_exists(output_dir)) {
    fs::dir_create(output_dir)
    output_dir <- normalizePath(output_dir)
  }

  file.copy(
    from = fs::path(system.file(
      fs::path("quarto_study_templates"),
      package = "metawoRld"
    ), "study.qmd.tmpl"),
    to = fs::path(output_dir, "study.qmd.tmpl"),
    overwrite = FALSE
  )

  # --- Prepare Data for Glue ---
  # Combine provided data and any extra arguments (...)
  # Be careful about name collisions, 'data' list might be safer
  glue_data <- c(
    list(
      timestamp = Sys.time(),
      current_year = format(Sys.Date(), "%Y"),
      project_name = config_data$project_name,
      project_description = config_data$project_description
    ),
    list(...)
  )

  # --- Process Templates (Copy/Glue) ---
  qmd_files_to_render <- character() # Keep track of main qmd files

  template_files <- list.files(template_dir, full.names = TRUE)
  if (!overwrite) {
    existing_files <- list.files(output_dir, full.names = TRUE)
    template_files <- template_files[!basename(template_files) %in% basename(existing_files)]
  }

  for (template_file_path in template_files) {
    output_file_path <- fs::path(output_dir, basename(template_file_path))

    # Create subdirectory in output if needed
    output_file_dir <- dirname(output_file_path)
    if (!fs::dir_exists(output_file_dir)) {
      fs::dir_create(output_file_dir)
    }

    # Check file type - only glue .qmd and potentially .yml files
    if (grepl("\\.(qmd|yml|yaml)$", template_file_path, ignore.case = TRUE)) {
      message("Gluing: ", basename(template_file_path))
      # Read template content
      template_content <- readr::read_file(template_file_path)

      # Fill template with glue
      filled_content <- glue::glue(template_content, .envir = as.environment(glue_data),
                                   .open = "{{", .close = "}}")

      # Write filled content to output directory
      readr::write_file(filled_content, output_file_path)

      # Identify the main qmd file(s) for rendering later
      # Heuristic: index.qmd or the only .qmd file if it's not a project structure
      if (grepl("index\\.qmd$", output_file_path, ignore.case=TRUE) ||
          (grepl("\\.qmd$", output_file_path, ignore.case=TRUE) && !fs::file_exists(fs::path(output_dir, "_quarto.yml")))) {
        qmd_files_to_render <- c(qmd_files_to_render, output_file_path)
      }


    } else {
      # Copy other files (CSS, images, etc.) directly
      message("Copying: ", basename(template_file_path))
      fs::file_copy(template_file_path, output_file_path, overwrite = TRUE)
    }
  }

  # --- Render (Optional) ---
  render_target <- NULL
  if (fs::file_exists(fs::path(output_dir, "_quarto.yml"))) {
    # It's a project, render the directory
    render_target <- output_dir
    message("Identified Quarto Project at: ", render_target)
  } else if (length(qmd_files_to_render) > 0) {
    # Render the specific qmd file(s) found (usually just one)
    # If multiple non-index qmd files exist without _quarto.yml, this might need refinement
    render_target <- qmd_files_to_render[1] # Render the first one found (e.g., template.qmd)
    message("Identified Quarto Document: ", render_target)
  } else {
    warning("No '_quarto.yml' or primary '.qmd' file found in the processed template output. Cannot determine rendering target.")
  }


  if (render && !is.null(render_target)) {
    # Check if quarto package is available
    if (!requireNamespace("quarto", quietly = TRUE)) {
      warning("Package 'quarto' needed for rendering but is not installed. Skipping rendering.", call. = FALSE)
    } else {
      message("Rendering Quarto output in: ", render_target)
      # Ensure quarto is findable
      # quarto_path <- Sys.getenv("QUARTO_PATH") # Or use quarto::quarto_path()
      # if (quarto_path == "" || !file.exists(quarto_path)) {
      #    warning("Quarto binary not found. Skipping rendering. Set QUARTO_PATH or install Quarto CLI.")
      # } else {
      tryCatch({
        quarto::quarto_render(input = render_target, quiet = FALSE) # Set quiet=TRUE for less verbose output
        message("Rendering complete.")
        if (open) {
          # Try to find the output file/index.html to open
          output_html <- fs::path(output_dir, "_site", "index.html") # Project default
          if (!fs::file_exists(output_html) && grepl("\\.qmd$", render_target)) {
            output_html <- sub("\\.qmd$", ".html", render_target) # Single file default
          }
          if (fs::file_exists(output_html)) {
            utils::browseURL(output_html)
          } else {
            message("Could not determine output HTML file to open.")
          }
        }
      }, error = function(e) {
        warning("Quarto rendering failed: ", e$message, call. = FALSE)
      })
      # }
    }
  } else if (render && is.null(render_target)) {
    warning("Rendering requested but no target file/project could be determined.")
  }


  # --- Return Output Path ---
  invisible(render_target %||% output_dir) # Return render target or output dir
}

#' Generate or Update the Full Project Website
#'
#' @description
#' This is a high-level function that orchestrates the generation of all
#' components of the metawoRld project's Quarto website. It is designed
#' to be used in automated contexts like GitHub Actions.
#'
#' The function performs the following steps:
#' 1. Generates the base website structure (e.g., `_quarto.yml`, `index.qmd`)
#'    using the project's configuration, overwriting existing files to ensure
#'    the site is up-to-date with the current package templates.
#' 2. Generates individual `.qmd` pages for each study found in the project.
#' 3. Renders the entire Quarto project to produce the final website (e.g., in `_site/`).
#'
#' @param proj_path Path to the metawoRld project. Defaults to the current
#'   working directory (`.`).
#' @param render Logical. If `TRUE` (the default), the function will call
#'   `quarto::quarto_render()` to build the website after generating the source
#'   files. Requires the `quarto` package.
#'
#' @return Invisibly returns the project path. The primary effect is the
#'   creation and rendering of website files.
#' @export
#'
#' @examples
#' \dontrun{
#' # Setup a temporary project for demonstration
#' temp_dir <- tempfile("metawoRld-website")
#' create_metawoRld(
#'   path = temp_dir,
#'   project_name = "Website Example",
#'   project_description = "A project to demonstrate website generation."
#' )
#'
#' # Generate the full website
#' generate_website(proj_path = temp_dir)
#'
#' # Check if the output directory exists
#' site_dir <- file.path(temp_dir, "_site")
#' print(paste("Website generated in:", site_dir, "-", dir.exists(site_dir)))
#'
#' # Clean up
#' unlink(temp_dir, recursive = TRUE)
#' }
generate_website <- function(proj_path = ".", render = TRUE) {

  # Step 1: Generate base files, force overwrite
  .generate_webpage(proj_path = proj_path, overwrite = TRUE, render = FALSE)

  # Step 2: Generate individual study pages
  generate_study_webpage(proj_path = proj_path)

  # Step 3: Render the full quarto site
  if (render) {
    if (!requireNamespace("quarto", quietly = TRUE)) {
      warning("The 'quarto' package is not installed. Cannot render the website. ",
              "Please install it with: install.packages('quarto')",
              call. = FALSE)
      return(invisible(proj_path))
    }
    message("Rendering Quarto website...")
    quarto::quarto_render(input = proj_path, as_job = FALSE)
    message("Website rendering complete.")
  }

  invisible(proj_path)
}

#' Generate Individual Study Webpages
#'
#' @title Generate Individual Study Webpages for a metawoRld Project
#'
#' @description
#' This function iterates through all studies within a `metawoRld` project.
#' For each study, it uses the `study.qmd.tmpl` template and the study's
#' metadata to generate an individual Quarto (`.qmd`) file. These files
#' are saved in a `study/` subdirectory within the project.
#'
#' The generated `.qmd` files can then be rendered as part of the overall
#' project website, allowing each study to have its own descriptive page.
#'
#' @importFrom readr read_file write_file
#' @importFrom fs path dir_create
#' @importFrom glue glue
#'
#' @param proj_path Character string. The path to the root directory of the
#'   metawoRld project. Defaults to the current working directory (`.`).
#'
#' @return Invisibly returns the path to the `study` directory where
#'   webpages are generated. The primary effect is file creation.
#' @export
#'
#' @seealso
#' \\code{\\link{create_metawoRld}} for creating a new project.
#' \\code{\\link{load_metawoRld}} for loading project data which is used by this function.
#' \\code{\\link{.generate_webpage}} for generating the main project website files.
#'
#' @examples
#' \dontrun{
#' # --- Setup: Create a temporary project and add a study ---
#' temp_proj_path <- file.path(tempdir(), "study_page_test")
#' create_metawoRld(
#'   path = temp_proj_path,
#'   project_name = "Study Webpage Test",
#'   project_description = "Testing generate_study_webpage()"
#' )
#'
#' # Add some dummy study data (required for the function to work)
#' study_meta <- list(
#'   study_id = "Study01", title = "My First Study",
#'   authors = list("Author A", "Author B"), year = 2023,
#'   journal = "Journal of Examples", study_design = "Cohort",
#'   country = "R Land", sample_type = "Virtual",
#'   abstract = "This is a sample abstract for the study.",
#'   outcome_groups = list(
#'     og1 = list(name = "Group 1", definition = "Participants in group 1.")
#'   ),
#'   measurement_methods = list(
#'     mm1 = list(analysis_type = "Simulated", unit = "units")
#'   )
#' )
#' study_data_df <- data.frame(
#'   measurement_id = "m1",
#'   method_ref_id = "mm1",
#'   cytokine_name = "FakoStat",
#'   outcome_group_ref_id = "og1",
#'   n = 10,
#'   value1 = 5,
#'   statistic_type = "mean_sd"
#' )
#' add_study_data(temp_proj_path, "Study01", study_meta, study_data_df)
#'
#' # Generate the webpage for this study
#' generate_study_webpage(proj_path = temp_proj_path)
#'
#' # Check for the created file:
#' study_page_file <- file.path(temp_proj_path, "study", "Study01.qmd")
#' print(paste("Study page generated:", fs::file_exists(study_page_file)))
#' if (fs::file_exists(study_page_file)) {
#'   # cat(readLines(study_page_file), sep = "
") # View content
#' }
#'
#' # --- Clean up ---
#' unlink(temp_proj_path, recursive = TRUE)
#' }
generate_study_webpage <- function(proj_path = ".") {

  template_content <- readr::read_file(fs::path(proj_path, "study.qmd.tmpl"))

  output_file_dir <- fs::path(proj_path, "study")

  df <- metawoRld::load_metawoRld(proj_path, verbose = FALSE)
  study_list <- df$studies_metadata

  if (!fs::dir_exists(output_file_dir)) {
    fs::dir_create(output_file_dir)
  }

  format_study_groups <- function(outcome_groups) {
    sapply(outcome_groups, function(group) {
      paste0("#### ", group$name, "\n\n", group$definition)
    }, USE.NAMES = FALSE) %>%
      paste(collapse = "\n\n")
  }

  format_methods <- function(measurement_methods) {
    sapply(measurement_methods, function(method) {
      paste0("#### ", method$analysis_type %||% "", "\n\n", method$kit_manufacturer %||% "")
    }, USE.NAMES = FALSE) %>%
      paste(collapse = "\n\n")
  }

  for (x in names(study_list)) {
    message("Gluing: ", x)
    output_file_path <- fs::path(proj_path, "study", paste0(.sanitize_id(x), ".qmd"))

    glue_data <- list(
      timestamp = Sys.time(),
      current_year = format(Sys.Date(), "%Y"),
      title = study_list[[x]]$title,
      data_path = fs::path(proj_path, "data", .sanitize_id(x), "data.csv"),
      abstract = study_list[[x]]$abstract %||% "",
      authors = paste(study_list[[x]]$authors, collapse = ", "),
      sample_type = study_list[[x]]$sample_type %||% "",
      methods_formatted = format_methods(study_list[[x]]$measurement_methods),
      study_group_formatted = format_study_groups(study_list[[x]]$outcome_groups)
    )

    # Fill template with glue
    filled_content <- glue::glue(template_content, .envir = as.environment(glue_data),
                                 .open = "{{", .close = "}}")

    # Write filled content to output directory
    readr::write_file(filled_content, output_file_path)
  }
}
