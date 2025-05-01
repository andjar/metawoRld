#' Generate a Quarto Webpage from a Template
#'
#' Creates and optionally renders a Quarto webpage using package templates.
#'
#' @param data Your input data or parameters needed for the report.
#' @param output_dir The directory where the Quarto files and rendered output
#'   should be saved.
#' @param template_name The name of the template directory within
#'   `inst/quarto_templates` (e.g., "basic_report", "full_website").
#' @param render Should the Quarto project/document be rendered? (Requires
#'   the quarto package and Quarto CLI). Defaults to TRUE.
#' @param open Should the rendered output be opened? Defaults to `render`.
#' @param ... Additional arguments passed to glue::glue() for template filling.
#'
#' @return Invisibly returns the path to the main output file or directory.
#'
#' @importFrom glue glue
#' @importFrom fs dir_create path file_exists dir_copy file_copy
#' @importFrom readr read_file write_file
#'
#' @examples
#' \dontrun{
#'   # Create a temporary directory for output
#'   temp_dir <- tempdir()
#'   my_output_dir <- file.path(temp_dir, "my_report")
#'
#'   generate_webpage(
#'     data = list(title = "My Dynamic Report", value = 42),
#'     output_dir = my_output_dir,
#'     template_name = "basic_report",
#'     report_date = Sys.Date() # Example of passing extra vars via ...
#'   )
#'
#'   # Clean up
#'   unlink(my_output_dir, recursive = TRUE)
#' }
.generate_webpage <- function(proj_path = ".",
                             output_dir = NA,
                             template_folder = "quarto_base_templates",
                             render = FALSE,
                             open = render,
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
    from = fs::path(template_dir, "quarto_study_templates", "study.qmd.tmpl"),
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
  existing_files <- list.files(output_dir, full.names = TRUE)
  template_files <- template_files[!basename(template_files) %in% basename(existing_files)]

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

#' Generate a Quarto Webpage from a Template
#'
#' Creates and optionally renders a Quarto webpage using package templates.
#'
#' @param data Your input data or parameters needed for the report.
#' @param output_dir The directory where the Quarto files and rendered output
#'   should be saved.
#' @param template_name The name of the template directory within
#'   `inst/quarto_templates` (e.g., "basic_report", "full_website").
#' @param render Should the Quarto project/document be rendered? (Requires
#'   the quarto package and Quarto CLI). Defaults to TRUE.
#' @param open Should the rendered output be opened? Defaults to `render`.
#' @param ... Additional arguments passed to glue::glue() for template filling.
#'
#' @return Invisibly returns the path to the main output file or directory.
#' @export
#' @importFrom glue glue
#' @importFrom fs dir_create path file_exists dir_copy file_copy
#' @importFrom readr read_file write_file
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
