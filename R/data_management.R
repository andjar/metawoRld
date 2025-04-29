# --- Internal Helper Functions for Validation ---

#' Validate study metadata against schema
#' @noRd
#' @keywords internal
.validate_metadata <- function(metadata_list, schema, study_id) {
  if (!rlang::is_list(metadata_list)) {
    rlang::abort(glue::glue("Metadata for study '{study_id}' must be provided as a list."))
  }
  if (!"metadata_fields" %in% names(schema)) {
    rlang::abort("Schema is missing the 'metadata_fields' definition.")
  }
  fields_def <- schema$metadata_fields
  required <- fields_def$required %||% character() # Use %||% for safety if NULL
  optional <- fields_def$optional %||% character()
  all_defined <- c(required, optional)
  provided <- names(metadata_list)

  # Check required fields
  missing_req <- setdiff(required, provided)
  if (length(missing_req) > 0) {
    rlang::abort(glue::glue("Metadata for study '{study_id}' is missing required fields: {paste(missing_req, collapse=', ')}"))
  }

  # Check for unexpected fields (optional, could be a warning)
  unexpected <- setdiff(provided, all_defined)
  if (length(unexpected) > 0) {
    rlang::warn(glue::glue("Metadata for study '{study_id}' contains fields not defined in schema: {paste(unexpected, collapse=', ')}"))
  }

  # Basic check for required complex structures if defined
  if ("measurement_methods" %in% required && (!"measurement_methods" %in% provided || !rlang::is_list(metadata_list$measurement_methods))) {
    rlang::abort(glue::glue("Required field 'measurement_methods' for study '{study_id}' is missing or not a list."))
  }
  if ("outcome_groups" %in% required && (!"outcome_groups" %in% provided || !rlang::is_list(metadata_list$outcome_groups))) {
    rlang::abort(glue::glue("Required field 'outcome_groups' for study '{study_id}' is missing or not a list."))
  }

  # Could add more checks here (e.g., types) later if needed

  invisible(TRUE) # Return TRUE on success
}

#' Validate study data.frame against schema
#' @noRd
#' @keywords internal
.validate_data_df <- function(data_df, schema, study_id) {
  if (!is.data.frame(data_df)) {
    rlang::abort(glue::glue("Data for study '{study_id}' must be provided as a data frame."))
  }
  if (!"data_fields" %in% names(schema)) {
    rlang::abort("Schema is missing the 'data_fields' definition.")
  }
  fields_def <- schema$data_fields
  required <- fields_def$required %||% character()
  optional <- fields_def$optional %||% character()
  all_defined <- c(required, optional)
  provided_cols <- names(data_df)

  # Check required columns
  missing_req <- setdiff(required, provided_cols)
  if (length(missing_req) > 0) {
    rlang::abort(glue::glue("Data frame for study '{study_id}' is missing required columns: {paste(missing_req, collapse=', ')}"))
  }

  # Check for unexpected columns (optional, could be a warning)
  unexpected <- setdiff(provided_cols, all_defined)
  if (length(unexpected) > 0) {
    rlang::warn(glue::glue("Data frame for study '{study_id}' contains columns not defined in schema: {paste(unexpected, collapse=', ')}"))
  }

  # Could add column type checks here later

  invisible(TRUE) # Return TRUE on success
}

#' Validate linkage between data.df and metadata list
#' @noRd
#' @keywords internal
.validate_linkages <- function(data_df, metadata_list, schema, study_id) {
  # Check if required linking columns/structures exist
  if (!"method_ref_id" %in% names(data_df)) {
    rlang::warn(glue::glue("Data frame for study '{study_id}' is missing 'method_ref_id' column needed for linkage. Skipping linkage validation."))
    return(invisible(TRUE)) # Can't validate if column missing
  }
  if (!"measurement_methods" %in% names(metadata_list) || !rlang::is_list(metadata_list$measurement_methods)) {
    rlang::warn(glue::glue("Metadata for study '{study_id}' is missing 'measurement_methods' list needed for linkage. Skipping linkage validation."))
    return(invisible(TRUE)) # Can't validate if structure missing
  }
  if (!"group_label" %in% names(data_df)) {
    rlang::warn(glue::glue("Data frame for study '{study_id}' is missing 'group_label' column needed for linkage. Skipping linkage validation."))
    return(invisible(TRUE))
  }
  if (!"outcome_groups" %in% names(metadata_list) || !rlang::is_list(metadata_list$outcome_groups)) {
    rlang::warn(glue::glue("Metadata for study '{study_id}' is missing 'outcome_groups' list needed for linkage. Skipping linkage validation."))
    return(invisible(TRUE))
  }


  # --- Validate method_ref_id ---
  valid_method_ids <- names(metadata_list$measurement_methods)
  used_method_ids <- unique(stats::na.omit(data_df$method_ref_id)) # Ignore NAs for check
  invalid_methods <- setdiff(used_method_ids, valid_method_ids)

  if (length(invalid_methods) > 0) {
    rlang::abort(glue::glue(
      "Data for study '{study_id}' contains 'method_ref_id' values not found ",
      "as keys in metadata$measurement_methods: {paste(invalid_methods, collapse=', ')}"
    ))
  }

  # --- Validate group_label ---
  valid_group_labels <- names(metadata_list$outcome_groups)
  # Alternative: If outcome_groups is a list of lists, each with a 'name' field?
  # valid_group_labels <- vapply(metadata_list$outcome_groups, function(g) g$name %||% NA_character_, character(1))
  # Adapt based on the *actual* structure defined in your schema/usage
  if(is.null(valid_group_labels)){
    rlang::abort(glue::glue("Could not extract valid group labels (keys) from metadata$outcome_groups for study '{study_id}'. Ensure it's a named list."))
  }

  used_group_labels <- unique(stats::na.omit(data_df$group_label))
  invalid_groups <- setdiff(used_group_labels, valid_group_labels)

  if (length(invalid_groups) > 0) {
    rlang::abort(glue::glue(
      "Data for study '{study_id}' contains 'group_label' values not found ",
      "as keys in metadata$outcome_groups: {paste(invalid_groups, collapse=', ')}"
    ))
  }

  invisible(TRUE)
}

#' @title Add Study Data to a metawoRld Project
#'
#' @description
#' Adds the metadata (YAML) and quantitative data (CSV) for a single study
#' to the project, performing validation against the project's schema.
#'
#' @param path Character string. The path to the root directory of the
#'   metawoRld project. Defaults to the current working directory (`.`).
#' @param study_id Character string. A unique identifier for the study (e.g.,
#'   PMID, DOI, or a custom ID). This will be used as the directory name
#'   within the `data/` folder. Should be filesystem-friendly.
#' @param metadata_list List. A named list containing the study's metadata,
#'   conforming to the `metadata_fields` defined in the project's schema.
#'   Must include `measurement_methods` and `outcome_groups` as named lists
#'   if linkage validation is expected.
#' @param data_df Data frame. A data frame containing the quantitative data,
#'   conforming to the `data_fields` defined in the project's schema.
#'   Must include `method_ref_id` and `group_label` columns matching keys in
#'   `metadata_list` for linkage validation.
#' @param overwrite Logical. If a directory for `study_id` already exists,
#'   should its contents be overwritten? Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the created or updated study directory.
#' @export
#'
#' @importFrom fs path dir_exists file_exists dir_create path_norm file_copy
#' @importFrom yaml write_yaml
#' @importFrom readr write_csv
#' @importFrom rlang list2 %||% inform warn abort is_list is_character is_logical is_scalar_logical is_scalar_character
#' @importFrom glue glue
#' @importFrom tools file_path_sans_ext
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' # --- Setup: Create a temporary project ---
#' proj_path <- file.path(tempdir(), "add_study_test")
#' create_metawoRld(
#'   path = proj_path,
#'   project_name = "Add Study Test",
#'   project_description = "Testing add_study_data()"
#' )
#'
#' # --- Prepare Sample Data ---
#' study_meta <- list(
#'   study_id = "PMID12345",
#'   title = "Test Study One",
#'   authors = list("Doe J"), year = 2023, journal = "Test Journal",
#'   study_design = "Case-Control", country = "Testland", sample_type = "Serum",
#'   outcome_groups = list(
#'      grpA = list(name = "Cases", definition = "Has condition X"),
#'      grpB = list(name = "Controls", definition = "Healthy participants")
#'    ),
#'   measurement_methods = list(
#'     method1 = list(analysis_type = "ELISA", target_cytokine = "IL-TEST", unit = "pg/mL"),
#'     method2 = list(analysis_type = "LC-MS", target_cytokine = "CYTO-X", unit = "ng/mL")
#'    ),
#'   datafindr_assessment = list(relevance_score = 0.9, rationale = "Looks good")
#' )
#'
#' study_data <- data.frame(
#'   measurement_id = c("m1", "m2", "m3", "m4"),
#'   method_ref_id = c("method1", "method1", "method2", "method2"),
#'   cytokine_name = c("IL-TEST", "IL-TEST", "CYTO-X", "CYTO-X"),
#'   group_label = c("grpA", "grpB", "grpA", "grpB"),
#'   gestational_age_timing = rep("T2", 4),
#'   n = c(20, 40, 18, 35),
#'   statistic_type = rep("mean_sd", 4),
#'   value1 = c(10.1, 5.5, 105.2, 80.1),
#'   value2 = c(2.1, 1.5, 25.0, 15.5),
#'   notes = c("", "Lower detection limit", "", "")
#' )
#'
#' # --- Add the study data ---
#' add_study_data(
#'   path = proj_path,
#'   study_id = "PMID12345",
#'   metadata_list = study_meta,
#'   data_df = study_data
#' )
#'
#' # Verify files were created
#' list.files(file.path(proj_path, "data", "PMID12345"))
#'
#' # Try adding again without overwrite (should fail)
#' tryCatch(
#'   add_study_data(proj_path, "PMID12345", study_meta, study_data),
#'   error = function(e) print(e$message)
#' )
#'
#' # Add again with overwrite
#' add_study_data(
#'  path = proj_path,
#'  study_id = "PMID12345",
#'  metadata_list = study_meta, # Can modify data here if needed
#'  data_df = study_data,
#'  overwrite = TRUE
#' )
#'
#' # --- Example of failed validation ---
#' study_data_bad_link <- study_data
#' study_data_bad_link$method_ref_id[1] <- "method_BAD" # Non-existent method
#' tryCatch(
#'   add_study_data(proj_path, "PMID_bad_link", study_meta, study_data_bad_link),
#'   error = function(e) print(e$message)
#' )
#'
#' study_meta_missing_req <- study_meta
#' study_meta_missing_req$study_design <- NULL # Remove a required field
#' tryCatch(
#'   add_study_data(proj_path, "PMID_missing_req", study_meta_missing_req, study_data),
#'   error = function(e) print(e$message)
#' )
#'
#' # --- Clean up ---
#' unlink(proj_path, recursive = TRUE)
#' }
add_study_data <- function(path = ".",
                           study_id,
                           metadata_list,
                           data_df,
                           overwrite = FALSE) {

  # --- Input Type Validation ---
  if (!rlang::is_scalar_character(path) || path == "") {
    rlang::abort("`path` must be a non-empty character string.")
  }
  if (missing(study_id) || !rlang::is_scalar_character(study_id) || study_id == "") {
    rlang::abort("`study_id` must be a non-empty character string.")
  }
  if (missing(metadata_list) || !rlang::is_list(metadata_list)) {
    rlang::abort("`metadata_list` must be provided as a list.")
  }
  if (missing(data_df) || !is.data.frame(data_df)) {
    rlang::abort("`data_df` must be provided as a data frame.")
  }
  if (!rlang::is_scalar_logical(overwrite)) {
    rlang::abort("`overwrite` must be TRUE or FALSE.")
  }

  # --- Project and Schema Validation ---
  proj_path <- fs::path_norm(path)
  config_path <- fs::path(proj_path, "_metawoRld.yml")
  if (!fs::file_exists(config_path)) {
    rlang::abort(glue::glue("Project configuration file '_metawoRld.yml' not found in: {proj_path}"))
  }

  project_schema <- get_schema(proj_path)
  if (is.null(project_schema)) {
    rlang::abort(glue::glue("Could not retrieve schema from '_metawoRld.yml' in: {proj_path}"))
  }

  # --- Data Validation using Helpers ---
  # Wrap in tryCatch? No, let aborts propagate.
  .validate_metadata(metadata_list, project_schema, study_id)
  .validate_data_df(data_df, project_schema, study_id)
  .validate_linkages(data_df, metadata_list, project_schema, study_id)
  # If we reached here, validation passed

  # --- Directory Handling ---
  study_dir <- fs::path(proj_path, "data", .sanitize_id(study_id))

  if (fs::dir_exists(study_dir)) {
    if (!overwrite) {
      rlang::abort(glue::glue("Study directory already exists: {study_dir}. Use `overwrite = TRUE` to replace."))
    } else {
      rlang::inform(glue::glue("Overwriting existing study directory: {study_dir}"))
      # Optionally, could delete contents first, but write_yaml/write_csv will overwrite anyway
      # fs::dir_delete(study_dir) # Careful with this!
      # fs::dir_create(study_dir)
    }
  } else {
    fs::dir_create(study_dir)
    rlang::inform(glue::glue("Created study directory: {study_dir}"))
  }

  # --- Write Files ---
  metadata_file <- fs::path(study_dir, "metadata.yml")
  data_file <- fs::path(study_dir, "data.csv")

  tryCatch({
    yaml::write_yaml(metadata_list, file = metadata_file)
    rlang::inform(glue::glue("Written metadata to: {metadata_file}"))
  }, error = function(e) {
    rlang::abort(glue::glue("Failed to write metadata YAML for study '{study_id}': {e$message}"))
  })

  tryCatch({
    # Use na = "" to write NA values as empty strings in CSV
    readr::write_csv(data_df, file = data_file, na = "")
    rlang::inform(glue::glue("Written data to: {data_file}"))
  }, error = function(e) {
    rlang::abort(glue::glue("Failed to write data CSV for study '{study_id}': {e$message}"))
  })

  # --- Return Path ---
  invisible(study_dir)
}

#' @title Load All Study Data from a metawoRld Project
#'
#' @description
#' Scans the project's `data/` directory, reads the `metadata.yml` and
#' `data.csv` file for each valid study subdirectory, and compiles the
#' information into R objects.
#'
#' @param path Character string. The path to the root directory of the
#'   metawoRld project. Defaults to the current working directory (`.`).
#' @param verbose Logical. Print informative messages about studies being loaded
#'   or skipped? Defaults to `TRUE`.
#'
#' @return A list with two components:
#'   \describe{
#'     \item{`studies_data`}{A single data frame combining the data from all
#'       `data.csv` files. It includes an added `study_id` column. It attempts
#'       to join relevant information like 'unit' from `measurement_methods`
#'       and 'group_name' from `outcome_groups` based on the links defined
#'       in the study's `metadata.yml`. Columns that cannot be joined or are
#'       missing in specific studies will contain `NA`.}
#'     \item{`studies_metadata`}{A list where names are the `study_id`s and
#'       values are the full parsed metadata lists read from each study's
#'       `metadata.yml` file.}
#'   }
#' Returns `NULL` invisibly if the data directory is missing or no valid studies
#' are found. Issues warnings for studies skipped due to missing files or errors.
#'
#' @export
#'
#' @importFrom fs path dir_exists file_exists dir_ls path_file
#' @importFrom yaml read_yaml yaml.load_file
#' @importFrom readr read_csv cols col_character guess_parser show_col_types
#' @importFrom dplyr bind_rows mutate left_join select relocate all_of across starts_with
#' @importFrom purrr map map_dfr map_chr safely list_rbind set_names discard
#' @importFrom rlang warn inform abort is_list %||% set_names
#' @importFrom tidyselect everything
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # --- Setup: Create a temporary project and add two studies ---
#' proj_path <- file.path(tempdir(), "load_study_test")
#' create_metawoRld(
#'   path = proj_path,
#'   project_name = "Load Study Test",
#'   project_description = "Testing load_metawoRld()"
#' )
#'
#' # Study 1 Data
#' meta1 <- list(
#'   study_id = "S1", title = "Study One", authors = list("A"), year = 2021,
#'   journal = "J1", study_design="Cohort", country="X", sample_type="Serum",
#'   outcome_groups = list(g1=list(name="Case", def="..."), g2=list(name="Ctrl", def="...")),
#'   measurement_methods = list(m1=list(unit="pg/mL", analysis_type="E")),
#'   datafindr_assessment = list(relevance_score=1)
#' )
#' data1 <- data.frame(measurement_id="m1a", method_ref_id="m1", cytokine_name="CK1",
#'                     group_label="g1", gestational_age_timing="T1", n=10,
#'                     statistic_type="mean_sd", value1=5, value2=1)
#' add_study_data(proj_path, "S1", meta1, data1)
#'
#' # Study 2 Data
#' meta2 <- list(
#'   study_id = "S2", title = "Study Two", authors = list("B"), year = 2022,
#'   journal = "J2", study_design="Case-Ctrl", country="Y", sample_type="Plasma",
#'   outcome_groups = list(grpA=list(name="High", def="..."), grpB=list(name="Low", def="...")),
#'   measurement_methods = list(
#'      assayX = list(unit="ng/L", analysis_type="Luminex", target="CK1"),
#'      assayY = list(unit="pg/mL", analysis_type="ELISA", target="CK2")
#'    ),
#'    datafindr_assessment = list(relevance_score=0.8)
#' )
#' data2 <- data.frame(measurement_id=c("m2a", "m2b"), method_ref_id=c("assayX", "assayY"),
#'                     cytokine_name=c("CK1", "CK2"), group_label=c("grpA", "grpA"),
#'                     gestational_age_timing=c("T3","T3"), n=c(25, 25),
#'                     statistic_type=c("median_iqr", "mean_sem"),
#'                     value1=c(500, 12.3), value2=c("400-650", 2.1))
#' add_study_data(proj_path, "S2", meta2, data2)
#'
#' # Add an empty folder (should be skipped)
#' fs::dir_create(file.path(proj_path, "data", "EmptyFolder"))
#'
#' # --- Load the data ---
#' loaded_data <- load_metawoRld(proj_path)
#'
#' # Explore the results
#' print("--- Combined Data Frame ---")
#' print(loaded_data$studies_data)
#'
#' print("--- List of Metadata ---")
#' print(names(loaded_data$studies_metadata))
#' print(loaded_data$studies_metadata$S1$title)
#'
#' # --- Clean up ---
#' unlink(proj_path, recursive = TRUE)
#' }
load_metawoRld <- function(path = ".", verbose = TRUE) {

  # --- Input Validation ---
  if (!rlang::is_scalar_character(path) || path == "") {
    rlang::abort("`path` must be a non-empty character string.")
  }
  if (!rlang::is_scalar_logical(verbose)) {
    rlang::abort("`verbose` must be TRUE or FALSE.")
  }

  proj_path <- fs::path_norm(path)
  data_path <- fs::path(proj_path, "data")

  if (!fs::dir_exists(data_path)) {
    rlang::warn(glue::glue("Data directory not found: {data_path}"))
    return(invisible(NULL))
  }

  # --- Find Study Directories ---
  study_dirs <- fs::dir_ls(data_path, type = "directory")
  if (length(study_dirs) == 0) {
    rlang::warn(glue::glue("No subdirectories found in: {data_path}"))
    return(invisible(NULL))
  }
  study_ids <- .desanitize_id(fs::path_file(study_dirs))

  # --- Processing Functions (Safely) ---
  # Use purrr::safely to wrap file reading, so one bad study doesn't stop all
  safe_read_yaml <- purrr::safely(yaml::yaml.load_file)
  safe_read_csv <- purrr::safely(readr::read_csv, otherwise = NULL) # Return NULL on error

  # --- Loop Through Studies ---
  all_metadata <- list()
  all_data_list <- list()

  if (verbose) rlang::inform(glue::glue("Found {length(study_dirs)} potential study directories in '{data_path}'."))

  for (i in seq_along(study_dirs)) {
    s_dir <- study_dirs[[i]]
    s_id <- study_ids[[i]]
    if (verbose) rlang::inform(glue::glue("Processing '{s_id}'..."))

    meta_file <- fs::path(s_dir, "metadata.yml")
    data_file <- fs::path(s_dir, "data.csv")

    # Check files exist
    if (!fs::file_exists(meta_file)) {
      rlang::warn(glue::glue("Skipping '{s_id}': 'metadata.yml' not found."))
      next()
    }
    if (!fs::file_exists(data_file)) {
      rlang::warn(glue::glue("Skipping '{s_id}': 'data.csv' not found."))
      next()
    }

    # Read metadata safely
    meta_result <- safe_read_yaml(meta_file)
    if (!is.null(meta_result$error)) {
      rlang::warn(glue::glue("Skipping '{s_id}': Error reading metadata.yml: {meta_result$error$message}"))
      next()
    }
    metadata <- meta_result$result
    if (!rlang::is_list(metadata)) {
      rlang::warn(glue::glue("Skipping '{s_id}': metadata.yml did not parse as a list."))
      next()
    }

    # Read data safely
    # Guess columns, but suppress messages; handle potential parsing issues later if needed
    data_result <- safe_read_csv(
      data_file,
      show_col_types = FALSE,
      guess_max = 1000 # Increase rows to guess from if needed
      # Consider adding col_types = readr::cols(.default = "c") for robustness
      # if type guessing causes issues, then convert types explicitly later.
    )

    if (!is.null(data_result$error)) {
      rlang::warn(glue::glue("Skipping '{s_id}': Error reading data.csv: {data_result$error$message}"))
      next()
    }
    study_data <- data_result$result
    if (!is.data.frame(study_data) || nrow(study_data) == 0) {
      rlang::warn(glue::glue("Skipping '{s_id}': data.csv is empty or not a valid data frame."))
      next()
    }

    # --- Store Results ---
    all_metadata[[s_id]] <- metadata
    all_data_list[[s_id]] <- dplyr::mutate(study_data, study_id = s_id, .before = 1)

    if (verbose) rlang::inform(glue::glue("Successfully loaded '{s_id}'."))

  } # End loop through studies

  # --- Combine and Process Data ---
  if (length(all_data_list) == 0) {
    rlang::warn("No valid study data could be loaded.")
    return(list(studies_data = dplyr::tibble(), studies_metadata = list()))
  }

  # Combine all data.frames, filling missing columns with NA
  combined_data <- data.table::rbindlist(all_data_list, fill = TRUE)

  # --- Attempt to Join Key Metadata ---
  # 1. Create lookup tables from metadata
  method_unit_lookup <- purrr::map_dfr(names(all_metadata), function(s_id) {
    meta <- all_metadata[[s_id]]
    methods <- meta$measurement_methods %||% list()
    if (!is.list(methods) || length(methods) == 0) return(NULL)

    methods_df <- purrr::map_dfr(names(methods), function(method_id) {
      method_details <- methods[[method_id]] %||% list()
      dplyr::tibble(
        study_id = s_id,
        method_ref_id = method_id,
        unit = as.character(method_details$unit %||% NA_character_),
        analysis_type = as.character(method_details$analysis_type %||% NA_character_)
      )
    })
    return(methods_df)
  })

  group_name_lookup <- purrr::map_dfr(names(all_metadata), function(s_id) {
    meta <- all_metadata[[s_id]]
    groups <- meta$outcome_groups %||% list()
    if (!is.list(groups) || length(groups) == 0) return(NULL)

    groups_df <- purrr::map_dfr(names(groups), function(group_id) {
      group_details <- groups[[group_id]] %||% list()
      dplyr::tibble(
        study_id = s_id,
        group_label = group_id, # The key used in data.csv
        group_name = as.character(group_details$name %||% group_id), # Use label if name missing
        group_definition = as.character(group_details$def %||% NA_character_)
      )
    })
    return(groups_df)
  })

  # 2. Perform joins
  if (nrow(method_unit_lookup) > 0 && "method_ref_id" %in% names(combined_data)) {
    # Ensure join columns are compatible types
    combined_data <- combined_data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(intersect(c("study_id", "method_ref_id"), names(.))), as.character))

    combined_data <- dplyr::left_join(
      combined_data,
      method_unit_lookup,
      by = c("study_id", "method_ref_id")
    )
  } else if (verbose && "method_ref_id" %in% names(combined_data)) {
    rlang::inform("Could not create method lookup table; skipping unit/analysis_type join.")
  }

  if (nrow(group_name_lookup) > 0 && "group_label" %in% names(combined_data)) {
    combined_data <- combined_data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(intersect(c("study_id", "group_label"), names(.))), as.character))

    combined_data <- dplyr::left_join(
      combined_data,
      group_name_lookup,
      by = c("study_id", "group_label")
    )
  } else if (verbose && "group_label" %in% names(combined_data)) {
    rlang::inform("Could not create group lookup table; skipping group_name/definition join.")
  }

  # 3. Final Formatting
  combined_data <- combined_data %>%
    dplyr::relocate(dplyr::starts_with("study_id"), .before = 1) %>%
    dplyr::relocate(dplyr::any_of(c("unit", "analysis_type")), .after = dplyr::any_of("cytokine_name")) %>%
    dplyr::relocate(dplyr::any_of(c("group_name", "group_definition")), .after = dplyr::any_of("group_label"))

  if (verbose) rlang::inform(glue::glue("Successfully loaded and combined data for {length(all_data_list)} studies."))

  return(list(
    studies_data = combined_data,
    studies_metadata = all_metadata
  ))

}

#' @title Add a Study Template Directory to a metawoRld Project
#'
#' @description
#' Creates a new study directory within the project's `data/` folder,
#' containing template `metadata.yml` and `data.csv` files based on the
#' project's schema. These files can then be manually edited.
#'
#' @param path Character string. The path to the root directory of the
#'   metawoRld project. Defaults to the current working directory (`.`).
#' @param study_id Character string. A unique identifier for the study (e.g.,
#'   PMID, DOI, or a custom ID). This will be used as the directory name.
#'   Should be filesystem-friendly.
#' @param overwrite Logical. If a directory for `study_id` already exists,
#'   should its contents be overwritten with new templates? Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the created study directory containing
#'   the template files.
#' @export
#'
#' @importFrom fs path dir_exists file_exists dir_create path_norm
#' @importFrom yaml write_yaml
#' @importFrom readr write_csv
#' @importFrom dplyr tibble bind_cols
#' @importFrom purrr map set_names
#' @importFrom rlang list2 %||% inform warn abort is_scalar_character is_scalar_logical list2
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # --- Setup: Create a temporary project ---
#' proj_path <- file.path(tempdir(), "add_template_test")
#' create_metawoRld(
#'   path = proj_path,
#'   project_name = "Add Template Test",
#'   project_description = "Testing add_study_template()"
#' )
#'
#' # --- Add a template for a new study ---
#' add_study_template(
#'   path = proj_path,
#'   study_id = "FutureStudy2024"
#' )
#'
#' # Verify template files were created
#' list.files(file.path(proj_path, "data", "FutureStudy2024"))
#'
#' # View the contents of the template metadata.yml
#' cat(readLines(file.path(proj_path, "data", "FutureStudy2024", "metadata.yml")), sep = "\n")
#'
#' # View the contents of the template data.csv
#' cat(readLines(file.path(proj_path, "data", "FutureStudy2024", "data.csv")), sep = "\n")
#'
#' # Try adding again without overwrite (should fail)
#' tryCatch(
#'   add_study_template(proj_path, "FutureStudy2024"),
#'   error = function(e) print(e$message)
#' )
#'
#' # Add again with overwrite
#' add_study_template(
#'  path = proj_path,
#'  study_id = "FutureStudy2024",
#'  overwrite = TRUE
#' )
#'
#' # --- Clean up ---
#' unlink(proj_path, recursive = TRUE)
#' }
add_study_template <- function(path = ".",
                               study_id,
                               overwrite = FALSE) {

  # --- Input Type Validation ---
  if (!rlang::is_scalar_character(path) || path == "") {
    rlang::abort("`path` must be a non-empty character string.")
  }
  if (missing(study_id) || !rlang::is_scalar_character(study_id) || study_id == "") {
    rlang::abort("`study_id` must be a non-empty character string.")
  }
  if (!rlang::is_scalar_logical(overwrite)) {
    rlang::abort("`overwrite` must be TRUE or FALSE.")
  }

  # --- Project and Schema Validation ---
  proj_path <- fs::path_norm(path)
  config_path <- fs::path(proj_path, "_metawoRld.yml")
  if (!fs::file_exists(config_path)) {
    rlang::abort(glue::glue("Project configuration file '_metawoRld.yml' not found in: {proj_path}"))
  }

  project_schema <- get_schema(proj_path)
  if (is.null(project_schema) || !"metadata_fields" %in% names(project_schema) || !"data_fields" %in% names(project_schema)) {
    rlang::abort(glue::glue("Could not retrieve a valid schema (with metadata_fields and data_fields) from '_metawoRld.yml' in: {proj_path}"))
  }

  # --- Directory Handling ---
  study_dir <- fs::path(proj_path, "data", .sanitize_id(study_id))

  if (fs::dir_exists(study_dir)) {
    if (!overwrite) {
      rlang::abort(glue::glue("Study directory already exists: {study_dir}. Use `overwrite = TRUE` to replace."))
    } else {
      rlang::inform(glue::glue("Overwriting existing study directory with templates: {study_dir}"))
      # We'll just overwrite files, no need to delete/recreate dir unless issues arise
    }
  } else {
    fs::dir_create(study_dir)
    rlang::inform(glue::glue("Created study directory: {study_dir}"))
  }

  # --- Generate Template metadata.yml ---
  meta_schema <- project_schema$metadata_fields
  required_meta <- meta_schema$required %||% character()
  optional_meta <- meta_schema$optional %||% character()

  template_meta_list <- list()

  # Add required fields with placeholders
  for (field in required_meta) {
    if (field == "study_id") {
      template_meta_list[[field]] <- study_id # Pre-fill study_id
    } else if (field %in% c("outcome_groups", "measurement_methods")) {
      # Provide minimal example structure for key complex fields
      if (field == "outcome_groups"){
        template_meta_list[[field]] <- list(
          group_key1 = list(name = "REQUIRED: Define Group 1 Name", definition = "REQUIRED: Define Group 1 Criteria"),
          group_key2 = list(name = "REQUIRED: Define Group 2 Name", definition = "REQUIRED: Define Group 2 Criteria")
        )
      } else if (field == "measurement_methods"){
        template_meta_list[[field]] <- list(
          method_key1 = list(
            analysis_type = "REQUIRED: e.g., ELISA",
            target_cytokine = "REQUIRED: e.g., IL-6",
            unit = "REQUIRED: e.g., pg/mL",
            kit_manufacturer = "OPTIONAL: e.g., R&D Systems",
            kit_catalog_number = "OPTIONAL: e.g., D6050"
          )
        )
      }
    } else {
      template_meta_list[[field]] <- "REQUIRED: Please provide value"
    }
  }

  # Add optional fields with placeholders
  for (field in optional_meta) {
    # Handle potential complex optional fields if necessary, simple placeholder otherwise
    if (field == "datafindr_assessment") {
      template_meta_list[[field]] <- list(
        relevance_score = "OPTIONAL: e.g., 0.9",
        rationale = "OPTIONAL: AI or manual assessment rationale",
        model_used = "OPTIONAL: e.g., gpt-4",
        extraction_date = "OPTIONAL: YYYY-MM-DD"
      )
    } else {
      template_meta_list[[field]] <- "OPTIONAL"
    }
  }

  metadata_file <- fs::path(study_dir, "metadata.yml")
  tryCatch({
    # Use literal blocks for potentially long placeholder strings
    yaml::write_yaml(template_meta_list, file = metadata_file, indent.mapping.sequence = TRUE)
    rlang::inform(glue::glue("Written metadata template to: {metadata_file}"))
  }, error = function(e) {
    rlang::abort(glue::glue("Failed to write metadata YAML template for study '{study_id}': {e$message}"))
  })


  # --- Generate Template data.csv ---
  data_schema <- project_schema$data_fields
  required_data <- data_schema$required %||% character()
  optional_data <- data_schema$optional %||% character()
  all_data_cols <- unique(c(required_data, optional_data)) # Ensure unique columns

  # Create an empty tibble/df with the correct column names (all as character initially for simplicity)
  template_df <- purrr::map(all_data_cols, ~ character(0)) %>%
    purrr::set_names(all_data_cols) %>%
    dplyr::bind_cols() # Use bind_cols to handle empty case correctly -> tibble()

  # Ensure columns are in the desired order (required first, then optional)
  col_order <- intersect(c(required_data, optional_data), names(template_df))
  if(length(col_order) > 0 && ncol(template_df) > 0) {
    template_df <- template_df[, col_order, drop = FALSE]
  }


  data_file <- fs::path(study_dir, "data.csv")
  tryCatch({
    readr::write_csv(template_df, file = data_file, na = "") # Write header only
    rlang::inform(glue::glue("Written data template (header only) to: {data_file}"))
  }, error = function(e) {
    rlang::abort(glue::glue("Failed to write data CSV template for study '{study_id}': {e$message}"))
  })

  # --- Return Path ---
  invisible(study_dir)
}

#' @title Validate a Single Study's Data Files Against the Project Schema
#'
#' @description
#' Reads the `metadata.yml` and `data.csv` files for a specific study ID
#' within a metawoRld project and checks their structure and content against
#' the rules defined in the project's `_metawoRld.yml` schema. It also checks
#' internal consistency (e.g., links between data and metadata).
#'
#' This is useful for checking manually edited files (e.g., those created using
#' `add_study_template`).
#'
#' @param study_id Character string. The unique identifier of the study to validate.
#' @param path Character string. The path to the root directory of the
#'   metawoRld project. Defaults to the current working directory (`.`).
#' @param check_linkages Logical. Should the consistency checks between
#'   `data.csv` (`method_ref_id`, `group_label`) and `metadata.yml`
#'   (`measurement_methods`, `outcome_groups`) be performed? Defaults to `TRUE`.
#'
#' @return Returns `TRUE` (invisibly) if all validation checks pass.
#'   If any check fails, the function stops with an informative error message
#'   using `rlang::abort()`.
#'
#' @export
#'
#' @importFrom fs path dir_exists file_exists path_norm
#' @importFrom yaml yaml.load_file
#' @importFrom readr read_csv show_col_types
#' @importFrom rlang %||% inform warn abort is_list is_scalar_character is_scalar_logical
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # --- Setup: Create a project and add a template ---
#' proj_path <- file.path(tempdir(), "validate_study_test")
#' create_metawoRld(
#'   path = proj_path,
#'   project_name = "Validate Study Test",
#'   project_description = "Testing validate_study()"
#' )
#' add_study_template(proj_path, "ManualStudy01")
#'
#' # --- Scenario 1: Validate the raw template (might fail required field checks) ---
#' # This will likely fail because placeholders like "REQUIRED: ..." are still present.
#' tryCatch(
#'   validate_study("ManualStudy01", path = proj_path),
#'   error = function(e) print(paste("Validation Failed (as expected for raw template):", e$message))
#' )
#'
#' # --- Scenario 2: Manually Edit Files (Simulated) ---
#' # Imagine the user fills the files. Let's simulate correct filling:
#' meta_file <- file.path(proj_path, "data", "ManualStudy01", "metadata.yml")
#' data_file <- file.path(proj_path, "data", "ManualStudy01", "data.csv")
#'
#' # Create valid-looking content programmatically for the example
#' valid_meta <- list(
#'   study_id = "ManualStudy01", title = "Manually Entered Study",
#'   authors = list("User U"), year = 2024, journal = "Data Entry Journal",
#'   study_design = "Cross-sectional", country = "Local", sample_type = "Serum",
#'   outcome_groups = list(
#'     g1 = list(name = "Group A", definition = "Criteria A"),
#'     g2 = list(name = "Group B", definition = "Criteria B")
#'   ),
#'   measurement_methods = list(
#'     m_elisa = list(analysis_type = "ELISA", target_cytokine = "CYTOK", unit = "pg/mL")
#'   )
#'   # Optional fields omitted for brevity
#' )
#' yaml::write_yaml(valid_meta, meta_file)
#'
#' valid_data <- data.frame(
#'   measurement_id = "meas1", method_ref_id = "m_elisa", cytokine_name = "CYTOK",
#'   group_label = "g1", gestational_age_timing = "Any", n = 50,
#'   statistic_type = "mean_sd", value1 = 10.0, value2 = 2.5
#' )
#' readr::write_csv(valid_data, data_file)
#'
#' # --- Scenario 3: Validate the correctly filled study ---
#' validate_study("ManualStudy01", path = proj_path) # Should now pass and print success
#'
#' # --- Scenario 4: Introduce an error (e.g., bad group_label in data.csv) ---
#' invalid_data <- valid_data
#' invalid_data$group_label[1] <- "g_BAD" # This label doesn't exist in metadata
#' readr::write_csv(invalid_data, data_file)
#'
#' tryCatch(
#'   validate_study("ManualStudy01", path = proj_path),
#'   error = function(e) print(paste("Validation Failed (invalid group link):", e$message))
#' )
#'
#' # --- Scenario 5: Validate without checking linkages ---
#' # This should pass even with the bad group_label, as linkage check is skipped
#' validate_study("ManualStudy01", path = proj_path, check_linkages = FALSE)
#'
#' # --- Clean up ---
#' unlink(proj_path, recursive = TRUE)
#' }
validate_study <- function(study_id, path = ".", check_linkages = TRUE) {

  # --- Input Type Validation ---
  if (missing(study_id) || !rlang::is_scalar_character(study_id) || study_id == "") {
    rlang::abort("`study_id` must be a non-empty character string.")
  }
  if (!rlang::is_scalar_character(path) || path == "") {
    rlang::abort("`path` must be a non-empty character string.")
  }
  if (!rlang::is_scalar_logical(check_linkages)) {
    rlang::abort("`check_linkages` must be TRUE or FALSE.")
  }

  # --- Project and Schema Validation ---
  proj_path <- fs::path_norm(path)
  config_path <- fs::path(proj_path, "_metawoRld.yml")
  if (!fs::file_exists(config_path)) {
    rlang::abort(glue::glue("Project configuration file '_metawoRld.yml' not found in: {proj_path}"))
  }

  project_schema <- get_schema(proj_path)
  if (is.null(project_schema) || !"metadata_fields" %in% names(project_schema) || !"data_fields" %in% names(project_schema)) {
    rlang::abort(glue::glue("Could not retrieve a valid schema (with metadata_fields and data_fields) from '_metawoRld.yml' in: {proj_path}"))
  }

  # --- Locate Study Files ---
  study_dir <- fs::path(proj_path, "data", .sanitize_id(study_id))
  if (!fs::dir_exists(study_dir)) {
    rlang::abort(glue::glue("Study directory not found: {study_dir}"))
  }
  metadata_file <- fs::path(study_dir, "metadata.yml")
  data_file <- fs::path(study_dir, "data.csv")

  if (!fs::file_exists(metadata_file)) {
    rlang::abort(glue::glue("Metadata file not found for study '{study_id}': {metadata_file}"))
  }
  if (!fs::file_exists(data_file)) {
    rlang::abort(glue::glue("Data file not found for study '{study_id}': {data_file}"))
  }

  # --- Read Study Files ---
  metadata_list <- tryCatch({
    yaml::yaml.load_file(metadata_file)
  }, error = function(e) {
    rlang::abort(glue::glue("Failed to read or parse YAML file for study '{study_id}': {metadata_file}\nOriginal error: {e$message}"))
  })
  if (!rlang::is_list(metadata_list)) { # yaml.load_file might return non-list on empty/weird file
    rlang::abort(glue::glue("Content of metadata file for study '{study_id}' is not a valid YAML list/map: {metadata_file}"))
  }


  data_df <- tryCatch({
    # Read without specifying types initially for validation flexibility
    readr::read_csv(data_file, show_col_types = FALSE, guess_max = 1000)
  }, error = function(e) {
    rlang::abort(glue::glue("Failed to read or parse CSV file for study '{study_id}': {data_file}\nOriginal error: {e$message}"))
  })
  if (!is.data.frame(data_df)) { # Should be caught by read_csv error, but belt-and-suspenders
    rlang::abort(glue::glue("Content of data file for study '{study_id}' did not parse as a data frame: {data_file}"))
  }

  # --- Perform Validations ---
  # Use the same internal helpers as add_study_data
  validation_passed <- TRUE
  validation_errors <- list()

  # Use tryCatch around each validation step to potentially collect multiple errors,
  # but for now, we'll let them abort individually for simplicity.
  # If collecting errors, initialize validation_passed = TRUE and append errors to list.

  rlang::inform(glue::glue("Starting validation for study: '{study_id}'..."))

  # 1. Validate Metadata Structure
  tryCatch({
    .validate_metadata(metadata_list, project_schema, study_id)
    rlang::inform("- Metadata structure vs schema: OK")
  }, error = function(e) {
    # Re-throw the error from the validation function
    rlang::abort(e$message, parent = e)
  })


  # 2. Validate Data Frame Structure
  tryCatch({
    .validate_data_df(data_df, project_schema, study_id)
    rlang::inform("- Data frame structure vs schema: OK")
  }, error = function(e) {
    rlang::abort(e$message, parent = e)
  })


  # 3. Validate Linkages (Optional)
  if (check_linkages) {
    tryCatch({
      # Check if data_df is empty before attempting linkage validation
      if(nrow(data_df) == 0) {
        rlang::inform("- Linkages between data and metadata: SKIPPED (data frame is empty)")
      } else {
        .validate_linkages(data_df, metadata_list, project_schema, study_id)
        rlang::inform("- Linkages between data and metadata: OK")
      }
    }, error = function(e) {
      rlang::abort(e$message, parent = e)
    })
  } else {
    rlang::inform("- Linkages between data and metadata: SKIPPED (check_linkages = FALSE)")
  }

  # --- Report Success ---
  rlang::inform(glue::glue("Validation successful for study: '{study_id}'"))
  invisible(TRUE)
}

#' @title Validate All Studies in a metawoRld Project
#'
#' @description
#' Iterates through all study subdirectories within the project's `data/`
#' directory and runs `validate_study()` on each one to check conformity
#' against the project schema and internal consistency.
#'
#' @param path Character string. The path to the root directory of the
#'   metawoRld project. Defaults to the current working directory (`.`).
#' @param check_linkages Logical. Passed down to `validate_study()` for each
#'   study. Should the consistency checks between `data.csv` and `metadata.yml`
#'   be performed? Defaults to `TRUE`.
#'
#' @return A list containing:
#'   \describe{
#'     \item{`overall_status`}{Character string: "PASS" if all studies validated
#'       successfully, "FAIL" otherwise.}
#'     \item{`validated_studies`}{Integer: The number of study directories checked.}
#'     \item{`passed_studies`}{Integer: The number of studies that passed validation.}
#'     \item{`failed_studies_count`}{Integer: The number of studies that failed validation.}
#'     \item{`failed_studies_details`}{A list (named by `study_id`) containing the
#'       error messages for studies that failed validation. Empty if all passed.}
#'   }
#' Prints informative messages about the validation progress and summary.
#'
#' @export
#'
#' @importFrom fs path dir_exists dir_ls path_file
#' @importFrom purrr map safely list_transpose keep discard compact
#' @importFrom rlang inform warn abort is_scalar_character is_scalar_logical
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # --- Setup: Create a project, add one valid study and one template ---
#' proj_path <- file.path(tempdir(), "validate_world_test")
#' create_metawoRld(
#'   path = proj_path,
#'   project_name = "Validate World Test",
#'   project_description = "Testing validate_world()"
#' )
#'
#' # Add a valid study
#' meta1 <- list(
#'   study_id = "S1", title = "Valid Study", authors = list("A"), year = 2021,
#'   journal = "J1", study_design="Cohort", country="X", sample_type="Serum",
#'   outcome_groups = list(g1=list(name="Case", def="..."), g2=list(name="Ctrl", def="...")),
#'   measurement_methods = list(m1=list(unit="pg/mL", analysis_type="E"))
#' )
#' data1 <- data.frame(measurement_id="m1a", method_ref_id="m1", cytokine_name="CK1",
#'                     group_label="g1", gestational_age_timing="T1", n=10,
#'                     statistic_type="mean_sd", value1=5, value2=1)
#' add_study_data(proj_path, "S1", meta1, data1)
#'
#' # Add a template study (which will likely fail validation initially)
#' add_study_template(proj_path, "TemplateStudy")
#'
#' # Add an empty folder (should be skipped by validation logic within validate_study)
#' fs::dir_create(file.path(proj_path, "data", "EmptyFolder"))
#' # Add a folder with only metadata.yml (should fail in validate_study)
#' fs::dir_create(file.path(proj_path, "data", "MetadataOnly"))
#' file.copy(file.path(proj_path, "data", "S1", "metadata.yml"),
#'           file.path(proj_path, "data", "MetadataOnly", "metadata.yml"))
#'
#' # --- Run validation ---
#' validation_results <- validate_world(path = proj_path)
#'
#' # Print the results
#' print(validation_results)
#'
#' # --- Clean up ---
#' unlink(proj_path, recursive = TRUE)
#' }
validate_world <- function(path = ".", check_linkages = TRUE) {

  # --- Input Validation ---
  if (!rlang::is_scalar_character(path) || path == "") {
    rlang::abort("`path` must be a non-empty character string.")
  }
  if (!rlang::is_scalar_logical(check_linkages)) {
    rlang::abort("`check_linkages` must be TRUE or FALSE.")
  }

  proj_path <- fs::path_norm(path)
  data_path <- fs::path(proj_path, "data")

  if (!fs::dir_exists(data_path)) {
    rlang::warn(glue::glue("Data directory not found: {data_path}. No studies to validate."))
    return(list(
      overall_status = "PASS",
      validated_studies = 0,
      passed_studies = 0,
      failed_studies_count = 0,
      failed_studies_details = list()
    ))
  }

  # --- Find Study Directories ---
  study_dirs <- fs::dir_ls(data_path, type = "directory")
  study_ids <- .desanitize_id(fs::path_file(study_dirs))

  if (length(study_dirs) == 0) {
    rlang::inform(glue::glue("No study subdirectories found in: {data_path}. No studies to validate."))
    return(list(
      overall_status = "PASS",
      validated_studies = 0,
      passed_studies = 0,
      failed_studies_count = 0,
      failed_studies_details = list()
    ))
  }

  rlang::inform(glue::glue("Starting validation for {length(study_ids)} studies in '{data_path}'..."))

  # --- Validate Each Study Safely ---
  # Use purrr::safely to wrap validate_study
  safe_validate_study <- purrr::safely(validate_study, otherwise = FALSE, quiet = FALSE)

  # Run validation for all studies
  validation_outputs <- purrr::map(study_ids, ~{
    rlang::inform(glue::glue("--> Validating study: '{.x}'"))
    # Call the safe version, passing arguments
    safe_validate_study(study_id = .x, path = path, check_linkages = check_linkages)
  }, .progress = "Validating studies") # Add progress bar if many studies

  # Name the output list with study IDs
  names(validation_outputs) <- study_ids

  # --- Process Results ---
  # Transpose the list: results[[study_id]]$result / results[[study_id]]$error
  results_transposed <- purrr::list_transpose(validation_outputs, simplify = FALSE)

  # Get study IDs that passed (result is TRUE)
  passed_ids <- names(purrr::keep(results_transposed$result, ~ isTRUE(.x)))

  # Get study IDs that failed (error is not NULL)
  # Note: result might be FALSE if safely returns `otherwise` value
  failed_results <- purrr::discard(results_transposed$error, is.null)
  failed_ids <- names(failed_results)

  # Extract error messages for failed studies
  failed_details <- purrr::map(failed_results, ~ .x$message %||% "Unknown validation error")


  # --- Report Summary ---
  n_checked <- length(study_ids)
  n_passed <- length(passed_ids)
  n_failed <- length(failed_ids)
  overall_status <- ifelse(n_failed == 0, "PASS", "FAIL")

  rlang::inform("--- Validation Summary ---")
  rlang::inform(glue::glue("Studies checked: {n_checked}"))
  rlang::inform(glue::glue("Studies passed: {n_passed}"))
  rlang::inform(glue::glue("Studies failed: {n_failed}"))

  if (n_failed > 0) {
    rlang::warn("Validation failed for the following studies:")
    for(s_id in failed_ids) {
      # Remove newlines from error message for cleaner printing here
      err_msg <- gsub("\\r?\\n|\\r", " ", failed_details[[s_id]])
      rlang::warn(glue::glue("- {s_id}: {err_msg}"))
    }
  }

  # --- Return Structured Results ---
  invisible(list(
    overall_status = overall_status,
    validated_studies = n_checked,
    passed_studies = n_passed,
    failed_studies_count = n_failed,
    failed_studies_details = failed_details
  ))
}
