# In metawoRld package (e.g., R/utils.R)

#' Sanitize an Identifier for Filesystem Use
#'
#' Replaces characters commonly problematic in filenames/paths (like '/', ':')
#' with safe, unique representations. Reversible with `.desanitize_id()`.
#'
#' @param id Character string identifier (e.g., DOI, PMID).
#' @return A sanitized character string suitable for filenames.
#' @noRd # Internal helper
#' @keywords internal
#' @export
.sanitize_id <- function(id) {
  if (is.null(id) || id == "") return("")
  id <- gsub("/", "_fslash_", id, fixed = TRUE)
  id <- gsub(":", "_colon_", id, fixed = TRUE)
  id <- gsub("\\?", "_qmark_", id, fixed = TRUE)
  id <- gsub("\\*", "_star_", id, fixed = TRUE)
  id <- gsub("<", "_lt_", id, fixed = TRUE)
  id <- gsub(">", "_gt_", id, fixed = TRUE)
  id <- gsub("\\|", "_pipe_", id, fixed = TRUE)
  id <- gsub("\"", "_quote_", id, fixed = TRUE)
  # Add other characters as needed based on observation
  # Keep it relatively simple to avoid excessive ugliness
  return(id)
}

#' Desanitize a Filesystem Identifier
#'
#' Reverses the replacements made by `.sanitize_id()`.
#'
#' @param sanitized_id Character string identifier sanitized by `.sanitize_id()`.
#' @return The original identifier string.
#' @noRd # Internal helper
#' @keywords internal
#' @export
.desanitize_id <- function(sanitized_id) {

  if (is.null(sanitized_id)) return("")

  id <- sanitized_id
  # Reverse substitutions in reverse order of application if dependencies exist (none here)
  id <- gsub("_quote_", "\"", id, fixed = TRUE)
  id <- gsub("_pipe_", "|", id, fixed = TRUE)
  id <- gsub("_gt_", ">", id, fixed = TRUE)
  id <- gsub("_lt_", "<", id, fixed = TRUE)
  id <- gsub("_star_", "*", id, fixed = TRUE)
  id <- gsub("_qmark_", "?", id, fixed = TRUE)
  id <- gsub("_colon_", ":", id, fixed = TRUE)
  id <- gsub("_fslash_", "/", id, fixed = TRUE)
  return(id)
}
