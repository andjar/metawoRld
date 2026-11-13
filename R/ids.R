#' Normalize study identifiers
#'
#' Brings DOIs and PMIDs into one canonical form so the same paper always maps
#' to the same cache entry and study folder: surrounding whitespace is removed,
#' `https://doi.org/`, `doi:` and `PMID:` prefixes are stripped, and DOIs are
#' lower-cased (DOIs are case-insensitive).
#'
#' @param x Character vector of identifiers.
#' @return Character vector of the same length.
#' @export
#' @examples
#' normalize_identifier(c(" https://doi.org/10.1000/ABC.1 ", "PMID: 12345", "doi:10.1/X"))
normalize_identifier <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  x <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", x, ignore.case = TRUE)
  x <- sub("^doi:\\s*", "", x, ignore.case = TRUE)
  x <- sub("^pmid:?\\s*", "", x, ignore.case = TRUE)
  is_doi <- grepl("^10\\.[0-9]{4,9}/", x)
  x[is_doi] <- tolower(x[is_doi])
  x
}

#' Detect the type of a study identifier
#'
#' @param x Character vector of (preferably normalized) identifiers.
#' @return Character vector: `"doi"`, `"pmid"` or `"other"`.
#' @export
#' @examples
#' identifier_type(c("10.1000/abc", "12345678", "Smith2020"))
identifier_type <- function(x) {
  x <- normalize_identifier(x)
  ifelse(grepl("^10\\.[0-9]{4,9}/", x), "doi",
         ifelse(grepl("^[0-9]{1,9}$", x), "pmid", "other"))
}

# Reversible encoding table. Order matters only for decoding, which is done in
# reverse. Kept compatible with earlier versions of metawoRld/DataFindR.
.id_codes <- c(
  "/" = "_fslash_", ":" = "_colon_", "?" = "_qmark_", "*" = "_star_",
  "<" = "_lt_", ">" = "_gt_", "|" = "_pipe_", "\"" = "_quote_",
  "\\" = "_bslash_", " " = "_space_"
)

#' Encode an identifier so it can be used as a file or folder name
#'
#' Replaces characters that are not allowed in file names (such as `/` in
#' DOIs) with reversible codes. Use [desanitize_id()] to get the original back.
#'
#' @param id Character vector of identifiers.
#' @return Character vector of file-system-safe strings (`""` for missing input).
#' @export
#' @examples
#' sanitize_id("10.1000/abc:1")
#' desanitize_id(sanitize_id("10.1000/abc:1"))
sanitize_id <- function(id) {
  if (is.null(id)) return(character(0))
  id <- as.character(id)
  id[is.na(id)] <- ""
  for (ch in names(.id_codes)) id <- gsub(ch, .id_codes[[ch]], id, fixed = TRUE)
  id
}

#' @rdname sanitize_id
#' @param sanitized_id Character vector produced by [sanitize_id()].
#' @export
desanitize_id <- function(sanitized_id) {
  if (is.null(sanitized_id)) return(character(0))
  id <- as.character(sanitized_id)
  id[is.na(id)] <- ""
  for (ch in rev(names(.id_codes))) id <- gsub(.id_codes[[ch]], ch, id, fixed = TRUE)
  id
}

# Surname of an author written as "Hansen A", "Hansen, A.", "Tavakkol Afshari J"
# or "van der Berg AB": everything except trailing initials.
.surname <- function(author) {
  a <- trimws(as.character(author %||% ""))
  if (!length(a) || is.na(a[1]) || !nzchar(a[1])) return("")
  a <- a[1]
  if (grepl(",", a)) return(trimws(sub(",.*$", "", a)))
  tokens <- strsplit(a, "\\s+")[[1]]
  while (length(tokens) > 1 && grepl("^([A-Z]\\.?-?){1,3}$", tokens[length(tokens)])) tokens <- tokens[-length(tokens)]
  paste(tokens, collapse = " ")
}
