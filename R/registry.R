# People, searches, records and screening (SPEC.md sections 3, 5 and 6).

#' Check or suggest study ids
#'
#' Study ids are lowercase letters, digits and hyphens, 3-40 characters,
#' usually first author + year (`hansen2019`, `hansen2019b`).
#'
#' @param study_id Character vector of candidate ids.
#' @return `is_study_id()`: logical vector. `check_study_id()`: the id,
#'   invisibly, or an error.
#' @export
is_study_id <- function(study_id) {
  is.character(study_id) & !is.na(study_id) & grepl("^[a-z0-9][a-z0-9-]{2,39}$", study_id)
}

#' @rdname is_study_id
#' @export
check_study_id <- function(study_id) {
  if (length(study_id) != 1 || !is_study_id(study_id)) {
    rlang::abort(c(paste0("Invalid study id: '", paste(study_id, collapse = ", "), "'."),
                   "i" = "Use 3-40 lowercase letters, digits or hyphens, e.g. 'hansen2019'. See suggest_study_id()."))
  }
  invisible(study_id)
}

#' @rdname is_study_id
#' @param authors Author names (first author first), e.g. `"Hansen A"`.
#' @param year Publication year.
#' @param path Project root, used to avoid ids that are already taken.
#' @return `suggest_study_id()`: a free id such as `"hansen2019"` or
#'   `"hansen2019b"`.
#' @export
suggest_study_id <- function(authors, year, path = ".") {
  proj_path <- .check_project(path)
  first <- trimws(as.character(unlist(authors))[1] %||% "")
  surname <- sub("[ ,].*$", "", first)
  surname <- .to_ascii(surname)
  surname <- tolower(gsub("[^A-Za-z]", "", surname %||% ""))
  if (!nzchar(surname)) surname <- "study"
  year <- suppressWarnings(as.integer(year))
  base <- substr(paste0(surname, if (!is.na(year)) year else ""), 1, 38)
  if (nchar(base) < 3) base <- paste0(base, "-x")
  taken <- c(list_studies(proj_path), .submission_study_ids(proj_path))
  if (!base %in% taken) return(base)
  for (l in letters[-1]) if (!paste0(base, l) %in% taken) return(paste0(base, l))
  rlang::abort("No free study id found.")
}

# ---- people --------------------------------------------------------------

#' People and tools of a project
#'
#' `people.yml` lists the people (with roles) and tools that may appear in
#' provenance, verification and screening records.
#'
#' @param path Project root.
#' @return `read_people()`: a tibble with `id`, `name`, `orcid`, `roles`
#'   (`; `-separated) and `type` (`"person"` or `"tool"`).
#' @export
read_people <- function(path = ".") {
  proj_path <- .check_project(path)
  f <- fs::path(proj_path, "people.yml")
  y <- if (fs::file_exists(f)) yaml::read_yaml(f) %||% list() else list()
  row <- function(p, type) dplyr::tibble(
    id = as.character(p$id %||% NA), name = as.character(p$name %||% NA),
    orcid = as.character(p$orcid %||% NA), roles = paste(unlist(p$roles), collapse = "; "), type = type
  )
  dplyr::bind_rows(
    dplyr::tibble(id = character(0), name = character(0), orcid = character(0), roles = character(0), type = character(0)),
    lapply(y$people %||% list(), row, type = "person"),
    lapply(y$tools %||% list(), row, type = "tool")
  )
}

#' @rdname read_people
#' @param id Short id (lowercase letters, digits, hyphens; 2-20 characters).
#' @param name Full name (people) or tool name.
#' @param orcid Optional ORCID iD.
#' @param roles Roles: any of `admin`, `screener`, `extractor`, `verifier`,
#'   `arbiter`.
#' @param tool Add a tool (e.g. DataFindR) instead of a person?
#' @return `add_person()`: the updated tibble, invisibly. Re-adding an
#'   existing id updates it.
#' @export
add_person <- function(path = ".", id, name, orcid = NULL,
                       roles = c("extractor", "verifier"), tool = FALSE) {
  proj_path <- .check_project(path)
  if (!rlang::is_scalar_character(id) || !grepl("^[a-z0-9][a-z0-9-]{1,19}$", id)) {
    rlang::abort("`id` must be 2-20 lowercase letters, digits or hyphens, e.g. 'ahj'.")
  }
  valid_roles <- c("admin", "screener", "extractor", "verifier", "arbiter")
  if (!tool && length(setdiff(roles, valid_roles))) {
    rlang::abort(paste0("Unknown role(s): ", paste(setdiff(roles, valid_roles), collapse = ", ")))
  }
  f <- fs::path(proj_path, "people.yml")
  y <- if (fs::file_exists(f)) yaml::read_yaml(f) %||% list() else list()
  key <- if (tool) "tools" else "people"
  entry <- Filter(Negate(is.null), list(id = id, name = name, orcid = orcid, roles = if (!tool) as.list(roles)))
  lst <- y[[key]] %||% list()
  ids <- vapply(lst, function(p) as.character(p$id %||% ""), character(1))
  if (id %in% ids) lst[[match(id, ids)]] <- entry else lst[[length(lst) + 1]] <- entry
  y[[key]] <- lst
  yaml::write_yaml(list(people = y$people %||% list(), tools = y$tools %||% list()), f)
  invisible(read_people(proj_path))
}

.known_actors <- function(proj_path) read_people(proj_path)$id

.check_actor <- function(proj_path, id) {
  if (!rlang::is_scalar_character(id) || !id %in% .known_actors(proj_path)) {
    rlang::abort(c(paste0("Unknown person or tool: '", id, "'."),
                   "i" = "Add them with add_person() (or add_person(..., tool = TRUE) for tools)."))
  }
  invisible(id)
}

# ---- searches -----------------------------------------------------------

#' Record a literature search
#'
#' Appends the search to `searches.yml`. A re-run of the same query is a new
#' search with its own id.
#'
#' @param path Project root.
#' @param id Search id, e.g. `"pubmed-2026-11"`.
#' @param database Database name.
#' @param query The exact query.
#' @param results Number of hits.
#' @param person Person or tool id who ran it.
#' @param date Date the search was run. Defaults to today.
#' @return The search entry, invisibly.
#' @export
add_search <- function(path = ".", id, database, query, results = NA, person, date = Sys.Date()) {
  proj_path <- .check_project(path)
  if (!rlang::is_scalar_character(id) || !grepl("^[a-z0-9][a-z0-9-]*$", id)) rlang::abort("`id` must be lowercase letters, digits and hyphens.")
  .check_actor(proj_path, person)
  f <- fs::path(proj_path, "searches.yml")
  y <- if (fs::file_exists(f)) yaml::read_yaml(f) %||% list() else list()
  ids <- vapply(y$searches %||% list(), function(s) as.character(s$id), character(1))
  if (id %in% ids) rlang::abort(paste0("A search with id '", id, "' already exists."))
  entry <- list(id = id, database = database, query = query, date = as.character(date),
                results = if (is.na(results)) NULL else as.integer(results), person = person)
  y$searches <- c(y$searches %||% list(), list(Filter(Negate(is.null), entry)))
  yaml::write_yaml(y, f)
  invisible(entry)
}

# ---- records ------------------------------------------------------------

.records_cols <- c("record_id", "doi", "pmid", "title", "year", "journal", "authors", "searches", "study_id", "duplicate_of")
.screening_cols <- c("record_id", "stage", "decision", "reason", "person", "kind", "timestamp")

#' The record registry
#'
#' `records.csv` holds one row per unique record (search hit); see
#' `SPEC.md` section 6.
#'
#' @param path Project root.
#' @return `read_records()`: a tibble with the registry columns.
#' @export
read_records <- function(path = ".") {
  proj_path <- .check_project(path)
  .read_char_csv(fs::path(proj_path, "records.csv"), .records_cols)
}

#' @rdname read_records
#' @param records Data frame with at least `title`, and ideally `doi`,
#'   `pmid`, `year`, `journal`, `authors` (a character string, `; `-separated).
#' @param search Search id the records came from (see [add_search()]), or
#'   `"manual"`.
#' @return `add_records()`: a tibble with `record_id` and `new` (whether the
#'   record was not in the registry before), invisibly. Records already
#'   present get the search id added to their `searches`.
#' @export
add_records <- function(path = ".", records, search = "manual") {
  proj_path <- .check_project(path)
  if (!is.data.frame(records) || !"title" %in% names(records)) rlang::abort("`records` must be a data frame with a `title` column.")
  reg <- read_records(proj_path)
  r <- as.data.frame(records, stringsAsFactors = FALSE)
  for (c in setdiff(.records_cols, names(r))) r[[c]] <- NA_character_
  r <- r[, .records_cols]
  r[] <- lapply(r, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  r$doi <- ifelse(nzchar(r$doi), normalize_identifier(r$doi), "")
  r$pmid <- ifelse(nzchar(r$pmid), normalize_identifier(r$pmid), "")
  r$record_id <- ifelse(nzchar(r$record_id), r$record_id, record_id(r$doi, r$pmid, r$title, r$year))
  new <- logical(nrow(r))
  for (i in seq_len(nrow(r))) {
    j <- match(r$record_id[i], reg$record_id)
    if (is.na(j)) {
      row <- r[i, ]
      row$searches <- search
      reg <- rbind(reg, row)
      new[i] <- TRUE
    } else {
      s <- unique(c(strsplit(reg$searches[j], "; ", fixed = TRUE)[[1]], search))
      reg$searches[j] <- paste(s[nzchar(s)], collapse = "; ")
    }
  }
  .write_char_csv(reg, fs::path(proj_path, "records.csv"))
  invisible(dplyr::tibble(record_id = r$record_id, new = new))
}

#' @rdname read_records
#' @param record_id Record id(s).
#' @param study_id Study id to link the record(s) to.
#' @export
link_record <- function(path = ".", record_id, study_id) {
  proj_path <- .check_project(path)
  check_study_id(study_id)
  reg <- read_records(proj_path)
  miss <- setdiff(record_id, reg$record_id)
  if (length(miss)) rlang::abort(paste0("Unknown record(s): ", paste(miss, collapse = ", ")))
  reg$study_id[reg$record_id %in% record_id] <- study_id
  .write_char_csv(reg, fs::path(proj_path, "records.csv"))
  invisible(reg)
}

#' Record ids
#'
#' The DOI when known, else `pmid:<PMID>`, else `rec-` and 12 hex digits of
#' a hash of the normalized title and year.
#'
#' @param doi,pmid,title,year Vectors of equal length (use `NA` or `""` for
#'   missing values).
#' @return Character vector.
#' @export
record_id <- function(doi = NA, pmid = NA, title = NA, year = NA) {
  n <- max(length(doi), length(pmid), length(title), length(year))
  doi <- rep_len(as.character(doi), n); pmid <- rep_len(as.character(pmid), n)
  title <- rep_len(as.character(title), n); year <- rep_len(as.character(year), n)
  doi <- normalize_identifier(doi); pmid <- normalize_identifier(pmid)
  key <- paste(tolower(gsub("[^A-Za-z0-9]", "", title)), ifelse(is.na(year), "", year))
  hashed <- paste0("rec-", substr(vapply(key, digest::digest, character(1), algo = "sha1", serialize = FALSE), 1, 12))
  ifelse(nzchar(doi), doi, ifelse(nzchar(pmid), paste0("pmid:", pmid), hashed))
}

# ---- screening ----------------------------------------------------------

#' Screening decisions
#'
#' `screening.csv` is append-only. Each person (or tool) records their own
#' decision; a `consensus` decision settles disagreements.
#'
#' @param path Project root.
#' @param record_id Record id(s) (must be in `records.csv`).
#' @param stage `"title_abstract"` or `"full_text"`.
#' @param decision `"include"`, `"exclude"` or `"unsure"` (recycled).
#' @param reason Exclusion reason (recycled).
#' @param person Person or tool id.
#' @param kind `"individual"` (default) or `"consensus"`.
#' @return The appended rows, invisibly.
#' @export
add_screening <- function(path = ".", record_id, stage = c("title_abstract", "full_text"),
                          decision, reason = "", person, kind = c("individual", "consensus")) {
  proj_path <- .check_project(path)
  stage <- match.arg(stage)
  kind <- match.arg(kind)
  .check_actor(proj_path, person)
  decision <- tolower(decision)
  if (!all(decision %in% c("include", "exclude", "unsure"))) rlang::abort("`decision` must be include, exclude or unsure.")
  miss <- setdiff(record_id, read_records(proj_path)$record_id)
  if (length(miss)) rlang::abort(paste0("Unknown record(s), add them with add_records() first: ", paste(miss, collapse = ", ")))
  rows <- data.frame(record_id = record_id, stage = stage, decision = decision, reason = reason,
                     person = person, kind = kind, timestamp = .now(), stringsAsFactors = FALSE)
  reason <- rows$reason; reason[is.na(reason)] <- ""; rows$reason <- reason
  f <- fs::path(proj_path, "screening.csv")
  all <- rbind(.read_char_csv(f, .screening_cols), rows)
  .write_char_csv(all, f)
  invisible(rows)
}

#' @rdname add_screening
#' @return `screening_status()`: one row per record and stage with
#'   `outcome` (`include`, `exclude`, `unsure`, `conflict`), `reason` and the
#'   number of individual decisions.
#' @export
screening_status <- function(path = ".") {
  proj_path <- .check_project(path)
  s <- .read_char_csv(fs::path(proj_path, "screening.csv"), .screening_cols)
  empty <- dplyr::tibble(record_id = character(0), stage = character(0), outcome = character(0),
                         reason = character(0), n_decisions = integer(0))
  if (!nrow(s)) return(empty)
  s$ord <- seq_len(nrow(s))
  out <- lapply(split(s, paste(s$record_id, s$stage, sep = "\r")), function(d) {
    cons <- d[d$kind == "consensus", , drop = FALSE]
    ind <- d[d$kind == "individual", , drop = FALSE]
    ind <- ind[!duplicated(ind$person, fromLast = TRUE), , drop = FALSE]
    if (nrow(cons)) {
      last <- cons[nrow(cons), ]
      outcome <- last$decision; reason <- last$reason
    } else if (length(unique(ind$decision)) == 1) {
      outcome <- ind$decision[1]; reason <- paste(unique(ind$reason[nzchar(ind$reason)]), collapse = "; ")
    } else {
      outcome <- "conflict"; reason <- ""
    }
    dplyr::tibble(record_id = d$record_id[1], stage = d$stage[1], outcome = outcome, reason = reason, n_decisions = nrow(ind))
  })
  dplyr::bind_rows(c(list(empty), unname(out)))
}

#' Counts for a PRISMA 2020 flow diagram
#'
#' Computed only from `records.csv`, `screening.csv` and the accepted
#' studies.
#'
#' @param path Project root.
#' @return A tibble with `stage` and `n`, including one row per full-text
#'   exclusion reason.
#' @export
prisma_counts <- function(path = ".") {
  proj_path <- .check_project(path)
  rec <- read_records(proj_path)
  st <- screening_status(proj_path)
  dup <- sum(nzchar(rec$duplicate_of))
  ta <- st[st$stage == "title_abstract", , drop = FALSE]
  ft <- st[st$stage == "full_text", , drop = FALSE]
  reasons <- ft$reason[ft$outcome == "exclude"]
  reasons[!nzchar(reasons)] <- "Not specified"
  rt <- if (length(reasons)) sort(table(reasons), decreasing = TRUE) else integer(0)
  dplyr::bind_rows(
    dplyr::tibble(
      stage = c("Records identified", "Duplicates removed", "Records screened (title/abstract)",
                "Records excluded (title/abstract)", "Awaiting decision (title/abstract)",
                "Reports assessed (full text)", "Reports excluded (full text)"),
      n = c(nrow(rec), dup, nrow(ta), sum(ta$outcome == "exclude"), sum(ta$outcome %in% c("unsure", "conflict")),
            nrow(ft), sum(ft$outcome == "exclude"))
    ),
    dplyr::tibble(stage = paste0("  Excluded: ", names(rt)), n = as.integer(rt)),
    dplyr::tibble(stage = c("Studies included", "Reports of included studies"),
                  n = c(length(list_studies(proj_path)), sum(nzchar(rec$study_id))))
  )
}

# ---- csv helpers ----------------------------------------------------------

.read_char_csv <- function(file, cols) {
  if (!fs::file_exists(file)) {
    out <- as.data.frame(stats::setNames(rep(list(character(0)), length(cols)), cols), stringsAsFactors = FALSE)
    return(dplyr::as_tibble(out))
  }
  d <- readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()), na = character(0),
                       progress = FALSE, show_col_types = FALSE)
  for (c in setdiff(cols, names(d))) d[[c]] <- if (nrow(d)) "" else character(0)
  d[, unique(c(cols, names(d))), drop = FALSE]
}

.write_char_csv <- function(d, file) {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  d[] <- lapply(d, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  data.table::fwrite(d, file, na = "")
  invisible(file)
}

.now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Transliterate common Latin letters to ASCII (independent of the locale).
.to_ascii <- function(x) {
  from <- c("\u00e6", "\u00c6", "\u00f8", "\u00d8", "\u00e5", "\u00c5", "\u00e4", "\u00c4", "\u00f6", "\u00d6",
            "\u00fc", "\u00dc", "\u00df", "\u00e9", "\u00e8", "\u00ea", "\u00eb", "\u00c9", "\u00e1", "\u00e0",
            "\u00e2", "\u00e3", "\u00ed", "\u00ec", "\u00ee", "\u00ef", "\u00f3", "\u00f2", "\u00f4", "\u00f5",
            "\u00fa", "\u00f9", "\u00fb", "\u00f1", "\u00e7", "\u0161", "\u017e", "\u010d", "\u0107", "\u0142",
            "\u00fd")
  to <- c("ae", "Ae", "o", "O", "a", "A", "a", "A", "o", "O", "u", "U", "ss", "e", "e", "e", "e", "E", "a", "a",
          "a", "a", "i", "i", "i", "i", "o", "o", "o", "o", "u", "u", "u", "n", "c", "s", "z", "c", "c", "l", "y")
  x <- enc2utf8(as.character(x))
  for (i in seq_along(from)) x <- gsub(from[i], to[i], x, fixed = TRUE, useBytes = TRUE)
  out <- iconv(x, "UTF-8", "ASCII", sub = "")
  out[is.na(out)] <- ""
  out
}
