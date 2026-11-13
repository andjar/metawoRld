# Starting a review with sensible defaults (used by the app's Start screen).

#' Start a new review with sensible defaults
#'
#' A one-step way to start: creates the project, registers you, and sets up
#' the comparison you are interested in, so the first study can be entered
#' right away. Everything can be refined later (in the app's Setup screen or
#' by editing the files).
#'
#' @param path Folder to create.
#' @param title Short title of the review.
#' @param question The review question (a sentence or two).
#' @param design `"two_groups"` (compare two groups, e.g. cases and
#'   controls: a standardized mean difference), `"one_group"` (describe one
#'   group: a pooled mean) or `"blank"`.
#' @param groups Names of the groups: two for `"two_groups"` (group of
#'   interest first), one for `"one_group"`.
#' @param variable Optional first outcome or measurement, e.g. `"IL-6"`.
#' @param unit Its unit, e.g. `"pg/mL"`.
#' @param inclusion_criteria,exclusion_criteria Character vectors.
#' @param person_id,person_name You: a short id (e.g. initials) and your name.
#' @param git Keep the project in Git (for sharing and history)? Defaults to
#'   `TRUE` when Git is installed.
#' @param website Add the website templates? Defaults to `TRUE`.
#' @return The project path, invisibly.
#' @export
#' @examples
#' p <- new_review(file.path(tempdir(), "my-review"), "IL-6 in preeclampsia",
#'                 "Is IL-6 higher in preeclampsia than in normotensive pregnancy?",
#'                 groups = c("Preeclampsia", "Normotensive pregnancy"),
#'                 variable = "IL-6", unit = "pg/mL",
#'                 person_id = "ahj", person_name = "Anders", git = FALSE)
#' read_analyses(p)[[1]]$compare
new_review <- function(path, title, question = "", design = c("two_groups", "one_group", "blank"),
                       groups = c("Cases", "Controls"), variable = NULL, unit = NULL,
                       inclusion_criteria = character(0), exclusion_criteria = character(0),
                       person_id, person_name, git = nzchar(Sys.which("git")), website = TRUE) {
  design <- match.arg(design)
  if (!rlang::is_scalar_character(title) || !nzchar(trimws(title))) rlang::abort("Please give the review a title.")
  groups <- trimws(as.character(groups))
  groups <- groups[nzchar(groups)]
  need <- switch(design, two_groups = 2, one_group = 1, blank = 0)
  if (length(groups) < need) rlang::abort(paste0("Please name ", need, " group", if (need > 1) "s", "."))
  groups <- groups[seq_len(need)]
  if (anyDuplicated(tolower(groups))) rlang::abort("The two groups need different names.")
  variable <- if (!is.null(variable) && nzchar(trimws(variable))) trimws(variable) else NULL

  create_metawoRld(path, project_name = title, project_description = if (nzchar(trimws(question %||% ""))) question else title,
                   inclusion_criteria = inclusion_criteria[nzchar(trimws(inclusion_criteria))],
                   exclusion_criteria = exclusion_criteria[nzchar(trimws(exclusion_criteria))],
                   git_init = FALSE, website = website)
  add_person(path, person_id, person_name, roles = c("admin", "screener", "extractor", "verifier", "arbiter"))

  rules <- read_rules(path)
  if (length(groups)) rules$group_categories <- stats::setNames(lapply(groups, function(g) list(synonyms = list())), groups)
  if (!is.null(variable)) rules$variables <- stats::setNames(list(Filter(Negate(is.null), list(unit = unit))), variable)
  .write_rules(fs::path(path, "_rules.yml"), rules)
  if (!is.null(variable) && design != "blank") {
    a <- if (design == "two_groups") {
      list(name = paste0(variable, ": ", groups[1], " vs ", groups[2]), variable = variable, measure = "SMD", compare = groups)
    } else {
      list(name = paste0(variable, " in ", groups[1]), variable = variable, measure = "MN", group = groups[1])
    }
    write_analyses(path, list(a))
  }
  .ensure_gitattributes(path)
  if (isTRUE(git) && nzchar(.git_bin())) {
    .git(path, c("init", "-q"))
    old <- options(metawoRld.person = person_id); on.exit(options(old), add = TRUE)
    commit_project(path, paste("Start review:", title))
  }
  invisible(path)
}

#' Write the project's meta-analysis definitions
#'
#' Replaces the analyses in `_analyses.yml`, keeping the explanatory
#' comments at the top of the file.
#'
#' @param path Project root.
#' @param analyses A list of analyses (as returned by [read_analyses()], or
#'   lists with at least `name`, `variable`, `measure` and `compare` or
#'   `group`). Each is checked.
#' @return The analyses, invisibly.
#' @export
write_analyses <- function(path = ".", analyses) {
  proj_path <- .check_project(path)
  analyses <- unname(analyses)
  checked <- lapply(seq_along(analyses), function(i) .check_analysis(analyses[[i]], i))
  nms <- vapply(checked, `[[`, character(1), "name")
  if (anyDuplicated(nms)) rlang::abort(paste0("Duplicate analysis names: ", paste(unique(nms[duplicated(nms)]), collapse = ", ")))
  clean <- lapply(analyses, function(a) {
    a <- a[intersect(c("name", "variable", "measure", "compare", "group", "subgroups", "timepoint", "levels",
                       "verified_only", "method"), names(a))]
    a <- Filter(function(v) length(v) && !all(is.na(v)) && !identical(v, ""), a)
    if (identical(a$levels, "reported")) a$levels <- NULL
    if (identical(a$method, "REML")) a$method <- NULL
    if (isFALSE(a$verified_only)) a$verified_only <- NULL
    if (!is.null(a$compare)) a$compare <- as.list(a$compare)
    if (!is.null(a$subgroups)) a$subgroups <- as.list(a$subgroups)
    a
  })
  f <- fs::path(proj_path, "_analyses.yml")
  old <- if (fs::file_exists(f)) readLines(f, warn = FALSE) else character(0)
  first <- which(!grepl("^\\s*#", old) & nzchar(trimws(old)))[1]
  header <- if (is.na(first)) old else old[seq_len(first - 1)]
  body <- if (length(clean)) yaml::as.yaml(list(analyses = clean)) else "analyses: []\n"
  writeLines(c(header, sub("\n$", "", body)), f)
  invisible(read_analyses(proj_path))
}
