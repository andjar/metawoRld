# Sharing a project through Git, for people who do not use Git.
#
# The system `git` command is used, so the user's existing sign-in (a
# credential manager, GitHub Desktop, SSH keys) works unchanged.

.git_bin <- function() Sys.getenv("METAWORLD_GIT", Sys.which("git"))

.git <- function(proj_path, args, error = TRUE) {
  bin <- .git_bin()
  if (!nzchar(bin)) rlang::abort("Git is not installed. Install it from https://git-scm.com to share the project.")
  out <- suppressWarnings(system2(bin, c("-C", shQuote(proj_path), args), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0L
  if (error && status != 0) {
    rlang::abort(c(paste0("git ", args[1], " failed:"), rlang::set_names(utils::tail(out, 8), "x")),
                 class = "metawoRld_git_error", git_output = out)
  }
  structure(as.character(out), status = status)
}

.is_git_repo <- function(proj_path) {
  nzchar(.git_bin()) && identical(attr(.git(proj_path, c("rev-parse", "--is-inside-work-tree"), error = FALSE), "status"), 0L)
}

#' Sharing status of a project
#'
#' @param path Project root.
#' @return A list: `repo` (is the project a Git repository?), `branch`,
#'   `remote` (URL of `origin`, or `NA`), `changes` (number of changed or
#'   new files not yet shared), `ahead` and `behind` (commits not yet pushed /
#'   pulled, as of the last fetch).
#' @export
sync_status <- function(path = ".") {
  proj_path <- .check_project(path)
  if (!.is_git_repo(proj_path)) return(list(repo = FALSE, branch = NA_character_, remote = NA_character_,
                                           changes = NA_integer_, ahead = NA_integer_, behind = NA_integer_))
  branch <- .git(proj_path, c("rev-parse", "--abbrev-ref", "HEAD"), error = FALSE)[1]
  remote <- .git(proj_path, c("remote", "get-url", "origin"), error = FALSE)
  remote <- if (identical(attr(remote, "status"), 0L)) remote[1] else NA_character_
  changes <- length(.git(proj_path, c("status", "--porcelain"), error = FALSE))
  ab <- .git(proj_path, c("rev-list", "--left-right", "--count", "HEAD...@{upstream}"), error = FALSE)
  counts <- if (identical(attr(ab, "status"), 0L)) as.integer(strsplit(ab[1], "\\s+")[[1]]) else c(NA_integer_, NA_integer_)
  list(repo = TRUE, branch = branch, remote = remote, changes = changes, ahead = counts[1], behind = counts[2])
}

#' Share and fetch work with the rest of the team
#'
#' One step for people who do not use Git: saves (commits) your changes,
#' fetches the team's changes from the shared repository (e.g. GitHub) and
#' merges them, then sends (pushes) your changes. New submissions from
#' others then appear in [list_submissions()]. Each extraction lives in its
#' own folder and only arbiters write `data/`, so conflicts are rare;
#' `screening.csv`, which several people append to, is merged line by line.
#'
#' @param path Project root.
#' @param message Commit message for your changes. Defaults to a summary.
#' @param push Send your changes? Defaults to `TRUE`. Set `FALSE` to only
#'   fetch.
#' @return A list with `committed` (logical), `new_submissions` (tibble of
#'   submissions that arrived), `pushed` (logical) and `message` (a sentence
#'   for the user), invisibly.
#' @export
sync_project <- function(path = ".", message = NULL, push = TRUE) {
  proj_path <- .check_project(path)
  if (!.is_git_repo(proj_path)) rlang::abort("This project is not a Git repository; nothing to sync.")
  .ensure_gitattributes(proj_path)
  before <- list_submissions(proj_path)$extraction_id
  committed <- commit_project(proj_path, message %||% "Update from metawoRld")
  has_remote <- !is.na(sync_status(proj_path)$remote)
  pushed <- FALSE
  if (has_remote) {
    branch <- sync_status(proj_path)$branch
    .git(proj_path, c("fetch", "-q", "origin"), error = FALSE)
    on_remote <- length(.git(proj_path, c("ls-remote", "--heads", "origin", branch), error = FALSE)) > 0
    upstream <- identical(attr(.git(proj_path, c("rev-parse", "--abbrev-ref", "@{upstream}"), error = FALSE), "status"), 0L)
    if (on_remote && !upstream) .git(proj_path, c("branch", paste0("--set-upstream-to=origin/", branch)), error = FALSE)
    pull <- if (on_remote) .git(proj_path, c(.git_identity(proj_path), "pull", "--no-rebase", "--no-edit"), error = FALSE)
            else structure(character(0), status = 0L)
    if (!identical(attr(pull, "status"), 0L)) {
      conflicts <- .git(proj_path, c("diff", "--name-only", "--diff-filter=U"), error = FALSE)
      if (length(conflicts) && nzchar(conflicts[1])) {
        .git(proj_path, c("merge", "--abort"), error = FALSE)
        rlang::abort(c("Your changes and the team's changes touch the same lines; nothing was merged.",
                       rlang::set_names(paste0("conflict in ", conflicts), "x"),
                       "i" = "Ask the project admin to merge, or resolve the conflict in Git."),
                     class = "metawoRld_git_conflict")
      }
      rlang::abort(c("Could not fetch the team's changes:", rlang::set_names(utils::tail(pull, 5), "x")),
                   class = "metawoRld_git_error")
    }
    if (push) {
      ps <- .git(proj_path, c("push"), error = FALSE)
      if (!identical(attr(ps, "status"), 0L)) {
        ps <- .git(proj_path, c("push", "--set-upstream", "origin", sync_status(proj_path)$branch), error = FALSE)
      }
      pushed <- identical(attr(ps, "status"), 0L)
      if (!pushed) {
        rlang::warn(c("Your changes are saved locally but could not be sent:", rlang::set_names(utils::tail(ps, 3), "x"),
                      "i" = "Without write access, push to a fork and open a pull request."))
      }
    }
  }
  subs <- list_submissions(proj_path)
  new <- subs[!subs$extraction_id %in% before, , drop = FALSE]
  msg <- paste0(
    if (committed) "Your changes were saved. " else "",
    if (!has_remote) "No shared repository (remote) is set up, so nothing was sent or fetched."
    else paste0(nrow(new), " new submission", if (nrow(new) == 1) "" else "s", " from the team",
                if (nrow(new)) paste0(" (", paste(unique(new$person), collapse = ", "), ")") else "", ". ",
                if (push) if (pushed) "Your work was sent." else "Your work could not be sent." else "")
  )
  invisible(list(committed = committed, new_submissions = new, pushed = pushed, message = trimws(msg)))
}

#' Save (commit) the project's changes
#'
#' Used after actions in the metawoRld app, so the Git history reads like a
#' log of the review. Does nothing if the project is not a Git repository or
#' nothing changed.
#'
#' @param path Project root.
#' @param message Commit message.
#' @return `TRUE` if a commit was made, invisibly.
#' @export
commit_project <- function(path = ".", message) {
  proj_path <- .check_project(path)
  if (!.is_git_repo(proj_path)) return(invisible(FALSE))
  if (!length(.git(proj_path, c("status", "--porcelain"), error = FALSE))) return(invisible(FALSE))
  .git(proj_path, c("add", "-A"))
  .git(proj_path, c(.git_identity(proj_path), "commit", "-q", "-m", shQuote(message)))
  invisible(TRUE)
}

# A commit needs an author; if Git has none configured, use the metawoRld person.
.git_identity <- function(proj_path) {
  if (length(.git(proj_path, c("config", "user.email"), error = FALSE))) return(character(0))
  who <- getOption("metawoRld.person", "metawoRld")
  c("-c", shQuote(paste0("user.name=", who)), "-c", shQuote(paste0("user.email=", who, "@metawoRld.invalid")))
}

# Git merges screening.csv (append-only) line by line instead of conflicting.
.ensure_gitattributes <- function(proj_path) {
  f <- fs::path(proj_path, ".gitattributes")
  line <- "screening.csv merge=union"
  x <- if (fs::file_exists(f)) readLines(f, warn = FALSE) else character(0)
  if (!line %in% x) writeLines(c(x, line), f)
  invisible(f)
}
