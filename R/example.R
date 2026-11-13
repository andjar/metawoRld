#' Build the worked example project
#'
#' Creates a small but complete metawoRld project on maternal interleukin-6
#' (IL-6) in preeclampsia compared with normotensive pregnancy, using values
#' from four open-access studies shipped with the package
#' (`system.file("extdata", "il6-preeclampsia", package = "metawoRld")`; see
#' the README there for the sources). It runs the whole workflow: people, a
#' search log, records, screening by two people with a consensus decision,
#' submissions, acceptance, dual extraction of one study with
#' reconciliation, a refinement of groups during the review (onset of
#' preeclampsia added as a group fact; one study's early/late-onset groups
#' linked to its total with [amend_study()]), and some verified values. Use it to try the package, the
#' review app and the dashboard.
#'
#' This is a documentation example, not a systematic review. The people are
#' example personas.
#'
#' @param path Folder to create. Must not exist yet.
#' @param website Add the Quarto website templates? Defaults to `TRUE`.
#' @return The project path, invisibly.
#' @export
#' @examples
#' p <- example_project(file.path(tempdir(), "il6-example"))
#' list_studies(p)
#' prisma_counts(p)
#' \dontrun{
#' review_values(p)
#' generate_website(p)
#' }
example_project <- function(path, website = TRUE) {
  src <- system.file("extdata", "il6-preeclampsia", package = "metawoRld")
  ext <- function(f) yaml::read_yaml(fs::path(src, "extractions", paste0(f, ".yml")))
  q <- function(expr) suppressMessages(expr)

  q(create_metawoRld(
    path,
    project_name = "IL-6 in preeclampsia (example)",
    project_description = paste(
      "Worked example for the metawoRld documentation: circulating interleukin-6 in women with",
      "preeclampsia compared with normotensive pregnant women. Not a systematic review."
    ),
    inclusion_criteria = c("Women with preeclampsia and normotensive pregnant controls",
                           "Maternal serum or plasma IL-6 reported per group"),
    exclusion_criteria = c("Other hypertensive disorders only (e.g. HELLP syndrome)",
                           "Group sizes or IL-6 values not reported"),
    git_init = FALSE, website = website
  ))

  # People (example personas)
  add_person(path, "ext1", "Example extractor 1", roles = c("screener", "extractor", "verifier"))
  add_person(path, "ext2", "Example extractor 2", roles = c("screener", "extractor", "verifier"))
  add_person(path, "arb", "Example arbiter", roles = c("admin", "arbiter", "verifier"))

  # Vocabulary, group categories and the planned meta-analyses
  rules <- read_rules(path)
  rules$variables <- list(
    `IL-6` = list(synonyms = list("Interleukin-6", "IL6"), unit = "pg/mL"),
    `Maternal age` = list(synonyms = list("Age"), unit = "years"),
    `Gestational age at sampling` = list(synonyms = list("Gestational age"), unit = "weeks")
  )
  rules$group_categories <- list(
    PE = list(description = "Preeclampsia", synonyms = list("Preeclampsia", "Pre-eclampsia")),
    Control = list(description = "Normotensive pregnancy",
                   synonyms = list("Normal pregnant", "Normotensive controls", "Healthy pregnant controls",
                                   "Normotensive pregnancy")),
    GH = list(description = "Gestational hypertension", synonyms = list("Gestational hypertension"))
  )
  .write_rules(fs::path(path, "_rules.yml"), rules)
  writeLines(c(
    "analyses:",
    "  - name: IL-6 in preeclampsia vs normotensive pregnancy",
    "    variable: IL-6",
    "    measure: SMD",
    "    compare: [PE, Control]",
    "    timepoint: Antepartum",
    "  - name: IL-6 in normotensive pregnancy",
    "    variable: IL-6",
    "    measure: MN",
    "    group: Control",
    "    timepoint: Antepartum"
  ), fs::path(path, "_analyses.yml"))

  # Search log and records
  add_search(path, "example-2026-11", "PubMed and journal websites",
             "interleukin-6 AND (preeclampsia OR pre-eclampsia), open-access full texts (documentation example, not a systematic search)",
             results = 6, person = "arb", date = as.Date("2026-11-13"))
  recs <- utils::read.csv(fs::path(src, "records.csv"), colClasses = "character")
  rid <- add_records(path, recs, search = "example-2026-11")$record_id

  # Title/abstract screening by two people; one conflict settled by consensus
  add_screening(path, rid, "title_abstract", c("include", "include", "include", "include", "exclude", "include"),
                c("", "", "", "", "HELLP syndrome, not preeclampsia", ""), person = "ext1")
  add_screening(path, rid, "title_abstract", c("include", "include", "include", "include", "include", "include"),
                person = "ext2")
  add_screening(path, rid[5], "title_abstract", "exclude", "HELLP syndrome, not preeclampsia",
                person = "arb", kind = "consensus")
  # Full-text screening
  add_screening(path, rid[-5], "full_text", c("include", "include", "include", "include", "exclude"),
                c("", "", "", "", "Group sizes not reported"), person = "arb")

  # Single extraction: submit and accept
  for (id in c("afshari2005", "gencheva2022", "dewi2026")) {
    e <- ext(id)
    sid <- q(submit_study(path, id, e$metadata, e$data_points, person = "ext1", channel = "entry_form"))
    q(accept_submission(path, id, sid, person = "arb"))
  }

  # Dual extraction of one study, reconciled by the arbiter
  e1 <- ext("vitoratos2010-ext1")
  e2 <- ext("vitoratos2010-ext2")
  s1 <- q(submit_study(path, "vitoratos2010", e1$metadata, e1$data_points, person = "ext1", channel = "entry_form"))
  s2 <- q(submit_study(path, "vitoratos2010", e2$metadata, e2$data_points, person = "ext2", channel = "csv_import"))
  cmp <- compare_extractions(path, "vitoratos2010", c(s1, s2))
  dis <- cmp[!cmp$agree, , drop = FALSE]
  rows <- unique(dis$key[dis$part == "data"])
  choices <- data.frame(
    part = c("metadata", rep("data", length(rows))),
    key = c("notes", rows),
    field = c("notes", rep("(row)", length(rows))),
    from = c(s1, ifelse(grepl("mean_se", rows), s1, NA)),
    value = c(NA, ifelse(grepl("mean_se", rows), NA, "drop")),
    stringsAsFactors = FALSE
  )
  q(reconcile(path, "vitoratos2010", c(s1, s2), choices = choices, person = "arb"))

  # The Gencheva report has no DOI or PMID to match on, so link it by hand
  link_record(path, rid[3], "gencheva2022")

  # Refining groups during the review: onset of preeclampsia becomes a group
  # fact, and Gencheva's early/late-onset groups are linked to its PE total
  add_group_fact(path, "onset", c("early", "late", "not reported"),
                 "Onset of preeclampsia as defined by the study (early or late onset).")
  rules <- read_rules(path)
  rules$group_categories$PE_early <- list(description = "Early-onset preeclampsia",
                                          where = list(category = "PE", onset = "early"))
  rules$group_categories$PE_late <- list(description = "Late-onset preeclampsia",
                                         where = list(category = "PE", onset = "late"))
  .write_rules(fs::path(path, "_rules.yml"), rules)
  am <- q(amend_study(path, "gencheva2022", person = "ext1", note = "Onset subgroups linked to the PE total.",
                      groups = list(list(group_id = "pe_early", parent_group_id = "pe", onset = "early"),
                                    list(group_id = "pe_late", parent_group_id = "pe", onset = "late"))))
  q(accept_submission(path, "gencheva2022", am, person = "arb"))

  # Some values verified against the papers; the rest wait in review_values()
  set_verification(path, "afshari2005", c("dp1", "dp2"), reviewer = "ext2")
  set_verification(path, "dewi2026", c("dp1", "dp2"), reviewer = "ext2")
  set_verification(path, "vitoratos2010", c("dp1", "dp2", "dp3", "dp4"), reviewer = "arb",
                   note = "Checked against Table 2 (mean +/- SE).")
  invisible(path)
}
