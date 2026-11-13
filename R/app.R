# The metawoRld app: one front door for the whole review, without writing R.

.app_state <- new.env(parent = emptyenv())

#' Open the metawoRld app
#'
#' A Shiny app for running a review without writing R:
#'
#' * **Start**: create a review with a short wizard, open an existing
#'   project, or explore the worked example.
#' * **Overview**: progress and what to do next.
#' * **Inbox**: submissions waiting to be accepted; compare two extractions
#'   of the same study and pick the right values (reconcile).
#' * **Review**: verify values against the papers (the [review_values()] app).
#' * **Setup**: people, variables, group categories, planned analyses,
#'   criteria and the form.
#' * **Check**: [check_project()] in plain language.
#' * **Publish**: build and open the living meta-analysis website.
#' * **Sync**: share work with the team through Git ([sync_project()]); new
#'   submissions from others then appear in the Inbox.
#'
#' Everything the app does is done with the package's functions, so it can
#' also be done (and reproduced) from R.
#'
#' @param path A project to open. Defaults to none (the Start screen).
#' @param person Your id in the project's `people.yml`. Defaults to the
#'   `metawoRld.person` option.
#' @param launch.browser Passed to [shiny::runApp()].
#' @return `launch()` runs the app; `launch_app()` returns the app object.
#' @export
launch <- function(path = NULL, person = getOption("metawoRld.person"), launch.browser = TRUE) {
  shiny::runApp(launch_app(path, person), launch.browser = launch.browser)
}

#' @rdname launch
#' @export
launch_app <- function(path = NULL, person = getOption("metawoRld.person")) {
  rlang::check_installed(c("shiny", "bslib", "DT"), reason = "to use the metawoRld app.")
  if (!is.null(path)) .app_state$path <- as.character(.check_project(path))
  if (!is.null(person)) .app_state$person <- person
  shiny::shinyApp(.app_ui(), .app_server)
}

# ---- UI ----------------------------------------------------------------------

.app_ui <- function() {
  has_git <- nzchar(.git_bin())
  bslib::page_navbar(
    title = "metawoRld", id = "nav", fillable = FALSE,
    theme = bslib::bs_theme(version = 5, primary = "#2c6e9e"),
    header = shiny::tags$head(shiny::tags$style(shiny::HTML(.app_css))),
    bslib::nav_panel("Start", value = "start", icon = shiny::icon("house"), .start_ui(has_git)),
    bslib::nav_panel("Overview", value = "overview", shiny::uiOutput("overview")),
    bslib::nav_panel("Inbox", value = "inbox", .inbox_ui()),
    bslib::nav_panel("Review", value = "review", shiny::uiOutput("review_panel")),
    bslib::nav_panel("Setup", value = "setup", .setup_ui()),
    bslib::nav_panel("Check", value = "check", .check_ui()),
    bslib::nav_panel("Publish", value = "publish", .publish_ui()),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::uiOutput("project_badge", inline = TRUE)),
    bslib::nav_item(shiny::uiOutput("person_ui", inline = TRUE)),
    bslib::nav_item(shiny::actionButton("sync_open", shiny::HTML("&#8645; Sync"), class = "btn-sm btn-outline-primary"))
  )
}

.start_ui <- function(has_git) {
  home <- fs::path_home("metawoRld")
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header(shiny::strong("Start a new review")),
      shiny::textInput("new_title", "Title", placeholder = "e.g. IL-6 in preeclampsia", width = "100%"),
      shiny::textAreaInput("new_question", "Question", rows = 2, width = "100%",
                           placeholder = "e.g. Is IL-6 higher in women with preeclampsia than in normotensive pregnant women?"),
      shiny::radioButtons("new_design", "What will you compare?", inline = FALSE,
                          c("Two groups (e.g. cases vs controls)" = "two_groups",
                            "One group (e.g. a pooled mean or prevalence)" = "one_group",
                            "Decide later" = "blank")),
      shiny::conditionalPanel("input.new_design != 'blank'",
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::textInput("new_group1", "Group of interest", "Cases"),
          shiny::conditionalPanel("input.new_design == 'two_groups'", shiny::textInput("new_group2", "Compared with", "Controls"))),
        bslib::layout_columns(col_widths = c(6, 6),
          shiny::textInput("new_variable", "Main outcome or measurement (optional)", placeholder = "e.g. IL-6"),
          shiny::textInput("new_unit", "Unit", placeholder = "e.g. pg/mL"))),
      bslib::layout_columns(col_widths = c(6, 6),
        shiny::textAreaInput("new_incl", "Inclusion criteria (one per line)", rows = 3),
        shiny::textAreaInput("new_excl", "Exclusion criteria (one per line)", rows = 3)),
      bslib::layout_columns(col_widths = c(4, 8),
        shiny::textInput("new_pid", "Your initials", placeholder = "e.g. ahj"),
        shiny::textInput("new_pname", "Your name")),
      shiny::textInput("new_folder", "Save in folder", value = fs::path(home, "my-review"), width = "100%"),
      shiny::checkboxInput("new_git", "Keep history and share with a team (Git)", value = has_git),
      shiny::actionButton("new_create", "Create review", class = "btn-primary", style = "align-self: flex-start")
    ),
    shiny::div(
      bslib::card(
        bslib::card_header(shiny::strong("Open a review")),
        shiny::textInput("open_folder", "Project folder", width = "100%", placeholder = fs::path(home, "my-review")),
        shiny::actionButton("open_go", "Open", class = "btn-outline-primary", style = "align-self: flex-start"),
        shiny::uiOutput("recent_ui")
      ),
      bslib::card(
        bslib::card_header(shiny::strong("Explore the worked example")),
        shiny::p("A small project on IL-6 in preeclampsia with real values from four open-access studies:",
                 "submissions, a reconciled dual extraction, verified values and analyses."),
        shiny::actionButton("example_go", "Open the example", class = "btn-outline-secondary", style = "align-self: flex-start")
      )
    )
  )
}

.inbox_ui <- function() {
  bslib::layout_columns(
    col_widths = c(5, 7),
    bslib::card(bslib::card_header("Waiting for a decision"), DT::DTOutput("inbox_table"),
                shiny::div(class = "small text-muted", "Submissions arrive from the entry form, CSV files, DataFindR or colleagues (Sync).")),
    shiny::uiOutput("inbox_detail")
  )
}

.setup_ui <- function() {
  bslib::navset_card_underline(
    id = "setup_tabs",
    bslib::nav_panel("People", DT::DTOutput("people_table"), shiny::hr(),
      shiny::h6("Add a person"),
      bslib::layout_columns(col_widths = c(2, 4, 6),
        shiny::textInput("pp_id", "Initials"), shiny::textInput("pp_name", "Name"),
        shiny::checkboxGroupInput("pp_roles", "Roles", inline = TRUE, selected = c("extractor", "verifier"),
                                  c("admin", "screener", "extractor", "verifier", "arbiter"))),
      shiny::actionButton("pp_add", "Add person", class = "btn-outline-primary btn-sm", style = "align-self: flex-start")),
    bslib::nav_panel("Variables", shiny::p(class = "text-muted small", "The outcomes and measurements you extract, with their unit and other names used in papers."),
      DT::DTOutput("var_table"), shiny::hr(),
      bslib::layout_columns(col_widths = c(3, 2, 5, 2),
        shiny::textInput("var_name", "Variable"), shiny::textInput("var_unit", "Unit"),
        shiny::textInput("var_syn", "Other names (comma-separated)"),
        shiny::div(class = "pt-4", shiny::actionButton("var_add", "Add / update", class = "btn-outline-primary btn-sm")))),
    bslib::nav_panel("Groups", shiny::p(class = "text-muted small", "Categories that study groups are mapped to (e.g. cases and controls). Advanced: a rule over group facts, e.g. category = PE; onset = early."),
      DT::DTOutput("cat_table"), shiny::hr(),
      bslib::layout_columns(col_widths = c(3, 4, 3, 2),
        shiny::textInput("cat_name", "Category"), shiny::textInput("cat_syn", "Other names (comma-separated)"),
        shiny::textInput("cat_where", "Rule (optional)", placeholder = "category = PE; onset = early"),
        shiny::div(class = "pt-4", shiny::actionButton("cat_add", "Add / update", class = "btn-outline-primary btn-sm")))),
    bslib::nav_panel("Analyses", shiny::p(class = "text-muted small", "Planned meta-analyses; they are re-run whenever the website is built."),
      DT::DTOutput("an_table"),
      shiny::actionButton("an_delete", "Delete selected", class = "btn-outline-danger btn-sm mt-2", style = "align-self: flex-start"), shiny::hr(),
      bslib::layout_columns(col_widths = c(4, 3, 5),
        shiny::textInput("an_name", "Name"), shiny::selectInput("an_variable", "Variable", NULL),
        shiny::selectInput("an_measure", "Effect measure", c(
          "Standardized mean difference (two groups)" = "SMD", "Mean difference (two groups)" = "MD",
          "Ratio of means (two groups)" = "ROM", "Odds ratio (two groups)" = "OR",
          "Pooled mean (one group)" = "MN", "Pooled proportion (one group)" = "PLO"))),
      bslib::layout_columns(col_widths = c(3, 3, 3, 3),
        shiny::selectInput("an_g1", "Group", NULL), shiny::uiOutput("an_g2_ui"),
        shiny::textInput("an_timepoint", "Time point (optional)"),
        shiny::selectInput("an_levels", "Groups with subgroups", c("Use the reported total" = "reported", "Pool complete subgroups" = "pool"))),
      shiny::actionButton("an_add", "Add analysis", class = "btn-outline-primary btn-sm", style = "align-self: flex-start")),
    bslib::nav_panel("Criteria", bslib::layout_columns(col_widths = c(6, 6),
        shiny::textAreaInput("crit_incl", "Inclusion criteria (one per line)", rows = 6, width = "100%"),
        shiny::textAreaInput("crit_excl", "Exclusion criteria (one per line)", rows = 6, width = "100%")),
      shiny::actionButton("crit_save", "Save criteria", class = "btn-outline-primary btn-sm", style = "align-self: flex-start")),
    bslib::nav_panel("Form", shiny::p(class = "text-muted small", "What is extracted from each study (forms/extraction.yml). Advanced users can edit the file directly."),
      shiny::uiOutput("form_view"), shiny::hr(), shiny::h6("Add a fact to groups (e.g. onset of disease)"),
      bslib::layout_columns(col_widths = c(3, 4, 5),
        shiny::textInput("gf_name", "Name", placeholder = "onset"), shiny::textInput("gf_values", "Allowed values (comma-separated, optional)", placeholder = "early, late, not reported"),
        shiny::textInput("gf_desc", "Description")),
      shiny::actionButton("gf_add", "Add to the form", class = "btn-outline-primary btn-sm", style = "align-self: flex-start"))
  )
}

.check_ui <- function() {
  shiny::div(
    shiny::div(class = "d-flex gap-2 mb-3", shiny::actionButton("check_run", "Check the project", class = "btn-primary")),
    shiny::uiOutput("check_result")
  )
}

.publish_ui <- function() {
  bslib::card(
    bslib::card_header("Living meta-analysis website"),
    shiny::p("Builds the dashboard (overview, meta-analyses, results, one page per study) with Quarto."),
    shiny::actionButton("publish_go", "Build the website", class = "btn-primary", style = "align-self: flex-start"),
    shiny::uiOutput("publish_result")
  )
}

# ---- server -------------------------------------------------------------------

.app_server <- function(input, output, session) {
  proj <- shiny::reactiveVal(.app_state$path)
  tick <- shiny::reactiveVal(0)
  bump <- function() tick(tick() + 1)
  person <- shiny::reactiveVal(.app_state$person %||% getOption("metawoRld.person"))
  notify <- function(msg, type = "message") shiny::showNotification(msg, type = type, duration = if (type == "error") NULL else 6)
  try_do <- function(expr, success = NULL) {
    out <- tryCatch(expr, error = function(e) e)
    if (inherits(out, "error")) {
      errs <- out$validation_errors %||% character(0)
      notify(shiny::HTML(paste0(htmltools::htmlEscape(conditionMessage(out)),
                                if (length(errs)) paste0("<ul>", paste0("<li>", htmltools::htmlEscape(errs), "</li>", collapse = ""), "</ul>"))), "error")
      return(invisible(NULL))
    }
    if (!is.null(success)) notify(success)
    invisible(out)
  }
  auto_commit <- function(msg) if (isTRUE(.app_state$auto_commit %||% TRUE)) try(commit_project(proj(), msg), silent = TRUE)
  open_project <- function(p) {
    p <- try_do(as.character(.check_project(p)))
    if (is.null(p)) return()
    .app_state$path <- p
    .remember_recent(p)
    session$reload()
  }

  # tabs visible only with a project
  project_tabs <- c("overview", "inbox", "review", "setup", "check", "publish")
  shiny::observe({
    has <- !is.null(proj())
    for (t in project_tabs) if (has) bslib::nav_show("nav", t) else bslib::nav_hide("nav", t)
    if (has) bslib::nav_select("nav", "overview")
  })

  output$project_badge <- shiny::renderUI({
    p <- proj(); if (is.null(p)) return(NULL)
    shiny::span(class = "navbar-text me-3 small", shiny::icon("folder-open"), read_config(p)$project_name %||% fs::path_file(p))
  })
  output$person_ui <- shiny::renderUI({
    p <- proj(); tick()
    if (is.null(p)) return(NULL)
    pp <- read_people(p)
    if (!nrow(pp)) return(shiny::span(class = "navbar-text me-3 small text-warning", "Add yourself in Setup > People"))
    choices <- stats::setNames(pp$id, paste0(pp$name, " (", pp$id, ")"))
    shiny::div(class = "me-2", style = "width: 210px", shiny::selectInput("person", NULL, choices, selected = person() %||% pp$id[1], width = "100%"))
  })
  shiny::observeEvent(input$person, {
    person(input$person); .app_state$person <- input$person
    options(metawoRld.person = input$person)
  })

  # ---- Start
  output$recent_ui <- shiny::renderUI({
    r <- .recent_projects()
    if (!length(r)) return(NULL)
    shiny::div(class = "mt-3", shiny::div(class = "small text-muted", "Recent"),
      lapply(seq_along(r), function(i) shiny::div(shiny::actionLink(paste0("recent_", i), r[i]))))
  })
  shiny::observe({
    r <- .recent_projects()
    lapply(seq_along(r), function(i) shiny::observeEvent(input[[paste0("recent_", i)]], open_project(r[i]), ignoreInit = TRUE, once = TRUE))
  })
  shiny::observeEvent(input$open_go, open_project(input$open_folder))
  shiny::observeEvent(input$example_go, {
    p <- fs::path(tempdir(), paste0("il6-example-", format(Sys.time(), "%H%M%S")))
    shiny::withProgress(message = "Building the example...", try_do(example_project(p)))
    .app_state$person <- "arb"
    open_project(p)
  })
  shiny::observeEvent(input$new_create, {
    lines <- function(x) trimws(strsplit(x %||% "", "\n")[[1]])
    p <- try_do(new_review(
      path = path.expand(input$new_folder), title = input$new_title, question = input$new_question,
      design = input$new_design, groups = c(input$new_group1, input$new_group2 %||% ""),
      variable = input$new_variable, unit = if (nzchar(input$new_unit %||% "")) input$new_unit,
      inclusion_criteria = lines(input$new_incl), exclusion_criteria = lines(input$new_excl),
      person_id = tolower(trimws(input$new_pid)), person_name = input$new_pname, git = isTRUE(input$new_git)))
    if (!is.null(p)) { .app_state$person <- tolower(trimws(input$new_pid)); open_project(p) }
  })

  # ---- Overview
  status <- shiny::reactive({
    p <- proj(); tick()
    if (is.null(p)) return(NULL)
    subs <- list_submissions(p)
    vs <- tryCatch(verification_status(p), error = function(e) NULL)
    chk <- suppressMessages(tryCatch(check_project(p), error = function(e) NULL))
    list(studies = length(list_studies(p)), pending = sum(subs$status == "pending"),
         values = if (is.null(vs)) 0 else nrow(vs), verified = if (is.null(vs)) 0 else sum(vs$status == "verified"),
         errors = sum(chk$level == "error"), warnings = sum(chk$level == "warning"),
         people = nrow(read_people(p)), analyses = length(tryCatch(read_analyses(p), error = function(e) list())),
         site = fs::file_exists(fs::path(p, "_site", "index.html")), sync = sync_status(p))
  })
  output$overview <- shiny::renderUI({
    s <- status(); if (is.null(s)) return(NULL)
    vb <- function(title, value, icon, theme) bslib::value_box(title, value, showcase = shiny::icon(icon), theme = theme)
    pct <- if (s$values) paste0(round(100 * s$verified / s$values), "%") else "-"
    steps <- list()
    step <- function(text, tab) steps[[length(steps) + 1]] <<- shiny::tags$li(text, " ", shiny::actionLink(paste0("go_", tab), shiny::HTML(paste0("Go to ", tab, " &rarr;"))))
    if (!s$people) step("Register the people who work on the review.", "setup")
    if (s$pending) step(paste0(s$pending, " submission", if (s$pending > 1) "s are" else " is", " waiting for a decision."), "inbox")
    if (s$values - s$verified > 0) step(paste0(s$values - s$verified, " value", if (s$values - s$verified > 1) "s are" else " is", " not yet verified against the papers."), "review")
    if (s$errors) step(paste0("The project check found ", s$errors, " problem", if (s$errors > 1) "s", "."), "check")
    if (!s$analyses) step("Plan a meta-analysis.", "setup")
    if (s$studies && !s$site) step("Build the website to see the living meta-analysis.", "publish")
    if (isTRUE(s$sync$repo) && isTRUE(s$sync$changes > 0)) step("You have changes not yet shared with the team (Sync).", "sync")
    if (!s$studies && !s$pending) steps <- c(steps, list(shiny::tags$li("Add the first study: in R with ", shiny::code("submit_study()"),
      ", from a CSV file with ", shiny::code("submission_template()"), ", or with DataFindR.")))
    shiny::tagList(
      bslib::layout_columns(col_widths = c(3, 3, 3, 3), fill = FALSE,
        vb("Studies accepted", s$studies, "book", "primary"),
        vb("Waiting in the inbox", s$pending, "inbox", if (s$pending) "warning" else "secondary"),
        vb("Values verified", pct, "check-double", "success"),
        vb("Check", if (s$errors) paste(s$errors, "errors") else "OK", "shield-halved", if (s$errors) "danger" else "light")),
      bslib::card(bslib::card_header("Next steps"),
                  if (length(steps)) shiny::tags$ol(steps) else shiny::p("Everything is up to date."))
    )
  })
  for (tab in c("setup", "inbox", "review", "check", "publish")) local({
    t <- tab; shiny::observeEvent(input[[paste0("go_", t)]], bslib::nav_select("nav", t))
  })
  shiny::observeEvent(input$go_sync, .sync_modal(proj()))

  # ---- Inbox
  pending <- shiny::reactive({ p <- proj(); tick(); if (is.null(p)) NULL else list_submissions(p, status = "pending") })
  output$inbox_table <- DT::renderDT({
    s <- pending(); shiny::req(s)
    show <- data.frame(Study = s$study_id, From = s$person, Channel = s$channel, `Data points` = s$n_data_points,
                       Warnings = s$n_warnings, Submitted = substr(s$created, 1, 16), check.names = FALSE)
    DT::datatable(show, selection = "single", rownames = FALSE, options = list(dom = "tp", pageLength = 12, language = list(emptyTable = "Nothing waiting.")))
  })
  sel_sub <- shiny::reactive({
    s <- pending(); i <- input$inbox_table_rows_selected
    if (is.null(s) || !length(i) || i > nrow(s)) NULL else s[i, ]
  })
  output$inbox_detail <- shiny::renderUI({
    s <- sel_sub(); p <- proj()
    if (is.null(s)) return(bslib::card(bslib::card_body(class = "text-muted", "Select a submission to see what it contains.")))
    sub <- tryCatch(read_submission(p, s$study_id, s$extraction_id), error = function(e) NULL)
    if (is.null(sub)) return(bslib::card(bslib::card_body("This submission cannot be read.")))
    others <- pending()[pending()$study_id == s$study_id, , drop = FALSE]
    m <- sub$metadata
    gtab <- if (length(m$groups)) .groups_df(m$groups) else NULL
    d <- as.data.frame(sub$data)
    cols <- intersect(c("measurement_id", "variable_name", "group_id", "timepoint", "statistic_type", "value1", "value2", "value3", "sample_size", "source"), names(d))
    warns <- unlist(sub$submission$gate$warnings)
    bslib::card(
      bslib::card_header(shiny::strong(s$study_id), shiny::HTML(" &middot; "), s$extraction_id,
                         if (identical(sub$submission$kind, "amendment")) shiny::span(class = "badge text-bg-info ms-2", "amendment")),
      shiny::div(shiny::strong(m$title %||% ""), shiny::br(),
                 shiny::span(class = "text-muted small", paste(unlist(m$authors)[1], if (length(unlist(m$authors)) > 1) "et al.", m$year %||% ""),
                             shiny::HTML(" &middot; by "), s$person, " via ", s$channel)),
      if (length(warns)) shiny::div(class = "alert alert-warning small py-2 mt-2", shiny::strong("Checks: "),
                                    shiny::tags$ul(class = "mb-0", lapply(warns, shiny::tags$li))),
      if (nzchar(sub$submission$note %||% "")) shiny::div(class = "small mt-2", shiny::em(sub$submission$note)),
      if (!is.null(gtab)) shiny::tagList(shiny::h6(class = "mt-3", "Groups"),
        shiny::tags$table(class = "table table-sm small", shiny::tags$thead(shiny::tags$tr(lapply(names(gtab), shiny::tags$th))),
          shiny::tags$tbody(lapply(seq_len(nrow(gtab)), function(i) shiny::tags$tr(lapply(gtab[i, ], function(v) shiny::tags$td(ifelse(is.na(v), "", v)))))))),
      shiny::h6("Data points"),
      shiny::div(style = "max-height: 300px; overflow: auto", shiny::tags$table(class = "table table-sm small",
        shiny::tags$thead(shiny::tags$tr(lapply(cols, shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(d)), function(i) shiny::tags$tr(lapply(cols, function(cn) shiny::tags$td(ifelse(is.na(d[[cn]][i]), "", as.character(d[[cn]][i]))))))))),
      .changes_ui(p, s),
      if (nrow(others) > 1) shiny::div(class = "alert alert-info small py-2",
        nrow(others), " extractions of this study are waiting. Compare them and pick the right values:",
        shiny::actionButton("inbox_compare", "Compare and reconcile", class = "btn-sm btn-info ms-2")),
      shiny::div(class = "d-flex gap-2 align-items-end mt-2",
        shiny::actionButton("inbox_accept", shiny::HTML("&#10003; Accept"), class = "btn-success"),
        shiny::textInput("inbox_reason", NULL, placeholder = "Reason (for rejecting)", width = "260px"),
        shiny::actionButton("inbox_reject", shiny::HTML("&#10007; Reject"), class = "btn-outline-danger"))
    )
  })
  shiny::observeEvent(input$inbox_accept, {
    s <- sel_sub(); shiny::req(s)
    r <- try_do(accept_submission(proj(), s$study_id, s$extraction_id, person = person()),
                paste0("Accepted ", s$study_id, "."))
    if (!is.null(r)) { auto_commit(paste0("Accept ", s$study_id, " (", s$extraction_id, ") by ", person() %||% "?")); bump() }
  })
  shiny::observeEvent(input$inbox_reject, {
    s <- sel_sub(); shiny::req(s)
    try_do(reject_submission(proj(), s$study_id, s$extraction_id, reason = input$inbox_reason, person = person()),
           paste0("Rejected ", s$extraction_id, "."))
    auto_commit(paste0("Reject ", s$study_id, " (", s$extraction_id, ")")); bump()
  })
  cmp_state <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$inbox_compare, {
    s <- sel_sub(); shiny::req(s)
    ids <- pending()$extraction_id[pending()$study_id == s$study_id]
    cmp <- try_do(compare_extractions(proj(), s$study_id, ids))
    if (is.null(cmp)) return()
    dis <- cmp[!cmp$agree, , drop = FALSE]
    cmp_state(list(study_id = s$study_id, ids = ids, dis = dis))
    who <- vapply(ids, function(x) pending()$person[pending()$extraction_id == x], character(1))
    rows <- lapply(seq_len(nrow(dis)), function(i) {
      vals <- vapply(ids, function(x) { v <- dis[[x]][i]; if (is.na(v)) "(absent)" else v }, character(1))
      shiny::tags$tr(shiny::tags$td(shiny::span(class = "small", dis$part[i]), shiny::br(), shiny::code(class = "small", dis$key[i])),
                     shiny::tags$td(dis$field[i]),
                     shiny::tags$td(shiny::radioButtons(paste0("pick_", i), NULL, inline = FALSE,
                                                        choiceNames = paste0(who, ": ", substr(vals, 1, 60)), choiceValues = ids)))
    })
    shiny::showModal(shiny::modalDialog(
      title = paste0("Reconcile ", s$study_id, ": ", nrow(dis), " disagreement", if (nrow(dis) != 1) "s"), size = "xl", easyClose = TRUE,
      if (!nrow(dis)) shiny::p("The extractions agree on everything.") else shiny::tagList(
        shiny::p(class = "small text-muted", "For each disagreement, pick the extraction that is right (check the paper). ",
                 "Values all extractions agree on are accepted as they are; provenance records who agreed."),
        shiny::tags$table(class = "table table-sm", shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Where"), shiny::tags$th("Field"), shiny::tags$th("Keep"))),
                          shiny::tags$tbody(rows))),
      footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("reconcile_go", "Reconcile", class = "btn-primary"))
    ))
  })
  shiny::observeEvent(input$reconcile_go, {
    st <- cmp_state(); shiny::req(st)
    picks <- vapply(seq_len(nrow(st$dis)), function(i) input[[paste0("pick_", i)]] %||% st$ids[1], character(1))
    ch <- .choices_from_picks(st$dis, picks)
    r <- try_do(reconcile(proj(), st$study_id, st$ids, base = st$ids[1], choices = ch, person = person()),
                paste0("Reconciled ", st$study_id, "."))
    if (!is.null(r)) { shiny::removeModal(); auto_commit(paste0("Reconcile ", st$study_id, " by ", person() %||% "?")); bump() }
  })

  # ---- Review (the review_values() module)
  review_started <- FALSE
  output$review_panel <- shiny::renderUI({
    p <- proj(); if (is.null(p)) return(NULL)
    .review_ui("review", p, reviewer = person(), standalone = FALSE)
  })
  shiny::observe({
    p <- proj()
    if (!is.null(p) && !review_started) { review_started <<- TRUE; .review_server("review", p, refresh = tick) }
  })

  # ---- Setup
  rules_r <- shiny::reactive({ tick(); shiny::req(proj()); read_rules(proj()) })
  output$people_table <- DT::renderDT({
    tick(); shiny::req(proj())
    DT::datatable(as.data.frame(read_people(proj())), rownames = FALSE, selection = "none", options = list(dom = "t"))
  })
  shiny::observeEvent(input$pp_add, {
    r <- try_do(add_person(proj(), tolower(trimws(input$pp_id)), input$pp_name, roles = input$pp_roles), paste0("Added ", input$pp_name, "."))
    if (!is.null(r)) { auto_commit(paste0("Add person ", input$pp_id)); bump() }
  })
  output$var_table <- DT::renderDT({
    v <- rules_r()$variables
    df <- data.frame(Variable = names(v), Unit = vapply(v, function(x) as.character(x$unit %||% ""), ""),
                     `Other names` = vapply(v, function(x) paste(unlist(x$synonyms), collapse = ", "), ""), check.names = FALSE)
    DT::datatable(df, rownames = FALSE, selection = "none", options = list(dom = "t", language = list(emptyTable = "No variables yet.")))
  })
  shiny::observeEvent(input$var_add, {
    nm <- trimws(input$var_name); shiny::req(nzchar(nm))
    r <- rules_r(); v <- r$variables[[nm]] %||% list()
    if (nzchar(input$var_unit)) v$unit <- trimws(input$var_unit)
    syn <- trimws(strsplit(input$var_syn, ",")[[1]]); syn <- syn[nzchar(syn)]
    if (length(syn)) v$synonyms <- as.list(unique(c(unlist(v$synonyms), syn)))
    r$variables[[nm]] <- if (length(v)) v else list(unit = NULL)
    try_do(.write_rules(fs::path(proj(), "_rules.yml"), r), paste0("Saved variable ", nm, "."))
    auto_commit(paste0("Variable ", nm)); bump()
  })
  output$cat_table <- DT::renderDT({
    cc <- rules_r()$group_categories
    df <- data.frame(Category = names(cc), `Other names` = vapply(cc, function(x) paste(unlist(x$synonyms), collapse = ", "), ""),
                     Rule = vapply(cc, function(x) if (length(x$where)) paste(names(x$where), "=", vapply(x$where, function(w) paste(unlist(w), collapse = "|"), ""), collapse = "; ") else "", ""),
                     check.names = FALSE)
    DT::datatable(df, rownames = FALSE, selection = "none", options = list(dom = "t", language = list(emptyTable = "No group categories yet.")))
  })
  shiny::observeEvent(input$cat_add, {
    nm <- trimws(input$cat_name); shiny::req(nzchar(nm))
    r <- rules_r(); cc <- r$group_categories[[nm]] %||% list()
    syn <- trimws(strsplit(input$cat_syn, ",")[[1]]); syn <- syn[nzchar(syn)]
    if (length(syn)) cc$synonyms <- as.list(unique(c(unlist(cc$synonyms), syn)))
    w <- .parse_where(input$cat_where)
    if (length(w)) cc$where <- w
    r$group_categories[[nm]] <- if (length(cc)) cc else list(synonyms = list())
    try_do(.write_rules(fs::path(proj(), "_rules.yml"), r), paste0("Saved category ", nm, "."))
    auto_commit(paste0("Group category ", nm)); bump()
  })
  analyses_r <- shiny::reactive({ tick(); shiny::req(proj()); tryCatch(read_analyses(proj()), error = function(e) list()) })
  output$an_table <- DT::renderDT({
    a <- analyses_r()
    df <- data.frame(Name = names(a), Variable = vapply(a, `[[`, "", "variable"), Measure = vapply(a, `[[`, "", "measure"),
                     Groups = vapply(a, function(x) if (length(x$compare)) paste(x$compare, collapse = " vs ") else x$group %||% "", ""),
                     `Time point` = vapply(a, function(x) if (is.na(x$timepoint)) "" else x$timepoint, ""), check.names = FALSE)
    DT::datatable(df, rownames = FALSE, selection = "single", options = list(dom = "t", language = list(emptyTable = "No analyses yet.")))
  })
  shiny::observe({
    r <- rules_r()
    shiny::updateSelectInput(session, "an_variable", choices = names(r$variables))
    shiny::updateSelectInput(session, "an_g1", choices = names(r$group_categories))
  })
  output$an_g2_ui <- shiny::renderUI({
    if (input$an_measure %in% .measures_two) shiny::selectInput("an_g2", "Compared with", names(rules_r()$group_categories),
                                                                 selected = names(rules_r()$group_categories)[2])
  })
  shiny::observeEvent(input$an_add, {
    two <- input$an_measure %in% .measures_two
    a <- list(name = if (nzchar(input$an_name)) input$an_name else paste0(input$an_variable, ": ", input$an_g1, if (two) paste0(" vs ", input$an_g2)),
              variable = input$an_variable, measure = input$an_measure,
              compare = if (two) c(input$an_g1, input$an_g2), group = if (!two) input$an_g1,
              timepoint = if (nzchar(input$an_timepoint)) input$an_timepoint, levels = input$an_levels)
    r <- try_do(write_analyses(proj(), c(unname(analyses_r()), list(Filter(Negate(is.null), a)))), paste0("Added analysis '", a$name, "'."))
    if (!is.null(r)) { auto_commit(paste0("Analysis ", a$name)); bump() }
  })
  shiny::observeEvent(input$an_delete, {
    i <- input$an_table_rows_selected; shiny::req(length(i))
    a <- unname(analyses_r())
    r <- try_do(write_analyses(proj(), a[-i]), "Analysis deleted.")
    if (!is.null(r)) { auto_commit("Delete analysis"); bump() }
  })
  shiny::observe({
    p <- proj(); tick(); shiny::req(p)
    cfg <- read_config(p)
    shiny::updateTextAreaInput(session, "crit_incl", value = paste(unlist(cfg$inclusion_criteria), collapse = "\n"))
    shiny::updateTextAreaInput(session, "crit_excl", value = paste(unlist(cfg$exclusion_criteria), collapse = "\n"))
  })
  shiny::observeEvent(input$crit_save, {
    f <- fs::path(proj(), "_metawoRld.yml"); cfg <- yaml::read_yaml(f)
    lines <- function(x) { x <- trimws(strsplit(x, "\n")[[1]]); as.list(x[nzchar(x)]) }
    cfg$inclusion_criteria <- lines(input$crit_incl); cfg$exclusion_criteria <- lines(input$crit_excl)
    try_do(yaml::write_yaml(cfg, f), "Criteria saved.")
    auto_commit("Update criteria"); bump()
  })
  output$form_view <- shiny::renderUI({
    tick(); shiny::req(proj())
    s <- get_schema(proj(), "extraction")
    describe <- function(node) {
      fields <- .schema_fields(node)
      shiny::tags$ul(class = "small", lapply(fields, function(f) {
        n <- node[[f]]
        type <- n$`_type`
        extra <- if (identical(type, "enum")) paste0(": ", paste(unlist(n$`_values`), collapse = ", ")) else ""
        shiny::tags$li(shiny::strong(f), " ", shiny::span(class = "text-muted", paste0("(", type, if (isFALSE(n$`_required`)) ", optional", extra, ")")),
                       if (!is.null(n$`_description`)) shiny::div(class = "text-muted", n$`_description`),
                       if (identical(type, "array") && !is.null(n$`_items`)) describe(n$`_items`))
      }))
    }
    shiny::tagList(shiny::p(class = "small", "Form version ", shiny::code(s$`_form`$version)),
                   bslib::layout_columns(col_widths = c(6, 6),
                     shiny::div(shiny::h6("About each study"), describe(s$metadata)),
                     shiny::div(shiny::h6("Each reported value"), describe(s$data_points$`_items`))))
  })
  shiny::observeEvent(input$gf_add, {
    vals <- trimws(strsplit(input$gf_values, ",")[[1]]); vals <- vals[nzchar(vals)]
    v <- try_do(add_group_fact(proj(), trimws(input$gf_name), if (length(vals)) vals, input$gf_desc))
    if (!is.null(v)) { notify(paste0("Added '", input$gf_name, "' to groups; the form is now version ", v, ".")); auto_commit(paste0("Group fact ", input$gf_name)); bump() }
  })

  # ---- Check
  check_res <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$check_run, {
    check_res(list(project = suppressMessages(check_project(proj())), values = check_values(proj())))
  })
  output$check_result <- shiny::renderUI({
    r <- check_res()
    if (is.null(r)) return(shiny::p(class = "text-muted", "Checks forms, data, file hashes, provenance, people, records and screening. The same check runs on GitHub."))
    issues <- r$project
    vf <- r$values
    badge <- function(l) shiny::span(class = paste0("badge text-bg-", if (l == "error") "danger" else "warning"), l)
    shiny::tagList(
      if (!nrow(issues)) shiny::div(class = "alert alert-success", shiny::icon("circle-check"), " No problems found.")
      else shiny::tags$table(class = "table table-sm",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th(""), shiny::tags$th("Where"), shiny::tags$th("What"))),
        shiny::tags$tbody(lapply(seq_len(nrow(issues)), function(i) shiny::tags$tr(shiny::tags$td(badge(issues$level[i])),
          shiny::tags$td(shiny::code(issues$where[i])), shiny::tags$td(issues$message[i]))))),
      shiny::h6(class = "mt-3", "Values flagged for review"),
      if (!nrow(vf)) shiny::p(class = "small text-muted", "None.")
      else shiny::p(class = "small", nrow(vf), " value", if (nrow(vf) > 1) "s have" else " has", " flags (",
                    paste(names(table(vf$check)), table(vf$check), sep = ": ", collapse = ", "),
                    "). They are shown in ", shiny::actionLink("go_review2", "Review"), ".")
    )
  })
  shiny::observeEvent(input$go_review2, bslib::nav_select("nav", "review"))

  # ---- Publish
  publish_msg <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$publish_go, {
    has_quarto <- requireNamespace("quarto", quietly = TRUE) || nzchar(Sys.getenv("QUARTO_PATH", Sys.which("quarto")))
    if (!has_quarto) { publish_msg(shiny::div(class = "alert alert-warning mt-3", "Quarto is needed to build the website: ", shiny::a(href = "https://quarto.org/docs/get-started/", target = "_blank", "install Quarto"), ".")); return() }
    r <- shiny::withProgress(message = "Building the website (this can take a minute)...", try_do(generate_website(proj())))
    site <- fs::path(proj(), "_site")
    if (!is.null(r) && fs::file_exists(fs::path(site, "index.html"))) {
      prefix <- paste0("site_", substr(rlang::hash(proj()), 1, 8))
      shiny::addResourcePath(prefix, site)
      publish_msg(shiny::div(class = "alert alert-success mt-3", "The website is built. ",
                             shiny::a(href = paste0(prefix, "/index.html"), target = "_blank", "Open it"),
                             ". To publish it on GitHub Pages, see ", shiny::code("use_github_action_quarto()"), "."))
    }
  })
  output$publish_result <- shiny::renderUI(publish_msg())

  # ---- Sync
  shiny::observeEvent(input$sync_open, { shiny::req(proj()); .sync_modal(proj()) })
  shiny::observeEvent(input$sync_go, {
    r <- try_do(sync_project(proj(), message = paste("Work by", person() %||% "?")))
    if (!is.null(r)) { shiny::removeModal(); notify(r$message); bump() }
  })
  shiny::observeEvent(input$sync_init, {
    try_do({ .git(proj(), c("init", "-q")); .ensure_gitattributes(proj()); commit_project(proj(), "Start keeping history") }, "Git history started.")
    shiny::removeModal(); .sync_modal(proj())
  })
  shiny::observeEvent(input$sync_remote_go, {
    url <- trimws(input$sync_remote); shiny::req(nzchar(url))
    try_do(.git(proj(), c("remote", "add", "origin", shQuote(url))), "Shared repository connected.")
    shiny::removeModal(); .sync_modal(proj())
  })
  shiny::observeEvent(input$sync_auto, .app_state$auto_commit <- isTRUE(input$sync_auto))
}

.sync_modal <- function(proj_path) {
  st <- sync_status(proj_path)
  body <- if (!nzchar(.git_bin())) {
    shiny::p("Git is not installed, so the project cannot be shared from here. Install it from ",
             shiny::a(href = "https://git-scm.com", target = "_blank", "git-scm.com"), ".")
  } else if (!st$repo) {
    shiny::tagList(shiny::p("This project does not keep a history yet. Start one to share it with a team and see every change."),
                   shiny::actionButton("sync_init", "Start keeping history (Git)", class = "btn-primary"))
  } else {
    shiny::tagList(
      shiny::tags$dl(class = "row small",
        shiny::tags$dt(class = "col-4", "Shared repository"), shiny::tags$dd(class = "col-8", if (is.na(st$remote)) shiny::em("none yet") else shiny::code(st$remote)),
        shiny::tags$dt(class = "col-4", "Your unsaved changes"), shiny::tags$dd(class = "col-8", st$changes, " file(s)"),
        shiny::tags$dt(class = "col-4", "Not yet sent / fetched"), shiny::tags$dd(class = "col-8", if (is.na(st$ahead)) "-" else paste0(st$ahead, " / ", st$behind), " change(s)")),
      if (is.na(st$remote)) shiny::div(class = "mb-3",
        shiny::p(class = "small", "To work as a team, create an empty repository (e.g. on GitHub) and paste its address:"),
        shiny::div(class = "d-flex gap-2", shiny::textInput("sync_remote", NULL, placeholder = "https://github.com/you/your-review.git", width = "380px"),
                   shiny::actionButton("sync_remote_go", "Connect", class = "btn-outline-primary"))),
      shiny::p(class = "small text-muted", "Sync saves your work, fetches the team's work (new submissions appear in the Inbox) and sends yours."),
      shiny::checkboxInput("sync_auto", "Save automatically after each action in the app", value = isTRUE(.app_state$auto_commit %||% TRUE))
    )
  }
  shiny::showModal(shiny::modalDialog(title = "Share with the team", body, easyClose = TRUE,
    footer = shiny::tagList(shiny::modalButton("Close"), if (isTRUE(st$repo)) shiny::actionButton("sync_go", shiny::HTML("&#8645; Sync now"), class = "btn-primary"))))
}

# For a submission to a study that is already accepted: what would change.
.changes_ui <- function(proj_path, s) {
  ch <- tryCatch(compare_with_accepted(proj_path, s$study_id, s$extraction_id), error = function(e) NULL)
  if (is.null(ch)) return(NULL)
  if (!nrow(ch)) return(shiny::div(class = "alert alert-secondary small py-2 mt-2",
                                   "This study is already accepted; this submission is identical to it."))
  cell <- function(x) shiny::tags$td(if (is.na(x)) shiny::em("(none)") else x)
  shiny::div(class = "alert alert-warning small py-2 mt-2",
    shiny::strong("This study is already accepted. Accepting replaces it; these values would change:"),
    shiny::tags$table(class = "table table-sm small mb-0 mt-1",
      shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Where"), shiny::tags$th("Field"), shiny::tags$th("Accepted"), shiny::tags$th("This submission"))),
      shiny::tags$tbody(lapply(seq_len(min(nrow(ch), 30)), function(i) shiny::tags$tr(
        shiny::tags$td(ch$id[i]), shiny::tags$td(ch$field[i]), cell(ch$accepted[i]), cell(ch$submitted[i]))))),
    if (nrow(ch) > 30) shiny::div(paste("...and", nrow(ch) - 30, "more.")))
}

# ---- helpers ---------------------------------------------------------------------

# reconcile() choices from one pick (extraction id) per disagreement row.
.choices_from_picks <- function(dis, picks) {
  if (!nrow(dis)) return(NULL)
  out <- list()
  done_meta <- character(0)
  for (i in seq_len(nrow(dis))) {
    x <- picks[i]
    if (dis$part[i] == "metadata") {
      if (dis$key[i] %in% done_meta) next
      done_meta <- c(done_meta, dis$key[i])
      out[[length(out) + 1]] <- data.frame(part = "metadata", key = dis$key[i], field = dis$key[i], from = x, value = NA_character_)
    } else if (dis$field[i] == "(row)") {
      absent <- is.na(dis[[x]][i])
      out[[length(out) + 1]] <- data.frame(part = "data", key = dis$key[i], field = "(row)",
                                           from = if (absent) NA_character_ else x, value = if (absent) "drop" else NA_character_)
    } else {
      out[[length(out) + 1]] <- data.frame(part = "data", key = dis$key[i], field = dis$field[i], from = x, value = NA_character_)
    }
  }
  do.call(rbind, out)
}

# "category = PE; onset = early|late" -> list(category = "PE", onset = c("early", "late"))
.parse_where <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(list())
  parts <- trimws(strsplit(x, ";")[[1]])
  parts <- parts[grepl("=", parts)]
  stats::setNames(lapply(parts, function(p) { v <- trimws(strsplit(sub("^[^=]*=", "", p), "\\|")[[1]]); as.list(v[nzchar(v)]) }),
                  trimws(sub("=.*$", "", parts)))
}

.recent_file <- function() {
  if (isFALSE(getOption("metawoRld.remember_recent", TRUE))) return(NULL)
  d <- tryCatch(tools::R_user_dir("metawoRld", "config"), error = function(e) NULL)
  if (is.null(d)) NULL else fs::path(d, "recent.txt")
}
.recent_projects <- function() {
  f <- .recent_file()
  if (is.null(f) || !fs::file_exists(f)) return(character(0))
  r <- readLines(f, warn = FALSE)
  r[fs::file_exists(fs::path(r, "_metawoRld.yml"))]
}
.remember_recent <- function(p) {
  f <- .recent_file(); if (is.null(f)) return(invisible())
  if (grepl(normalizePath(tempdir(), mustWork = FALSE), p, fixed = TRUE)) return(invisible())
  tryCatch({ fs::dir_create(fs::path_dir(f)); writeLines(utils::head(unique(c(p, .recent_projects())), 8), f) }, error = function(e) NULL)
  invisible()
}

.app_css <- "
.navbar .form-group { margin-bottom: 0; }
.navbar .selectize-input { min-height: 30px; padding: 3px 8px; }
.bslib-value-box .value-box-value { white-space: nowrap; }
"
