#' Review extracted values against the papers
#'
#' Opens a Shiny app for verifying data points one at a time. For each value
#' it shows a plain-language reading of the numbers (e.g. "median (IQR) = 4.2
#' (3.1 to 5.5) pg/mL"), the group and method it belongs to, flags from
#' [check_values()], where it was reported (`source`, `source_page`,
#' `source_quote`) and, if available, the full text opened at that page.
#'
#' Actions (with keyboard shortcuts):
#' * **Verify** (`V`): the value matches the paper. If you edited any field,
#'   the button saves the correction first and records what changed.
#' * **Reject** (`R`): the value is wrong or should not be used; it is left
#'   out of [load_metawoRld()] by default.
#' * **Next / previous** (`N` / `P`, or the arrow keys).
#' * **Map variable**: add an unknown variable name as a synonym in
#'   `_rules.yml`, or define it as a new variable.
#'
#' Decisions are stored in `data/<study>/verification.csv` with reviewer,
#' time and a fingerprint of the values; if the values change later (e.g.
#' re-extraction), the data point is marked `"changed"` and comes back into
#' the queue.
#'
#' @param path Project root.
#' @param fulltext_dir Optional folder with full texts named
#'   `<study_id>.pdf` or `<sanitize_id(doi)>.pdf` (or .html/.txt). Defaults to
#'   `<path>/fulltext` if it exists.
#' @param reviewer Reviewer name or initials (can also be set in the app).
#' @param launch.browser Passed to [shiny::runApp()].
#' @return Runs the app. `review_values_app()` returns the app object.
#' @export
review_values <- function(path = ".", fulltext_dir = NULL, reviewer = NULL, launch.browser = TRUE) {
  app <- review_values_app(path, fulltext_dir, reviewer)
  shiny::runApp(app, launch.browser = launch.browser)
}

#' @rdname review_values
#' @export
review_values_app <- function(path = ".", fulltext_dir = NULL, reviewer = NULL) {
  rlang::check_installed(c("shiny", "bslib", "DT"), reason = "to review values.")
  proj_path <- .check_project(path)
  schema <- get_schema(proj_path, "extraction")
  items <- schema$data_points$`_items`
  stat_types <- unlist(items$statistic_type$`_values`) %||% character(0)
  if (is.null(fulltext_dir) && fs::dir_exists(fs::path(proj_path, "fulltext"))) fulltext_dir <- fs::path(proj_path, "fulltext")
  ft_prefix <- paste0("fulltext_", substr(rlang::hash(proj_path), 1, 8))
  if (!is.null(fulltext_dir)) shiny::addResourcePath(ft_prefix, fulltext_dir)

  ui <- bslib::page_sidebar(
    title = paste("Value review:", read_config(proj_path)$project_name %||% ""),
    theme = bslib::bs_theme(version = 5),
    shiny::tags$head(shiny::tags$style(.review_css), shiny::tags$script(shiny::HTML(.review_js))),
    sidebar = bslib::sidebar(
      width = 300,
      shiny::textInput("reviewer", "Reviewer", reviewer %||% getOption("metawoRld.reviewer", "")),
      shiny::checkboxGroupInput("status_filter", "Show",
                                c("Unverified" = "unverified", "Changed since review" = "changed",
                                  "Verified" = "verified", "Rejected" = "rejected"),
                                selected = c("unverified", "changed")),
      shiny::checkboxInput("flagged_only", "Only values with flags", FALSE),
      shiny::selectInput("study_filter", "Study", choices = c("All studies" = "")),
      shiny::hr(),
      shiny::uiOutput("progress"),
      shiny::hr(),
      shiny::div(class = "small text-muted",
                 shiny::HTML("<b>Keys</b>: <kbd>V</kbd> verify / save &middot; <kbd>R</kbd> reject &middot; <kbd>N</kbd>/<kbd>&rarr;</kbd> next &middot; <kbd>P</kbd>/<kbd>&larr;</kbd> previous"))
    ),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Queue"),
        DT::DTOutput("queue")
      ),
      shiny::div(
        bslib::card(
          bslib::card_header(shiny::uiOutput("dp_header")),
          shiny::uiOutput("dp_summary"),
          shiny::uiOutput("dp_flags"),
          shiny::uiOutput("dp_source"),
          shiny::uiOutput("dp_map"),
          shiny::hr(),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::textInput("f_variable_name", "Variable"),
            shiny::selectInput("f_group_id", "Group", choices = NULL),
            shiny::textInput("f_timepoint", "Timepoint")
          ),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::selectInput("f_statistic_type", "Statistic", choices = stat_types),
            shiny::numericInput("f_sample_size", "n", NA),
            shiny::textInput("f_unit", "Unit (if not the method's)")
          ),
          bslib::layout_columns(
            col_widths = c(3, 3, 3, 3),
            shiny::numericInput("f_value1", "value1", NA),
            shiny::numericInput("f_value2", "value2", NA),
            shiny::numericInput("f_value3", "value3", NA),
            shiny::textInput("f_p_value", "p-value")
          ),
          shiny::textInput("note", "Note (optional; e.g. reason for rejection)", width = "100%"),
          shiny::div(
            class = "d-flex gap-2 flex-wrap",
            shiny::uiOutput("verify_button", inline = TRUE),
            shiny::actionButton("reject", shiny::HTML("&#10007; Reject"), class = "btn-outline-danger"),
            shiny::div(class = "ms-auto d-flex gap-2",
              shiny::actionButton("prev", shiny::HTML("&larr; Previous"), class = "btn-outline-secondary"),
              shiny::actionButton("nxt", shiny::HTML("Skip &rarr;"), class = "btn-outline-secondary"))
          )
        ),
        shiny::uiOutput("fulltext")
      )
    )
  )

  server <- function(input, output, session) {
    rules <- shiny::reactiveVal(read_rules(proj_path))
    store <- shiny::reactiveValues()
    current <- shiny::reactiveVal(NULL)    # list(study_id, measurement_id)

    load_one <- function(id) {
      dir <- .study_dir(proj_path, id)
      s <- tryCatch(.read_study_dir(dir, schema), error = function(e) NULL)
      if (is.null(s)) return(NULL)
      d <- as.data.frame(s$data)
      if (!nrow(d)) return(NULL)
      list(
        meta = s$metadata, data = d,
        status = .status_for(s$data, .read_verification(dir), schema),
        flags = .check_study_values(s$data, s$metadata, shiny::isolate(rules()))
      )
    }
    for (id in list_studies(proj_path)) store[[id]] <- load_one(id)
    shiny::updateSelectInput(session, "study_filter", choices = c("All studies" = "", rlang::set_names(
      shiny::isolate(names(shiny::reactiveValuesToList(store))),
      vapply(shiny::isolate(names(shiny::reactiveValuesToList(store))), function(i) .short_title(shiny::isolate(store[[i]]$meta), i), character(1))
    )))

    all_points <- shiny::reactive({
      sts <- shiny::reactiveValuesToList(store)
      rows <- lapply(names(sts), function(id) {
        s <- sts[[id]]
        if (is.null(s)) return(NULL)
        d <- s$data
        groups <- .lookup_field(s$meta$groups, "group_id", "name")
        fl <- s$flags
        dplyr::tibble(
          study_id = id,
          label = .study_label(s$meta, id),
          measurement_id = as.character(d$measurement_id),
          variable = as.character(d$variable_name),
          group = unname(groups[as.character(d$group_id)]) %||% NA_character_,
          status = s$status$status,
          n_error = vapply(d$measurement_id, function(m) sum(fl$measurement_id == m & fl$severity == "error"), integer(1)),
          n_warning = vapply(d$measurement_id, function(m) sum(fl$measurement_id == m & fl$severity == "warning"), integer(1))
        )
      })
      dplyr::bind_rows(dplyr::tibble(study_id = character(0), label = character(0), measurement_id = character(0), variable = character(0),
                                     group = character(0), status = character(0), n_error = integer(0), n_warning = integer(0)), rows)
    })

    queue <- shiny::reactive({
      q <- all_points()
      q <- q[q$status %in% input$status_filter, , drop = FALSE]
      if (isTRUE(input$flagged_only)) q <- q[q$n_error + q$n_warning > 0, , drop = FALSE]
      if (nzchar(input$study_filter %||% "")) q <- q[q$study_id == input$study_filter, , drop = FALSE]
      q
    })

    key_of <- function(q, i) if (i >= 1 && i <= nrow(q)) list(study_id = q$study_id[i], measurement_id = q$measurement_id[i]) else NULL
    pos_of <- function(q, k) if (is.null(k)) NA_integer_ else which(q$study_id == k$study_id & q$measurement_id == k$measurement_id)[1]

    # keep a valid selection
    shiny::observe({
      q <- queue()
      k <- shiny::isolate(current())
      if (!nrow(q)) {
        if (!is.null(k) && !is.na(pos_of(all_points(), k))) return()
        current(NULL)
      } else if (is.null(k) || is.na(pos_of(all_points(), k))) {
        current(key_of(q, 1))
      }
    })

    output$progress <- shiny::renderUI({
      a <- all_points()
      n <- nrow(a)
      ver <- sum(a$status == "verified")
      shiny::tagList(
        shiny::strong(sprintf("%d of %d values verified", ver, n)),
        shiny::div(class = "progress my-2", shiny::div(class = "progress-bar bg-success", style = sprintf("width:%.1f%%", if (n) 100 * ver / n else 0))),
        shiny::div(class = "small text-muted", sprintf("%d rejected, %d changed since review, %d with flags, %d in queue",
                                                        sum(a$status == "rejected"), sum(a$status == "changed"),
                                                        sum(a$n_error + a$n_warning > 0), nrow(queue())))
      )
    })

    output$queue <- DT::renderDT({
      q <- queue()
      flag <- ifelse(q$n_error > 0, "<span class='flag-error' title='error'>&#9940;</span>",
                     ifelse(q$n_warning > 0, "<span class='flag-warning' title='warning'>&#9888;</span>", ""))
      DT::datatable(
        data.frame(` ` = flag, Study = q$label, Variable = q$variable, Group = q$group, Status = q$status, check.names = FALSE),
        selection = list(mode = "single", target = "row"), rownames = FALSE, escape = -1,
        options = list(pageLength = 15, dom = "ftp", scrollX = TRUE, ordering = FALSE)
      )
    })
    proxy <- DT::dataTableProxy("queue")
    shiny::observeEvent(input$queue_rows_selected, {
      k <- key_of(queue(), input$queue_rows_selected)
      if (!is.null(k) && !identical(k, current())) current(k)
    })
    shiny::observe({
      p <- pos_of(queue(), current())
      if (!is.na(p) && !identical(as.integer(p), shiny::isolate(input$queue_rows_selected))) {
        DT::selectRows(proxy, p)
        DT::selectPage(proxy, (p - 1) %/% 15 + 1)
      }
    })

    cur <- shiny::reactive({
      k <- current()
      shiny::req(k)
      s <- store[[k$study_id]]
      shiny::req(s)
      i <- which(s$data$measurement_id == k$measurement_id)[1]
      shiny::req(!is.na(i))
      list(key = k, meta = s$meta, row = s$data[i, , drop = FALSE],
           status = s$status[s$status$measurement_id == k$measurement_id, ],
           flags = s$flags[s$flags$measurement_id == k$measurement_id, , drop = FALSE])
    })

    # fill the form
    shiny::observeEvent(cur(), {
      c <- cur(); r <- c$row
      g <- .lookup_field(c$meta$groups, "group_id", "name")
      shiny::updateSelectInput(session, "f_group_id", choices = rlang::set_names(names(g), paste0(names(g), ": ", g)), selected = .val(r$group_id))
      shiny::updateTextInput(session, "f_variable_name", value = .val(r$variable_name))
      shiny::updateTextInput(session, "f_timepoint", value = .val(r$timepoint))
      shiny::updateSelectInput(session, "f_statistic_type", selected = .val(r$statistic_type))
      shiny::updateTextInput(session, "f_unit", value = .val(r$unit))
      shiny::updateTextInput(session, "f_p_value", value = .val(r$p_value))
      for (f in c("sample_size", "value1", "value2", "value3")) {
        shiny::updateNumericInput(session, paste0("f_", f), value = if (f %in% names(r)) r[[f]] else NA)
      }
      shiny::updateTextInput(session, "note", value = "")
    })

    form <- shiny::reactive({
      list(variable_name = input$f_variable_name, group_id = input$f_group_id, timepoint = input$f_timepoint,
           statistic_type = input$f_statistic_type, sample_size = input$f_sample_size, unit = input$f_unit,
           value1 = input$f_value1, value2 = input$f_value2, value3 = input$f_value3, p_value = input$f_p_value)
    })
    changed_fields <- shiny::reactive({
      r <- cur()$row
      f <- form()
      names(f)[vapply(names(f), function(n) {
        old <- if (n %in% names(r)) r[[n]] else NA
        new <- f[[n]]
        norm <- function(x) if (is.null(x) || length(x) == 0 || is.na(x) || identical(x, "")) "" else if (is.numeric(x)) format(x, digits = 15) else as.character(x)
        !identical(norm(old), norm(new))
      }, logical(1))]
    })

    output$verify_button <- shiny::renderUI({
      ch <- changed_fields()
      if (length(ch)) shiny::actionButton("verify", shiny::HTML(paste0("&#10003; Save correction &amp; verify (", htmltools::htmlEscape(paste(ch, collapse = ", ")), ")")), class = "btn-warning")
      else shiny::actionButton("verify", shiny::HTML("&#10003; Verify"), class = "btn-success")
    })

    output$dp_header <- shiny::renderUI({
      c <- cur()
      st <- c$status$status
      cls <- c(unverified = "secondary", verified = "success", rejected = "danger", changed = "warning")[[st]]
      shiny::div(
        class = "d-flex justify-content-between align-items-center",
        shiny::div(shiny::strong(.short_title(c$meta, c$key$study_id, 80)),
                   shiny::span(class = "text-muted ms-2", shiny::HTML(paste0(htmltools::htmlEscape(c$key$study_id), " &middot; ", htmltools::htmlEscape(c$key$measurement_id))))),
        shiny::span(class = paste0("badge bg-", cls), st)
      )
    })

    output$dp_summary <- shiny::renderUI({
      c <- cur(); r <- c$row
      g <- .find_obj(c$meta$groups, "group_id", .val(r$group_id))
      m <- .find_obj(c$meta$methods, "method_id", .val(r$method_id))
      unit <- .val(r$unit); if (!nzchar(unit)) unit <- .val(m$unit)
      cmp <- .find_obj(c$meta$groups, "group_id", .val(r$comparison_group_id))
      shiny::div(
        class = "dp-summary",
        shiny::p(class = "lead mb-1", shiny::HTML(paste0(
          "<b>", htmltools::htmlEscape(.val(r$variable_name)), "</b>",
          if (nzchar(.val(r$timepoint))) paste0(" at ", htmltools::htmlEscape(.val(r$timepoint))) else "",
          " in <b>", htmltools::htmlEscape(.val(g$name, .val(r$group_id))), "</b>",
          if (!is.na(.num(r$sample_size))) paste0(" (n = ", .num(r$sample_size), ")") else ""
        ))),
        shiny::p(class = "fs-4 mb-1", shiny::HTML(.describe_statistic(r, unit))),
        shiny::p(class = "small text-muted mb-0", shiny::HTML(paste(c(
          if (nzchar(.val(m$name))) paste0("Method: ", htmltools::htmlEscape(.val(m$name))) else if (identical(.val(r$method_id), "POP_CHAR")) "Population characteristic" else NULL,
          if (nzchar(.val(g$definition))) paste0("Group: ", htmltools::htmlEscape(.val(g$definition))) else NULL,
          if (!is.null(g$sample_size)) paste0("Group n = ", g$sample_size) else NULL,
          if (nzchar(.val(r$p_value))) paste0("p = ", htmltools::htmlEscape(.val(r$p_value)),
                                               if (length(cmp)) paste0(" vs ", htmltools::htmlEscape(.val(cmp$name))) else "") else NULL,
          if (nzchar(.val(r$notes))) paste0("Notes: ", htmltools::htmlEscape(.val(r$notes))) else NULL,
          if (!is.na(c$status$note) && nzchar(c$status$note)) paste0("Review: ", htmltools::htmlEscape(c$status$note), " (", c$status$reviewer, ")") else NULL
        ), collapse = " &middot; ")))
      )
    })

    output$dp_flags <- shiny::renderUI({
      f <- cur()$flags
      if (!nrow(f)) return(shiny::div(class = "text-success small my-2", shiny::HTML("&#10003; No flags")))
      shiny::tags$ul(class = "flags my-2", lapply(seq_len(nrow(f)), function(i) {
        shiny::tags$li(class = paste0("flag-", f$severity[i]), shiny::strong(gsub("_", " ", f$check[i])), ": ", f$message[i])
      }))
    })

    output$dp_source <- shiny::renderUI({
      r <- cur()$row
      page <- .num(r$source_page)
      shiny::div(
        class = "source my-2",
        shiny::span(class = "text-muted", "Reported in: "),
        shiny::strong(.val(r$source, "(not recorded)")),
        if (!is.na(page)) shiny::HTML(paste0(" &middot; page ", page)),
        if (nzchar(.val(r$source_quote))) shiny::tags$blockquote(class = "quote", shiny::HTML(paste0("&ldquo;", htmltools::htmlEscape(.val(r$source_quote)), "&rdquo;")))
      )
    })

    output$dp_map <- shiny::renderUI({
      c <- cur()
      if (!any(c$flags$check == "unknown_variable")) return(NULL)
      vars <- names(rules()$variables)
      shiny::div(
        class = "d-flex gap-2 align-items-end",
        shiny::selectizeInput("map_to", paste0("Map '", .val(c$row$variable_name), "' to"),
                              choices = c("", vars), options = list(create = TRUE, placeholder = "existing variable, or type a new name")),
        shiny::actionButton("map", "Map variable", class = "btn-outline-primary mb-3")
      )
    })

    output$fulltext <- shiny::renderUI({
      shiny::req(!is.null(fulltext_dir))
      c <- cur()
      files <- .find_fulltext(fulltext_dir, c$key$study_id, proj_path)
      if (!length(files)) return(bslib::card(bslib::card_body(class = "small text-muted", "No full text found in ", fulltext_dir, " for ", c$key$study_id, ".*")))
      page <- .num(c$row$source_page)
      src <- paste0(ft_prefix, "/", utils::URLencode(fs::path_file(files[1]), reserved = TRUE),
                    if (grepl("\\.pdf$", files[1], ignore.case = TRUE) && !is.na(page)) paste0("#page=", page) else "")
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Full text", shiny::tags$a(class = "float-end small", href = src, target = "_blank", "open in new tab")),
        shiny::tags$iframe(src = src, style = "width:100%; height:75vh; border:0;")
      )
    })

    # ---- actions --------------------------------------------------------------
    advance <- function(after_key) {
      q <- shiny::isolate(queue())
      p <- pos_of(q, after_key)
      nxt <- if (is.na(p)) key_of(q, 1) else key_of(q, p + 1) %||% key_of(q, p - 1)
      list(next_key = nxt)
    }
    reload <- function(study_id, next_key) {
      store[[study_id]] <- load_one(study_id)
      q <- shiny::isolate(queue())
      if (!is.null(next_key) && !is.na(pos_of(q, next_key))) current(next_key)
      else current(key_of(q, 1) %||% shiny::isolate(current()))
    }
    reviewer_name <- function() {
      r <- trimws(input$reviewer %||% "")
      if (!nzchar(r)) {
        shiny::showNotification("Enter your name or initials under Reviewer first.", type = "warning")
        return(NULL)
      }
      r
    }
    act <- function(fun) {
      c <- shiny::isolate(cur())
      rev <- reviewer_name()
      if (is.null(rev)) return()
      nav <- advance(c$key)
      res <- tryCatch(fun(c, rev), error = function(e) e)
      if (inherits(res, "error")) {
        msg <- if (!is.null(res$validation_errors)) paste(res$validation_errors, collapse = "\n") else conditionMessage(res)
        shiny::showNotification(paste("Not saved:", msg), type = "error", duration = NULL)
        return()
      }
      reload(c$key$study_id, nav$next_key)
    }

    do_verify <- function() act(function(c, rev) {
      ch <- shiny::isolate(changed_fields())
      note <- shiny::isolate(input$note)
      note <- if (nzchar(note)) note else NULL
      if (length(ch)) {
        f <- shiny::isolate(form())[ch]
        do.call(update_data_point, c(list(proj_path, c$key$study_id, c$key$measurement_id), f,
                                     list(verify = TRUE, reviewer = rev, note = note)))
      } else {
        set_verification(proj_path, c$key$study_id, c$key$measurement_id, "verified", rev, note)
      }
    })
    do_reject <- function() act(function(c, rev) {
      note <- shiny::isolate(input$note)
      set_verification(proj_path, c$key$study_id, c$key$measurement_id, "rejected", rev, if (nzchar(note)) note else NULL)
    })
    step <- function(d) {
      q <- queue()
      p <- pos_of(q, current())
      k <- if (is.na(p)) key_of(q, 1) else key_of(q, p + d)
      if (!is.null(k)) current(k)
    }

    shiny::observeEvent(input$verify, do_verify())
    shiny::observeEvent(input$reject, do_reject())
    shiny::observeEvent(input$nxt, step(1))
    shiny::observeEvent(input$prev, step(-1))
    shiny::observeEvent(input$key, {
      switch(input$key$k, v = do_verify(), r = do_reject(), n = , ArrowRight = step(1), p = , ArrowLeft = step(-1), NULL)
    })

    shiny::observeEvent(input$map, {
      c <- cur()
      to <- trimws(input$map_to %||% "")
      shiny::req(nzchar(to))
      res <- tryCatch(add_variable_synonym(proj_path, .val(c$row$variable_name), to), error = function(e) e)
      if (inherits(res, "error")) return(shiny::showNotification(conditionMessage(res), type = "error"))
      rules(read_rules(proj_path))
      for (id in names(shiny::reactiveValuesToList(store))) store[[id]] <- load_one(id)
      shiny::showNotification(paste0("'", .val(c$row$variable_name), "' mapped to '", to, "' in _rules.yml."))
    })
  }

  shiny::shinyApp(ui, server)
}

# ---- helpers -----------------------------------------------------------------

.val <- function(x, default = "") {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return(default)
  as.character(x[[1]])
}
.num <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else suppressWarnings(as.numeric(x[[1]]))

.find_obj <- function(objs, id_field, id) {
  if (is.null(objs) || !nzchar(id)) return(list())
  if (is.data.frame(objs)) objs <- lapply(seq_len(nrow(objs)), function(k) .df_row(objs, k))
  for (o in objs) if (identical(as.character(o[[id_field]]), id)) return(o)
  list()
}

.study_label <- function(meta, id) {
  a <- unlist(meta$authors)
  if (!length(a)) return(id)
  paste0(.surname(a[1]), if (length(a) > 1) " et al." else "", " ", .val(meta$year))
}

.short_title <- function(meta, id, n = 50) {
  t <- .val(meta$title, id)
  a <- unlist(meta$authors)
  lab <- if (length(a)) paste0(.surname(a[1]), if (length(a) > 1) " et al." else "", " ", .val(meta$year)) else ""
  t <- if (nchar(t) > n) paste0(substr(t, 1, n - 3), "...") else t
  trimws(paste(lab, if (nzchar(lab)) "-" else "", t))
}

.regex_escape <- function(x) gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", x)

.describe_statistic <- function(r, unit) {
  f <- function(x) { v <- .num(x); if (is.na(v)) "<span class='missing'>?</span>" else format(v, digits = 6) }
  v1 <- f(r$value1); v2 <- f(r$value2); v3 <- f(r$value3)
  u <- if (nzchar(unit)) paste0(" <span class='unit'>", htmltools::htmlEscape(unit), "</span>") else ""
  s <- .val(r$statistic_type)
  txt <- switch(
    s,
    mean_sd = paste0("mean &plusmn; SD = <b>", v1, " &plusmn; ", v2, "</b>", u),
    mean_se = paste0("mean &plusmn; SE = <b>", v1, " &plusmn; ", v2, "</b>", u),
    mean_ci95 = paste0("mean (95% CI) = <b>", v1, " (", v2, " to ", v3, ")</b>", u),
    median_iqr = paste0("median (IQR) = <b>", v1, " (", v2, " to ", v3, ")</b>", u),
    median_range = paste0("median (range) = <b>", v1, " (", v2, " to ", v3, ")</b>", u),
    geometric_mean_sd = paste0("geometric mean (SD) = <b>", v1, " (", v2, ")</b>", u),
    n_percent = paste0("n (%) = <b>", v1, " (", v2, "%)</b>"),
    count = paste0("count = <b>", v1, "</b>"),
    proportion = paste0("proportion = <b>", v1, "</b>"),
    paste0(htmltools::htmlEscape(s), ": <b>", v1, ", ", v2, ", ", v3, "</b>", u)
  )
  txt
}

.review_css <- "
#queue table { font-size: .85rem; }
.flags { padding-left: 1.2rem; margin-bottom: .5rem; }
.flag-error { color: #b02a37; }
.flag-warning { color: #997404; }
.quote { border-left: 3px solid #adb5bd; padding: .25rem .75rem; margin: .5rem 0 0; font-family: var(--bs-font-monospace); font-size: .9rem; }
.dp-summary .unit { color: #6c757d; font-size: .8em; }
.dp-summary .missing { color: #b02a37; }
table.dataTable tbody tr.selected td { box-shadow: inset 0 0 0 9999px rgba(13,110,253,.15) !important; color: inherit !important; }
"

.review_js <- "
document.addEventListener('keydown', function(e) {
  var t = e.target.tagName;
  if (t === 'INPUT' || t === 'TEXTAREA' || t === 'SELECT' || e.target.isContentEditable || e.ctrlKey || e.metaKey || e.altKey) return;
  var k = e.key.length === 1 ? e.key.toLowerCase() : e.key;
  if (['v','r','n','p','ArrowRight','ArrowLeft'].indexOf(k) === -1) return;
  e.preventDefault();
  Shiny.setInputValue('key', {k: k, t: Date.now()}, {priority: 'event'});
});
"

# Full text of a study: <study_id>.<ext>, or named after one of its reports
# (sanitized DOI or PMID, as DataFindR stores them).
.find_fulltext <- function(fulltext_dir, study_id, proj_path) {
  reports <- tryCatch(read_study(proj_path, study_id)$study$reports, error = function(e) list())
  keys <- c(study_id, unlist(lapply(reports, function(r) c(r$doi, r$pmid))))
  keys <- unique(sanitize_id(keys[nzchar(keys)]))
  for (k in keys) {
    files <- fs::dir_ls(fulltext_dir, regexp = paste0("^", .regex_escape(fs::path(fulltext_dir, k)), "\\.(pdf|html?|txt|md)$"), ignore.case = TRUE)
    if (length(files)) return(files)
  }
  character(0)
}
