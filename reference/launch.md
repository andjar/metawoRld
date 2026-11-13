# Open the metawoRld app

A Shiny app for running a review without writing R:

## Usage

``` r
launch(
  path = NULL,
  person = getOption("metawoRld.person"),
  launch.browser = TRUE
)

launch_app(path = NULL, person = getOption("metawoRld.person"))
```

## Arguments

- path:

  A project to open. Defaults to none (the Start screen).

- person:

  Your id in the project's `people.yml`. Defaults to the
  `metawoRld.person` option.

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

`launch()` runs the app; `launch_app()` returns the app object.

## Details

- **Start**: create a review with a short wizard, open an existing
  project, or explore the worked example.

- **Overview**: progress and what to do next.

- **Inbox**: submissions waiting to be accepted; compare two extractions
  of the same study and pick the right values (reconcile).

- **Review**: verify values against the papers (the
  [`review_values()`](https://andjar.github.io/metawoRld/reference/review_values.md)
  app).

- **Setup**: people, variables, group categories, planned analyses,
  criteria and the form.

- **Check**:
  [`check_project()`](https://andjar.github.io/metawoRld/reference/check_project.md)
  in plain language.

- **Publish**: build and open the living meta-analysis website.

- **Sync**: share work with the team through Git
  ([`sync_project()`](https://andjar.github.io/metawoRld/reference/sync_project.md));
  new submissions from others then appear in the Inbox.

Everything the app does is done with the package's functions, so it can
also be done (and reproduced) from R.
