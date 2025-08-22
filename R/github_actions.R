#' Setup GitHub Action for Quarto Website Rendering
#'
#' @description
#' This function sets up a GitHub Actions workflow to automatically render the
#' metawoRld project's Quarto website and deploy it to GitHub Pages.
#'
#' It creates:
#' - A workflow file at `.github/workflows/quarto-render.yml`.
#' - An R script at `inst/actions/render_web.R` which is executed by the action.
#'
#' The workflow is triggered on pushes to the `main` or `master` branches.
#'
#' @param overwrite Logical. If `TRUE`, it will overwrite existing workflow and
#'   script files.
#' @importFrom fs path dir_create file_exists
#' @importFrom readr write_lines
#' @export
use_github_action_quarto <- function(overwrite = FALSE) {
  # Path for the R script
  script_dir <- fs::path("inst", "actions")
  script_path <- fs::path(script_dir, "render_web.R")

  # Path for the GH Action workflow
  workflow_dir <- fs::path(".github", "workflows")
  workflow_path <- fs::path(workflow_dir, "quarto-render.yml")

  # Create directories if they don't exist
  fs::dir_create(script_dir)
  fs::dir_create(workflow_dir)

  # Check if files exist
  if (!overwrite && (fs::file_exists(script_path) || fs::file_exists(workflow_path))) {
    message("Workflow file or render script already exist. Use `overwrite = TRUE` to replace them.")
    return(invisible(FALSE))
  }

  # Content for the R script
  script_content <- c(
    '# This script is executed by a GitHub Action to generate the project website.',
    '# It assumes the working directory is the root of the metawoRld project.',
    '',
    '# The action should have installed the metawoRld package already.',
    'library(metawoRld)',
    '',
    '# Generate and render the full website.',
    'generate_website()'
  )

  # Content for the YAML workflow file
  yaml_content <- c(
    'on:',
    '  push:',
    '    branches:',
    '      - main',
    '      - master',
    '',
    'name: Render and Deploy metawoRld Website',
    '',
    'jobs:',
    '  build-deploy:',
    '    runs-on: ubuntu-latest',
    '    permissions:',
    '      contents: write # for committing to gh-pages branch',
    '    steps:',
    '      - name: Check out repository',
    '        uses: actions/checkout@v3',
    '',
    '      - name: Set up Quarto',
    '        uses: quarto-dev/quarto-actions/setup@v2',
    '',
    '      - name: Set up R',
    '        uses: r-lib/actions/setup-r@v2',
    '        with:',
    '          use-public-rspm: true',
    '',
    '      - name: Install R Dependencies',
    '        uses: r-lib/actions/setup-r-dependencies@v2',
    '        with:',
    "          extra-packages: |",
    "            any::quarto",
    "            local::.",
    "          needs: |",
    "            quarto",
    '',
    '      - name: Render website',
    '        run: Rscript "inst/actions/render_web.R"',
    '',
    '      - name: Deploy to GitHub Pages',
    '        uses: peaceiris/actions-gh-pages@v3',
    '        with:',
    '          github_token: ${{ secrets.GITHUB_TOKEN }}',
    '          publish_dir: ./_site'
  )

  # Write the files
  readr::write_lines(script_content, script_path)
  message("Created render script at ", script_path)

  readr::write_lines(yaml_content, workflow_path)
  message("Created GitHub Action workflow at ", workflow_path)

  invisible(TRUE)
}
