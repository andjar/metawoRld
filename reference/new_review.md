# Start a new review with sensible defaults

A one-step way to start: creates the project, registers you, and sets up
the comparison you are interested in, so the first study can be entered
right away. Everything can be refined later (in the app's Setup screen
or by editing the files).

## Usage

``` r
new_review(
  path,
  title,
  question = "",
  design = c("two_groups", "one_group", "blank"),
  groups = c("Cases", "Controls"),
  variable = NULL,
  unit = NULL,
  inclusion_criteria = character(0),
  exclusion_criteria = character(0),
  person_id,
  person_name,
  git = nzchar(Sys.which("git")),
  website = TRUE
)
```

## Arguments

- path:

  Folder to create.

- title:

  Short title of the review.

- question:

  The review question (a sentence or two).

- design:

  `"two_groups"` (compare two groups, e.g. cases and controls: a
  standardized mean difference), `"one_group"` (describe one group: a
  pooled mean) or `"blank"`.

- groups:

  Names of the groups: two for `"two_groups"` (group of interest first),
  one for `"one_group"`.

- variable:

  Optional first outcome or measurement, e.g. `"IL-6"`.

- unit:

  Its unit, e.g. `"pg/mL"`.

- inclusion_criteria, exclusion_criteria:

  Character vectors.

- person_id, person_name:

  You: a short id (e.g. initials) and your name.

- git:

  Keep the project in Git (for sharing and history)? Defaults to `TRUE`
  when Git is installed.

- website:

  Add the website templates? Defaults to `TRUE`.

## Value

The project path, invisibly.

## Examples

``` r
p <- new_review(file.path(tempdir(), "my-review"), "IL-6 in preeclampsia",
                "Is IL-6 higher in preeclampsia than in normotensive pregnancy?",
                groups = c("Preeclampsia", "Normotensive pregnancy"),
                variable = "IL-6", unit = "pg/mL",
                person_id = "ahj", person_name = "Anders", git = FALSE)
#> Created metawoRld project at /tmp/Rtmpn5cnev/my-review
read_analyses(p)[[1]]$compare
#> [1] "Preeclampsia"           "Normotensive pregnancy"
```
