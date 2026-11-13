# Getting started

``` r

library(metawoRld)
```

## Create a project

``` r

path <- file.path(tempdir(), "neoflow")
create_metawoRld(
  path,
  project_name = "Cerebral Doppler in neonates",
  project_description = "Reference values for cerebral blood flow velocities in neonates.",
  inclusion_criteria = c("Human neonates or infants under 1 year",
                         "Cerebral Doppler ultrasound",
                         "Flow velocities or indices reported"),
  exclusion_criteria = c("Case report", "Animal study", "Fewer than five participants"),
  git_init = FALSE
)
#> Created metawoRld project at /tmp/Rtmpppt8fO/neoflow
list.files(path)
#>  [1] "_analyses.yml"  "_metawoRld.yml" "_quarto.yml"    "_rules.yml"    
#>  [5] "data"           "data.qmd"       "forms"          "index.qmd"     
#>  [9] "NEWS.md"        "people.yml"     "prompts"        "README.md"     
#> [13] "records.csv"    "screening.csv"  "searches.yml"   "studies.qmd"   
#> [17] "study.qmd.tmpl" "styles.css"     "submissions"
```

Open `forms/extraction.yml` and adapt it to your review before
extracting data. The default is generic.

Register the people (and tools) who work on the review. Every screening
decision, extraction and verification refers to one of them:

``` r

add_person(path, "ahj", "Anders Hagen Jarmund",
           roles = c("admin", "screener", "extractor", "verifier", "arbiter"))
add_person(path, "kb", "Kari Berg", roles = c("screener", "extractor"))
add_person(path, "datafindr", "DataFindR", tool = TRUE)
options(metawoRld.person = "ahj")
```

## Searches, records and screening

``` r

add_search(path, "pubmed-2026-11", "PubMed", "neonat* AND Doppler AND cerebral",
           results = 2, person = "ahj")
add_records(path, data.frame(
  title = c("Cerebral blood flow in term neonates", "Doppler in piglets"),
  doi = c("10.1000/example.1", "10.1000/example.2"), year = 2021
), search = "pubmed-2026-11")
rec <- read_records(path)$record_id
add_screening(path, rec, "title_abstract", c("include", "exclude"), c("", "Animal study"), person = "ahj")
add_screening(path, rec, "title_abstract", c("include", "exclude"), person = "kb")
screening_status(path)
#> # A tibble: 2 × 5
#>   record_id         stage          outcome reason         n_decisions
#>   <chr>             <chr>          <chr>   <chr>                <int>
#> 1 10.1000/example.1 title_abstract include ""                       2
#> 2 10.1000/example.2 title_abstract exclude "Animal study"           2
```

## Submit and accept a study

Every channel (the entry form, CSV files, DataFindR, scripts) submits
through
[`submit_study()`](https://andjar.github.io/metawoRld/reference/submit_study.md).
Here a study is submitted from R:

``` r

id <- submit_study(
  path,
  study_id = suggest_study_id("Smith J", 2021, path),
  person = "ahj", channel = "script",
  metadata = list(
    title = "Cerebral blood flow in term neonates",
    authors = c("Smith J", "Doe A"), year = 2021L, country = "Norway",
    study_design = "Prospective cohort", population = "Healthy term neonates",
    methods = list(list(method_id = "m1", name = "Pulsed Doppler, middle cerebral artery", unit = "cm/s")),
    groups = list(list(group_id = "g1", name = "Term neonates", sample_size = 40L)),
    doi = "10.1000/example.1"
  ),
  data = data.frame(
    measurement_id = c("dp1", "dp2"), method_id = "m1", group_id = "g1",
    variable_name = c("Peak systolic velocity", "Resistance index"),
    timepoint = "Day 1", sample_size = 40, statistic_type = "mean_sd",
    value1 = c(35.2, 0.72), value2 = c(6.1, 0.06), source = "Table 2"
  )
)
#> Submitted smith2021 (20261113T095413-ahj-extraction): 2 data point(s).
list_submissions(path)[, c("study_id", "extraction_id", "status", "n_warnings")]
#> # A tibble: 1 × 4
#>   study_id  extraction_id                  status  n_warnings
#>   <chr>     <chr>                          <chr>        <int>
#> 1 smith2021 20261113T095413-ahj-extraction pending          0
```

Problems are reported all at once and nothing is written. For example,
using a group that is not defined:

``` r

submit_study(path, "bad2021", metadata = list(title = "x"),
             data = data.frame(group_id = "g9"), person = "ahj")
#> Error in `.abort_validation()`:
#> ! Validation failed for study 'bad2021' (15 problem(s)):
#> ✖ metadata.authors: required value is missing.
#> ✖ metadata.year: required value is missing.
#> ✖ metadata.country: required value is missing.
#> ✖ metadata.study_design: required value is missing.
#> ✖ metadata.population: required value is missing.
#> ✖ metadata.methods: required value is missing.
#> ✖ metadata.groups: required value is missing.
#> ✖ data_points[1].measurement_id: required value is missing.
#> ✖ data_points[1].method_id: required value is missing.
#> ✖ data_points[1].variable_name: required value is missing.
#> ✖ data_points[1].sample_size: required value is missing.
#> ✖ data_points[1].statistic_type: required value is missing.
#> ✖ data_points[1].value1: required value is missing.
#> ✖ data_points[1].source: required value is missing.
#> ✖ data_points.group_id: value(s) 'g9' not defined in metadata.groups[].group_id (none defined)
```

Accepting the submission writes `data/smith2021/` with provenance and
file hashes, and links the record with the same DOI to the study:

``` r

accept_submission(path, "smith2021", id)
#> Accepted 20261113T095413-ahj-extraction into data/smith2021.
read_study(path, "smith2021")$provenance[, c("measurement_id", "person", "channel")]
#> # A tibble: 3 × 3
#>   measurement_id person channel
#>   <chr>          <chr>  <chr>  
#> 1 _metadata      ahj    script 
#> 2 dp1            ahj    script 
#> 3 dp2            ahj    script
read_records(path)[, c("record_id", "study_id")]
#> # A tibble: 2 × 2
#>   record_id         study_id   
#>   <chr>             <chr>      
#> 1 10.1000/example.1 "smith2021"
#> 2 10.1000/example.2 ""
```

With dual extraction, two people submit the same study;
[`compare_extractions()`](https://andjar.github.io/metawoRld/reference/compare_extractions.md)
lists the disagreements and
[`reconcile()`](https://andjar.github.io/metawoRld/reference/reconcile.md)
resolves them, recording who agreed with each accepted value.

To enter data without R, write blank files with
`submission_template(path, "entry")`, fill them in with any editor, and
submit them with
`submit_study(path, "smith2021", "entry/metadata.yml", "entry/data.csv", person = "kb", channel = "csv_import")`.

## Load the data

``` r

world <- load_metawoRld(path)
#> Loaded 1 studies and 2 data points.
world$studies
#> # A tibble: 1 × 8
#>   study_id  title            authors year  country study_design population doi  
#>   <chr>     <chr>            <chr>   <chr> <chr>   <chr>        <chr>      <chr>
#> 1 smith2021 Cerebral blood … Smith … 2021  Norway  Prospective… Healthy t… 10.1…
world$studies_data[, c("study_id", "variable_name", "value1", "value2", "method_unit", "group_name")]
#> # A tibble: 2 × 6
#>   study_id  variable_name          value1 value2 method_unit group_name   
#>   <chr>     <chr>                   <dbl>  <dbl> <chr>       <chr>        
#> 1 smith2021 Peak systolic velocity  35.2    6.1  cm/s        Term neonates
#> 2 smith2021 Resistance index         0.72   0.06 cm/s        Term neonates
```

## Review values against the papers

Extracted values, whether typed by hand or produced by an LLM, should be
checked against the paper.
[`check_values()`](https://andjar.github.io/metawoRld/reference/check_values.md)
flags values that are internally inconsistent or implausible, using
built-in checks and the optional vocabulary in `_rules.yml`:

``` r

check_values(path)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: study_id <chr>, measurement_id <chr>, check <chr>,
#> #   severity <chr>, message <chr>
```

[`review_values()`](https://andjar.github.io/metawoRld/reference/review_values.md)
opens an app that shows each data point in plain language next to its
flags, source quote and the full text (from `fulltext/`) at the right
page. Verify, correct or reject values with the buttons or the keys `V`,
`R`, `N` and `P`. Every decision is stored with reviewer and time in
`data/<study_id>/verification.csv`, and corrections are logged:

``` r

set_verification(path, "smith2021", "dp1", "verified")
update_data_point(path, "smith2021", "dp2", value2 = 0.07)
verification_status(path)
#> # A tibble: 2 × 6
#>   study_id  measurement_id status   reviewer timestamp            note          
#>   <chr>     <chr>          <chr>    <chr>    <chr>                <chr>         
#> 1 smith2021 dp1            verified ahj      2026-11-13T09:54:14Z ""            
#> 2 smith2021 dp2            verified ahj      2026-11-13T09:54:14Z "Corrected: v…
```

If a value changes after review (for example after re-extraction) its
status becomes `"changed"` and it returns to the review queue.
[`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md)
leaves out rejected values and can keep verified values only
(`verified_only = TRUE`).

``` r

review_values(path)
```

## Check the project

[`check_project()`](https://andjar.github.io/metawoRld/reference/check_project.md)
is the one check to run before committing and in CI. It validates every
study, recomputes the file hashes in `datapackage.json` (catching data
edited outside metawoRld), and reports missing provenance, unknown
people and pending submissions:

``` r

check_project(path)
#> check_project(): 1 study(ies), 0 error(s), 0 warning(s).
prisma_counts(path)
#> # A tibble: 9 × 2
#>   stage                                  n
#>   <chr>                              <int>
#> 1 Records identified                     2
#> 2 Duplicates removed                     0
#> 3 Records screened (title/abstract)      2
#> 4 Records excluded (title/abstract)      1
#> 5 Awaiting decision (title/abstract)     0
#> 6 Reports assessed (full text)           0
#> 7 Reports excluded (full text)           0
#> 8 Studies included                       1
#> 9 Reports of included studies            1
```

``` r

use_github_action_check(path)     # run check_project() on every push and pull request
```

## Screening and extraction with DataFindR

``` r

library(DataFindR)
Sys.setenv(DATAFINDR_EMAIL = "you@example.org")   # polite use of NCBI/Crossref

# 1. Records: logged with add_search() and add_records()
hits <- df_search_pubmed(
  '("Infant, Newborn"[Mesh] OR neonat*) AND Doppler AND (cerebral OR brain)',
  path, person = "ahj"
)

# 2. Screening: the LLM's decisions go to screening.csv as the "datafindr" tool
#    (local models work too, e.g. ellmer::chat_ollama())
chat_screen <- ellmer::chat_openai(model = "gpt-4.1-mini")
screen <- df_assess_batch(chat_screen, hits$identifier, path)
df_shiny_assess(path, chat_screen)          # human decisions for "Needs manual review"

# 3. Extraction from full texts saved as fulltext/<df_fulltext_filename(id)>
df_pending_extraction(path)
chat_extract <- ellmer::chat_anthropic()
df_extract_batch(chat_extract, file.path(path, "fulltext"), path)
df_shiny_extract(path, chat_extract, fulltext_dir = file.path(path, "fulltext"))

# 4. Submit through the one door, then accept after review
df_import_batch(metawoRld_path = path)      # submit_study() per extraction
list_submissions(path, status = "pending")
df_status(path)
```

## Living meta-analysis

Define meta-analyses in `_analyses.yml`. Group categories (e.g. cases
and controls) and variable names come from `_rules.yml`; DataFindR
passes both to the LLM so extracted names follow this vocabulary.

``` yaml
analyses:
  - name: Peak systolic velocity in preterm vs term neonates
    variable: Peak systolic velocity
    measure: SMD
    compare: [Preterm, Term]
```

``` r

ma <- run_meta_analysis(path, "Peak systolic velocity in preterm vs term neonates")
ma
plot_forest(ma)
plot_cumulative(ma)      # how the pooled estimate evolved over time
```

Medians with IQR/range, SEs and CIs are converted to means and SDs when
needed, units are converted using `_rules.yml`, and subgroups of the
same category within a study are combined. Studies that cannot be used
are listed with the reason.

## Publish

``` r

generate_website(path)            # dashboard: overview, meta-analyses, results, studies, data
use_github_action_quarto(path)    # rebuild on GitHub at every push
```
