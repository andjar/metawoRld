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
#> Created metawoRld project at /tmp/RtmpfZHeLL/neoflow
list.files(path)
#>  [1] "_assessment_prompt.txt" "_assessment_schema.yml" "_extraction_prompt.txt"
#>  [4] "_extraction_schema.yml" "_metawoRld.yml"         "_quarto.yml"           
#>  [7] "data"                   "data.qmd"               "index.qmd"             
#> [10] "README.md"              "studies.qmd"            "study.qmd.tmpl"        
#> [13] "styles.css"
```

Open `_extraction_schema.yml` and adapt it to your review before
extracting data. The default is generic.

## Add a study by hand

``` r

add_study_data(
  path,
  study_id = "10.1000/example.1",
  metadata = list(
    title = "Cerebral blood flow in term neonates",
    authors = c("Smith J", "Doe A"), year = 2021L, country = "Norway",
    study_design = "Prospective cohort", population = "Healthy term neonates",
    methods = list(list(method_id = "m1", name = "Pulsed Doppler, middle cerebral artery", unit = "cm/s")),
    groups = list(list(group_id = "g1", name = "Term neonates", sample_size = 40L))
  ),
  data = data.frame(
    measurement_id = c("dp1", "dp2"), method_id = "m1", group_id = "g1",
    variable_name = c("Peak systolic velocity", "Resistance index"),
    timepoint = "Day 1", sample_size = 40, statistic_type = "mean_sd",
    value1 = c(35.2, 0.72), value2 = c(6.1, 0.06), source = "Table 2"
  )
)
```

Problems are reported all at once. For example, using a group that is
not defined:

``` r

add_study_data(path, "bad", metadata = list(title = "x"),
               data = data.frame(group_id = "g9"))
#> Error in `.abort_validation()`:
#> ! Validation failed for study 'bad' (15 problem(s)):
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

## Load the data

``` r

world <- load_metawoRld(path)
#> Loaded 1 studies and 2 data points.
world$studies
#> # A tibble: 1 × 7
#>   study_id          title          authors year  country study_design population
#>   <chr>             <chr>          <chr>   <chr> <chr>   <chr>        <chr>     
#> 1 10.1000/example.1 Cerebral bloo… Smith … 2021  Norway  Prospective… Healthy t…
world$studies_data[, c("study_id", "variable_name", "value1", "value2", "method_unit", "group_name")]
#> # A tibble: 2 × 6
#>   study_id          variable_name          value1 value2 method_unit group_name 
#>   <chr>             <chr>                   <dbl>  <dbl> <chr>       <chr>      
#> 1 10.1000/example.1 Peak systolic velocity  35.2    6.1  cm/s        Term neona…
#> 2 10.1000/example.1 Resistance index         0.72   0.06 cm/s        Term neona…
```

## Screening and extraction with DataFindR

``` r

library(DataFindR)
Sys.setenv(DATAFINDR_EMAIL = "you@example.org")   # polite use of NCBI/Crossref

# 1. Records
hits <- df_search_pubmed(
  '("Infant, Newborn"[Mesh] OR neonat*) AND Doppler AND (cerebral OR brain)',
  path
)

# 2. Screening (parallel; use mode = "batch" for half price with OpenAI/Anthropic/Gemini)
chat_screen <- ellmer::chat_openai(model = "gpt-4.1-mini")
screen <- df_assess_batch(chat_screen, hits$identifier, path)
df_shiny_assess(path, chat_screen)          # review "Needs manual review"

# 3. Extraction from full texts saved as fulltext/<df_fulltext_filename(id)>
df_pending_extraction(path)
chat_extract <- ellmer::chat_anthropic()
df_extract_batch(chat_extract, file.path(path, "fulltext"), path)
df_shiny_extract(path, chat_extract, fulltext_dir = file.path(path, "fulltext"))

# 4. Import and check
df_import_batch(metawoRld_path = path)
df_status(path)
df_prisma_counts(path)
```

## Publish

``` r

generate_website(path)            # renders into _site/
use_github_action_quarto(path)    # rebuild on GitHub at every push
```
