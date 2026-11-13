# metawoRld project specification

Version: 1 (draft) · Status: under review · Applies from metawoRld 0.3.0

This document defines what a metawoRld project is on disk: its folders,
identifiers, file formats and the rules every input channel must follow. It is
the contract between metawoRld (which owns and validates the data) and the
tools that put data in (DataFindR, the entry form, CSV imports, or any other
program in any language).

The key words MUST, SHOULD and MAY are used as in RFC 2119.

## 1. Principles

1. **One door.** Data become part of a project only by passing the intake gate
   (`submit_study()` or an equivalent check). Channels never write to `data/`.
2. **Plain, open files.** Everything is text: YAML, CSV and JSON, readable
   without metawoRld. Data packages follow the
   [Frictionless Data Package](https://datapackage.org/) standard.
3. **Every value is traceable** to a person or tool, a source document and a
   place in it, and every change is logged.
4. **Generic core, project-specific forms.** What is extracted is defined by
   the project's forms, not by metawoRld.
5. **Git is the history.** The project is a Git repository; metawoRld adds
   meaning (provenance, verification) but does not replace Git.

## 2. Project layout

```
my-review/
├── _metawoRld.yml          project settings and criteria          (edited by people)
├── _rules.yml              vocabulary and value checks             (edited by people)
├── _analyses.yml           planned meta-analyses                   (edited by people)
├── people.yml              people and roles                        (§5)
├── searches.yml            search log                              (§6)
├── records.csv             one row per unique record               (§6)
├── screening.csv           screening decisions, append-only        (§6)
├── forms/
│   ├── extraction.yml      required: the extraction form           (§7)
│   ├── screening.yml       optional: title/abstract screening form
│   ├── <other>.yml         optional: e.g. rob2.yml, nos.yml
│   └── *.schema.json       generated JSON Schemas (do not edit)
├── prompts/                LLM prompts used by DataFindR (optional)
├── submissions/            staged, unverified data                 (§9)
│   └── <study_id>/<extraction_id>/ …
├── data/                   accepted data                           (§8)
│   └── <study_id>/ …
├── fulltext/               full texts (git-ignored; never published)
├── .gitattributes          screening.csv merge=union (parallel screening merges line by line)
└── NEWS.md                 release notes                           (§12)
```

`_metawoRld.yml` MUST contain `spec_version: 1`. Website files (`_quarto.yml`,
`index.qmd`, …) and generated folders (`_site/`, `figures/`) are not part of
the data and are not covered here.

## 3. Identifiers

| Thing | Format | Example |
|---|---|---|
| Study | lowercase `[a-z0-9-]`, 3–40 chars, usually first author + year (+ letter) | `hansen2019`, `hansen2019b` |
| Report (DOI) | DOI, lower-cased, no `https://doi.org/` prefix | `10.1000/abc.123` |
| Report (PMID) | digits | `31772108` |
| Record | DOI if known, else `pmid:<PMID>`, else `rec-<first 12 hex of SHA-1 of normalized title + year>` | `pmid:31772108` |
| Person | lowercase `[a-z0-9-]`, 2–20 chars, usually initials | `ahj` |
| Extraction | `<UTC yyyymmddThhmmss>-<person or tool>-<form>` | `20261113T001502-ahj-extraction` |
| Data point | `measurement_id`, unique within a study, `[A-Za-z0-9_.-]` | `dp12` |
| Search | lowercase `[a-z0-9-]` | `pubmed-2026-11` |

A **study** is the unit of analysis (one cohort or trial). A **report** is one
publication. A study has one or more reports; a report belongs to at most one
study. Records (search hits) are reports before screening; an included record
is linked to a study.

Study ids are chosen once and never reused.

## 4. Files common to all tables

- CSV files MUST be UTF-8, comma-separated, with a header row, `""` quoting
  as needed, and empty cells for missing values. Each CSV that holds data is
  described by a Frictionless Table Schema (generated from the forms).
- YAML files MUST be UTF-8 YAML 1.1 that `yaml::read_yaml()` reads. Field
  names MUST NOT be words YAML reads as booleans (`y`, `n`, `yes`, `no`, `on`,
  `off`, `true`, `false`).
- Timestamps are ISO 8601 in UTC: `2026-11-13T00:15:02Z`.
- Hashes are written `sha256:<64 hex digits>`.

## 5. People: `people.yml`

```yaml
people:
  - id: ahj
    name: Anders Hagen Jarmund
    orcid: 0000-0002-3923-1637      # optional
    roles: [admin, extractor, verifier, arbiter, screener]
tools:
  - id: datafindr
    name: DataFindR
```

Roles: `admin` (changes forms and settings), `screener`, `extractor`,
`verifier` (checks values), `arbiter` (resolves disagreements). Every
provenance, verification and screening row refers to a person or tool `id`
from this file. E-mail addresses SHOULD NOT be stored.

## 6. Searches and records

`searches.yml` lists every search that was run (a re-run is a new entry):

```yaml
searches:
  - id: pubmed-2026-11
    database: PubMed
    query: '"interleukin-6" AND preeclampsia'
    date: 2026-11-13
    results: 412
    person: ahj
```

`records.csv` has one row per unique record:

| column | required | meaning |
|---|---|---|
| record_id | yes | §3 |
| doi, pmid | no | identifiers |
| title, year, journal, authors | title yes | bibliographic data (`authors` `; `-separated) |
| searches | yes | search ids that found it, `; `-separated (or `manual`) |
| study_id | no | set when the record is linked to a study |
| duplicate_of | no | record_id this one duplicates |

`screening.csv` is append-only; the latest row per (record, stage, person) is
that person's decision, and the latest `consensus` row per (record, stage) is
the outcome:

| column | meaning |
|---|---|
| record_id | §3 |
| stage | `title_abstract` or `full_text` |
| decision | `include`, `exclude` or `unsure` |
| reason | exclusion reason (free text or a criterion from `_metawoRld.yml`) |
| person | person or tool id |
| kind | `individual` or `consensus` |
| timestamp | §4 |

PRISMA counts are computed from these three files only.

## 7. Forms

A form is a YAML file in `forms/` using the node format introduced in
metawoRld 0.1 (`_type`, `_description`, `_required`, `_items`, `_values`,
`_ref`, `_ref_allow`), plus a header:

```yaml
_form:
  name: extraction          # must match the file name
  kind: extraction          # extraction | assessment
  version: 1.0.0            # semantic version; major = breaking change
  entry:                    # how the entry form lays out tables (optional)
    rows: variable_name
    columns: group_id
    cells: [value1, value2, value3, sample_size]
    context: [method_id, timepoint, statistic_type, source, source_page]
```

- `forms/extraction.yml` MUST exist and MUST define `metadata` (object) and
  `data_points` (array of objects).
- An `assessment` form (screening, risk of bias) defines one object per
  study (or per record, for screening).
- `metawoRld::export_schemas()` writes `forms/<name>.schema.json` (JSON
  Schema 2020-12) and Frictionless Table Schemas. These are generated and
  MUST NOT be edited by hand.

### 7.1 Groups, subgroups and categories

A study's groups are recorded as the paper defines them, once. Analyses
then decide which of them to compare.

- **Hierarchy.** `groups[].parent_group_id` names the larger reported group
  a group is part of (early-onset preeclampsia within all preeclampsia; a
  subset of the whole cohort). It MUST refer to a group of the same study
  and MUST NOT form a loop.
- **Facts.** Every other field of `groups` in the extraction form
  (`category`, and project fields such as `onset`) is a fact about the
  group. A subgroup inherits each fact from its nearest ancestor unless it
  sets its own. Facts are declared in the form (preferably as enums with a
  `not reported` value), never as free tags. A fact that is absent (on the
  group and its ancestors) means *not assessed*.
- **Categories** are defined in `_rules.yml`, either by names/synonyms
  (matched against the `category` fact or the group's own name) or as a rule
  over facts: `PE_early: {where: {category: PE, onset: early}}`. A group can
  belong to several categories.
- **One level per study.** An analysis uses exactly one set of participants
  per study and arm:
  - among the groups of a category, only the top-most are candidates (a
    subgroup is never used together with its parent);
  - a candidate's own reported value is used (`levels: reported`, the
    default); if it has none, or with `levels: pool`, complete subgroups
    (one split, same own facts, sizes adding up to the parent's n) are
    pooled with the Cochrane Handbook formulas;
  - separate candidates of one category (e.g. mild and severe preeclampsia
    without a reported total) are combined into one arm;
  - arms that overlap (one group inside the other) exclude the study;
  - in subgroup analyses (`subgroups:`), each study contributes one row per
    subgroup, the subgroups of a study MUST NOT overlap, and a reference
    group shared by `k` subgroups contributes `n/k` participants to each.
  The result records, per study and arm, which groups were used and how
  (`selection`), and the dashboard shows it.
- **Consistency.** Subgroup sizes of one split SHOULD NOT exceed the
  parent's n, and complete subgroups SHOULD pool to the parent's reported
  mean and SD (checks `subgroup_size`, `subgroup_total`).
- **Refinement.** Adding a fact (`add_group_fact()`) is a minor form
  version; `refinement_queue()` lists groups where it is not assessed, and
  changes to accepted studies are amendments (section 9).

## 8. Accepted study: `data/<study_id>/`

```
data/hansen2019/
├── datapackage.json      Frictionless descriptor (generated)
├── study.yml             identity and reports (system-defined)
├── metadata.yml          the extraction form's `metadata` part
├── data.csv              the extraction form's `data_points`
├── provenance.csv        where every value came from
├── verification.csv      reviewers' decisions per data point
└── assessments/          optional: <form>.yml per assessment form
```

`study.yml`:

```yaml
study_id: hansen2019
reports:
  - doi: 10.1000/abc.123
    pmid: "31772108"
    role: primary           # primary | secondary
status: included            # included | excluded_after_extraction | retracted
form_version: 1.0.0         # extraction form version the data follow
notes: ""
```

`provenance.csv` has one row per data point (and one row with
`measurement_id` = `_metadata` for the metadata):

| column | meaning |
|---|---|
| measurement_id | data point, or `_metadata` |
| extraction_id | the extraction (§9) the accepted value came from |
| agreed_with | other extraction ids with the same value (`; `-separated) |
| person | person or tool id that produced the value |
| channel | `entry_form`, `datafindr`, `csv_import`, `manual_edit`, … |
| tool | `name@version`, e.g. `DataFindR@0.2.0` |
| model | LLM model, if any |
| source_sha256 | hash of the full text the value was read from, if known |
| timestamp | when the value was produced |

`verification.csv` is as in metawoRld 0.2 (`measurement_id, status,
reviewer, timestamp, row_hash, note`), with `reviewer` a person id.

`datapackage.json` lists `metadata.yml`, `data.csv`, `provenance.csv` and
`verification.csv` as resources with a `hash` and, for CSV files, a Table
Schema. It MUST include `"metawoRld": {"spec_version": 1, "study_id": …,
"form": "extraction", "form_version": …}` and a `licenses` entry (default
`CC-BY-4.0`).

## 9. Submissions: `submissions/<study_id>/<extraction_id>/`

A submission is one extraction of one study by one person or tool:

```
submissions/hansen2019/20261113T001502-ahj-extraction/
├── submission.yml        who, how, when, status, gate report
├── study.yml             proposed identity/reports (new studies only)
├── metadata.yml
├── data.csv
└── provenance.csv        optional: per-value details (e.g. LLM model)
```

`submission.yml`:

```yaml
extraction_id: 20261113T001502-ahj-extraction
study_id: hansen2019
form: extraction
form_version: 1.0.0
person: ahj               # or a tool id
channel: entry_form
tool: metawoRld@0.3.0
source_sha256: sha256:…   # optional
created: 2026-11-13T00:15:02Z
status: pending           # pending | accepted | rejected | superseded
kind: extraction          # extraction (default) | amendment
blind: false              # true: hidden from other extractors until reconciled
gate:                     # written by submit_study()
  checked: 2026-11-13T00:15:03Z
  errors: []              # schema / link errors (submission refused if any)
  warnings: []            # value checks from _rules.yml
```

Rules:

1. A channel creates a submission with `submit_study()`, or writes the folder
   itself and then calls `check_submission()`. Submissions with errors are not
   written (or are marked `rejected` with the errors listed).
2. Several submissions may exist for one study (dual extraction).
3. `accept_submission()` (one extraction) or `reconcile()` (two or more)
   writes `data/<study_id>/`, records which extraction each accepted value
   came from in `provenance.csv`, and marks submissions `accepted` or
   `superseded`. Submissions are kept, so disagreements can be audited.
4. When a study already has accepted data, accepting a new submission
   replaces the study's data (with `overwrite = TRUE`); verification of data
   points whose id and values are unchanged is kept (row hashes, §10).
5. Changes to an accepted study (a new group fact, linking subgroups to
   their parent, a group or data points missed at first, a correction) are
   **amendments**: submissions with `kind: amendment` made with
   `amend_study()` from the accepted study. When accepted, provenance of
   values the amendment did not change is kept; changed values and the
   metadata are attributed to the amendment.

## 10. Integrity

- **Values:** verification rows store a hash of the values they refer to
  (computed over the data point's value fields with `rlang::hash()`, as in
  0.2). A changed value invalidates its verification.
- **Sources:** `source_sha256` records which file a value was read from,
  without storing the file.
- **Files:** `datapackage.json` holds a hash for every resource.
  `check_project()` recomputes them and fails if a data file changed
  without `datapackage.json` (and therefore provenance) being updated through
  metawoRld.
- **History:** Git commits are the audit trail.
- **Releases:** a release is a signed annotated Git tag (`git tag -s`, SSH or
  GPG key) archived on Zenodo with a DOI. A release MAY add an
  OpenTimestamps proof of `datapackage.json` hashes.

`check_project()` is the single check run locally and in CI. It exits with a
non-zero status when it finds errors and reports, per study: schema and link
errors, hash mismatches, data points without provenance, references to
unknown people, and submissions left pending.

## 11. Channels

A channel is any program that produces submissions. It:

- MUST produce files that pass `check_submission()`;
- MUST set `person` (or a tool id from `people.yml`), `channel` and `tool`;
- SHOULD set `source_sha256` and, per data point, `source` and `source_page`;
- MUST NOT write to `data/`, `records.csv` or `screening.csv` except through
  metawoRld functions (`add_records()`, `add_screening()`).

Channels written in other languages validate against the generated JSON
Schemas and the rules in this document; the authoritative check is
`check_submission()`.

### 11.1 Sharing through Git

A project is usually a Git repository shared by the team. Sharing does not
need Git skills (`sync_project()`, or Sync in the app): commit, pull (merge),
push. The layout keeps conflicts rare: each extraction has its own folder
under `submissions/`, only arbiters write `data/`, and `screening.csv` is
append-only and merged with Git's `union` driver (`.gitattributes`). Tools
that act on a project MAY commit after each action with a message naming
the action, the study and the person.

## 12. Versions and releases

- `spec_version` in `_metawoRld.yml`: this document's version.
- `form_version` in each form: bump major for breaking changes (renamed or
  removed fields), minor for added optional fields. Every submission and
  accepted study records the form version it follows, and `check_project()`
  warns when it differs in major version from the current form.
- Dataset releases use `vMAJOR.MINOR.PATCH`: major for a new form major
  version, minor when studies are added, patch for corrections. `NEWS.md`
  lists studies added or changed and pooled estimates that moved (a `draft_news()` helper is
  planned).
- Data are released under CC BY 4.0 unless `_metawoRld.yml` says otherwise.
  Full texts are never released.

## 13. Open points

- Whether assessments (risk of bias) of one study by several people are
  stored as submissions too (proposed: yes, same mechanism).
- Whether `screening.csv` should also be written by DataFindR directly through
  `add_screening()` (proposed: yes; its cache then becomes optional).
