# Report codes (`report/`)

R Markdown reports that are knit to PDF and written into
`../Gates-results/Reports/`. They stitch together the per-country plots in
`Gates-results/ReportPlots/<country>/` and tables in
`Gates-results/estimates/<country>/`. There are **two variants of the main
report**: `report.Rmd` (latest-vs-earliest survey, flat folders — reproduces the
previously published PDFs) and `report_anytwoyear.Rmd` (any chosen old/new year
pair, per-year-pair folders).

## Files

| File | Status | Purpose |
|------|--------|---------|
| `report.Rmd` | **active** (original) | Full multi-indicator SAE report for the **latest vs earliest** survey (min/max year). Reads the **flat** `ReportPlots/<country>/` and `estimates/<country>/`. Use this to reproduce the previously published `Multi-Indicator-SAE-<country>.pdf`. Params: `country`, `source_path`, `middle_path`. |
| `report_anytwoyear.Rmd` | **active** (any pair) | Same report for **any chosen old/new survey-year pair**. Adds `yr1`/`yr2` params (default `null` → min/max) and reads the **per-year-pair** folders `ReportPlots/<country>/<yr1>-<yr2>/` and `estimates/<country>/<yr1>-<yr2>/`. Needed for 3+ survey countries (e.g. Ethiopia 2019 vs 2024). Output: `Multi-Indicator-SAE-<country>-<yr1>-<yr2>.pdf`. |
| `report_SA.Rmd` | **active** (South Africa) | Single-survey-year variant (South Africa has only one survey, so no old/new comparison). |
| `Indicator.Rmd` | **active** | Indicator glossary + the API-consistency issue table; reads the `all_checkapi*` outputs produced by `check/`. Renders to `Indicator.pdf`. |
| `render_reports.R` | **active** | Driver: loops over countries and calls `rmarkdown::render()` for `report.Rmd`, `report_short.Rmd`, and `report_anytwoyear.Rmd`. |
| `report_short.Rmd` | **active** | The "mini" report (`Mini-Multi-Indicator-SAE-<country>.pdf`); flat folders (min/max). |
| `extra_Nigeria.Rmd` / `.tex` / `.pdf` | one-off / inactive | A Nigeria-specific extra write-up; not part of the standard per-country pipeline. |
| `Indicator.pdf`, `extra_Nigeria.pdf` / `.tex` | build artifacts | Rendered outputs kept in the folder; not source. |

## Rendering

`render_reports.R` renders the `.Rmd` **in this folder** and writes the PDFs to
`../Gates-results/Reports/`, looping over countries:

```r
render(input = "SAE4LMIC/report/report.Rmd",
       output_format = rmarkdown::pdf_document(toc = TRUE,
                                               number_sections = FALSE,
                                               keep_tex = TRUE),
       params = list(country = cty, source_path = ".../GATES",
                     middle_path = "Gates-results/ReportPlots"),
       output_file = paste0("Multi-Indicator-SAE-", cty, ".pdf"),
       output_dir  = ".../Gates-results/Reports",
       envir = new.env())
```

### Why it renders with `pdf_document`, not `bookdown::pdf_book`

`bookdown::pdf_book` writes its PDF **next to the input file** and ignores
`rmarkdown::render(output_dir=)`. That previously forced a copy of `report.Rmd`
into `Gates-results/Reports/` (so "next to the input" was the desired output
folder), and the two copies drifted apart. Since neither report uses any
bookdown-only feature (`\@ref`, `{#labels}`), `render_reports.R` now renders with
`rmarkdown::pdf_document`, which **honors `output_dir`** — so `report/` is the
single source and PDFs still land in `Gates-results/Reports/`.

## Notes

- Paths inside the reports are absolute via `params$source_path`, so images and
  tables resolve regardless of where the `.Rmd` is rendered — only the *output
  PDF location* is affected by the issue above.
- **Two main-report variants:**
  - `report.Rmd` — flat, latest-vs-earliest (min/max). Reads
    `ReportPlots/<country>/` and `estimates/<country>/`. Reproduces the previous
    PDFs. No `yr1`/`yr2` params.
  - `report_anytwoyear.Rmd` — any pair. `yr1`/`yr2` params (default `null` →
    min/max) select `ReportPlots/<country>/<yr1>-<yr2>/` and
    `estimates/<country>/<yr1>-<yr2>/`. `render_reports.R` has a dedicated block
    that sets `sel_yr1`/`sel_yr2`. The plots + tables for that pair must exist
    first: run `plots/report_year_pair(country, yr1, yr2, with_tables = TRUE)`.
- `report_short.Rmd` reads the flat paths (min/max only) — convert it like
  `report_anytwoyear.Rmd` if you need a year-pair mini report.
