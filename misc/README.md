# Additional codes (`misc/`)

Supporting R scripts that are **not** part of the main automated pipeline
(`prep/` → `models/` → `plots/`/`webplots/` → `report/`). None of these are
sourced by the pipeline drivers unless noted; most are run manually.

Files are grouped into two categories: **common** (reusable helpers meant to be
sourced) and **archive** (superseded snapshots kept for provenance).


> The per-survey prefix/core tracker (`result_prefix_tracker.csv`) lives in
> `../info/` (see the metadata table in the top-level README).

## Catalog

### common — reusable helpers (safe to `source()`)
| File | Provides | Purpose |
|---|---|---|
| `resolve_qs.R` | `resolve_qs()` | Resolve a result `.qs` path across the two naming conventions (`plain` from `model-core.R` vs `new_` from `model-core-newvarfix.R`), preferring `plain` when both exist. Sourced by `plots/report_helper.R` and `webplots/fct_reading_result.R`. Dependency-free (base R). |
| `reading_result.R` | `read_two_model_result()` | Read Admin-1 + Admin-2 result `.qs` files across a country's surveys into a nested `res[[iso3]][[year]][[indicator]]` list. Note: currently ends with a hardcoded example call; treat as a helper + demo. |

### archive — superseded snapshots (kept for provenance, not on any active path)
| File | Provides | Purpose |
|---|---|---|
| `directEST_1030.R` | `directEST_1030()` | Dated local snapshot of `surveyPrev::directEST` (Oct-2025 dev copy), from before the variance-fix landed in the package. |
| `directEST_1219.R` | `directEST_1030()` | Newer snapshot of the same function (Dec-2025). |
| `fhModel_1030.R` | `fhModel_1030()` | Dated local snapshot of `surveyPrev::fhModel`. |

## Conventions
- Keep this folder **flat**; record grouping here in the catalog rather than in
  subfolders.
- When adding a file, add a catalog row and tag it common / archive.
  Check/validation scripts belong in the top-level `check/` folder, not here.
- Core, hot-path production utilities belong with their pipeline (e.g. data-prep
  helpers in `prep/prepare_functions.R`), not here.
