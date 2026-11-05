# Credible Peace — data construction pipeline

R code behind my BSc thesis, *"Credible Peace: Institutions, Ceasefires, and the Political Economy of Post-Conflict Recovery"* (Utrecht University, 2026 — graded 9.5/10; selected among Forbes Italia's 100 Top Graduates 2026).

The thesis asks whether the quality of a country's political institutions **before** a conflict shapes how its local economies recover **after** a ceasefire is signed. Answering it meant building the dataset first, and that is what most of this code does.

**Output:** a reproducible panel of ~595,000 observations — 243 ceasefire episodes across 30 countries (2012–2020), observed at PRIO-GRID cell level in ten-day periods around each ceasefire date.

---

## The data problem

Eight public sources, none of which share a unit of observation:

| Source | Unit | Contributes |
|---|---|---|
| ETH/PRIO Ceasefires (CFD) | ceasefire declaration | ceasefire dates, type, parties |
| UCDP GED v25.1 | geolocated conflict event | violence locations, event dates |
| UCDP ACD | conflict-year | conflict linkage, episode onset |
| NASA Black Marble VNP46A2 | 500 m pixel, daily | night-light luminosity (outcome) |
| V-Dem v16 | country-year | pre-conflict institutional quality |
| PRIO-GRID | 0.5° grid cell | spatial backbone |
| Geo-PKO v2.1 | deployment-cell-month | peacekeeping presence |
| World Bank WDI | country-year | economic controls |

Reconciling these — declaration-level vs. event-level vs. daily pixel vs. country-year — is the substance of the pipeline.

---

## If you have five minutes

Four sections worth reading, in order of relevance to data work:

**1. The filter cascade and attrition log — lines 100–128 and 239–460**
Every sample restriction is applied as a named, numbered filter with a stated rationale, and each one is written to an attrition log recording how many ceasefires were dropped and why. Seven filters take the raw declarations down to the final 243 episodes. The log prints as a table, so the path from source to sample is auditable rather than asserted.

**2. Cell classification from event data — lines 731–830**
Classifying each (ceasefire × grid cell) pair as *salient* or *other* depending on whether conflict events occurred in the 180 days before the ceasefire. Includes diagnostics for the edge cases — ceasefires with zero cells, and ceasefires with no "other" cells — rather than silently dropping them.

**3. Satellite quality masking and the range join — lines 1207–1305**
Night-light values are masked to `NA` where the mandatory quality flag indicates cloud or stray-light contamination, with the masking rate reported. Daily observations are then range-joined to the panel's ten-day periods and aggregated. This is the step where the sources with the most incompatible frequencies are reconciled.

**4. Estimation and robustness — lines 2375 onwards**
Regression-discontinuity-in-time with fixed effects and cluster-robust inference, followed by pre-trend tests, bandwidth sensitivity (±90 days), donut and placebo checks.

**Result:** a robust null. An estimate that initially looked significant turned out to be driven by a single dominant country (Syria), which the leave-one-out checks surface.

---

## Pipeline structure

| Stage | What it does |
|---|---|
| 1 | Setup, package management, paths, attrition-log helper |
| 2 | Ceasefire data: parsing, date construction, seven-filter cascade |
| 3 | UCDP GED: conflict linkage, filtering, parsing |
| 4 | Cell classification (salient / other / all), country FE assignment |
| 5 | Panel skeleton: period structure, sanity checks |
| 6 | Night lights: loading, quality masking, range join, aggregation, log transform |
| 7 | V-Dem merge, episode onset year, controls, final panel |
| 8 | Regressions, pre-trend tests, robustness checks |

---

## Reproducing it

```r
# R 4.5.1
# Packages are installed and loaded by install_and_load_packages() in Stage 1.
# tidyverse · lubridate · data.table · haven · countrycode · fixest · modelsummary
```

Set `base_path` in Stage 1.2 to a directory containing `data/raw/`. The pipeline creates `data/processed/`, `figures/` and `tables/` on first run.

**Raw data is not included in this repository.** All eight sources are publicly available from their providers under their own licences; the table above lists versions. The night-light extract was produced separately in the Google Earth Engine Code Editor and exported as a daily cell-level CSV.

`set.seed(42)` is set in Stage 1.3.

---

## Notes

This is thesis code, written over several months by one person, and it reads like it in places. I have left it as submitted rather than tidying it retrospectively — including the diagnostic `cat()` blocks, which were how I checked each stage as I built it.

Pierfrancesco Biondi — pierfra.biondi@gmail.com
