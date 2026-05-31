# Final Exercise — Vienna Urban Heat Island (2005–2025)

Integrated geostatistical / time-series pipeline and final report for the
VU Environmental Statistics course (Group 2).

## Contents

- `final_script.R` — the full integrated pipeline (download, preprocessing,
  indicators, ideal-night filter, homogenisation, trend analysis, heatwave
  amplification, land-cover regression, figures, and a `collect_documents()`
  step that gathers the rendered documents into `docs/`).
- `docs/` — the final report and its sources:
  - `final_report.Rmd` / `.md` / `.pdf` / `.html` — the report (full narrative
    plus the pipeline presented as ordered code-chunk steps, with a table of
    contents and all figures).
  - `final_script.Rmd` / `.md` — a typeset view of the pipeline.
  - `preamble.tex` — LaTeX preamble used for the PDF render.

The raw GeoSphere data (`data/`) and generated figures (`output/figures/`) are
not committed here; run `final_script.R` from a project root to regenerate them.
