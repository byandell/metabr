# Developer Guide Blueprint, Implementation Plan & Walkthrough for `metabr`

This document records the developer guide prompt blueprint, implementation plan, and completion walkthrough for **`metabr`** (v0.2.3).

---

## 1. Actionable Prompt & Blueprint

### Purpose

Create a comprehensive developer guide infrastructure across R package repositories (`metabr`, `modulr`, `foundrHarmony`) detailing codebase architecture, function index, quality control / data flow methodology, and automated deployment via `pkgdown` and GitHub Actions.

### Blueprint Specifications

1. **Root Guide (`DEVELOPER.md`)**:
   - High-level package introduction, ecosystem role, directory structure, function reference, developer quick start (`devtools::load_all()`), and release guidelines.

2. **Vignettes & Articles (`vignettes/devel_guide/`)**:
   - `index.Rmd`: Overview, ecosystem integration flowchart, and local development commands.
   - `qc_pipeline.Rmd` / `modules.Rmd`: In-depth methodology detailing mathematical formulations, algorithm stages, sub-flowcharts, and S3 class diagnostic routines.
   - `data_ingestion.Rmd` / `data_flow.Rmd`: Data format specifications, column schemas, parser functions, and export routines.

3. **`pkgdown` Site Configuration (`_pkgdown.yml`)**:
   - Bootstrap 5 template.
   - Dynamic Mermaid JS v10 dynamic flowchart rendering header block.
   - Custom **Developer Guide** navbar dropdown menu linking to rendered HTML articles.
   - Categorized function reference sections.

4. **GitHub Actions Deployment (`.github/workflows/pkgdown.yaml`)**:
   - Automated workflow to build `pkgdown` site in CI/CD and deploy HTML assets directly to the **`gh-pages`** branch.
   - Configured `.gitignore` (`docs/`) and `.Rbuildignore`.

---

## 2. Implementation Plan

### User Review & Approval

- All documentation files placed inside `DEVELOPER.md` (root) and `vignettes/devel_guide/` (`index.Rmd`, `qc_pipeline.Rmd`, `data_ingestion.Rmd`).
- `_pkgdown.yml` site configuration equipped with Bootstrap 5 and Mermaid JS header block.
- Automated GitHub Actions workflow (`.github/workflows/pkgdown.yaml`) created for `gh-pages` deployment.

### File Modifications & Additions

- **Root & Configs**:
  - `DEVELOPER.md`: Comprehensive package architecture & developer guide.
  - `_pkgdown.yml`: Site template, navbar, and function reference grouping.
  - `.github/workflows/pkgdown.yaml`: GitHub Actions deployment pipeline.
  - `.gitignore`: Added `docs/`.
  - `.Rbuildignore`: Added `_pkgdown.yml`, `.github`, `DEVELOPER.md`, `docs`.
  - `DESCRIPTION`: Updated URL to `https://byandell-sysgen.github.io/metabr`.

- **Vignettes**:
  - `vignettes/devel_guide/index.Rmd`: Overview article with top-level Mermaid flowchart.
  - `vignettes/devel_guide/qc_pipeline.Rmd`: QC correction factor formulas and sub-flowchart.
  - `vignettes/devel_guide/data_ingestion.Rmd`: IO specifications and tidy data frame schema.

---

## 3. Execution Walkthrough

### Created & Updated Files

| File Path | Description |
| --- | --- |
| [`DEVELOPER.md`](file:///Users/brianyandell/Documents/Research/byandell-sysgen/metabr/DEVELOPER.md) | Primary developer guide detailing package architecture, mass spectrometry raw input formats, QC correction mathematics, S3 class definitions, testing procedures, and release guidelines. |
| [`vignettes/devel_guide/index.Rmd`](file:///Users/brianyandell/Documents/Research/byandell-sysgen/metabr/vignettes/devel_guide/index.Rmd) | High-level developer overview, package purpose, local quick start, and top-level end-to-end Mermaid flowchart. |
| [`vignettes/devel_guide/qc_pipeline.Rmd`](file:///Users/brianyandell/Documents/Research/byandell-sysgen/metabr/vignettes/devel_guide/qc_pipeline.Rmd) | Technical reference detailing the 4-stage quality control correction algorithm (`qc_steps()`), correction factor ($\text{CF}$) formulas, retention time (`medRt`) handling, and `hand_auto()` curation discrepancy analysis. |
| [`vignettes/devel_guide/data_ingestion.Rmd`](file:///Users/brianyandell/Documents/Research/byandell-sysgen/metabr/vignettes/devel_guide/data_ingestion.Rmd) | Data specifications detailing raw CSV/Excel parsers, minute time-course loaders, long-format tidy data frame schemas, and `write_metab()` export rules. |
| [`_pkgdown.yml`](file:///Users/brianyandell/Documents/Research/byandell-sysgen/metabr/_pkgdown.yml) | `pkgdown` configuration with Bootstrap 5 styling, dynamic Mermaid JS v10 flowchart rendering block in headers, custom navbar, and organized function reference sections. |
| [`.github/workflows/pkgdown.yaml`](file:///Users/brianyandell/Documents/Research/byandell-sysgen/metabr/.github/workflows/pkgdown.yaml) | Automated GitHub Actions workflow to build and deploy the `pkgdown` documentation site to the `gh-pages` branch on every push to `main`/`master`. |
| [`.gitignore`](file:///Users/brianyandell/Documents/Research/byandell-sysgen/metabr/.gitignore) | Added `docs/` to `.gitignore` to prevent generated site files from cluttering the source branch. |
| [`.Rbuildignore`](file:///Users/brianyandell/Documents/Research/byandell-sysgen/metabr/.Rbuildignore) | Updated to ignore `_pkgdown.yml`, `.github`, `DEVELOPER.md`, and `docs` during package builds. |
| [`DESCRIPTION`](file:///Users/brianyandell/Documents/Research/byandell-sysgen/metabr/DESCRIPTION) | Updated package URL to `https://byandell-sysgen.github.io/metabr`. |

---

### Empirical Verification Output

```text
── Sitrep ──────────────────────────────────────────────────────────────────────
✔ URLs ok.
✔ Favicons ok.
✔ Open graph metadata ok.
✔ Articles metadata ok.
✔ Reference metadata ok.
── Initialising site ───────────────────────────────────────────────────────────
── Building home ───────────────────────────────────────────────────────────────
Writing `authors.html`
Reading DEVELOPER.md
Reading README.md
Writing `404.html`
── Building function reference ─────────────────────────────────────────────────
Writing `reference/index.html`
── Building articles ───────────────────────────────────────────────────────────
Writing `articles/index.html`
Reading vignettes/devel_guide/data_ingestion.Rmd
Writing `articles/devel_guide/data_ingestion.html`
Reading vignettes/devel_guide/index.Rmd
Writing `articles/devel_guide/index.html`
Reading vignettes/devel_guide/qc_pipeline.Rmd
Writing `articles/devel_guide/qc_pipeline.html`
── Finished building pkgdown site for package metabr ───────────────────────────
```

---

### GitHub Pages Publishing Instructions

1. **Commit and Push Source Files**:
   ```bash
   git add .gitignore .Rbuildignore _pkgdown.yml .github/ DESCRIPTION vignettes/ DEVELOPER.md inst/doc/devel_guide.md
   git commit -m "Add developer guides, pkgdown site config, and GitHub Actions workflow"
   git push origin main
   ```

2. **Enable GitHub Pages**:
   - Go to `https://github.com/byandell-sysgen/metabr/settings/pages`
   - Set **Source** to `Deploy from a branch` $\rightarrow$ Branch **`gh-pages`** and folder **`/ (root)`**.
