# metabr Developer Guide Overview & Architecture

## metabr Developer Guide Overview & Architecture

### Package Purpose & Ecosystem

**metabr** (v0.2.3) is an R package within the `foundr` / systems
genetics ecosystem providing specialized tools for metabolomics data
ingestion, mass spectrometry peak-area quality control (QC) batch
correction, normalization, and curation discrepancy analysis.

- **Author:** Brian S. Yandell (<brian.yandell@wisc.edu>)
- **License:** GPL-3
- **Minimum R Version:** $`\ge 3.5.0`$

#### Systems Genetics Data Ingestion Pipeline

``` mermaid
flowchart TD
    subgraph input [Raw Mass Spectrometry Input]
        csvFile["El-MAVEN / LC-MS CSV File"]
        excelFile["Batch-Corrected Excel File"]
    end

    subgraph metabrEngine [metabr Data Ingestion & QC Engine]
        readRaw["read_raw_metab() / read_batch_metab()"]
        qcSteps["qc_steps() Core Driver"]
        calcCF["calc_cf() Plate QC Averaging"]
        replaceCF["replace_missing_ave_cf() Fallbacks"]
        correctCF["correct_raw_cf() Intensity Scaling"]
        norm["normalize() log2 Mean-Centering"]

        readRaw --> qcSteps
        qcSteps --> calcCF
        calcCF --> replaceCF
        replaceCF --> correctCF
        correctCF --> norm
    end

    subgraph downstream [Ecosystem Integration]
        writeMetab["write_metab() Export Wide Data"]
        handAuto["hand_auto() Curation Comparison"]
        foundrHarm["foundrHarmony Ingestion"]
        foundrCore["foundr Variance Partitioning"]

        norm --> writeMetab
        norm --> handAuto
        writeMetab --> foundrHarm
        foundrHarm --> foundrCore
    end

    classDef input fill:#1f77b4,stroke:#333,stroke-width:2px,color:#fff
    classDef engine fill:#ff7f0e,stroke:#333,stroke-width:2px,color:#fff
    classDef output fill:#2ca02c,stroke:#333,stroke-width:2px,color:#fff

    class csvFile,excelFile input
    class readRaw,qcSteps,calcCF,replaceCF,correctCF,norm engine
    class writeMetab,handAuto,foundrHarm,foundrCore output
```

------------------------------------------------------------------------

### Developer Quick Start

#### Local Development Commands

To inspect, update, or test **metabr** locally:

``` r

# 1. Load local development files
devtools::load_all()

# 2. Generate roxygen documentation & NAMESPACE
devtools::document()

# 3. Build pkgdown documentation site locally
pkgdown::build_site(install = FALSE)

# 4. Run package verification
devtools::check(cran = FALSE, vignettes = FALSE)
```

------------------------------------------------------------------------

### Developer Guide Navigation

This Developer Guide is organized into the following detailed
sub-articles:

1.  **[Developer Guide Overview &
    Architecture](https://byandell-sysgen.github.io/metabr/articles/devel_guide/index.md)**:
    Package purpose, ecosystem architecture, end-to-end processing
    flowchart, and quick start instructions.
2.  **[QC Pipeline & Correction
    Methodology](https://byandell-sysgen.github.io/metabr/articles/devel_guide/qc_pipeline.md)**:
    Mathematical specifications for QC correction factors, compound-wise
    median retention time (`medRt`) handling, missing value imputation,
    and `hand_auto` curation diagnostics.
3.  **[Data Ingestion & IO Format
    Specifications](https://byandell-sysgen.github.io/metabr/articles/devel_guide/data_ingestion.md)**:
    Detailed schemas for raw mass spectrometry spreadsheets, minute
    time-course files, and long-format tidy data frame representations.
