# Data Ingestion & IO Format Specifications

## Data Ingestion & IO Format Specifications

This document outlines the file format requirements, column parsing
rules, and data frame schemas used throughout **metabr**.

------------------------------------------------------------------------

### 1. Input Data Formats & Loaders

``` mermaid
flowchart TD
    subgraph inputSources [Input Data Sources]
        rawExcel["Raw El-MAVEN / LC-MS (CSV/Excel)"]
        batchExcel["Batch-Corrected Multi-Sheet Excel"]
        minuteDir["Minute Time-Course Directory"]
    end

    subgraph parsers [Parser Functions]
        readRaw["read_raw_metab()"]
        readBatch["read_batch_metab()"]
        readMinute["read_metab_minute()"]
    end

    subgraph schema [Standard Long Format Schema]
        tidyDF["Tidy Long-Format Data Frame"]
    end

    rawExcel --> readRaw
    batchExcel --> readBatch
    minuteDir --> readMinute

    readRaw --> tidyDF
    readBatch --> tidyDF
    readMinute --> tidyDF

    classDef src fill:#1f77b4,stroke:#333,stroke-width:2px,color:#fff
    classDef prs fill:#ff7f0e,stroke:#333,stroke-width:2px,color:#fff
    classDef sch fill:#2ca02c,stroke:#333,stroke-width:2px,color:#fff

    class rawExcel,batchExcel,minuteDir src
    class readRaw,readBatch,readMinute prs
    class tidyDF sch
```

------------------------------------------------------------------------

### 2. Standard Tidy Long-Format Data Schema

All loader functions in `metabr` convert raw wide spreadsheets into a
standardized long-format data frame containing the following key
attributes:

| Column Name | Data Type | Description |
|----|----|----|
| `sample` | `character` | Full sample identifier string (e.g. `run01-08Jul26_M101_15min_1_pos`) |
| `compound` | `factor` | Metabolite name |
| `medRt` | `numeric` | Median retention time in minutes (present in untargeted assays) |
| `value` | `numeric` | Peak area intensity or AUC value |
| `mouse_id` | `character` | Animal subject identifier extracted from sample string |
| `run` | `character` | Mass spectrometry run index |
| `rundate` | `Date` | Run date parsed from sample header |
| `Batch` | `character` / `integer` | Analytical batch number |
| `Plate` | `character` / `integer` | 96-well plate identifier |
| `time` | `character` | Sampling timepoint (e.g. `15min`) |
| `rep` | `character` | Technical replicate index |

------------------------------------------------------------------------

### 3. Data Export (`write_metab`)

Processed and normalized long-format metabolite data frames can be
written back to wide-format CSV spreadsheets suitable for external
sharing using
[`write_metab()`](file:///Users/brianyandell/Documents/Research/byandell-sysgen/metabr/R/write_metab.R#L13).

``` r

# Export processed data frame to wide-format CSV
write_metab(qc_data, "inst/extdata/metabolomics_qc_corrected.csv")
```
