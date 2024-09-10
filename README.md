# README

QC Automation is automated for targeted data
[QC_Automation.Rmd](https://github.com/byandell/metabr/blob/master/QC_Automation.Rmd).
Key routine is [qc_steps.R](https://github.com/byandell/metabr/blob/master/R/qc_steps.R).

Untargeted data follow the same schema, but they may have multiple peaks
identified by `medRt`.

Data are written to CSV files with [write_metab.R](https://github.com/byandell/metabr/blob/master/R/write_metab.R).

Not clear yet how this will generalize to DO data.

Brian Yandell, 10 Sep 2024
