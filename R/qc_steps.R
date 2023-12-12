#' Title
#'
#' @param dirpath path to directory
#' @param filename name of file
#' @param exclude_first exclude first QC if `TRUE`
#'
#' @return
#' @export
qc_steps <- function(dirpath, filename, exclude_first = FALSE) {
  rawobject <- read_raw_metab(dirpath, filename)
  qcobject <- calc_cf(rawobject, exclude_first) |>
    replace_missing_ave_cf()
  correct_raw_cf(rawobject, qcobject)
}
#' 1. Transpose Data and Extract Info from Sample Names
#'
#' @param dirpath path to directory
#' @param filename name of file
read_raw_metab <- function(dirpath, filename) {
  # Data are in sheet 1 starting on line 1.
  readr::read_csv(file.path(dirpath, filename), show_col_types = FALSE) |>
  
  # Pivot traits, which begin after `parent` column.
  tidyr::pivot_longer(-(label:parent), names_to = "mouse_id", values_to = "value") |>
  dplyr::mutate(
    run = stringr::str_replace(mouse_id, "^run([0-9]+)-.*", "\\1"),
    rundate = 
      as.Date(
        stringr::str_replace(mouse_id, "^run[0-9]+-([0-9A-Za-z]+)_.*", "\\1"),
        "%d%b%y"))
}
#' 2. Pick up QC runs of every plate, and calculate correction factors (CF) for each metabolite.
#'
#' @param object object from `read_raw_metab`
#' @param exclude_first exclude first QC if `TRUE`
calc_cf <- function(rawobject, exclude_first = FALSE) {
  # For untargeted curation, there may be up to the five most intense peaks for each metabolite in each mode based on their `m/z`.
  # Thus one metabolite may have multiple entries distinguished by retention times.
  # We batch-correct on each entry separately by grouping by `compound` and `medRt`.
  # This works as well for targeted curation where there is only one `medRt`.

  # Pick up QC runs of every plate.
  qcobject <- rawobject |>
    dplyr::filter(stringr::str_detect(mouse_id, "_QC_")) |>
    # Exclude first QC value if desired (RP_pos data).
    dplyr::mutate(value = ifelse(dplyr::row_number() == 1 & exclude_first,
                                 NA, value)) |>
    dplyr::group_by(rundate, compound, medRt) |>
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::ungroup()

  # Join data with `compound` and `medRt` averages over `rundates`.
  qcobject <- 
    dplyr::left_join(
      qcobject,
    
      # average across `rundate` by compound.
      qcobject |>
        dplyr::group_by(compound, medRt) |>
        dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::ungroup(),
      by = c("compound", "medRt"),
      suffix = c("", "_ave")) |>
  
    # Divide `value` by `value_ave` (or make NA if `value_ave` is missing).
    dplyr::mutate(value = ifelse(value_ave != 0 & !is.na(value_ave),
                                value / value_ave, NA)) |>
    dplyr::select(-value_ave)
}
#' 3. Use average CF of all metabolites in a plate as CF for missing metabolites.
#' 
#' Missing values might be `NA` or `0` values.
#' @param object
replace_missing_ave_cf <- function(qcobject) {
  qc_date <- qcobject |>
    # Filter out `NA` and `0` values.
    dplyr::filter(!is.na(value), value != 0) |>
    dplyr::group_by(rundate, medRt) |>
    dplyr::summarize(value = mean(value), .groups = "drop") |>
    dplyr::ungroup()

  # Bads are any compounds with missing `value` or `value` = 0.
  bads <- (qcobject |>
    dplyr::filter(is.na(value) | value == 0) |>
    dplyr::count(compound, medRt))$compound

    # Join `rundata` averages to QC.
    dplyr::left_join(qcobject, qc_date,
                     by = c("rundate", "medRt"),
                     suffix = c("", "_ave")) |>
      # If bad `compound`, replace `value` by `value_ave`
      dplyr::mutate(value = ifelse(compound %in% bads, value_ave, value)) |>
      dplyr::select(-value_ave)
}

#' 4. Correct the Peak Area using CF for each metabolite 
#'
#' @param rawobject 
#' @param qcobject
correct_raw_cf <- function(rawobject, qcobject) {
  dplyr::left_join(rawobject, qcobject, by = c("rundate", "compound", "medRt"), suffix = c("", "_cf")) |>
    # Divide `value` by `value_qc` (or make NA if `value_ave` is missing, which should not happen).
    dplyr::mutate(value = ifelse(value_cf != 0 & !is.na(value_cf),
                                 value / value_cf, NA)) |>
    dplyr::select(-value_cf)
}
