#' Title
#'
#' @param dirpath path to directory
#' @param filename name of file
#' @param exclude_first exclude first QC if `TRUE`
#' @param Batch_Plate group by `Batch` and `Plate` if not `NULL`.
#'
#' @return data frame
#' @export
qc_steps <- function(dirpath, filename,
                     exclude_first = FALSE,
                     Batch_Plate = NULL) {
  rawobject <- read_raw_metab(dirpath, filename, Batch_Plate = Batch_Plate)
  qcobject <- calc_cf(rawobject, exclude_first) |>
    replace_missing_ave_cf()
  correct_raw_cf(rawobject, qcobject)
}
# 1. Transpose Data and Extract Info from Sample Names
# see R/read_raw_metab.R

#' 2. Pick up QC runs of every plate, and calculate correction factors (CF) for each metabolite.
#'
#' @param object object from `read_raw_metab`
#' @param exclude_first exclude first QC if `TRUE`
#' 
#' @rdname qc_steps
#' @export
#' @importFrom dplyr filter group_by left_join mutate row_number select summarize ungroup
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#' 
calc_cf <- function(rawobject, exclude_first = FALSE) {
  # For untargeted curation, there may be up to the five most intense peaks for
  # each metabolite in each mode based on their `m/z`. Thus one metabolite may
  # have multiple entries distinguished by retention times. We batch-correct on
  # each entry separately by grouping by `compound` and `medRt`.
  # This works as well for targeted curation where there is only one `medRt`.

  rawobject <- rawobject |>
    dplyr::filter(stringr::str_detect(.data$sample, "_QC_")) |>
    # Exclude first QC value if desired (RP_pos data).
    dplyr::mutate(value = ifelse(dplyr::row_number() == 1 & exclude_first,
                                 NA, value))

  # Pick up QC runs of every plate.
  if(all(c("Batch","Plate") %in% names(rawobject))) {
    qcobject <- rawobject |>
      dplyr::group_by(.data$Batch, .data$Plate, .data$compound, .data$medRt)
  } else {
    qcobject <- rawobject |>
      dplyr::group_by(.data$rundate, .data$compound, .data$medRt)
  }
  qcobject <- qcobject |>
    dplyr::summarize(value = mean(.data$value, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::ungroup()
  
  # average across `rundate` by compound.
  qcobjectave <- rawobject |>
    dplyr::group_by(.data$compound, .data$medRt) |>
    dplyr::summarize(value = mean(.data$value, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::ungroup()

  # Join data with `compound` and `medRt` averages over `rundates`.
  qcobject <- 
    dplyr::left_join(
      qcobject,
      qcobjectave,
      by = c("compound", "medRt"),
      suffix = c("", "_ave")) |>
  
    # Divide `value` by `value_ave` (or make NA if `value_ave` is missing).
    dplyr::mutate(value = ifelse(.data$value_ave != 0 & !is.na(.data$value_ave),
                                 .data$value / .data$value_ave, NA)) |>
    dplyr::select(-value_ave)
}
#' 3. Use average CF of all metabolites in a plate as CF for missing metabolites.
#' 
#' Missing values might be `NA` or `0` values.
#' 
#' @param object object from `calc_cf()`
#' 
#' @rdname qc_steps
#' @export
#' @importFrom dplyr count filter group_by left_join mutate select
#'             summarize ungroup
#' @importFrom rlang .data
#' 
replace_missing_ave_cf <- function(qcobject) {
  qc_date <- qcobject |>
    # Filter out `NA` and `0` values.
    dplyr::filter(!is.na(.data$value), .data$value != 0)
  if(all(c("Batch","Plate") %in% names(qc_date))) {
    qc_date <- qc_date |>
      dplyr::group_by(.data$Batch, .data$Plate, .data$medRt)
    bys <- c("Batch", "Plate", "medRt")
  } else {
    qc_date <- qc_date |>
      dplyr::group_by(.data$rundate, .data$medRt)
    bys <- c("rundate", "medRt")
  }
  
  qc_date <- qc_date |>
    dplyr::summarize(value = mean(.data$value), .groups = "drop") |>
    dplyr::ungroup()

  # Bads are any compounds with missing `value` or `value` = 0.
  bads <- (qcobject |>
    dplyr::filter(is.na(value) | value == 0) |>
    dplyr::count(compound, medRt))$compound

  # Join `rundata` averages to QC.
  dplyr::left_join(qcobject, qc_date, by = bys, suffix = c("", "_ave")) |>
    # If bad `compound`, replace `value` by `value_ave`
    dplyr::mutate(value = ifelse(.data$compound %in% bads,
                                 .data$value_ave, .data$value)) |>
    dplyr::select(-value_ave)
}

#' 4. Correct the Peak Area using CF for each metabolite 
#'
#' @param rawobject 
#' @param qcobject
#' 
#' @rdname qc_steps
#' @export
#' @importFrom dplyr left_join mutate select
#' @importFrom rlang .data
#' 
correct_raw_cf <- function(rawobject, qcobject) {
  if(all(c("Batch","Plate") %in% names(rawobject))) {
    bys <- c("Batch", "Plate", "compound", "medRt")
  } else {
    bys <- c("rundate", "compound", "medRt")
  }
  
  dplyr::left_join(rawobject, qcobject, by = bys, suffix = c("", "_cf")) |>
    # Divide `value` by `value_qc`
    # (or make NA if `value_ave` is missing, which should not happen).
    dplyr::mutate(value = ifelse(.data$value_cf != 0 & !is.na(.data$value_cf),
                                 .data$value / .data$value_cf, NA)) |>
    dplyr::select(-value_cf) |>
    # Remove QC samples.
    dplyr::filter(!grepl("_QC_", .data$sample))
}
