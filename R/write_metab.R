#' Write Metabolite File
#'
#' @param object data frame
#' @param filename name of saved file
#' @param targeted targeted if `TRUE`
#' @param ... ignored
#'
#' @return invisible
#' @export
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider unite
#' @importFrom utils write.csv
write_metab <- function(object, filename, targeted = TRUE, ...) {
  if(targeted) {
    object <- dplyr::select(object,
      sample, run, rundate, Batch, Plate, mouse_id, time, rep, compound, value)
  } else {
    object <- dplyr::select(object,
      sample, run, rundate, Batch, Plate, mouse_id, time, rep, compound, medRt, value) |>
      dplyr::mutate(medRt = round(medRt, 3)) |>
      tidyr::unite(compound, compound, medRt)
  }
  object <- tidyr::pivot_wider(object, id_cols = sample:rep,
    names_from = "compound", values_from = "value")
  names(object)[1:8] <- 
    c("Original Sample Name", "Run Order", "Run Date", "Batch", "Plate",
      "Strains", "Timepoints", "Replicates")
  
  invisible(utils::write.csv(object, filename, row.names = FALSE))
}