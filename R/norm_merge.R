#' Normalize each compound and do log2 transform
#'
#' @param object data frame
#'
#' @return data frame
#' @export
#'
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom rlang .data
normalize <- function(object) {
  object |>
    dplyr::group_by(.data$compound, .data$medRt) |>
    dplyr::mutate(value = log2(.data$value / mean(.data$value))) |>
    dplyr::ungroup()
}