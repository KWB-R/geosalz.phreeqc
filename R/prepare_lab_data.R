#' Add solution id
#'
#' @param samples data.frame with samples in wide format, i.e. parameters are
#' columns and each row is one samole
#'
#' @return add "solution_id"
#' @export
#' @importFrom dplyr mutate relocate

add_solution_id <- function(samples)  {

  samples %>%
  dplyr::mutate(solution_id = 1:nrow(samples)) %>%
  dplyr::relocate(.data$solution_id)
}


#' Tidy Samples
#'
#' @param samples data.frame with samples in wide format, i.e. parameters are
#' columns and each row is one sample
#' Coerce samples data.frame to as.character to address potential input format error
#' @return  data.frame with samples in tidy format, i.e. parameters are
#' columns and each row is one value
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr relocate
#' @importFrom rlang .data
#'
tidy_samples <- function(samples) {

  col_names <- names(samples)[!names(samples) %in% c("solution_id", "solution", "units")]

  samples %>%
    mutate_all(as.character) %>%
    add_solution_id() %>%
    tidyr::pivot_longer(cols = col_names,
                        names_to = "parameter",
                        values_to = "value") %>%
    dplyr::relocate(.data$solution_id)
}


#' Remove Missing Parameters
#'
#' @param samples_tidy data.frame with samples in tidy format, i.e. parameters are
#' columns and each row is one value
#' @param pattern_missing remove samples marked "n. a." or "- "
#' @param col_value value column (default: "value")
#'
#' @return data.frame without missing samples
#' @export
#' @importFrom stringr str_detect
#'
remove_missing_samples <- function(samples_tidy,
                                   pattern_missing = "n\\.\\s?a\\.|-",
                                   col_value = "value") {


samples_tidy[stringr::str_detect(samples_tidy[[col_value]],
                                   pattern = pattern_missing,
                                   negate = TRUE),]

}




#' Convert to numeric Laboratory Values
#'
#' @param samples_tidy data.frame with samples in tidy format, i.e. parameters are
#' columns and each row is one value
#' @param country "en" if value is given in English format (decimal point ".",
#' thousands separator ",") or "de" if value is given in German format (decimal
#' point ",", thousands separator ".").
#' @param detLimFactorBelow actor by which detection limit is multiplied in order
#' to get a valid value when the value was below the detection limit. Default value: 0.5
#' @param ... additional arguments passed to \link[kwb.base]{hsLabValToVal}
#'
#' @return data.frame with additional columns "outOfLimit" (with "<" or ">" sign)
#' and "numericValue"
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom kwb.base hsLabValToVal
convert_to_numeric_lab_values <- function(samples_tidy,
                                          country = "en",
                                          detLimFactorBelow = 0.5,
                                          ...
                                          )  {
samples_tidy <- remove_missing_samples(samples_tidy)

samples_tidy %>%
  dplyr::bind_cols(kwb.base::hsLabValToVal(samples_tidy$value,
                                           country = country,
                                           detLimFactorBelow = detLimFactorBelow,
                                           ...)
  )
}




