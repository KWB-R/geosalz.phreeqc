
#' Prepare PHREEQC Solutions Input File
#'
#' @param samples_tidy data.frame with samples in tidy format, i.e. parameters are
#' columns and each row is one value
#' @param title user defined title (default: "")
#' @param min_spaces minimum spaces after parameter name based on longest parameter
#' name (default: 3 spaces)
#' @return input text to be used as PHREEQC input
#' @export
#' @importFrom dplyr count select
#' @importFrom stringr str_length
#' @examples
#' file_samples <- system.file("extdata/phreeqc-input.csv",
#' package = "geosalz.phreeqc")
#' samples <- read.csv2(file_samples)
#' samples_tidy <- geosalz.phreeqc::tidy_samples(samples) %>%
#' geosalz.phreeqc::convert_to_numeric_lab_values()
#' prepare_solutions_input(samples_tidy)
#'
prepare_solutions_input <- function(samples_tidy,
                                    title = "",
                                    min_spaces = 3) {

longest_para_name <- ifelse(max(stringr::str_length(unique(samples_tidy$parameter))) < 5,
                            5,
                            max(stringr::str_length(unique(samples_tidy$parameter))))


solutions_meta <- samples_tidy %>%
  dplyr::count(.data$solution_id,
               .data$solution,
               .data$units) %>%
  dplyr::select(- .data$n)

solutions_txt <- lapply(seq_len(nrow(solutions_meta)), function(id) {

sample_metaunit <- solutions_meta[solutions_meta$solution_id == id,]
sample_tidy <- samples_tidy[samples_tidy$solution_id == id,]


spaces_front <- paste0(rep(" ", 8), collapse = "")

spaces_units_num <- min_spaces + longest_para_name - stringr::str_length("units")

spaces_units <- paste0(rep(" ", spaces_units_num), collapse = "")


solution_meta <- sprintf("SOLUTION %d  %s",
                         sample_metaunit$solution_id,
                         sample_metaunit$solution)
solution_unit <- sprintf("%sunits%s%s",
                         spaces_front,
                         spaces_units,
                         sample_metaunit$units)

spaces_parameter_num <- min_spaces + longest_para_name - stringr::str_length(sample_tidy$parameter)
spaces_parameter <- sapply(spaces_parameter_num, function(x) paste0(rep(" ", x), collapse = ""))

solution_values <- paste0(sprintf("%s%s%s%f",
                                  spaces_front,
                                  sample_tidy$parameter,
                                  spaces_parameter,
                                  sample_tidy$numericValue),
                          collapse = "\n"
                          )



sprintf("%s\n%s\n%s\n",
        solution_meta,
        solution_unit,
        solution_values)
}
)


sprintf("TITLE %s\n%sEND",
        title,
        paste(solutions_txt, collapse = ""))


}
