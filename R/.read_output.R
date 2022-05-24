if(FALSE) {

phreeqc_output <- readLines(
  system.file("extdata/phreeqc_output.txt",
  package = "geosalz.phreeqc")
)

end_of_simulations_seconds <- phreeqc_output %>%
  stringr::str_subset(pattern = "End of Run after\\s.*\\sSeconds.") %>%
  stringr::str_extract(pattern = "\\d+\\.\\d+") %>%
  as.numeric()


input_start_idx <- phreeqc_output %>%
  stringr::str_detect(pattern = "\\tTITLE") %>%
  which()

input_end_idx <- phreeqc_output %>%
  stringr::str_detect(pattern = "^TITLE") %>%
  which()

calc_start_idx <-  phreeqc_output %>%
  stringr::str_detect(pattern = "Beginning of initial solution calculations\\.") %>%
  which()

calc_end_idx <-  phreeqc_output %>%
  stringr::str_detect(pattern = "^End of simulation\\.") %>%
  which()

n_simulations <- length(calc_end_idx)

indices <- seq(n_simulations)
simulations <- tibble::tibble(
  simulation_id = seq(n_simulations),
  input_start_idx = input_start_idx,
  input_end_idx = input_end_idx,
  calc_start_idx = calc_start_idx,
  calc_end_idx = calc_end_idx
)



sim <- simulations[1, ]


indices <- sim$calc_start_idx:sim$calc_end_idx

calc_output <- phreeqc_output[indices]


solutions_start_idx <- grep("^Initial solution", calc_output)


solutions <- calc_output[solutions_start_idx] %>%
  stringr::str_split_fixed(pattern = "\\t", n = 2) %>%
  as.data.frame() %>%
  dplyr::rename(solution_id = "V1",
                solution_name = "V2") %>%
  dplyr::mutate(solution_id = stringr::str_remove_all(.data$solution_id,
                                             pattern = "Initial solution\\s?|\\.$"),
                simulation_id = sim$simulation_id)

solutions$start_idx <- solutions_start_idx
solutions$end_idx <-
  c(solutions_start_idx[2:length(solutions_start_idx)] - 4,
    length(calc_output) - 5)

solutions <- kwb.utils::moveColumnsToFront(solutions, columns = "simulation_id")

sol_list_raw <- lapply(seq(nrow(solutions)), function(i) {

  sol <- solutions[i,]

solution_output <- calc_output[sol$start_idx:sol$end_idx] %>%
  stringr::str_subset(pattern = "")


ind <- grep("^-+", solution_output)

sol_out_names <- solution_output[ind] %>%
  stringr::str_remove_all("-+") %>%
  janitor::make_clean_names()



sol_out <- tibble::tibble(
  name = sol_out_names,
  start_idx = ind + 1,
  end_idx = c(ind[2:length(ind)] - 1,
              length(solution_output))
)

stats::setNames(lapply(seq(nrow(sol_out)), function(i) {
  solution_output[sol_out$start_idx[i]:sol_out$end_idx[i]]
  }
), nm = sol_out_names)


})

solutions$blocks_raw <- sol_list_raw

solutions$solution_composition <- lapply(seq(nrow(solutions)), function(i) {
  read_solution_composition(solutions[i,]$blocks_raw[[1]]$solution_composition)
})

solutions$description_of_solution <- lapply(seq(nrow(solutions)), function(i) {
  read_solution_description(solutions[i,]$blocks_raw[[1]]$description_of_solution)
})

solutions$redox_couples <- lapply(seq(nrow(solutions)), function(i) {
  read_redox_couple(solutions[i,]$blocks_raw[[1]]$redox_couples)
})

solutions$distribution_of_species <- lapply(seq(nrow(solutions)), function(i) {
  read_species_distribution(solutions[i,]$blocks_raw[[1]]$distribution_of_species)
})

solutions$saturation_indices <- lapply(seq(nrow(solutions)), function(i) {
  read_saturation_indices(solutions[i,]$blocks_raw[[1]]$saturation_indices)
})

col_names <- names(solutions$blocks_raw[[1]])

solutions_output <- stats::setNames(lapply(col_names, function(col_name) {
  solutions %>%
  dplyr::select(tidyselect::all_of(c(names(solutions)[1:3], col_name))) %>%
  tidyr::unnest(cols = tidyselect::all_of(col_name))

}),nm = col_names)


openxlsx::write.xlsx(solutions_output,file = "phreeqc_output.xlsx")

}

read_solution_composition <- function(txt) {
  #txt <- sol_out$output[[1]]

  txt_clean <- txt %>%
    stringr::str_remove("\\t") %>%
    stringr::str_replace_all(" +", " ")

  sol_comp <- read.table(text = txt_clean,
                         sep = " ",
                         header = TRUE)

  stats::setNames(sol_comp, janitor::make_clean_names(names(sol_comp)))
}

read_solution_description <- function(txt) {
  #txt <- sol_out$output[[2]]

  txt_clean <- txt %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed(pattern = "\\s+=\\s+", n = 2) %>%
    tibble::as_tibble()


  names(txt_clean) <- c("parameter", "value")

  txt_clean

}


read_redox_couple <- function(txt) {
  #txt <- sol_out$output[[3]]

  txt_clean <- txt %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("Eh \\(volts\\)", "eh_volts") %>%
    stringr::str_replace_all("Redox couple", "redox_couple") %>%
    stringr::str_replace_all(" +", " ")


  read.table(text = txt_clean,
             sep = " ",
             header = TRUE)



}

read_saturation_indices <- function(txt) {
  #txt <- sol_out$output[[5]]
  txt_header <- txt[1] %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\sK,\\s+", "K_") %>%
    stringr::str_replace_all("\\satm", "atm") %>%
    stringr::str_replace_all("\\(", "_") %>%
    stringr::str_replace_all("log\\s", "log_") %>%
    stringr::str_remove_all("\\*{2}|\\)") %>%
    stringr::str_split_fixed("\\s+", n = 4) %>%
    janitor::make_clean_names()

  txt_header <- c(txt_header, "chemical_formula")


  txt_data <- txt[-1]

  txt_clean <- txt_data %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(" +", " ") %>%
    stringr::str_split_fixed("\\s", n = 5) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(V2 = as.numeric(.data$V2),
                  V3 = as.numeric(.data$V3),
                  V4 = as.numeric(.data$V4))


 names(txt_clean) <- txt_header

 txt_clean

}





