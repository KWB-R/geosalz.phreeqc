phreeqc_output <- system.file("extdata/phreeqc_output.txt",
                              package = "geosalz.phreeqc")

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
  id = seq(n_simulations),
  input_start_idx = input_start_idx,
  input_end_idx = input_end_idx,
  calc_start_idx = calc_start_idx,
  calc_end_idx = calc_end_idx
)



sim <- simulations[1, ]


indices <- sim$calc_start_idx:sim$calc_end_idx

calc_output <- phreeqc_output[indices]


solutions_start_idx <- calc_output %>%
  stringr::str_detect(pattern = "^Initial solution") %>%
  which()


solutions <- calc_output[solutions_start_idx] %>%
  stringr::str_split_fixed(pattern = "\\t", n = 2) %>%
  as.data.frame() %>%
  dplyr::rename(id = "V1",
                name = "V2") %>%
  dplyr::mutate(id = stringr::str_remove_all(.data$id,
                                             pattern = "Initial solution\\s?|\\.$"))

solutions$start_idx <- solutions_start_idx
solutions$end_idx <-
  c(solutions_start_idx[2:length(solutions_start_idx)] - 4,
    sim$calc_end_idx - 5)

sol <- solutions[1, ]


solution_output <- calc_output[sol$start_idx:sol$end_idx] %>%
  stringr::str_subset(pattern = "")


ind <- solution_output %>%
  stringr::str_detect("^-+") %>%
  which()

sol_out_names <- solution_output[ind] %>%
  stringr::str_remove_all("-+") %>%
  janitor::make_clean_names()



sol_out <- tibble::tibble(
  name = sol_out_names,
  start_idx = ind + 1,
  end_idx = c(ind[2:length(ind)] - 1,
              length(solution_output)),
  output = ""
)

for (i in seq(nrow(sol_out))) {
  sol_out$output[i] <-
    list(solution_output[sol_out$start_idx[i]:sol_out$end_idx[i]])
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
