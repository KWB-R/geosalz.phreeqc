#' Convert Simulation Results to list
#'
#' @param simulations tibble with simulation results as rewtrieved by
#' \code{read_simulations}
#'
#' @return list with simulation results in structure for export to EXCEL
#' @export
#' @importFrom stats setNames
#' @importFrom dplyr bind_rows

convert_simulations_to_list <- function(simulations) {

  output_names <- names(simulations$output$solutions)

  out <- stats::setNames(lapply(output_names, function(output_name) {
    dplyr::bind_rows(simulations$output$solutions[[output_name]])

  }), nm = output_names)

  c(simulations[c("end_of_simulations_seconds", "input")],
    out)

}


#' Helper Function Get End of Simulations Seconds
#'
#' @param phreeqc_output vector with lines of PHREEQC output file
#'
#' @return returns total runtime for simulation(s) in seconds
#' @keywords internal
#' @noRd
#' @noMd
#' @importFrom stringr str_subset str_extract
#'
get_end_of_simulations_seconds <- function(phreeqc_output) {
  phreeqc_output %>%
    stringr::str_subset(pattern = "End of Run after\\s.*\\sSeconds.") %>%
    stringr::str_extract(pattern = "\\d+\\.\\d+") %>%
    as.numeric()
}


#' Read Simulations
#'
#' @param phreeqc_output vector with lines of PHREEQC output file
#'
#' @return returns list with elements "end_of_simulations_seconds",input and
#' output
#' @export
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom rlang .data

read_simulations <- function(phreeqc_output) {
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

  input_raw <- lapply(seq(indices), function(i) {
    phreeqc_output[input_start_idx[i]:input_end_idx[i]]
  })

  input <- lapply(seq(indices), function(i) {
    read_output_input(txt = input_raw[[i]])
  })

  calc_raw <- lapply(seq(indices), function(i) {
    phreeqc_output[calc_start_idx[i]:calc_end_idx[i]]
  })

  calc <- lapply(calc_raw, read_output_solutions)


  inp <- tibble::tibble(
    simulation_id = seq(n_simulations),
    data =  input) %>%
    tidyr::unnest(.data$data)



  dat <- tibble::tibble(
    simulation_id = seq(n_simulations),
    #input = input,
    solutions = calc)  %>%
    tidyr::unnest(.data$solutions)


  list(end_of_simulations_seconds = get_end_of_simulations_seconds(phreeqc_output),
       input = inp,
       output = dat)

}




#' Helper Function: Read Output 'Solutions' Blocks
#'
#' @param calc_output with indices for starting and ending of calculation block
#'
#' @return list with 5 elementsd
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom stringr str_split_fixed str_remove_all
#' @importFrom janitor make_clean_names
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#'
read_output_solutions <- function(calc_output) {
  #calc_output <- phreeqc_output[sim$calc_start_idx:sim$calc_end_idx]
  #calc_output <- calc_raw[[1L]]

    solutions_start_idx <- grep("^Initial solution", calc_output)


    solutions <- calc_output[solutions_start_idx] %>%
      stringr::str_split_fixed(pattern = "\\t", n = 2) %>%
      as.data.frame() %>%
      dplyr::rename(solution_id = "V1",
                    solution_name = "V2") %>%
      dplyr::mutate(solution_id = stringr::str_remove_all(.data$solution_id,
                                                          pattern = "Initial solution\\s?|\\.$"))

    solutions$start_idx <- solutions_start_idx
    solutions$end_idx <-c(solutions_start_idx[2:length(solutions_start_idx)] - 4,
        length(calc_output) - 5)


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
      read_redox_couples(solutions[i,]$blocks_raw[[1]]$redox_couples)
    })

    solutions$distribution_of_species <- lapply(seq(nrow(solutions)), function(i) {
      read_species_distribution(solutions[i,]$blocks_raw[[1]]$distribution_of_species)
    })

    solutions$saturation_indices <- lapply(seq(nrow(solutions)), function(i) {
      read_saturation_indices(txt = solutions[i,]$blocks_raw[[1]]$saturation_indices)
    })


    col_names <- names(solutions$blocks_raw[[1]])

    stats::setNames(lapply(col_names, function(col_name) {
      solutions %>%
         dplyr::select(tidyselect::all_of(c(names(solutions)[1:2], col_name))) %>%
         tidyr::unnest(cols = tidyselect::all_of(col_name))

     }), nm = col_names)

}


#' Helper Function: Read Output 'Input' Block
#'
#' @param txt vector of strings with input block
#'
#' @return tibble with input
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom stringr str_remove str_remove_all
#' @importFrom dplyr mutate rename relocate
#' @importFrom tibble as_tibble
#' @importFrom tidyselect ends_with
#'
read_output_input <- function(txt) {
  #txt <- phreeqc_output[sim$input_start_idx:sim$input_end_idx]
  dataset_name <- txt[1] %>%
    stringr::str_remove("\\tTITLE\\s")

  txt_solutions <- txt[-1] %>%
  stringr::str_subset("^\\tEND|^-|TITLE", negate = TRUE) %>%
  stringr::str_remove_all("\\t")


  sol_names_idx <- grep("^SOLUTION", txt_solutions)

  sol_df <- txt_solutions[sol_names_idx] %>%
    stringr::str_remove("SOLUTION\\s") %>%
    stringr::str_replace(" +", " ") %>%
    stringr::str_split_fixed(pattern = "\\s", n = 2) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::rename(solution_id = .data$V1,
                  solution_name = .data$V2) %>%
    dplyr::mutate(dataset_name = dataset_name) %>%
    dplyr::relocate(.data$dataset_name, .before = .data$solution_id)

  sol_df$start_idx <- sol_names_idx + 1
  sol_df$end_idx <- c(sol_names_idx[2:length(sol_names_idx)] - 1, length(txt_solutions))
  sol_df$txt_raw <- ""
  for(i in seq(nrow(sol_df))) {
  sol_df$txt_raw[i] <- list(txt_solutions[sol_df$start_idx[i]:sol_df$end_idx[i]])
  }

  sol_df$input <- lapply(seq(nrow(sol_df)), function(i) {
    read_input_txt(sol_df[i,]$txt_raw[[1]])
    }
  )

  sol_df %>%
    dplyr::select(- tidyselect::ends_with("_idx"), - tidyselect::ends_with("_raw")) %>%
    tidyr::unnest(.data$input)

}


#' Helper Function: Read Output 'Input' for one solution
#'
#' @param txt vector of strings with input block
#'
#' @return tibble with input
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom stringr str_trim
#' @importFrom dplyr mutate if_else
#' @importFrom tibble as_tibble
#' @importFrom tidyselect ends_with
#'

read_input_txt <- function(txt) {

  dat <- txt %>%
    stringr::str_trim() %>%
    stringr::str_replace(" +", " ")


  unit <- stringr::str_split_fixed(dat[1], " ", n = 2) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::rename(value = .data$V2)

  dat[-1] %>%
    stringr::str_split_fixed(" ", n = 2) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::rename(parameter = .data$V1,
                  value = .data$V2) %>%
    dplyr::mutate(value = as.numeric(.data$value),
                  unit = dplyr::if_else(!.data$parameter %in% c("temp", "pH"),
                                        unit$value,
                                        ""))


}


#' Helper Function: Read 'Solution Composition' Block
#'
#' @param txt vector of strings with solute_composition block
#' @return tibble withsolute composition block
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom utils read.table
#'
read_solution_composition <- function(txt) {
  #txt <- sol_out$output[[1]]

  txt_clean <- txt %>%
    stringr::str_remove("\\t") %>%
    stringr::str_replace_all(" +", " ")

  sol_comp <- utils::read.table(text = txt_clean,
                         sep = " ",
                         header = TRUE)

  stats::setNames(sol_comp, janitor::make_clean_names(names(sol_comp)))
}


#' Helper Function: Read 'Solution Description' Block
#'
#' @param txt vector of strings with solute_description block
#' @return tibble with solute description block
#' @keywords internal
#' @noMd
#' @noRd
#'

read_solution_description <- function(txt) {
  #txt <- sol_out$output[[2]]

  txt_clean <- txt %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed(pattern = "\\s+=\\s+", n = 2) %>%
    as.data.frame() %>%
    tibble::as_tibble()


  names(txt_clean) <- c("parameter", "value")

  txt_clean

}


#' Helper Function: Read 'Redox Couples' Block
#'
#' @param txt vector of strings with redox_coupkes block
#' @return tibble with redox couples block
#' @keywords internal
#'

read_redox_couples <- function(txt) {
  #txt <- sol_out$output[[3]]

  txt_clean <- txt %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("Eh \\(volts\\)", "eh_volts") %>%
    stringr::str_replace_all("Redox couple", "redox_couple") %>%
    stringr::str_replace_all(" +", " ")


  utils::read.table(text = txt_clean,
                    sep = " ",
                    header = TRUE)



}

#' Helper Function: Read 'Saturation Indices' Block
#'
#' @param txt vector of strings with saturation_indices block
#' @return tibble with saturation indices block
#' @keywords internal
#'

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
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(V2 = as.numeric(.data$V2),
                  V3 = as.numeric(.data$V3),
                  V4 = as.numeric(.data$V4))


 names(txt_clean) <- txt_header

 txt_clean

}





