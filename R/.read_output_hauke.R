if (FALSE)
{
  file <- "inst/extdata/phreeqc_output.txt"

  x <- readLines(file)

  main_sections <- get_main_sections(x)

  length(main_sections)

  sections_input <- split_main_section(
    x = main_sections$reading_input_data_for_simulation_1
  )

  sections_output <- split_main_section(
    x = main_sections$beginning_of_initial_solution_calculations
  )


  head(x)
  sections_output$
}

# get_main_sections ------------------------------------------------------------
get_main_sections <- function(x)
{
  dash_row_indices <- matrix(grep("^-+$", x), ncol = 2L, byrow = TRUE)

  stopifnot(all(dash_row_indices[, 2L] - dash_row_indices[, 1L] == 2L))

  from <- dash_row_indices[, 1L]
  to <- kwb.utils::startsToEnds(from, lastStop = length(x))

  sections <- lapply(seq_along(from), function(i) {
    x[seq.int(from[i], to[i])]
  })

  names(sections) <- janitor::make_clean_names(sapply(sections, "[", 2L))

  # Remove title and empty rows at the beginning
  lapply(sections, function(xx) {

    xx <- xx[- (1:3)]

    # Use clipMatrix() which removes empty rows at the start or end of a matrix
    xx[grep("^\\s*$", xx)] <- NA
    xx <- as.character(kwb.utils::clipMatrix(matrix(xx, ncol = 1L)))

    # Convert NA back to ""
    xx[is.na(xx)] <- ""
    xx
  })
}


#x <- section

# split_main_section -----------------------------------------------------------
split_main_section <- function(x)
{
  ranges <- kwb.utils::startsToRanges(
    starts = grep("^\\t\\S", x),
    lastStop = length(x),
    startOffset = 0L,
    stopOffset = 1L
  )

  sections <- lapply(seq_len(nrow(ranges)), function(i) {
    x[ranges$from[i]:ranges$to[i]]
  })

  # Take element name from title line
  names(sections) <- janitor::make_clean_names(sapply(sections, "[", 1L))

  # Remove title lines themselves
  lapply(sections, "[", -1L)
}

# split_initial_solution_calculations ------------------------------------------
split_initial_solution_calculations <- function(x)
{
  head(x)

  ranges <- kwb.utils::startsToRanges(
    starts = grep("^-+[^-]+-+$", x),
    lastStop = length(x),
    startOffset = 2L,
    stopOffset = 2L
  )

  i <- 5L
  x[ranges$from[i]:ranges$to[i]]

}
