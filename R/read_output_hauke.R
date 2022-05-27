# Main program -----------------------------------------------------------------
if (FALSE)
{
  kwb.utils::assignPackageObjects("geosalz.phreeqc")
  file <- "inst/extdata/phreeqc_output.txt"
  result <- geosalz.phreeqc::read_output_file(file)

  result$initial_solution_calculations$initial_solution_4_603d9e1f$redox_couples
}

# read_output_file -------------------------------------------------------------

#' Read phreeqc output text file into a nested list structure
#'
#' @param file full path to phreeqc output file. Template:
#'   \code{system.file("extdata/phreeqc_output.txt", package =
#'   "geosalz.phreeqc")}
#' @export
read_output_file <- function(file)
{
  get <- kwb.utils::selectElements

  x <- kwb.utils::addClass(readLines(file), "phreeqc_output_file")

  main_sections <- split(x)

  # Extract sections "reading_input_data_..."
  section_names <- names(main_sections)
  input_names <- grep("^reading_input", section_names, value = TRUE)

  result <- lapply(stats::setNames(nm = input_names), function(input_name) {
    split(x = get(main_sections, input_name))
  })

  # Extract title
  result[["title"]] <- trim(get(main_sections, "title"))

  # Extract initial solution calculations
  element <- "beginning_of_initial_solution_calculations"
  calculations <- split(get(main_sections, element))

  calculations <- lapply(calculations, function(calculation) {
    #calculation <- calculations[[1L]]
    convert_sections(sections = split(calculation))
  })

  result[["initial_solution_calculations"]] <- calculations

  result[["runtime_seconds"]] <- extract_seconds(runtime_string = grep(
    "^end_of_run", names(main_sections), value = TRUE
  ))

  result
}

# extract_seconds --------------------------------------------------------------
extract_seconds <- function(runtime_string)
{
  pattern <- "^end_of_run_after_([0-9_]+)_seconds$"
  as.integer(gsub("_", "", gsub(pattern, "\\1", runtime_string)))
}

# split.phreeqc_output_file ----------------------------------------------------
split.phreeqc_output_file <- function(x)
{
  # indices of dash rows "surrounding" the "top level" header lines, arranged
  # in a two-column matrix
  dash_row_indices <- matrix(grep("^-+$", x), ncol = 2L, byrow = TRUE)

  # Check that top and bottom dash row have a distance of exactly two rows
  stopifnot(all(dash_row_indices[, 2L] - dash_row_indices[, 1L] == 2L))

  # Extract sections of lines that are limited by the top dash rows
  sections <- kwb.utils::extractRowRanges(x, starts = dash_row_indices[, 1L])

  # Name the sections by the title of the section
  names(sections) <- janitor::make_clean_names(sapply(sections, "[", 1L))

  # Cut title rows and empty rows on top of and at the bottom of each section
  sections <- lapply(sections, function(section) {

    #section <- sections[[6L]]

    # Remove title row and the row full of dashes below the title, then
    # remove empty rows on top or at the bottom
    trim_vector(section[- (1:2)])
  })

  # Set classes for relevant elements
  elements <- grep(
    "^end_of|title", names(sections), value = TRUE, invert = TRUE
  )

  replacements <- list(
    "^reading_" = "",
    "^beginning_of_" = "",
    "_\\d+$" = ""
  )

  for (element in elements) {
    simple_name <- kwb.utils::multiSubstitute(element, replacements)
    class_name <- paste0("phreeqc_", simple_name)
    sections[[element]] <- kwb.utils::addClass(sections[[element]], class_name)
  }

  sections
}

# split.phreeqc_input_data_for_simulation --------------------------------------
split.phreeqc_input_data_for_simulation <- function(x)
{
  if (length(x) == 0L) {
    return()
  }

  i <- 1L
  pattern <- "^\\tTITLE"
  stopifnot(grepl(pattern, x[i]))

  # Save the title in a variable and remove the title row
  title <- trim(gsub(pattern, "", x[i]))
  x <- x[-i]

  # Check and remove the END tag
  i <- length(x)
  pattern <- "^\\tEND$"
  stopifnot(grepl(pattern, x[i]))
  x <- x[-i]

  # Get the indices of the rows containing "SOLUTION"
  starts <- grep("SOLUTION", x)

  # Extract the blocks of lines between these rows
  blocks <- kwb.utils::extractRowRanges(x, starts = starts)

  # Provide metadata on each solution section: solution number and solution id
  headers <- kwb.utils::extractSubstring(
    pattern = "SOLUTION (\\d+)\\s+(\\S+)$",
    x = x[starts],
    index = c(solution_no = 1L, solution_id = 2L)
  )

  # Use the solution metadata as block name
  names(blocks) <- janitor::make_clean_names(x[starts])

  # Convert each block to a data frame with columns key and value
  solutions <- lapply(blocks, function(block) {
    stats::setNames(text_to_data_frame(block), c("key", "value"))
  })

  c(list(title = title), solutions)
}

# split.phreeqc_initial_solution_calculations ----------------------------------
split.phreeqc_initial_solution_calculations <- function(x)
{
  starts <- grep("Initial solution", x)

  stopifnot(starts[1L] == 1L)

  calculations_text <- kwb.utils::extractRowRanges(
    x,
    starts = starts,
    startOffset = 2L,
    stopOffset = 2L
  )

  names(calculations_text) <- janitor::make_clean_names(x[starts])

  lapply(
    calculations_text,
    kwb.utils::addClass,
    class = "phreeqc_initial_solution_calculation"
  )
}

# split.phreeqc_initial_solution_calculation -----------------------------------
split.phreeqc_initial_solution_calculation <- function(x)
{
  starts <- grep("^---", x)

  blocks <- kwb.utils::extractRowRanges(
    x,
    starts = starts,
    startOffset = 2L,
    stopOffset = 2L
  )

  stats::setNames(blocks, janitor::make_clean_names(x[starts]))
}

# convert_sections -------------------------------------------------------------
convert_sections <- function(sections)
{
  get <- kwb.utils::selectElements

  # Section 1
  name <- "solution_composition"
  sections[[name]] <- read_simple_table(get(sections, name))

  # Section 2
  name <- "description_of_solution"
  sections[[name]] <- read_key_value_assignments(get(sections, name))

  # Section 3
  name <- "redox_couples"
  sections[[name]] <- read_simple_table(get(sections, name))

  # Section 4
  name <- "distribution_of_species"
  x <- get(sections, name)
  #kwb.utils::headtail(data.frame(x = x), 20)
  sections[[name]] <- read_species_distribution(x)[-1L, ]

  # Section 5
  name <- "saturation_indices"
  sections[[name]] <- read_simple_table(get(sections, name))

  sections
}

# read_key_value_assignments ---------------------------------------------------
read_key_value_assignments <- function(x)
{
  data <- read.table(text = x, sep = "=", stringsAsFactors = FALSE)
  stats::setNames(data, c("key", "value"))
}

# read_simple_table ------------------------------------------------------------
read_simple_table <- function(x)
{
  indices_empty <- which(x == "")

  stopifnot(2L %in% indices_empty)

  indices_empty <- setdiff(indices_empty, 2L)

  n_empty <- length(indices_empty)

  stopifnot(n_empty <= 1L)

  if (n_empty) {
    indices_footer <- (indices_empty + 1L):length(x)
    footer <- x[indices_footer]
    x <- x[- indices_footer]
  } else {
    footer <- NULL
  }

  data <- text_to_data_frame(x[-(1:2)])

  result <- list(
    header = trim(x[1L]),
    data = data
  )

  if (is.null(footer)) {
    return(result)
  }

  c(result, list(footer = footer))
}
