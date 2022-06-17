# extract_between --------------------------------------------------------------
extract_between <- function(x, from_pattern, to_pattern)
{
  from_indices <- grep(from_pattern, x)
  to_indices <- grep(to_pattern, x)

  stopifnot(same_length(from_indices, to_indices))

  mapply(function(i, j) x[i:j], from_indices, to_indices, SIMPLIFY = FALSE)
}

# is_empty ---------------------------------------------------------------------
is_empty <- function(x)
{
  grep("^\\s*$", x)
}

# package_file -----------------------------------------------------------------
#' Path to File Stored in Package
#'
#' @param \dots segments of path to file, in the simplest form just a file name
#' @return full path to file within "extdata" folder of the installed package
#' @export
package_file <- function(...)
{
  file.path(system.file("extdata", package = "geosalz.phreeqc"), ...)
}

# same_length ------------------------------------------------------------------
same_length <- function(x, y)
{
  length(x) == length(y)
}

# trim_vector ------------------------------------------------------------------

#' Remove Empty Elements at the Start or End of a Vector
#'
#' @param x vector of character
#' @return \code{x} with empty elements at the beginning and end of \code{x}
#'   being removed
trim_vector <- function(x)
{
  # Idea: (Ab)use clipMatrix() to remove empty elements at the start or end of
  # a vector

  # Create a one column matrix m
  m <- matrix(x, ncol = 1L)

  # Set "" to NA as required by clipMatrix() to identify an empty field
  m[is_empty(m)] <- NA

  # Call clipMatrix() to remove empty rows on top or at the bottom of m
  m <- kwb.utils::clipMatrix(m)

  if (nrow(m) == 0L) {
    return(character(0L))
  }

  # Return the first (and only) column of m with NA set (back) to ""
  kwb.utils::defaultIfNA(m[, 1L], "")
}

# text_to_data_frame -----------------------------------------------------------
text_to_data_frame <- function(x)
{
  read.table(text = trim(x), header = FALSE, stringsAsFactors = FALSE)
}

# trim -------------------------------------------------------------------------
trim <- function(x)
{
  gsub("^\\s*", "", gsub("\\s*$", "", x))
}

