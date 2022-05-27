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

# trim_vector ------------------------------------------------------------------

#' Remove Empty Elements at the Start or End of a Vector
#'
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

