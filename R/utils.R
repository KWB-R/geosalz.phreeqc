# extract_blocks ---------------------------------------------------------------
extract_blocks <- function(x, starts, startOffset = 1L, stopOffset = 1L)
{
  ranges <- kwb.utils::startsToRanges(
    starts = starts,
    lastStop = length(x),
    startOffset = startOffset,
    stopOffset = stopOffset
  )

  lapply(seq_len(nrow(ranges)), function(i) x[ranges$from[i]:ranges$to[i]])
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

