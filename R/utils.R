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

