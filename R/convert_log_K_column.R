#data = txt_clean
convert_log_K_column <- function(data)
{
  log_k_column <- grep("^log_k", names(data), value = TRUE)

  # There must be exactly one log_k column
  stopifnot(length(log_k_column) == 1L)

  renamings <- kwb.utils::toLookupList(log_k_column, "log_k")

  data <- kwb.utils::renameColumns(data, renamings)

  #View(data)
  metadata <- kwb.utils::extractSubstring(
    pattern = "log_k_(.*)k_(.*)atm",
    x = log_k_column,
    index = c(temp_K = 1L, p_atm = 2L)
  )

  metadata[] <- lapply(metadata, as.numeric)

  cbind(data, metadata)
}
