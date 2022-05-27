#' Read Species Distribution
#'
#' @param txt txt
#'
#' @return read species
#' @keywords internal
#' @noRd
#' @noMd
#' @importFrom stringr str_remove str_trim str_replace_all str_split_fixed
#' @importFrom readr cols read_fwf fwf_widths
#' @importFrom janitor make_clean_names
#' @importFrom stats setNames
#' @importFrom kwb.utils defaultIfNA moveColumnsToFront rbindAll startsToRanges
read_species_distribution <- function(txt) {

  header_idx <- 1:2
  txt_header <- txt[header_idx]
  txt_data <- txt[-header_idx]

  tfile <- file.path(tempdir(), "txt_header.txt")

  writeLines(txt_header, tfile)


  suppressWarnings(expr = {
    header <- readr::read_fwf(
      tfile,
      col_positions = readr::fwf_widths(c(20,10,12,rep(10,4))),
      col_types = readr::cols(.default = "c")
    )
  })

  header <- paste(kwb.utils::defaultIfNA(header[1, ], ""), header[2, ]) %>%
    janitor::make_clean_names()


  #head(txt_data)

  # Add pseudo element as first row
  x <- c("XYZ", txt_data)


  starts <- grep("^\\S", x)
  ranges <- kwb.utils::startsToRanges(starts, lastStop = length(x))

  blocks <- lapply(seq_len(nrow(ranges)), function(i) {
    x[ranges$from[i]:ranges$to[i]] %>%
      stringr::str_remove("\t") %>%
      stringr::str_trim() %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_split_fixed(pattern = " ", n = 7)
  })

  names(blocks) <- stringr::str_split_fixed(x[starts], pattern = "\\s+", n = 2)[,1]

  data_frames <- lapply(blocks, function(block) {
    stats::setNames(as.data.frame(block), header)
  })

  dat_clean <- kwb.utils::rbindAll(data_frames, nameColumn = "element") %>%
    kwb.utils::moveColumnsToFront("element")

  dat_clean$element <- gsub("XYZ", "", dat_clean$element)

  dat_clean
}
