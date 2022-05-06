file <- "inst/extdata/phreeqc-input.csv"

inp <- read.csv2(file)

anonymize <- function(x, algo="crc32"){

  unq_hashes <- vapply(unique(x), function(object) digest::digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)

  unname(unq_hashes[x])
}

inp$solution <- anonymize(inp$solution)


write.csv2(x = inp,
           file = file,
           row.names = FALSE)


