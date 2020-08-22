read_dat <- function(dat) {
  ext <- tools::file_ext(dat)
  switch (ext,
    "rds" = readRDS(dat),
    "csv" = read.csv(dat),
    stop("No method for reading", ext) # use validate for this
  )
}
