project <- "data-explorer"
data_dir <- paste0(project, "/data/")

saveRDS(mtcars, paste0(data_dir, "mtcars.rds"))
saveRDS(iris, paste0(data_dir, "iris.rds"))
saveRDS(nycflights13::flights, paste0(data_dir, "flights.rds"))
