reactable_data <- function(.data) {
  reactable(
    data = .data,
    filterable = TRUE,
    minRows = 10,
    bordered = TRUE,
    highlight = TRUE,
    height = 500,
    defaultColDef = colDef(
      header = function(value) gsub(".", " ", value, fixed = TRUE),
      align = "center",
      maxWidth = 200,
      headerStyle = list(background = "#f7f7f8")
    )
  )
}

reactable_filter_summary <- function(.data) {
  reactable(
    data = .data,
    filterable = TRUE,
    minRows = 10,
    bordered = TRUE,
    highlight = TRUE,
    height = 500,
    defaultColDef = colDef(
      header = function(value) gsub(".", " ", value, fixed = TRUE),
      align = "center",
      headerStyle = list(background = "#f7f7f8")
    )
  )
}

reactable_filter_summary_alt <- function() {
  reactable(
    data = data.frame(Filter = NA_character_, Filter.Value = NA_character_),
    filterable = TRUE,
    minRows = 10,
    bordered = TRUE,
    highlight = TRUE,
    height = 500,
    defaultColDef = colDef(
      header = function(value) gsub(".", " ", value, fixed = TRUE),
      align = "center",
      headerStyle = list(background = "#f7f7f8")
    )
  )
}
