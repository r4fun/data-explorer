prepare_download <- function(raw_data, user_summary, data_button) {
  dev_summary <- user_summary %>%
    mutate_all(as.character) %>%
    bind_rows(tibble(
      Filter = "r4fun_dataset",
      Filter.Value = data_button
    ))

  wb <- createWorkbook()
  addWorksheet(wb, "Raw Data")
  addWorksheet(wb, "Filter Summary")
  addWorksheet(wb, "DO NOT EDIT", visible = FALSE)

  writeData(wb, "Raw Data", raw_data)
  writeData(wb, "Filter Summary", user_summary)
  writeData(wb, "DO NOT EDIT", dev_summary)

  protectWorksheet(wb, "DO NOT EDIT")
  wb
}
