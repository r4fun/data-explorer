ui_landing <- function() {
  fluidPage(
    h1("Landing Page"),
    actionButton("pick_data_button", "Pick a dataset")
  )
}
