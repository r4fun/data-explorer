ui_main <- function() {
  navbarPage(
    title = "Main Page",
    theme = shinytheme("lumen"),
    tabPanel(
      "Data explorer",
      column(
        width = 3,
        actionButton("change_data_button", "Change dataset", icon("exchange-alt"), width = "100%")
      ),
      column(
        width = 3,
        actionButton("apply_filters_button", "Apply filters", icon("filter"), width = "100%")
      ),
      column(
        width = 3,
        actionButton("rerun_button", "Rerun a report", icon("sync-alt"), width = "100%")
      ),
      column(
        width = 3,
        downloadButton("download_button", "Download", style = "width:100%;")
      ),
      column(
        width = 8,
        shiny_container(
          "Data",
          shinycssloaders::withSpinner(reactableOutput("data_table"), type = 8)
        )
      ),
      column(
        width = 4,
        shiny_container(
          "Filter Summary",
          reactableOutput("filter_summary_table")
        )
      )
    ),
    tabPanel(
      "About this app"
    ),
    footer = column(
      width = 12,
      align = "center",
      tags$p("Made with 💕 by the", tags$a("r4fun", href = "https://github.com/r4fun"), "group")
    )
  )
}
