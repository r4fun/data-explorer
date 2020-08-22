tagList(
  shinyjs::useShinyjs(),
  waiter::use_waiter(),
  sever::use_sever(),
  includeCSS("www/custom.css"),
  div(id = "ui_landing", ui_landing()),
  shinyjs::hidden(div(id = "ui_main", ui_main()))
)
