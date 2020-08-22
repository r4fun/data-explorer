shiny_card <- function(header, description, value) {
  # css
  style_card <- "display: flex;flex-direction: column;border: 1px solid #EF9A9A;border-radius: 4px;overflow: hidden;margin: 5px;"
  style_header <- "color: #D32F2F;text-align: center;font-size: 22px;font-weight: 600;border-bottom: 1px solid #EF9A9A;background-color: #FFEBEE;padding: 5px 10px;"
  style_main <- "font-size: 20px;display: flex;flex-direction: column;justify-content: center;align-items: center;padding: 15px 0;"
  style_main_desc <- "color: #D32F2F;font-size: 12px;text-align: center;"

  # html
  div(
    style = style_card,
    div(
      style = style_header,
      header
    ),
    div(
      style = style_main,
      value
      # icon
    ),
    div(
      style = style_main_desc,
      description
    )
  )
}
