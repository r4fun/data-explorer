shiny_container <- function(header, ...) {
  # css
  aside_style <- "
  display: block;
  position: relative;
  margin: 40px 0;"

  h3_style <- "
  font: bold 12px Sans-Serif;
  letter-spacing: 2px;
  text-align: center;
  text-transform: uppercase;
  background: #369;
  color: #fff;
  padding: 5px 10px;
  margin: 0 0 10px 0;
  line-height: 24px;
  "

  shiny::tags$aside(
    style = aside_style,
    shiny::tags$h3(
      style = h3_style,
      header
    ),
    ...
  )
}
