function(input, output, session) {
  # ----------------------------------------------------------------------------
  #' Pretty disconnection screen
  # ----------------------------------------------------------------------------
  sever(html = disconnected, bg_color = "white", color = "black")

  # ----------------------------------------------------------------------------
  #' Waiting screens
  # ----------------------------------------------------------------------------
  w <- Waiter$new(html = span("Initialising"))

  # ----------------------------------------------------------------------------
  #' Reactive values
  # ----------------------------------------------------------------------------
  rv <- reactiveValues(
    dat = data.frame(),
    dat_filtered = data.frame(),
    dat_title = character(),
    filters_on_close = list(),
    dat_rerun = data.frame()
  )

  # ----------------------------------------------------------------------------
  #' When actionButton is clicked, open modal for data selection
  # ----------------------------------------------------------------------------
  observeEvent(input$pick_data_button, {
    showModal(modalDialog(
      title = "Please pick one of the available datasets",
      radioGroupButtons(
        inputId = "data_button",
        label = NULL,
        choices = config$data_choices,
        selected = "",
        individual = TRUE
      )
    ))
  })

  # ----------------------------------------------------------------------------
  #' When actionButton is clicked, hide langing page, show main page
  # ----------------------------------------------------------------------------
  observeEvent(input$data_button, {
    w$show()
    dat_path <- paste0("data/", input$data_button)
    rv$dat <- read_dat(dat_path)
    rv$dat_filtered <- rv$dat
    rv$dat_title <- tools::file_path_sans_ext(input$data_button)
  })

  # ----------------------------------------------------------------------------
  #' When actionButton is clicked, hide langing page, show main page
  # ----------------------------------------------------------------------------
  observeEvent(input$rerun_button, {
    showModal(modalDialog(
      title = "Rerun a previous report",
      tags$p(
        "You have two options:",
        tags$ul(
          tags$li(tags$strong("Rerun and download"), ": This will identify the filter criteria and download a new report"),
          tags$li(tags$strong("Rerun and modify"), ": This will identify and set the filter criteria for further modifications")
        )
      ),
      actionButton(
        inputId = "rerun_download_button",
        label = "Rerun and download",
        icon = icon("download"),
        width = "100%"
      ),
      actionButton(
        inputId = "rerun_modify_button",
        label = "Rerun and modify",
        icon = icon("edit"),
        width = "100%"
      )
    ))
  })

  # ----------------------------------------------------------------------------
  #' When rerun_download_button is clicked, show modal to upload old report
  # ----------------------------------------------------------------------------
  observeEvent(input$rerun_download_button, {
    showModal(modalDialog(
      title = "Rerun and download",
      fileInput(
        inputId = "file_upload",
        label = "Please upload a previously downloaded report",
        accept = ".xlsx",
        width = "100%"
      ),
      shinyjs::disabled(downloadButton(
        outputId = "rerun_download_to_disk_button",
        label = "Download",
        width = "100%"
      ))
    ))
  })

  observeEvent(input$file_upload, {
    # browser()
    file <- input$file_upload
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))

    sheets <- getSheetNames(file$datapath)
    user_upload <- lapply(sheets, function(x) {
      read.xlsx(file$datapath, sheet = x)
    })
    names(user_upload) <- sheets

    dev_summary <- user_upload$`DO NOT EDIT`
    dataset <- dev_summary[dev_summary$Filter == "r4fun_dataset", ]$Filter.Value
    dat_title <- tools::file_path_sans_ext(dataset)
    report_filters <- dev_summary[dev_summary$Filter != "r4fun_dataset", ]
    report_filters_split <- split(report_filters, report_filters$Filter)

    server_upload <- readRDS(paste0("data/", dataset))
    for (i in seq_along(report_filters_split)) {
      x <- report_filters_split[[i]]
      col <- unique(x$Filter)
      vals <- unique(x$Filter.Value)
      server_upload <- server_upload[server_upload[[col]] %in% vals, ]
    }

    shinyjs::enable("rerun_download_to_disk_button")

    rv$dat_rerun <- list(
      data = prepare_download(
        raw_data = server_upload,
        user_summary = user_upload$`Filter Summary`,
        data_button = dataset
      ),
      filename = paste0(dat_title, "-RERUN-", Sys.Date(), ".xlsx", sep="")
    )
  })

  # ----------------------------------------------------------------------------
  #' When data is loaded, hide landing page, show main page, remove modal
  # ----------------------------------------------------------------------------
  observeEvent(rv$dat, {
    shinyjs::hide("ui_landing")
    shinyjs::show("ui_main")
    removeModal()
    w$hide()
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------------------------
  #' When the change dataset button is click, open a modal
  # ----------------------------------------------------------------------------
  observeEvent(input$change_data_button, {
    showModal(modalDialog(
      title = "Please pick one of the available datasets",
      radioGroupButtons(
        inputId = "data_button",
        label = NULL,
        choices = config$data_choices,
        selected = "",
        individual = TRUE
      )
    ))
  })

  # ----------------------------------------------------------------------------
  #' When the apply filters button is clicked, open a modal
  # ----------------------------------------------------------------------------
  observeEvent(input$apply_filters_button, {
    showModal(modalDialog(
      title = "Filter controls",
      selectInput(
        inputId = "filter_select",
        label = "Available filters",
        choices = names(rv$dat),
        multiple = TRUE,
        width = "100%",
        selected = {
          if (is.null(input$filter_select))
            NULL
          else
            input$filter_select
        }
      ),
      actionButton(
        inputId = "render_filters_button",
        label = "Render filters",
        icon = icon("rocket"),
        width = "100%"
      ),
      br(), br(),
      shinycssloaders::withSpinner(uiOutput("filter_render"), 8),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_filter_modal", "Confirm")
      )
    ))
  })

  output$filter_render <- renderUI({
    lapply(names(rv$dat), function(x) {
      shinyjs::hidden(
        div(
          id = paste0("dd_", x),
          selectInput(
            inputId = x,
            label = gsub("[.]", " ", x),
            choices = NULL,
            width = "100%",
            multiple = TRUE
          )
        )
      )
    })
  })

  observeEvent(input$render_filters_button, {
    for (i in names(rv$dat)) {
      if (i %in% input$filter_select) {
        if (is.null(input[[i]])) {
          updateSelectInput(
            session = session,
            inputId = i,
            choices = sort(unique(rv$dat[[i]]))
          )
          shinyjs::show(paste0("dd_", i))
        }
      } else {
        shinyjs::hide(paste0("dd_", i))
      }
    }
  })

  # ----------------------------------------------------------------------------
  #' When apply filters button is clicked, we check for previous selections and
  #' restore if possible
  # ----------------------------------------------------------------------------
  observeEvent(input$apply_filters_button, {
    if (length(rv$filters_on_close) > 0) {
      lapply(rv$filters_on_close, function(x) {
        col <- x$col
        selected_vals <- x$selected_vals
        all_vals <- x$all_vals
        updateSelectInput(session, col, choices = all_vals, selected = selected_vals)
        shinyjs::show(paste0("dd_", col))
      })
    }
  })

  # ----------------------------------------------------------------------------
  #' Reactive filter summary
  # ----------------------------------------------------------------------------
  dat_filter_summary <- reactive({
    out <- lapply(input$filter_select, function(x) {
      if (is.null(input[[x]])) {
        NULL
      } else {
        data.frame(
          Filter = x,
          Filter.Value = input[[x]]
        )
      }
    })

    dplyr::bind_rows(Filter(Negate(is.null), out))
  })

  # ----------------------------------------------------------------------------
  #' When the filter modal is confirmed "hits okay", we store the filters on
  #' close so that we can restore them in the future. We also update the
  #' reactable table
  # ----------------------------------------------------------------------------
  observeEvent(input$confirm_filter_modal, {
    removeModal()

    # preserve selections
    rv$filters_on_close <- lapply(input$filter_select, function(x) {
      if (!is.null(x))
        list(
          "col" = x,
          "selected_vals" = input[[x]],
          "all_vals" = sort(unique(rv$dat[[x]]))
        )
    })

    # filter the data for the reactable table
    df_summary <- dat_filter_summary()
    df_summary_split <- split(df_summary, df_summary$Filter)

    df <- rv$dat
    for (i in seq_along(df_summary_split)) {
      x <- df_summary_split[[i]]
      i_col <- unique(x$Filter)
      i_val <- unique(x$Filter.Value)
      df <- df[df[[i_col]] %in% i_val, ]
    }

    rv$dat_filtered <- df
  })

  # ----------------------------------------------------------------------------
  #' Data table output
  # ----------------------------------------------------------------------------
  output$data_table <- renderReactable({
    reactable_data(rv$dat_filtered)
  })

  # ----------------------------------------------------------------------------
  #' Filter summary table
  # ----------------------------------------------------------------------------
  output$filter_summary_table <- renderReactable({
    if (nrow(dat_filter_summary()) > 0) {
      reactable_filter_summary(dat_filter_summary())
    } else {
      reactable_filter_summary_alt()
    }
  })

  # ----------------------------------------------------------------------------
  #' Download handler(s)
  # ----------------------------------------------------------------------------
  output$download_button <- downloadHandler(
    filename = function() {
      paste0(rv$dat_title, "-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      shiny::withProgress(
        message = "Downloading report!",
        detail = "Please bear with us while we gather the data 🙏",
        value = 0, {
          out <- prepare_download(
            raw_data = rv$dat_filtered,
            user_summary = dat_filter_summary(),
            data_button = input$data_button
          )

          saveWorkbook(out, file)
        }
      )
    }
  )

  output$rerun_download_to_disk_button <- downloadHandler(
    filename = function() {
      rv$dat_rerun$filename
    },
    content = function(file) {
      shiny::withProgress(
        message = "Downloading report!",
        detail = "Please bear with us while we gather the data 🙏",
        value = 0, {
          saveWorkbook(
            rv$dat_rerun$data,
            file
          )
        }
      )
    }
  )
}
