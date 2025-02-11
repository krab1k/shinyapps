selectizeGroupsUI <-
  function(id,
           params,
           label = NULL,
           btn_label = "Reset filters",
           inline = TRUE) {
    # Namespace
    ns <- NS(id)

    if (inline) {
      selectizeGroupTag <- tagList(
        tags$b(label),
        tags$div(
          class = "btn-group-justified selectize-group",
          role = "group",
          `data-toggle` = "buttons",
          lapply(
            X = seq_along(params),
            FUN = function(x) {
              input <- params[[x]]
              tagSelect <- tags$div(
                class = "btn-group",
                id = ns(paste0("container-", input$inputId)),
                selectizeInput(
                  inputId = ns(input$inputId),
                  label = input$title,
                  choices = input$choices,
                  selected = input$selected,
                  multiple = ifelse(is.null(input$multiple), TRUE, input$multiple),
                  width = "100%",
                  options = list(
                    placeholder = input$placeholder,
                    plugins = list("remove_button"),
                    onInitialize = I('function() { this.setValue(""); }')
                  )
                )
              )
              return(tagSelect)
            }
          )
        ),
        actionLink(
          inputId = ns("reset_all"),
          label = btn_label,
          icon = icon("xmark"),
          style = "float: right;"
        )
      )
    } else {
      selectizeGroupTag <- tagList(
        tags$b(label),
        lapply(
          X = seq_along(params),
          FUN = function(x) {
            input <- params[[x]]
            tagSelect <- selectizeInput(
              inputId = ns(input$inputId),
              label = input$title,
              choices = input$choices,
              selected = input$selected,
              multiple = ifelse(is.null(input$multiple), TRUE, input$multiple),
              width = "100%",
              options = list(
                placeholder = input$placeholder,
                plugins = list("remove_button"),
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
            return(tagSelect)
          }
        ),
        actionLink(
          inputId = ns("reset_all"),
          label = btn_label,
          icon = icon("xmark"),
          style = "float: right;"
        )
      )
    }

    tagList(singleton(tagList(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "shinyWidgets/modules/styles-modules.css")
    )),
    selectizeGroupTag)

  }