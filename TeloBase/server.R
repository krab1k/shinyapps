library(shiny)
library(sodium)
library(data.table)
library(DT)
library(rlang)
library(stringr)
library(dplyr)
library(shinyWidgets)
library(plotly)
library(shinyBS)
library(taxize)
library(shinybusy)
library(emayili)
library(gsubfn)
library(stringi)
library(metacoder)
library(RCurl)
library(writexl)

#load general.r with functions
source("./general.R", local = TRUE)

#Function to run every day to update file in the tempdir -> should protect the tempdir from deletion?
updater = function(interval = 86400) {
  write(as.character(Sys.time()), paste0(tempdir(), "\\tempdir_saver.txt"))
  later::later(updater, interval)
}

updater()

shinyServer(function(input, output, session) {
  #initial authorisation of the database visitor
  values = reactiveValues(authorisation = "visitor")

  #initial load accs
  values$accs <-
    reactiveVal({
      read.csv2("./data/accs.csv", stringsAsFactors = FALSE)
    })
  values$accs_final <-
    reactiveVal({
      read.csv2("./data/accs.csv", stringsAsFactors = FALSE)
    })

  #initial load of accs time
  values$old_time_change_accs <-
    reactiveVal({
      as.character(file.mtime("./data/accs.csv"))
    })
  values$new_time_change_accs <-
    reactiveVal({
      as.character(file.mtime("./data/accs.csv"))
    })

  #initial load of telomere_sequences time
  values$old_time_change_telomere_sequences <-
    reactiveVal({
      as.character(file.mtime("./data/telomere_sequences.csv"))
    })
  values$new_time_change_telomere_sequences <-
    reactiveVal({
      as.character(file.mtime("./data/telomere_sequences.csv"))
    })

  #initial load of applied_sequences time
  values$old_time_change_applied_sequences <-
    reactiveVal({
      as.character(file.mtime("./data/applied_sequences.csv"))
    })
  values$new_time_change_applied_sequences <-
    reactiveVal({
      as.character(file.mtime("./data/applied_sequences.csv"))
    })

  #initial load of applied_reports time
  values$old_time_change_applied_reports <-
    reactiveVal({
      as.character(file.mtime("./data/reports.csv"))
    })
  values$new_time_change_applied_reports <-
    reactiveVal({
      as.character(file.mtime("./data/reports.csv"))
    })

  #load of telomere sequences for Telomere sequences tab
  values$telomere_sequences <-
    reactiveVal({
      read.csv2("./data/telomere_sequences.csv", stringsAsFactors = FALSE)
    })
  values$datatable_final <-
    reactiveVal({
      read.csv2("./data/telomere_sequences.csv", stringsAsFactors = FALSE)
    })

  #load of telomere sequences for Telomere sequences management tab
  values$telomere_sequences_management <-
    reactiveVal({
      data.frame(
        to.remove = "",
        read.csv2("./data/telomere_sequences.csv", stringsAsFactors = FALSE)
      )
    })
  values$datatable_management_final <-
    reactiveVal({
      data.frame(
        to.remove = "",
        read.csv2("./data/telomere_sequences.csv", stringsAsFactors = FALSE)
      )
    })

  #load of telomere sequences for Submitted sequences tab
  values$applied_sequences <-
    reactiveVal({
      read.csv2("./data/applied_sequences.csv", stringsAsFactors = FALSE)
    })
  values$datatable_applied_management_final <-
    reactiveVal({
      read.csv2("./data/applied_sequences.csv", stringsAsFactors = FALSE)
    })

  #values remembering how many sequences user needs to evaluate
  values$number_to_evaluate<-
    reactiveVal({
      NA
    })

  #load of reports for Reports tab
  values$applied_reports <-
    reactiveVal({
      read.csv2("./data/reports.csv", stringsAsFactors = FALSE)
    })
  values$datatable_applied_reports_final <-
    reactiveVal({
      read.csv2("./data/reports.csv", stringsAsFactors = FALSE)
    })

  #values remembering how many sequences user needs to evaluate
  values$number_to_evaluate_reports<-
    reactiveVal({
      NA
    })

  #values remembering metacoder plot
  values$metacoder_plot<-
    reactiveVal({
      NA
    })

  #values remembering lowest_selected_rank
  values$lowest_selected_rank<-
    reactiveVal({
      NA
    })

  #values remembering lowest_rank_name
  values$lowest_rank_name<-
    reactiveVal({
      NA
    })

  #values remembering unique_df_species
  values$unique_df_species<-
    reactiveVal({
      NA
    })

  #### START - PERIODIC UPDATE ACCS IF ON ADMIN TOOLS TAB ####

  observe({
    if (is.null(input$menu_options)) {
      #at start of the database there is no input menu_option
    } else {
      if (input$menu_options == "admin_tools") {
        #periodical check of the time of the last change of accs.csv (every 1s)
        invalidateLater(1000, session)
        values$new_time_change_accs(file.mtime("./data/accs.csv"))
      }
    }

  })

  #update of the accs in case that older version was uploaded
  observe({
    if (values$old_time_change_accs() == values$new_time_change_accs()) {
      #compare old time change accs with a new one updated every 1s
    }  else {
      values$accs(read.csv2("./data/accs.csv", stringsAsFactors = FALSE)) #load accs again when times are not matching
      values$old_time_change_accs(as.character(file.mtime("./data/accs.csv"))) #save new time accs change as old time accs change
    }

  })

  ## END - PERIODIC UPDATE ACCS IF ON TAB ##



  #### START - PERIODIC UPDATE OF TELOMERE SEQUENCES FOR MANAGEMENT IF ON ADMIN TOOLS TAB ####

  observe({
    if (is.null(input$menu_options)) {
      #at start of the database there is no input menu_option
    } else {
      if (input$menu_options == "admin_tools") {
        #periodical check of the time of the last change of telomere_sequences.csv (every 1s)
        invalidateLater(1000, session)
        values$new_time_change_telomere_sequences(file.mtime("./data/telomere_sequences.csv"))
      }
    }

  })

  #update of the accs in case that older version was uploaded
  observe({
    if (values$old_time_change_telomere_sequences() == values$new_time_change_telomere_sequences()) {
      #compare old time change telomere_sequences with a new one updated every 1s
    }  else {
      values$telomere_sequences_management(data.frame(
        to.remove = "",
        read.csv2("./data/telomere_sequences.csv", stringsAsFactors = FALSE)
      )) #load telomere_sequences again when times are not matching
      values$old_time_change_telomere_sequences(as.character(file.mtime("./data/telomere_sequences.csv"))) #save new time telomere_sequences change as old time telomere_sequences change
    }

  })

  ## END - PERIODIC UPDATE  OF TELOMERE SEQUENCES FOR MANAGEMENT IF ON ADMIN TAB ##



  #### START - PERIODIC UPDATE OF SUBMITTED SEQUENCES FOR MANAGEMENT IF ON SUBMITTED SEQUENCES TAB ####

  observe({
    if (is.null(input$menu_options)) {
      #at start of the database there is no input menu_option
    } else {
      if (input$menu_options == "sequences_to_approve") {
        #periodical check of the time of the last change of applied_sequences.csv (every 1s)
        invalidateLater(1000, session)
        values$new_time_change_applied_sequences(file.mtime("./data/applied_sequences.csv"))
      }
    }

  })

  #update of the applied_sequences in case that older version was uploaded
  observe({
    if (values$old_time_change_applied_sequences() == values$new_time_change_applied_sequences()) {
      #compare old time change applied_sequences with a new one updated every 1s
    }  else {
      values$applied_sequences(read.csv2("./data/applied_sequences.csv", stringsAsFactors = FALSE)) #load applied_sequences again when times are not matching
      values$old_time_change_applied_sequences(as.character(file.mtime("./data/applied_sequences.csv"))) #save new time applied_sequences change as old time applied_sequences change
    }

  })

  ## END - PERIODIC UPDATE  OF SUBMITTED SEQUENCES FOR MANAGEMENT IF ON SUBMITTED SEQUENCES TAB ##



  #### START - PERIODIC UPDATE OF REPORTS FOR MANAGEMENT IF ON REPORTS TAB ####

  observe({
    if (is.null(input$menu_options)) {
      #at start of the database there is no input menu_option
    } else {
      if (input$menu_options == "reports_to_check") {
        #periodical check of the time of the last change of reports.csv (every 1s)
        invalidateLater(1000, session)
        values$new_time_change_applied_reports(file.mtime("./data/reports.csv"))
      }
    }

  })

  #update of the reports in case that older version was uploaded
  observe({
    if (values$old_time_change_applied_reports() == values$new_time_change_applied_reports()) {
      #compare old time change reports with a new one updated every 1s
    }  else {
      values$applied_reports(read.csv2("./data/reports.csv", stringsAsFactors = FALSE)) #load reports again when times are not matching
      values$old_time_change_applied_reports(as.character(file.mtime("./data/reports.csv"))) #save new time reports change as old time reports change
    }

  })

  ## END - PERIODIC UPDATE  OF REPORTS FOR MANAGEMENT IF ON REPORTS TAB ##



  #### START - SIDEBAR MENU SCRIPTS ####

  #Output with basic visitor sidebar menu items with inclusion of menu_list that will implement logged user menu items or admin items
  output$sidebar_menu <- renderMenu({
    menu_list <- list(
      menuItem("About", tabName = "about", icon = icon("info")),
      menuItem("References", tabName = "references", icon = icon("book")),
      menuItem(
        "Telomere sequences",
        tabName = "telomere_sequences",
        icon = icon("table")
      ),
      menuItem(
        "Submit sequence",
        tabName = "apply_sequence",
        icon = icon("square-plus")
      ),

      menuItem(
        "Report issue(s)",
        tabName = "report_sequence",
        icon = icon("notes-medical")
      ),

      if (values$authorisation == "user" |
          values$authorisation == "admin") {
        menuItemOutput("menu_item_applied_sequences")
      },

      if (values$authorisation == "admin") {
        menuItemOutput("menu_item_applied_reports")
      },

      if (values$authorisation == "admin") {
        menuItem(
          "Admin tools",
          icon = icon("screwdriver-wrench"),
          tabName = "admin_tools",
          badgeLabel = "Admin",
          badgeColor = "red"
        )
      },

      if (values$authorisation == "user" |
          values$authorisation == "admin") {
        menuItem(
          input$user_input,
          icon = icon("user"),
          tabName = "user_management"
        )
      }

    )

    sidebarMenu(id = "menu_options", .list = menu_list)
  })

  output$menu_item_applied_sequences<- renderMenu({
    menuItem(
      "Submitted sequences",
      icon = icon("book-open-reader"),
      tabName = "sequences_to_approve",
      badgeLabel = values$number_to_evaluate(),
      badgeColor = "yellow"
    )
  })

  output$menu_item_applied_reports<- renderMenu({
    menuItem(
      "Reports",
      icon = icon("clipboard-list"),
      tabName = "reports_to_check",
      badgeLabel = values$number_to_evaluate_reports(),
      badgeColor = "red"
    )
  })

  ## END - SIDEBAR MENU SCRIPTS ##



  #### START - USER AUTHENTICATION SCRIPTS ####

  observeEvent(eventExpr = input$login_logout,
               #Login pop-up in case user has authorisation of visitor
               if (values$authorisation == "visitor") {
                 handlerExpr = {
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = "Login",
                       size = c("s"),
                       useShinyFeedback(),
                       textInput(inputId = "user_input", label = "User", value = ""),
                       passwordInput(inputId = "password_input", label = "Password"),
                       actionLink(inputId = "forgotten_password_button", label = "Have you forgotten the password?"),
                       footer = tagList(actionButton("login", "Log in"),
                                        modalButton('Close'))
                     )
                   )
                 }
                 #Log out function in case user is logged in.
               } else {
                 handlerExpr = {
                   values$authorisation <- "visitor"
                   updateActionLink(session, inputId = "login_logout", label = "Log in")
                 }
               })

  #Login button to check if user and password is correct -> if so, log in user.
  observeEvent(eventExpr = input$login,
               handlerExpr = {
                 hideFeedback("user_input")
                 hideFeedback("password_input")

                 accs <- values$accs()

                 if (any(as.vector(accs$users) == input$user_input) == FALSE) {
                   showFeedbackDanger(inputId = "user_input", text = "")
                   showFeedbackDanger(inputId = "password_input", text = "Wrong username or password.")
                 } else {
                   if (sodium::password_verify(as.character(accs$password[accs$users == input$user_input]), input$password_input) == FALSE) {
                     showFeedbackDanger(inputId = "user_input", text = "")
                     showFeedbackDanger(inputId = "password_input", text = "Wrong username or password.")
                   } else {
                     removeModal()
                     values$authorisation <-
                       as.character(accs$rights[accs$users == input$user_input])
                     updateActionLink(session, inputId = "login_logout", label = "Log out")
                   }
                 }

               })

  #Forgotten password ActionLink to send another password on the email
  observeEvent(eventExpr = input$forgotten_password_button,
               handlerExpr = {
                 showModal(
                   modalDialog(
                     easyClose = TRUE,
                     title = "Forgotten password",
                     size = c("s"),
                     textInput(
                       inputId = "contact_forgotten_password",
                       label = "Contact",
                       value = ""
                     ),
                     footer = tagList(actionButton("send_password", "Send"),
                                      modalButton('Close'))
                   )
                 )
               })

  #Send contact after clicking Send button in the modal dialog of the Forgotten password Modal
  observeEvent(eventExpr = input$send_password,
               handlerExpr = {
                 user_contact<- input$contact_forgotten_password
                 accs<- values$accs()
                 if(user_contact %in% accs$contact) {
                   new_password<- paste0(random(1),random(1),random(1),random(1))

                   email <- envelope(
                     to = user_contact,
                     from = Sys.getenv("SMTP_USER"),
                     subject = "New authentication details to TeloBase",
                     text = paste0("New password: ", new_password)
                   )

                   smtp <- server(host = Sys.getenv("SMTP_HOST"),
                                  port = as.integer(Sys.getenv("SMTP_PORT")),
                                  username = Sys.getenv("SMTP_USER"),
                                  password = Sys.getenv("SMTP_PASS"))
                   smtp(email, verbose = TRUE)

                   new_password_hash<- password_store(new_password)
                   accs$password[accs$contact == user_contact]<- new_password_hash
                   write.csv2(accs, file = "./data/accs.csv", row.names = FALSE)
                   removeModal()

                 } else {

                   removeModal()

                 }
               })

  ## END - USER AUTHENTICATION SCRIPTS ##


  #### START - ABOUT TAB ####

  #number of current unique species in the database
  output$number_unique_species <- renderText({paste("There are currently <b>",length(unique(values$telomere_sequences()$species[!is.na(values$telomere_sequences()$species)])), "</b> unique species included in the TeloBase with sequences distributed as listed below:")})

  #plot of the 10 sequences + other
  output$about_tab_plot <- renderPlotly({
    sequences<- read.csv2("./data/telomere_sequences.csv", stringsAsFactors = FALSE)
    sequences <-
      data.frame(seq(1:nrow(sequences)), sequences) #add row numbers
    colnames(sequences)[1] <- "row.names"
    species_with_EXPERIMENTAL <-
      sequences$species[sequences$status == "EXPERIMENTAL"] #filter species of the sequences that have EXPERIMENTAL status
    sequences_with_EXPERIMENTAL <-
      sequences$sequence[sequences$status == "EXPERIMENTAL"] #filter sequences of the sequences that have EXPERIMENTAL status
    rows_for_removal <-
      sequences$row.names[sequences$status == "MODEL" &
                            sequences$species %in% species_with_EXPERIMENTAL & sequences$sequence %in% sequences_with_EXPERIMENTAL] #find rows with MODEL status that also have one EXPERIMENTAL status
    sequences_left <-
      sequences[!sequences$row.names %in% rows_for_removal,] #remove from sequences dataframe sequences based on rownames of the rows_for_removal

    sequences_left<- sequences_left[!duplicated(sequences_left[c("species", "sequence", "location", "FTANS", "FTELS")]),] #remove duplicated sequences based on species, sequence and location

    sequences_left <-
      sequences_left[,-1] #remove row.names column

    statistics <-
      as.data.frame(with(sequences_left, table(sequence)))

    statistics<- statistics[order(-statistics$Freq),]
    other_sequences<- data.frame(sequence = "other", Freq = sum(statistics$Freq[11:nrow(statistics)]))
    statistics<- rbind(statistics[1:10,], other_sequences)
    statistics$sequence<- factor(statistics$sequence, levels = rev(statistics$sequence))
    statistics$group<- as.character(statistics$sequence)
    statistics$group[statistics$group != "TTAGGG" & statistics$group != "TTTAGGG" & statistics$group != "TTAGG" & statistics$group != "other"]<- "individual"
    statistics$group<- factor(statistics$group, levels = c("TTAGGG", "TTTAGGG", "TTAGG", "individual", "other"))

    plot <-
      plot_ly(
        statistics,
        type = "bar",
        x = statistics$Freq,
        y = statistics$sequence,
        text = ~ Freq,
        textposition = "none",
        name = "",
        hoverinfo = "skip",
        color = ~ group,
        colors = c("#dd4b39", "#00a65a", "#f39c12", "#0073b6", "grey"),
        height = 90+(length(unique(statistics$sequence))-1)*25
      )

    plot <-
      plot %>% layout(
        xaxis = list(title = "Unique species", fixedrange = TRUE),
        yaxis = list(title = "", fixedrange = TRUE),
        showlegend = FALSE,
        bargap = 5
      )

    plot <- plot %>% config(displayModeBar = FALSE)

    return(plot)
  })

  ## END - ABOUT TAB ##



  #### START - TELOMERE SEQUENCES TAB ####

  output$telomere_sequences_box <- renderUI({
    column(
      width = 12,
      tabBox(
        width = 9,
        id = "telomere_sequences_tab",
        tabPanel(
          "Telomere data table",
          dataTableOutput("telomere_sequences_datatable"),
          downloadButton(outputId = "download_filtered_telomere_sequences", label = "Download telomere sequences based on filter options"),
          downloadButton(outputId = "download_whole_telomere_sequences", label = "Download whole table")
        ),
        tabPanel(
          "Summary graphs based on filter options",
          fluidPage(
                   plotlyOutput("telomere_sequences_statistics", height = '100%'),
                   plotlyOutput("telomere_sequences_vs", height = '100%')
          )
        ),
        tabPanel(
          "Heat tree plot based on filter options",
          fluidPage(
            column(
              width = 6,
              uiOutput("metacoder_colour"),
              uiOutput("metacoder_sequence"),
              uiOutput("metacoder_label_size"),
              uiOutput("metacoder_node_size"),
              uiOutput("metacoder_edge_size"),
              tags$style(
                type = 'text/css',
                '.modal-dialog { width: 100%; }'
              ),
              actionButton("generate_metacoder_plot", "Generate heat tree plot")
            ),
            column(
              width = 6,
              box(
                width = NULL,
                title = "How to generate heat tree plot with highlighted sequence",
                background = "yellow",
                collapsible = TRUE,
                collapsed = TRUE,
                "1) Choose colour which will be used to highlight the sequence of your choosing.",
                br(),
                br(),
                "2) Select sequence from the selection box that you want to highlight.",
                br(),
                br(),
                "3) Click on 'Generate heat tree plot' button."
              ),
              box(
                width = NULL,
                title = "Conditions for building the heat tree plot",
                background = "red",
                collapsible = TRUE,
                collapsed = TRUE,
                "Heat tree plot communicates with the filter options on the right; however, there are some limitations:",
                br(),
                "- at least some taxonomy rank needs to be selected (e.g., some Kingdom at least)",
                br(),
                "- heat tree plot includes information about two taxonomy ranks below your lowest selected taxonomy rank (e.g., selecting Kingdom means that
                Phylum and Class under that Kingdom will be also plotted.)",
                br(),
                "- multiple choice selection of taxonomy ranks does work only for ranks that have common higher taxonomy rank (in such case information will be
                given only for one taxonomy rank below the selected option) (e.g., two selected options from Family with common Order will generate plot from Order
                to Genus; however, it will not include Species in it, altough options were taken from Family taxonomy rank.)"
              )
            )
          )
        )
      ),
      column(
        width = 3,
        box(
          width = NULL,
          title = "Multiple keyword search",
          background = "blue",
          "Multiple conditions can be applied with space separator and OR function is available for one word keywords with | between them. Search is case insensitive."
        ),
        box(
          width = NULL,
          title = "Raw data from TRFi pipeline",
          background = "red",
          collapsible = TRUE,
          collapsed = TRUE,
          "If you wish to search for additional sequences in tandem repeats gathered from SRA data that are currently not included in the dataset, you can download the file by clicking on this ",
          a("Link", href="https://www.dropbox.com/scl/fi/4v60g1las3mmobu1wu11w/raw_TRFi_files.xlsx?dl=0&rlkey=fcb2n2rnb0qkzhb3ykfypf513", target="_blank") ,
          ".",
          br(),
          br(),
          "Data are sorted to several Excel sheets based on the affiliation to Kingdom taxonomy rank. Selected telomere seqeuences used to find telomere repeats with POTENTIAL status in SRA data are highlighted
          (red, green, yellow colour for TT(T)AGG(G) and blue colour for other sequences; no 5% limit is applied there in contrast to sequences with POTENTIAL status in the TeloBase)."
        ),
        box(
          width = NULL,
          title = "Explanatory notes",
          background = "yellow",
          collapsible = TRUE,
          collapsed = TRUE,
          "FTANS - 100 % First TANdem repeat Share has the tandem repeat with the highest frequency in the dataset. The FTANS of less frequent tandems is expressed as a percentage relative to the most frequent tandem in the dataset.",
          br(),
          br(),
          "FTELS - 100 % First TELomere repeat Share has the first potential telomere sequence in the dataset. The FTELS of less frequent potential telomeres is expressed as a percentage relative to the most frequent potential telomere in the dataset.",
          br(),
          br(),
          "Filter to one - filters out the same sequences but the oldest reference (no preference in the same year) with the EXPERIMENTAL status if present (does not affect POTENTIAL status entries)"
        ),
        box(
          width = NULL,
          title = "Filter options",
          status = "warning",
          tags$head(
            #changing boxes to uniform style of the database
            tags$style(".selectize-input {border-radius:0px}"),
            tags$style(".selectize-input.dropdown-active {border-radius:0px}"),
            tags$style(".selectize-dropdown {border-radius:0px}"),
            tags$style(
              HTML(
                '
                .selectize-input.focus {
                border-color:#3c8dbc;
                outline: 0;
                -webkit-box-shadow:none !important;
                box-shadow:none !important;
                }
                '
              )
              )
              ),

          selectizeGroupsUI(
            #code is in general.R (modification of selectizeGroupUI so that the "x Restart filters" does not disappear)
            id = "filters",
            params = list(
              kingdom = list(inputId = "kingdom", title = "Kingdom"),
              phylum = list(inputId = "phylum", title = "Phylum"),
              class = list(inputId = "class", title = "Class"),
              order = list(inputId = "order", title = "Order"),
              family = list(inputId = "family", title = "Family"),
              genus = list(inputId = "genus", title = "Genus"),
              species = list(inputId = "species", title = "Species"),
              sequence = list(inputId = "sequence", title = "Sequence"),
              location = list(inputId = "location", title = "Location")
            ),
            inline = FALSE
          ),

          prettyRadioButtons("select_which_status", "Status selection",
                             choices = list("All" = 1, "Literature (EXPERIMENTAL+MODEL)" = 2,
                                            "POTENTIAL" = 3),
                             selected = 1,
                             status = "warning"),


          prettyCheckbox(
            inputId = "option_only_EXPERIMENTAL",
            label = "Filter to one",
            value = FALSE,
            status = "warning"
          ),

          chooseSliderSkin("Flat"),
          sliderInput("FTANS_slider", "FTANS", value = c(0, 100), min = 0, max = 100),
          sliderInput("FTELS_slider", "FTELS", value = c(0, 100), min = 0, max = 100)

              )
            )
        )
  })

  #filter values$telomere_sequences() based on pretty checkbox to remove MODEL if EXPERIMENTAL is there for the name or POTENTIAL if MODEL or EXPERIMENTAL is there for the name
  observe({
    if (!is.null(input$option_only_EXPERIMENTAL)) {
      if (input$option_only_EXPERIMENTAL == TRUE) {
        sequences <- res_mod() #save res_mod() as sequences
        sequences <-
          data.frame(seq(1:nrow(sequences)), sequences) #add row numbers
        colnames(sequences)[1] <- "row.names"

        if (input$select_which_status == 1 | input$select_which_status == 2) {
        species_with_EXPERIMENTAL <-
          sequences$species[sequences$status == "EXPERIMENTAL"] #filter species of the sequences that have EXPERIMENTAL status
        sequences_with_EXPERIMENTAL <-
          sequences$sequence[sequences$status == "EXPERIMENTAL"] #filter sequences of the sequences that have EXPERIMENTAL status
        rows_for_removal <-
          sequences$row.names[sequences$status == "MODEL" &
                                sequences$species %in% species_with_EXPERIMENTAL & sequences$sequence %in% sequences_with_EXPERIMENTAL] #find rows with MODEL status that also have one EXPERIMENTAL status
        sequences_left <-
          sequences[!sequences$row.names %in% rows_for_removal,] #remove from sequences dataframe sequences based on rownames of the rows_for_removal

        sequences_left<- sequences_left[!duplicated(sequences_left[c("species", "sequence", "location", "FTANS", "FTELS")]),] #remove duplicated sequences based on species, sequence and location

        sequences<- sequences_left
        }
        sequences <- sequences[,-1] #remove row.names column

        if (input$select_which_status == 1) {
        }
        if (input$select_which_status == 2) {
          sequences<- sequences[sequences$status == "EXPERIMENTAL" | sequences$status == "MODEL",]
        }
        if (input$select_which_status == 3) {
          sequences<- sequences[sequences$status == "POTENTIAL",]
        }
        if (input$select_which_status == 1 | input$select_which_status == 3) {
          sequences<- sequences[(is.na(sequences$FTANS) | sequences$FTANS >= input$FTANS_slider[1]/100 & sequences$FTANS <= input$FTANS_slider[2]/100) &
                                  (is.na(sequences$FTELS) | sequences$FTELS >= input$FTELS_slider[1]/100 & sequences$FTELS <= input$FTELS_slider[2]/100),]
        }

        values$datatable_final(sequences) #update values$datatable_final
        replaceData(
          proxy_sequences,
          values$datatable_final(),
          resetPaging = FALSE,
          rownames = FALSE
        ) #update the proxy of the table

      } else {
        sequences <- res_mod() #save res_mod() as sequences
        if (input$select_which_status == 1) {
        }
        if (input$select_which_status == 2) {
          sequences<- sequences[sequences$status == "EXPERIMENTAL" | sequences$status == "MODEL",]
        }
        if (input$select_which_status == 3) {
          sequences<- sequences[sequences$status == "POTENTIAL",]
        }
        if (input$select_which_status == 1 | input$select_which_status == 3) {
          sequences<- sequences[(is.na(sequences$FTANS) | sequences$FTANS >= input$FTANS_slider[1]/100 & sequences$FTANS <= input$FTANS_slider[2]/100) &
                                (is.na(sequences$FTELS) | sequences$FTELS >= input$FTELS_slider[1]/100 & sequences$FTELS <= input$FTELS_slider[2]/100),]
        }
        values$datatable_final(sequences) #update values$datatable_final
        replaceData(
          proxy_sequences,
          values$datatable_final(),
          resetPaging = FALSE,
          rownames = FALSE
        ) #update the proxy of the table
      }
    }
  })

  #call module for selectizeGroupsUI
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "filters",
    data = values$telomere_sequences(),
    vars = c(
      "kingdom",
      "phylum",
      "class",
      "order",
      "family",
      "genus",
      "species",
      "sequence",
      "location"
    )
  )

  #observe the replacement of the data using filters
  observe({
    replaceData(proxy_sequences,
                res_mod(),
                resetPaging = FALSE,
                rownames = FALSE)
  })

  #telomere sequences datatable
  output$telomere_sequences_datatable <- renderDataTable({
    datatable(
      isolate(values$datatable_final()),
      #render filtered telomere sequences df for the website
      class = "compact",
      extensions = c("RowGroup", "Responsive"),
      escape = FALSE,
      selection = "none",
      style = "bootstrap4",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 50, 100),
        rowGroup = list(dataSrc = c(13,7)),
        search = list(regex = TRUE),
        ordering = FALSE
      )
    ) %>%

      #vertical align
      formatStyle(columns = colnames(isolate(values$datatable_final())),
                  `vertical-align` = 'middle', height = 30) %>%
      #color if EXPERIMENTAL or MODEL sequence
      formatStyle(
        columns = "status",
        backgroundColor = styleEqual(c("POTENTIAL","MODEL", "EXPERIMENTAL"), c("#FCF2BE","#ffcccb", "#b7e2fc")),
        `text-align` = 'center'
      ) %>%
      #width of Reference
      formatStyle(columns = "reference",
                  width = 200) %>%
      #FTANS (show only two digits after . + make the colour showing %)
      formatStyle(columns = "FTANS",
                 background = styleColorBar(range(0, 1), "#b7e2fc"),
                 backgroundSize = '100% 50%',
                 backgroundRepeat = 'no-repeat',
                 backgroundPosition = 'right') %>%
      formatPercentage('FTANS', 2) %>%
      #FTELS (show only two digits after . + make the colour showing %)
      formatStyle(columns = "FTELS",
                  background = styleColorBar(range(0, 1), "#ffcccb"),
                  backgroundSize = '100% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatPercentage('FTELS', 2)
  })

  #save datatable of telomere sequences as proxy for updating when filter changes
  proxy_sequences <-
    DT::dataTableProxy("telomere_sequences_datatable")

  #plot of the selected sequences
  output$telomere_sequences_statistics <- renderPlotly({
    statistics <-
      as.data.frame(with(values$datatable_final(), table(sequence, status))) #saving filtered dataframe

    #Select the correct color palette given the filtered dataframe
    if ("EXPERIMENTAL" %in% statistics$status) {
      if ("MODEL" %in% statistics$status) {
        if ("POTENTIAL" %in% statistics$status) {
          colors <- c("#b7e2fc", "#ffcccb", "#FCF2BE")
        } else {
          colors <- c("#b7e2fc", "#ffcccb")
        }
      } else
        if ("POTENTIAL" %in% statistics$status) {
          colors <- c("#b7e2fc", "#FCF2BE")
        } else {
          colors <- "#b7e2fc"
        }

    } else {
      if ("MODEL" %in% statistics$status) {
        if ("POTENTIAL" %in% statistics$status) {
          colors <- c("#ffcccb", "#FCF2BE")
        } else {
          colors <- "#ffcccb"
        }
      } else {
        colors <- "#FCF2BE"
      }
    }

    plot <-
      plot_ly(
        statistics,
        type = "bar",
        x = ~ Freq,
        y = ~ reorder(sequence, Freq),
        text = ~ Freq,
        textposition = "none",
        name = "",
        hoverinfo = "text",
        hovertext = paste(
          "Sequence:",
          statistics$sequence,
          "<br> Frequency:",
          statistics$Freq,
          "<br> Status:",
          statistics$status
        ),
        color = ~ status,
        colors = colors,
        height = 90+(length(unique(statistics$sequence))-1)*25
      )

    plot <-
      plot %>% layout(
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(title = "", fixedrange = TRUE),
        barmode = "stack",
        showlegend = FALSE,
        bargap = 5
      )

    plot <- plot %>% config(displayModeBar = FALSE)

    return(plot)
  })

  #plot of the comparison between number of EXPERIMENTAL and MODEL
  output$telomere_sequences_vs <- renderPlotly({
    statistics <-
      as.data.frame(with(values$datatable_final(), table(status))) #saving filtered dataframe

    #Select the correct color palette given the filtered dataframe
    if ("EXPERIMENTAL" %in% statistics$status) {
      if ("MODEL" %in% statistics$status) {
        if ("POTENTIAL" %in% statistics$status) {
          colors_2nd_graph <- c("#b7e2fc", "#ffcccb", "#FCF2BE")
        } else {
          colors_2nd_graph <- c("#b7e2fc", "#ffcccb")
        }
      } else
        if ("POTENTIAL" %in% statistics$status) {
          colors_2nd_graph <- c("#b7e2fc", "#FCF2BE")
        } else {
          colors_2nd_graph <- "#b7e2fc"
        }

    } else {
      if ("MODEL" %in% statistics$status) {
        if ("POTENTIAL" %in% statistics$status) {
          colors_2nd_graph <- c("#ffcccb", "#FCF2BE")
        } else {
          colors_2nd_graph <- "#ffcccb"
        }
      } else {
        colors_2nd_graph <- "#FCF2BE"
      }
    }

    #create text based on the statistics Freq and if 0 replace it by " "
    statistics<- cbind(statistics, statistics.text = statistics$Freq)
    statistics$statistics.text[statistics$statistics.text == 0]<- " "

    plot <-
      plot_ly(
        statistics,
        labels = ~ status,
        values = ~ Freq,
        text = ~ statistics.text,
        sort = FALSE,
        textinfo = "text",
        marker = list(colors = colors_2nd_graph),
        hoverinfo = "text",
        hovertext = paste(
          "Status:",
          statistics$status,
          "<br> Frequency:",
          statistics$Freq
        ),
        height = 400
      ) %>%
      add_pie(hole = 0.6)

    plot <- plot %>% layout(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      barmode = "stack"
    )

    plot <- plot %>% config(displayModeBar = FALSE)

    return(plot)
  })

  #SelectInput for choosing a color that will be used by metacoder code below to highlight selected sequence
  output$metacoder_colour <- renderUI({
    selectizeInput('selected_metacoder_colour', 'Colour', choices = list("not highlighted" = "#e6e6e3" ,"Red" = "red", "Green" = "green", "Yellow" = "yellow", "Blue" = "blue"), width = "100%")
  })

  #SelectizeInput based on filtered dataset to contain only available sequences for metacoder plotting
  output$metacoder_sequence <- renderUI({
    unique_sequences <- unique(values$datatable_final()$sequence)
    selectizeInput('selected_metacoder_sequence', 'Sequence', choices = unique_sequences, multiple = TRUE, width = "100%",
                   options = list(
                     plugins = list("remove_button")
                   )
                   )
  })

  #SelectInput for choosing a label size that will be used by metacoder code below to highlight selected sequence
  output$metacoder_label_size <- renderUI({
    selectizeInput('selected_metacoder_label_size', 'Label size', choices = list("auto" = 0.01 ,"0.5x" = 0.005, "0.75x" = 0.0075, "1.25x" = 0.0125, "1.5x" = 0.015, "2x" = 0.02, "2.5x" = 0.025, "3x" = 0.03), width = "100%")
  })

  #SelectInput for choosing a node size that will be used by metacoder code below to change the size of the bubbles
  output$metacoder_node_size <- renderUI({
    selectizeInput('selected_metacoder_node_size', 'Node size', choices = list("auto" = 0.01 ,"1.5x" = 0.015, "2x" = 0.02, "3x" = 0.03), width = "100%")
  })

  #SelectInput for choosing a edge size that will be used by metacoder code below to change the thickness of the lines
  output$metacoder_edge_size <- renderUI({
    selectizeInput('selected_metacoder_edge_size', 'Line thickness', choices = list("auto" = 0.001 ,"1.5x" = 0.0015, "2x" = 0.002, "3x" = 0.003, "4x" = 0.004, "5x" = 0.005), width = "100%")
  })

  ## phylogenetic tree sequence higlight (metacoder -> temp() png -> rcurl::base64encode -> plotly)

  #metacoder plot of the filtered data (at least kingdom needs to be selected) with lowest selected taxonomy rank + two orders lower
  #(more filter options) possible if taxa rank above is the same (two families with the same order etc...) (then the selected rank is moved to taxa above)
  #selection of the sequence to highlight -> this can't be connected with the taxa because that would further filter data
  #selection of 4 colours to highlight filtered sequence
  #metacoder does not allow direct connection with plotly -> it is temporarily saved as png and loaded back to R to plot as a background to plotly (https://plotly.com/r/images/)

  #metacoder plot
  output$metacoder_plot <- renderPlotly({

    #showing progress message
    progress <- Progress$new(session, min=0, max=100)
    on.exit(progress$close())
    progress$set(message = 'Calculation in progress...', value = 0)

    metacoder_sequence_to_plot<- input$selected_metacoder_sequence #sequence to highlight based on the input

    #check if some sequence to highlight was selected and change the colour for plotting
    if(is.null(metacoder_sequence_to_plot)) {
      selected_metacoder_colour<- "#E6E6E3"
    } else {
      selected_metacoder_colour<- input$selected_metacoder_colour #load the colour from the input to plot sequences
    }

    #using taxonomy filters to read the selected data for filtration of the unique_df_species (taxonomy filters were resolved in generate phylogenetic tree button code)

    lowest_selected_rank<- values$lowest_selected_rank()
    lowest_rank_name<- values$lowest_rank_name()

    #loading the dataframe as resolved in the generate phylogenetic tree button code

    unique_df_species<- values$unique_df_species()

    #taxonomy_rank vector to find position of lowest_selected_rank
    taxonomy_rank<- c("species", "genus", "family", "order", "class", "phylum", "kingdom")

    #number of position for lowest_rank_to_plot in taxonomy_rank
    in_number_lowest_rank_to_plot<- which(taxonomy_rank == lowest_selected_rank)-2

    #correct position if genus or species was selected so the in_number_lowest_rank_to_plot will be always at least 1
    if(in_number_lowest_rank_to_plot < 1) {
      in_number_lowest_rank_to_plot<- 1
    }

    #translate in_number_lowest_rank_to_plot to the name by taxonomy_rank vector
    lowest_rank_to_plot<- taxonomy_rank[in_number_lowest_rank_to_plot]

    #filter unique_df_species to have only selected part based on the lowest_selected_rank

    dataframe_lowest_rank_name<- unique_df_species[unique_df_species[[match(lowest_selected_rank, colnames(unique_df_species))]] == lowest_rank_name,]

    #recalculate to_highlight based lowest_rank_to_plot and further filter data to by the same vector
    #which in lowest_rank_to_plot have the unique() to_highlight = 1
    rank_to_highlight<- unique(dataframe_lowest_rank_name[[match(lowest_rank_to_plot, colnames(dataframe_lowest_rank_name))]][dataframe_lowest_rank_name$to_highlight == 1 & !is.na(dataframe_lowest_rank_name$to_highlight)])

    dataframe_lowest_rank_name$to_highlight[dataframe_lowest_rank_name[[match(lowest_rank_to_plot, colnames(dataframe_lowest_rank_name))]] %in% rank_to_highlight]<- 1

    unique_df_lowest_rank<- dataframe_lowest_rank_name[!duplicated(dataframe_lowest_rank_name[[match(lowest_rank_to_plot, colnames(dataframe_lowest_rank_name))]]),] #lower the amount of information for further plotting -> should help with the speed of the plotting

    #filter NA sequences
    unique_df_lowest_rank<- unique_df_lowest_rank[!is.na(unique_df_lowest_rank$to_highlight),]


    #vector with taxonomy ranks to filter the point to which the plot will be done
    taxonomy_rank<- c("kingdom", "phylum", "class", "order", "family", "genus", "species")

    #filter taxonomy rank based on lowest_rank_to_plot
    taxonomy_rank_filtered<- taxonomy_rank[1:match(lowest_rank_to_plot, taxonomy_rank)]

    progress$set(message = 'Calculation in progress...', value = 25)

    parsed_datafile<- parse_tax_data(unique_df_lowest_rank, class_cols = taxonomy_rank_filtered)

    #sum the branches that needs to be highlighted and change the higher values to logical 0 and 1
    logical_branches<- unlist(obs_apply(parsed_datafile, "tax_data", sum, value = "to_highlight"))
    logical_branches[logical_branches > 0] <- 1

    #connect to parsed_datafile new data set called summary_data that will be used for the highlight
    parsed_datafile <- mutate_obs(parsed_datafile, data =  "summary_data",
                                  taxon_id = taxon_ids,
                                  branches_to_highlight = logical_branches
    )

    progress$set(message = 'Calculation in progress...', value = 50)

    set.seed(123)

    if(nrow(parsed_datafile$taxonomy_table()) == 1 & ncol(parsed_datafile$taxonomy_table()) == 3) {
      set.seed(8)
    }

    if(nrow(parsed_datafile$taxonomy_table()) == 1 & ncol(parsed_datafile$taxonomy_table()) == 4) {
      set.seed(73)
    }

    if(nrow(parsed_datafile$taxonomy_table()) == 1 & ncol(parsed_datafile$taxonomy_table()) == 5) {
      set.seed(64)
    }

    if(nrow(parsed_datafile$taxonomy_table()) == 1 & ncol(parsed_datafile$taxonomy_table()) == 6) {
      set.seed(8)
    }

    if(nrow(parsed_datafile$taxonomy_table()) == 1 & ncol(parsed_datafile$taxonomy_table()) == 7) {
      set.seed(88)
    }

    heattreeplot<- heat_tree(parsed_datafile,
                             node_color = branches_to_highlight,
                             node_label = taxon_names,
                             initial_layout = "reingold-tilford", layout = "davidson-harel",
                             node_size_range = c(as.numeric(input$selected_metacoder_node_size), as.numeric(input$selected_metacoder_node_size)),
                             node_label_size_range = c(as.numeric(input$selected_metacoder_label_size), as.numeric(input$selected_metacoder_label_size)),
                             edge_size_range = c(as.numeric(input$selected_metacoder_edge_size), as.numeric(input$selected_metacoder_edge_size)),
                             node_color_range = c("#E6E6E3",selected_metacoder_colour),
                             node_color_interval = 0:1,
                             make_node_legend = FALSE
    )

    progress$set(message = 'Calculation in progress...', value = 75)

    #create random name for temporarily stored .png file of the heattreeplot
    random_number_name<- paste0(stri_rand_strings(1, 8), ".png")

    #bind the name with the tempdir() path
    filepath_temp_heattreeplot<- file.path(tempdir(),random_number_name)

    #generate .png file
    png(file = filepath_temp_heattreeplot, height = 6000, width = 6000)
    print(heattreeplot)
    dev.off()

    progress$set(message = 'Calculation in progress...', value = 90)

    #save the heattreeplot to the values$metacoder_plot for downloadhandler
    values$metacoder_plot(heattreeplot)

    #base64Encode the .png file for plotly output
    txt <- RCurl::base64Encode(readBin(filepath_temp_heattreeplot, "raw", file.info(filepath_temp_heattreeplot)[1, "size"]), "txt")

    #remove .png file from the tempdir()
    file.remove(filepath_temp_heattreeplot)

    #Constants
    img_width = 6000
    img_height = 6000
    scale_factor = 0.5


    # Add invisible scatter trace.
    # This trace is added to help the autoresize logic work.
    fig <- plot_ly() %>%
      add_trace( x= c(0, img_width * scale_factor),
                 y= c(0, img_height * scale_factor),
                 type = 'scatter',  mode = 'markers', alpha = 0)

    # Configure axes
    xconfig <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0, img_width * scale_factor)
    )

    yconfig <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0, img_height * scale_factor),
      scaleanchor="x"
    )

    fig <- fig %>% layout(xaxis = xconfig, yaxis = yconfig)

    # Add image
    fig <- fig %>% layout(
      images = list(
        list(
          source = paste('data:image/png;base64', txt, sep=','),
          x=0,
          sizex=img_width * scale_factor,
          y=img_height * scale_factor,
          sizey=img_height * scale_factor,
          xref="x",
          yref="y",
          opacity=1.0,
          layer="below",
          sizing="stretch"
        )
      ))

    # Configure other layout
    m = list(r=0, l=0, b=0, t=0)
    fig <- config(fig, displaylogo = FALSE, modeBarButtonsToRemove = c("toImage", "select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian")) %>% layout(margin = m) %>%
      layout(plot_bgcolor='#ffff',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )

    return(fig)
  })

  #Actions of Generate phylogenetic tree button
  observeEvent(eventExpr = input$generate_metacoder_plot,
               handlerExpr = {

                 #save input from selectizeGroupUI filters
                 kingdom<- input[["filters-kingdom"]]
                 phylum<- input[["filters-phylum"]]
                 class<- input[["filters-class"]]
                 order<- input[["filters-order"]]
                 family<- input[["filters-family"]]
                 genus<- input[["filters-genus"]]
                 species<- input[["filters-species"]]

                 #create vector with vector lengths to check if no filter is filled and later which lowest taxonomy rank is actually filled
                 taxonomy_rank_length<- c(length(species), length(genus), length(family), length(order), length(class), length(phylum), length(kingdom))

                 #position of taxonomy_rank_length that has non-null value to find filter with lowest taxonomy rank that is actually filled
                 which_taxonomy_rank<- which(taxonomy_rank_length > 0)[1]

                 if(is.na(which_taxonomy_rank)) {
                   #modal dialog for confirmation of the row to be deleted
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = HTML(
                         '<div style="text-align: center;">
                         <span style="color:#dd4b39;font-size:200%;">
                         <i class="fas fa-exclamation"></i>
                         </span>
                         </div>'
                       ),
                       size = c("s"),
                       "No taxonomy rank was selected in the filters on the right.",
                       footer = tagList(
                         modalButton('Cancel')
                       )
                     )
                   )

                 } else {

                   datafile_raw <- values$datatable_final() #saving filtered dataframe

                   if(nrow(datafile_raw) < 1) {

                     showModal(
                       modalDialog(
                         easyClose = TRUE,
                         title = HTML(
                           '<div style="text-align: center;">
                         <span style="color:#dd4b39;font-size:200%;">
                         <i class="fas fa-exclamation"></i>
                         </span>
                         </div>'
                         ),
                         size = c("s"),
                         "There is no data to be plotted since the first tab Telomere sequences is empty. Check the filter selection.",
                         footer = tagList(
                           modalButton('Cancel')
                         )
                       )
                     )

                   } else {

                   #create taxonomy_rank vector to match which_taxonomy_rank to find the preciese taxonomy rank that was taken
                   taxonomy_rank<- c("species", "genus", "family", "order", "class", "phylum", "kingdom")

                   #set which taxonomy rank was taken last and  match it to the taxonomy_rank
                   lowest_selected_rank<- taxonomy_rank[which_taxonomy_rank]

                   #create list from the inputs of taxonomy filters to use which_taxonomy_rank to get the precise vector
                   taxonomy_rank_list<- list(species, genus, family, order, class, phylum, kingdom)

                   #last selected taxonomy rank using which_taxonomy_rank
                   lowest_rank_name<- taxonomy_rank_list[[which_taxonomy_rank]]

                   #vector for checking if there is more than 1 selected element in the rank
                   length_lowest_rank_name<- length(lowest_rank_name)

                   #checker if more elements in the lowest rank have the same taxonomy (if not later switched to FALSE, it will give a modal back)
                   taxonomy_checker<- TRUE

                   datafile_highlight<- data.frame(to_highlight = 0, datafile_raw) #prepare setup for the sequence highlight

                   metacoder_sequence_to_plot<- input$selected_metacoder_sequence #sequence to highlight based on the input

                   species_to_highlight<- datafile_raw$species[datafile_raw$sequence %in% metacoder_sequence_to_plot] #select species that do have metacoder_sequence_to_plot
                   datafile_highlight$to_highlight[datafile_highlight$species %in% species_to_highlight]<- 1 #change to_highlight for these species to 1 value

                   datafile_highlight<- datafile_highlight[,-c(3:8)] #remove uninportant columns to speed up the calculation
                   unique_df_species<- datafile_highlight[!duplicated(datafile_highlight$species),] #lower the amount of information for later plotting

                   #code in if changes lowest_selected_rank and lowest_rank_name accordingly
                   if (length_lowest_rank_name > 1) {

                      #check if two kingdoms are not selected
                     if ((which_taxonomy_rank+3) < 10) {

                       unique_check<- unique_df_species[,(which_taxonomy_rank+3)] #save vector with +1 taxonomy rank than in which_taxonomy_rank (to_highlight and name still present in the dataframe)
                       number_in_unique_check<- length(unique(unique_check)) #number of unique taxonomy ranks in taxonomy rank higher than which_taxonomy_rank

                       #check if more selected elements in lowest taxonomy rank selection have the same higher taxonomy rank
                       if (number_in_unique_check < 2) {

                         lowest_selected_rank<- taxonomy_rank[which_taxonomy_rank+1]
                         lowest_rank_name<- unique(unique_check)

                       } else {
                         taxonomy_checker<- FALSE
                       }

                     } else {
                       taxonomy_checker<- FALSE
                     }
                   }

                   if (taxonomy_checker == TRUE) {

                     values$lowest_selected_rank(lowest_selected_rank)
                     values$lowest_rank_name(lowest_rank_name)
                     values$unique_df_species(unique_df_species)

                     #modal dialog for showing the metacoder plot
                     showModal(
                       modalDialog(
                         easyClose = TRUE,
                         size = c("xl"),
                         plotlyOutput("metacoder_plot", height = '100%'),
                         footer = tagList(
                           downloadButton("download_metacoder_plot", "Download"),
                           modalButton('Cancel')
                         )
                       )
                     )

                   } else {

                     showModal(
                       modalDialog(
                         easyClose = TRUE,
                         title = HTML(
                           '<div style="text-align: center;">
                         <span style="color:#dd4b39;font-size:200%;">
                         <i class="fas fa-exclamation"></i>
                         </span>
                         </div>'
                         ),
                         size = c("s"),
                         "In lowest selected taxonomy filter, there are elements that do not have the same higher taxonomy.",
                         footer = tagList(
                           modalButton('Cancel')
                         )
                       )
                     )

                   }
                 }
                }
               }
              )
  ##

  #download metacoder plot
  output$download_metacoder_plot <- downloadHandler(
    filename <- function() {
      paste(Sys.Date(),'_metacoder_plot.png',sep='') },
    content <- function(file) {
      png(file, height = 6000, width = 6000)
      heattreeplot<- values$metacoder_plot()
      print(heattreeplot)
      dev.off()},
    contentType = 'image/png'
  )

  #download filtered telomere_sequences by guest
  output$download_filtered_telomere_sequences <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), '-telomere_sequences.xlsx', sep = '')
    },
    content = function(con) {
      filtered_telomere_sequences<- values$datatable_final()
      filtered_telomere_sequences$link<- gsub('<a href=\"' ,"", gsub('" target=\"_blank\">.*', "", filtered_telomere_sequences$reference))
      filtered_telomere_sequences$author_code<- gsub('.*>' ,"", gsub('</a>', "", filtered_telomere_sequences$reference))
      filtered_telomere_sequences<- filtered_telomere_sequences[,-5]
      filtered_telomere_sequences<- filtered_telomere_sequences %>% relocate(link, .after=status)
      filtered_telomere_sequences<- filtered_telomere_sequences %>% relocate(author_code, .after=link)
      writexl::write_xlsx(filtered_telomere_sequences, con)
    }
  )

  #download telomere_sequences by guest
  output$download_whole_telomere_sequences <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), '-telomere_sequences.xlsx', sep = '')
    },
    content = function(con) {
      whole_telomere_sequences <-
        read.csv2("./data/telomere_sequences.csv", stringsAsFactors = FALSE)
      whole_telomere_sequences$link<- gsub('<a href=\"' ,"", gsub('" target=\"_blank\">.*', "", whole_telomere_sequences$reference))
      whole_telomere_sequences$author_code<- gsub('.*>' ,"", gsub('</a>', "", whole_telomere_sequences$reference))
      whole_telomere_sequences<- whole_telomere_sequences[,-5]
      whole_telomere_sequences<- whole_telomere_sequences %>% relocate(link, .after=status)
      whole_telomere_sequences<- whole_telomere_sequences %>% relocate(author_code, .after=link)
      writexl::write_xlsx(whole_telomere_sequences, con)
    }
  )

  ## END - TELOMERE SEQUENCES TAB ##



  #### START - SUBMIT SEQUENCE TAB ####

  output$apply_sequences_box <- renderUI({
    fluidPage(column(
      width = 9,
      box(
        width = NULL,
        status = "warning",
        id = "apply_sequence_tab",
        title = "Submit sequence form",
        column(
          width = 6,
          #style for the selectInput
          tags$head(
            tags$style(".selectize-input {border-radius:0px}"),
            tags$style(".selectize-input.dropdown-active {border-radius:0px}"),
            tags$style(".selectize-dropdown {border-radius:0px}"),
            tags$style(
              HTML(
                '
                .selectize-input.focus {
                border-color:#3c8dbc;
                outline: 0;
                -webkit-box-shadow:none !important;
                box-shadow:none !important;}
                '
              )
              )
            ),
          useShinyFeedback(),
          textInput(inputId = "name_row_apply",
                    label = "Name"),
          actionLink(
            inputId = "check_taxonomy_button",
            label = "Autofill taxonomy information",
            icon = icon("pen-to-square")
          ),
          textInput(inputId = "sequence_row_apply",
                    label = "Sequence"),
          textInput(inputId = "location_row_apply",
                    label = "Location"),
          selectInput(
            inputId = "status_row_apply",
            label = "Status",
            choices = c("MODEL", "EXPERIMENTAL")
          ),
          textInput(inputId = "reference_row_apply",
                    label = "Reference (Name et al. + year e.g., Smith et al. 2020)"),
          textInput(inputId = "doi_row_apply",
                    label = "DOI (https://doi.org/ included)"),
          prettyCheckbox(
            inputId = "show_registration",
            label = "Include registration form to the TeloBase",
            value = FALSE,
            status = "warning"
          ),

          uiOutput("registration_form"),
          actionButton(
            inputId = "apply_sequence",
            label = "Submit sequence",
            icon = icon("pen-to-square")
          )

              ),

        column(
          width = 6,
          textInput(inputId = "species_row_apply",
                    label = "Species"),
          textInput(inputId = "genus_row_apply",
                    label = "Genus"),
          textInput(inputId = "family_row_apply",
                    label = "Family"),
          textInput(inputId = "order_row_apply",
                    label = "Order"),
          textInput(inputId = "class_row_apply",
                    label = "Class"),
          textInput(inputId = "phylum_row_apply",
                    label = "Phylum"),
          textInput(inputId = "kingdom_row_apply",
                    label = "Kingdom")
        )

            )
        ),

      column(
        width = 3,
        box(
          width = NULL,
          title = "About taxonomy",
          collapsible = TRUE,
          collapsed = TRUE,
          "Autofill taxonomy information is utilizing GBIF (Global Biodiversity Information Facility), NCBI (National Center for Biotechnology Information) and TOL (Open Tree of Life) databases using taxize package. Lower taxonomic ranks (species/genus/family) are
          resolved by all three options with priority set as GBIF>NCBI>TOL. If genus/family is already present in TeloBase, higher taxonomy ranks are filled utilizing internal taxonomy, otherwise higher taxonomic ranks use GBIF only.",
          br(),
          br(),
          "Sometimes Autofill might not work well with given name due to the missing or badly implemented information about taxonomy rank in GBIF. Please, always check the automatically filled data.",
          background = "yellow"
        ),
        box(
          width = NULL,
          title = "Sequence similarity check",
          "Applied sequence will be later checked against the TeloBase if its iteration is not already present to match it (reverse complement sequence included).",
          background = "red"
        ),
        box(
          width = NULL,
          title = "Status explanation",
          "Status MODEL is used for telomere sequences discovered through NGS data analysis or analysis of telomerase RNA subunit.",
          background = "yellow"
        )
      ))
  })

  #show the registration form based on checking prettycheckbox show_registration
  output$registration_form <- renderUI({
    if (input$show_registration == TRUE) {
      fluidRow(
        column(
          width = 6,
          textInput(
            inputId = "users_row_apply",
            label = "Username",
            value = ""
          ),
          textInput(
            inputId = "contact_row_apply",
            label = "Contact",
            value = ""
          )
        ),
        column(
          width = 6,
          passwordInput(
            inputId = "password_row_first_apply",
            label = "Password",
            value = ""
          ),
          passwordInput(
            inputId = "password_row_second_apply",
            label = "Repeat Password",
            value = ""

          )
        ),
        column(
          width = 12,
          actionLink(
            inputId = "show_GDPR",
            label = "How is my contact information used?",
            icon = icon("question")
          ),
          br(),
          br()
        )
      )
    }
  })


  #generate values for textInputs based on GBIF taxonomy when clicking autofill actionlink
  observeEvent(eventExpr = input$check_taxonomy_button,
               handlerExpr = {

                 hideFeedback("name_row_apply")
                 name_input<- input$name_row_apply

                 if (name_input == "") {
                   showFeedbackDanger(inputId = "name_row_apply", text = "Empty")
                 } else {

                   #using taxize package and name_input search for a taxonomy match
                   show_modal_spinner(spin = "orbit",
                                      text = "Processing...",
                                      color = "#dd4b39")

                   #save values$telomere_database() as reference that is addressed later when searching if genus or family is not already in the database
                   reference<- values$telomere_sequences()

                   #remove cf., af., aff. from the name that will be searched
                   searched_item<- str_replace_all(name_input, c("cf. " = "", "af. " = "", "aff. " = ""))

                   #cut more from the searched item than first 2 words (usually should be giving only the genus and species for better search in the db)
                   if (sapply(strsplit(searched_item, " "), length) > 1) {
                     searched_item<- word(searched_item,1,2, sep=" ")
                   }

                   #Create empty table with name rank and id that will be taken up instead of a query that was empty after 3 attempts by classification() (creates problem with atomic vector warning)
                   taxa_NA<- data.frame(NA,NA,NA)
                   colnames(taxa_NA)<- c("name", "rank", "id")

                   taxa_gbif<- classification(as.character(searched_item), db = "gbif", rows = 1)
                   taxa_ncbi<- classification(as.character(searched_item), db = "ncbi", rows = 1)
                   taxa_tol<- classification(as.character(searched_item), db = "tol", rows = 1)


                   #Filter classification results to only kingdom, phylum, class, order, family, genus and species taxonomy ranking and if empty, use taxa_NA
                   taxa_gbif_filtered<- if (is.atomic(taxa_gbif[[1]]) == FALSE) {
                     taxa_gbif[[1]][taxa_gbif[[1]]$rank %in% c("kingdom", "phylum", "class", "order", "family", "genus", "species"),]
                   } else {
                     taxa_NA
                   }

                   taxa_ncbi_filtered<- if (is.atomic(taxa_ncbi[[1]]) == FALSE) {
                     taxa_ncbi[[1]][taxa_ncbi[[1]]$rank %in% c("family", "genus", "species"),]
                   } else {
                     taxa_NA
                   }

                   taxa_tol_filtered<- if (is.atomic(taxa_tol[[1]]) == FALSE) {
                     taxa_tol[[1]][taxa_tol[[1]]$rank %in% c("family", "genus", "species"),]
                   } else {
                     taxa_NA
                   }

                   ## Fill taxa by GBIF, NCBi or TOL in this order otherwise NA

                   family<- if (!is_empty(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "family"]) && !is.na(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "family"])) {
                     taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "family"]
                   } else {
                     if (!is_empty(taxa_ncbi_filtered$name[taxa_ncbi_filtered$rank == "family"]) && !is.na(taxa_ncbi_filtered$name[taxa_ncbi_filtered$rank == "family"])) {
                       taxa_ncbi_filtered$name[taxa_ncbi_filtered$rank == "family"]
                     } else {
                       if (!is_empty(taxa_tol_filtered$name[taxa_tol_filtered$rank == "family"]) && !is.na(taxa_tol_filtered$name[taxa_tol_filtered$rank == "family"])) {
                         taxa_tol_filtered$name[taxa_tol_filtered$rank == "family"]
                       } else {
                         NA
                       }
                     }
                   }

                   genus<- if (!is_empty(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "genus"]) && !is.na(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "genus"])) {
                     taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "genus"]
                   } else {
                     if (!is_empty(taxa_ncbi_filtered$name[taxa_ncbi_filtered$rank == "genus"]) && !is.na(taxa_ncbi_filtered$name[taxa_ncbi_filtered$rank == "genus"])) {
                       taxa_ncbi_filtered$name[taxa_ncbi_filtered$rank == "genus"]
                     } else {
                       if (!is_empty(taxa_tol_filtered$name[taxa_tol_filtered$rank == "genus"]) && !is.na(taxa_tol_filtered$name[taxa_tol_filtered$rank == "genus"])) {
                         taxa_tol_filtered$name[taxa_tol_filtered$rank == "genus"]
                       } else {
                         NA
                       }
                     }
                   }

                   species<- if (!is_empty(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "species"]) && !is.na(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "species"])) {
                     taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "species"]
                   } else {
                     if (!is_empty(taxa_ncbi_filtered$name[taxa_ncbi_filtered$rank == "species"]) && !is.na(taxa_ncbi_filtered$name[taxa_ncbi_filtered$rank == "species"])) {
                       taxa_ncbi_filtered$name[taxa_ncbi_filtered$rank == "species"]
                     } else {
                       if (!is_empty(taxa_tol_filtered$name[taxa_tol_filtered$rank == "species"]) && !is.na(taxa_tol_filtered$name[taxa_tol_filtered$rank == "species"])) {
                         taxa_tol_filtered$name[taxa_tol_filtered$rank == "species"]
                       } else {
                         NA
                       }
                     }
                   }

                   #Unification of genus and species if they do not conform to each other (due to use of different db, etc.)
                   if (!is.na(genus) && !is.na(species) & genus != word(species, 1)) {
                     genus<- word(species, 1)
                   }

                   #Check if genus is already present in the reference to take the higher order of taxa from the reference
                   if (!is.na(genus) & genus %in% reference$genus) {
                     #save first encounter of the reference to the separate dataframe
                     reference_to_higher_taxa<- reference[reference$genus == genus & !is.na(reference$genus),][1,]

                     #save higher taxa information from the reference
                     family<- as.character(reference_to_higher_taxa$family)
                     order<- as.character(reference_to_higher_taxa$order)
                     class<- as.character(reference_to_higher_taxa$class)
                     phylum<- as.character(reference_to_higher_taxa$phylum)
                     kingdom<- as.character(reference_to_higher_taxa$kingdom)

                   } else {

                     #Check if family is already present in the reference to take the higher order of taxa from the reference
                     if (!is.na(family) & family %in% reference$family) {

                       #save first encounter of the reference to the separate dataframe
                       reference_to_higher_taxa<- reference[reference$family == family & !is.na(reference$family),][1,]

                       #save higher taxa information from the reference
                       order<- as.character(reference_to_higher_taxa$order)
                       class<- as.character(reference_to_higher_taxa$class)
                       phylum<- as.character(reference_to_higher_taxa$phylum)
                       kingdom<- as.character(reference_to_higher_taxa$kingdom)

                     } else {
                       if (!is.na(family)) {
                         #Do taxa search from the family again in GBIF (allows more unified taxonomy as differences are expected in higher order taxa)
                         taxa_gbif_second<- classification(as.character(family), db = "gbif", rows = 1)


                         #Filter classification results using family to only kingdom, phylum, class, order and if empty, use taxa_NA
                         taxa_gbif_filtered_second<- if (is.atomic(taxa_gbif_second[[1]]) == FALSE) {
                           taxa_gbif_second[[1]][taxa_gbif_second[[1]]$rank %in% c("kingdom", "phylum", "class", "order"),]
                         } else {
                           taxa_NA
                         }

                         ## Fill taxa by GBIF otherwise NA

                         kingdom<- if (!is_empty(taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "kingdom"]) &&
                                       !is.na(taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "kingdom"])) {
                           taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "kingdom"]
                         } else {
                           NA
                         }

                         phylum<- if (!is_empty(taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "phylum"]) &&
                                      !is.na(taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "phylum"])) {
                           taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "phylum"]
                         } else {
                           NA
                         }

                         class<- if (!is_empty(taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "class"]) &&
                                     !is.na(taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "class"])) {
                           taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "class"]
                         } else {
                           NA
                         }

                         order<- if (!is_empty(taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "order"]) &&
                                     !is.na(taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "order"])) {
                           taxa_gbif_filtered_second$name[taxa_gbif_filtered_second$rank == "order"]
                         } else {
                           NA
                         }
                       } else {

                         ##Use previously searched GBIF to fill higher taxa (if family had NA)

                         kingdom<- if (!is_empty(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "kingdom"]) &&
                                       !is.na(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "kingdom"])) {
                           taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "kingdom"]
                         } else {
                           NA
                         }

                         phylum<- if (!is_empty(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "phylum"]) &&
                                      !is.na(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "phylum"])) {
                           taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "phylum"]
                         } else {
                           NA
                         }

                         class<- if (!is_empty(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "class"]) &&
                                     !is.na(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "class"])) {
                           taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "class"]
                         } else {
                           NA
                         }

                         order<- if (!is_empty(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "order"]) &&
                                     !is.na(taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "order"])) {
                           taxa_gbif_filtered$name[taxa_gbif_filtered$rank == "order"]
                         } else {
                           NA
                         }
                       }
                     }
                   }

                   updateTextInput(session, inputId = "kingdom_row_apply", value = kingdom)

                   updateTextInput(session, inputId = "phylum_row_apply", value = phylum)

                   updateTextInput(session, inputId = "class_row_apply", value = class)

                   updateTextInput(session, inputId = "order_row_apply", value = order)

                   updateTextInput(session, inputId = "family_row_apply", value = family)

                   updateTextInput(session, inputId = "genus_row_apply", value = genus)

                   updateTextInput(session, inputId = "species_row_apply", value = species)

                   remove_modal_spinner()
                 }
               })

  #show information with regard to the data protection
  observeEvent(eventExpr = input$show_GDPR,
               handlerExpr = {
                 showModal(
                   modalDialog(
                     easyClose = TRUE,
                     title = HTML(
                       '<div style="text-align: center;">
                         <span style="color:#3a88b6;font-size:200%;">
                         <i class="fa fa-question"></i>
                         </span>
                         </div>'
                     ),
                     size = c("m"),
                     "Necessary contact information (e-mail adress) will be used only for the purposes to inform and later confirm registration of the member and
                     ocassionally to inform members about TeloBase updates.",
                     br(),
                     br(),
                     "No data is shared with the third parties and members can remove the contact information in the user management by e.g., writing @. instead of an
                     actual e-mail adress. However, it will not be possible to get a new password in case you forget it.",
                     br(),
                     br(),
                     "Sending out this form with filled registration form means that potential member understands and agrees with the use of their personal data
                     as stated above.",
                     footer = tagList(
                       modalButton('Cancel')
                     )
                   )
                 )
               })

  #generate application after clicking Submit sequence button
  observeEvent(eventExpr = input$apply_sequence,
               handlerExpr = {

                 hideFeedback("users_row_apply")
                 hideFeedback("contact_row_apply")
                 hideFeedback("password_row_first_apply")
                 hideFeedback("password_row_second_apply")
                 hideFeedback("name_row_apply")
                 hideFeedback("sequence_row_apply")
                 hideFeedback("location_row_apply")
                 hideFeedback("reference_row_apply")
                 hideFeedback("doi_row_apply")

                 accs <- #upload accs values to the accs
                   values$accs()

                 pass <-
                   TRUE #if TRUE, edit is executed, otherwise shinyFeedback is shown
                 if (input$show_registration == TRUE) {
                   if (input$users_row_apply != "") {
                     if (any(accs$users == input$users_row_apply)) {
                       showFeedbackDanger(inputId = "users_row_apply", text = "Username already used.")
                       pass <- FALSE
                     }

                   } else {
                     showFeedbackDanger(inputId = "users_row_apply", text = "Empty")
                     pass <- FALSE
                   }

                   if (input$contact_row_apply != "") {
                     if (any(accs$contact == input$contact_row_apply)) {
                       showFeedbackDanger(inputId = "contact_row_apply", text = "Contact already used.")
                       pass <- FALSE
                     }
                     if (!grepl(input$contact_row_apply,
                                pattern = "@",
                                fixed = TRUE) |
                         !grepl(input$contact_row_apply,
                                pattern = ".",
                                fixed = TRUE)) {
                       showFeedbackDanger(inputId = "contact_row_apply", text = "Not an e-mail format.")
                       pass <- FALSE
                     }

                   } else {
                     showFeedbackDanger(inputId = "contact_row_apply", text = "Empty")
                     pass <- FALSE
                   }

                   if (input$password_row_first_apply == "") {
                     showFeedbackDanger(inputId = "password_row_first_apply", text = "Empty")
                     pass <- FALSE
                   }

                   if (input$password_row_second_apply == "") {
                     showFeedbackDanger(inputId = "password_row_second_apply", text = "Empty")
                     pass <- FALSE
                   }

                   if (input$password_row_first_apply != input$password_row_second_apply) {
                     showFeedbackDanger(inputId = "password_row_first_apply", text = "")
                     showFeedbackDanger(inputId = "password_row_second_apply", text = "Passwords do not match.")
                     pass <- FALSE
                   }

                 }

                 if (input$name_row_apply == "") {
                     showFeedbackDanger(inputId = "name_row_apply", text = "Empty")
                     pass <- FALSE
                 }

                 if (input$sequence_row_apply == "") {
                   showFeedbackDanger(inputId = "sequence_row_apply", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$location_row_apply == "") {
                   showFeedbackDanger(inputId = "location_row_apply", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$reference_row_apply == "") {
                   showFeedbackDanger(inputId = "reference_row_apply", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$doi_row_apply != "") {
                   if (!grepl(input$doi_row_apply,
                              pattern = "https://doi.org/",
                              fixed = TRUE)) {
                     showFeedbackDanger(inputId = "doi_row_apply", text = "https://doi.org/ not included.")
                     pass <- FALSE
                   }

                 } else {
                   showFeedbackDanger(inputId = "doi_row_apply", text = "Empty")
                   pass <- FALSE
                 }

                 if (pass == TRUE) {

                   #test if iteration of the applied sequence is already part of the database and change it to that if so
                   sequence<- input$sequence_row_apply
                   ncs<- nchar(sequence)
                   combinations<- NA
                   reverse_sequence<- gsubfn::gsubfn(".", list("T" = "A", "A" = "T", "C" = "G", "G" = "C"), stri_reverse(sequence))
                   reverse_combinations<- NA
                   for (i in 0:(ncs-1)) {
                     combinations[i+1]<- paste(substr(sequence, ncs+1-i,ncs), substr(sequence, 1,ncs-i), sep = "")
                     reverse_combinations[i+1]<- paste(substr(reverse_sequence, ncs+1-i,ncs), substr(reverse_sequence, 1,ncs-i), sep = "")
                   }
                   all_combinations<- c(combinations, reverse_combinations)

                   #create a vector with sequences in the datatable and newly applied sequence
                   test_sequence_vector<- c(values$datatable_final()$sequence, sequence)

                   #test all the combinations to the already tested rows and if there is a match change it to the first TRUE value (there will be only one anyway)
                   applied_sequence_sequence<- test_sequence_vector[is.element(test_sequence_vector, all_combinations) == TRUE][1]

                   #values for the later row to upload based if user registers or not
                   if (input$show_registration == TRUE) {
                     applied_sequence_user<- input$users_row_apply
                     applied_sequence_contact<- input$contact_row_apply
                     applied_sequence_password<- password_store(input$password_row_first_apply)
                   } else {
                     applied_sequence_user<- NA
                     applied_sequence_contact<- NA
                     applied_sequence_password<- NA
                   }

                   #create row based on user input that will be uploaded to the applied_sequences.csv
                   applied_sequence <- data.frame(
                     'to.remove' = NA,
                     name = input$name_row_apply,
                     sequence = applied_sequence_sequence,
                     location = input$location_row_apply,
                     status = input$status_row_apply,
                     reference = paste(
                       '<a href=\"',
                       input$doi_row_apply,
                       '" target=\"_blank\">',
                       input$reference_row_apply,
                       '</a>',
                       sep = ""
                     ),
                     FTANS = NA,
                     FTELS = NA,
                     species = input$species_row_apply,
                     genus = input$genus_row_apply,
                     family = input$family_row_apply,
                     order = input$order_row_apply,
                     class = input$class_row_apply,
                     phylum = input$phylum_row_apply,
                     kingdom = input$kingdom_row_apply,
                     score = 0,
                     'who.voted' = NA,
                     users = applied_sequence_user,
                     rights = "user",
                     contact = applied_sequence_contact,
                     password = applied_sequence_password,
                     stringsAsFactors = FALSE
                   )
                   applied_sequences <-
                     read.csv2("./data/applied_sequences.csv", stringsAsFactors = FALSE)
                   new_applied_sequences <-
                     rbind(applied_sequence, applied_sequences)

                   #actualization of the datatable_applied_management_final -> altough will always restart it for the user when new
                   values$datatable_applied_management_final(new_applied_sequences)

                   write.csv2(new_applied_sequences,
                              file = "./data/applied_sequences.csv",
                              row.names = FALSE)

                   showNotification(
                     "Application form was sent.",
                     closeButton = FALSE,
                     duration = 2,
                     type = "warning"
                   )

                   #send an e-mail if user is also registering when applying sequence
                   if (input$show_registration == TRUE) {
                     email <- envelope(
                       to = input$contact_row_apply,
                       from = Sys.getenv("SMTP_USER"),
                       subject = "Thank you for submission and registration to TeloBase",
                       text = "Thank you for your sequence submission and registration to the TeloBase. After your sequence will be approved, you will be informed by an e-mail that your registration was completed.
                       Your TeloBase team"
                     )

                     smtp <- server(host = Sys.getenv("SMTP_HOST"),
                                    port = as.integer(Sys.getenv("SMTP_PORT")),
                                    username = Sys.getenv("SMTP_USER"),
                                    password = Sys.getenv("SMTP_PASS"))
                     smtp(email, verbose = TRUE)
                   }

                   #remove previous values
                   updateTextInput(session, inputId = "name_row_apply", value = "")
                   updateTextInput(session, inputId = "sequence_row_apply", value = "")
                   updateTextInput(session, inputId = "location_row_apply", value = "")
                   updateSliderInput(session, inputId = "status_row_apply", value = "MODEL")
                   updateTextInput(session, inputId = "reference_row_apply", value = "")
                   updateTextInput(session, inputId = "doi_row_apply", value = "")

                   updateTextInput(session, inputId = "kingdom_row_apply", value = "")
                   updateTextInput(session, inputId = "phylum_row_apply", value = "")
                   updateTextInput(session, inputId = "class_row_apply", value = "")
                   updateTextInput(session, inputId = "order_row_apply", value = "")
                   updateTextInput(session, inputId = "family_row_apply", value = "")
                   updateTextInput(session, inputId = "genus_row_apply", value = "")
                   updateTextInput(session, inputId = "species_row_apply", value = "")

                   updateTextInput(session, inputId = "users_row_apply", value = "")
                   updateTextInput(session, inputId = "contact_row_apply", value = "")
                   updateTextInput(session, inputId = "password_row_first_apply", value = "")
                   updateTextInput(session, inputId = "password_row_second_apply", value = "")

                 }

               })

  ## END - SUBMIT SEQUENCE TAB ##



  #### START - REPORT SEQUENCE TAB ####

  output$report_sequences_box <- renderUI({
    fluidPage(column(
      width = 9,
      box(
        width = NULL,
        status = "danger",
        id = "report_sequence_tab",
        title = "Report issue(s) form",
        column(
          width = 6,
          #style for the selectInput
          tags$head(
            tags$style(".selectize-input {border-radius:0px}"),
            tags$style(".selectize-input.dropdown-active {border-radius:0px}"),
            tags$style(".selectize-dropdown {border-radius:0px}"),
            tags$style(
              HTML(
                '
                .selectize-input.focus {
                border-color:#3c8dbc;
                outline: 0;
                -webkit-box-shadow:none !important;
                box-shadow:none !important;}
                '
              )
              )
              ),
          useShinyFeedback(),
          textInput(inputId = "name_row_report",
                    label = "Name"),
          textInput(inputId = "sequence_row_report",
                    label = "Sequence"),
          textInput(inputId = "reference_row_report",
                    label = "Reference (Name et al. + year e.g., Smith et al. 2020)"),
          actionButton(
            inputId = "report_sequence",
            label = "Report sequence",
            icon = icon("pen-to-square")
          )
        ),
        column(
          width = 6,
          textAreaInput(inputId = "comment_row_report",
                    label = "Comment", rows = 7, resize = "none")
        )

        )),

      column(
        width = 3,
        box(
          width = NULL,
          title = "Reporting issue(s)",
          "Please fill out all boxes as listed in Telomere sequences for easier correction by admins and in the Comment box write what is wrong.",
          br(),
          br(),
          "If you want to report more than one sequence (e.g., changed name of the Phylum, Order etc. affecting more than one sequence), name then irrelevant boxes as variable.",
          background = "red"
          )
      )
        )
  })

  #generate report after clicking Report sequence button
  observeEvent(eventExpr = input$report_sequence,
               handlerExpr = {

                 hideFeedback("name_row_report")
                 hideFeedback("sequence_row_report")
                 hideFeedback("reference_row_report")
                 hideFeedback("comment_row_report")

                 pass <-
                   TRUE #if TRUE, edit is executed, otherwise shinyFeedback is shown

                 if (input$name_row_report == "") {
                   showFeedbackDanger(inputId = "name_row_report", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$sequence_row_report == "") {
                   showFeedbackDanger(inputId = "sequence_row_report", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$reference_row_report == "") {
                   showFeedbackDanger(inputId = "reference_row_report", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$comment_row_report == "") {
                   showFeedbackDanger(inputId = "comment_row_report", text = "Empty")
                   pass <- FALSE
                 }

                 if (pass == TRUE) {

                   #create row based on user input that will be uploaded to the reports.csv
                   reported_sequence <- data.frame(
                     'to.remove' = NA,
                     name = input$name_row_report,
                     sequence = input$sequence_row_report,
                     reference = input$reference_row_report,
                     comment = input$comment_row_report,
                     score = 0,
                     stringsAsFactors = FALSE
                   )
                   reported_sequences <-
                     read.csv2("./data/reports.csv", stringsAsFactors = FALSE)
                   new_reported_sequences <-
                     rbind(reported_sequence, reported_sequences)

                   #actualization of the datatable_applied_reports_final -> altough will always restart it for the user when new
                   values$datatable_applied_reports_final(new_reported_sequences)

                   write.csv2(new_reported_sequences,
                              file = "./data/reports.csv",
                              row.names = FALSE)

                   showNotification(
                     "Report form was sent.",
                     closeButton = FALSE,
                     duration = 2,
                     type = "error"
                   )

                   #remove previous values
                   updateTextInput(session, inputId = "name_row_report", value = "")
                   updateTextInput(session, inputId = "sequence_row_report", value = "")
                   updateTextInput(session, inputId = "reference_row_report", value = "")
                   updateTextInput(session, inputId = "comment_row_report", value = "")

                 }

               })

  ## END - REPORT SEQUENCE TAB ##



  #### START - SUBMITTED SEQUENCES TAB ####

  output$applied_sequences_box <- renderUI({
    fluidPage(
      column(
        width = 9,
        box(
          width = NULL,
          title = "Submitted sequences",
          status = "warning",
          dataTableOutput("applied_sequences_management"),
          tags$script(
            "$(document).on('click', '#applied_sequences_management button', function () {
            Shiny.onInputChange('lastClick_appliedsequencesId',this.id);
            Shiny.onInputChange('lastClick_appliedsequences', Math.random())});"
          )
        )
      ),
      column(width = 3,
             box(width = NULL,
                 title = "Score system",
                 "Entries can be upvoted or downvoted based on the correctness of the application. Each member has one vote for each sequence (unless it is edited as mentioned below).",
                 br(),
                 br(),
                 "Entry that recieves score value of 2 will be regarded as correct and be implemented into the database.",
                 br(),
                 br(),
                 "Contrary, entry that recieves score value of -2 will be regarded as incorrect and voting will be no longer be possible.",
                 br(),
                 br(),
                 "Note that there is also an option to edit the entry if some minor mistakes have been made. Editing will restart the score.",
                 background = "yellow"),
             box(
               width = NULL,
               title = "Status explanation",
               "Status MODEL is used for telomere sequences discovered through NGS data analysis or analysis of telomerase RNA subunit.",
               background = "yellow")
             ))
  })

  #Datatable of the submitted sequences
  output$applied_sequences_management <- renderDataTable({
    applied_sequences <-
      values$datatable_applied_management_final() #upload reactive val with telomere_sequences df
    colnames(applied_sequences)[1] <- "to.remove"
    applied_sequences$to.remove <-
      #add upvote, downvote and edit butttons to every row of the df
      paste0(
        '
        <div class="btn-group" style="width: 102px" role="group" aria-label="Basic example">
        <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="up_',
        1:nrow(applied_sequences),
        '"><i class="fa fa-thumbs-o-up"></i></button>
        <button class="btn btn-warning btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="modify_',
        1:nrow(applied_sequences),
        '"><i class="fa fa-pencil-square-o"></i></button>
        <button class="btn btn-danger btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="down_',
        1:nrow(applied_sequences),
        '"><i class="fa fa-thumbs-o-down"></i></button>
        </div>
        '
      )

    #remove sequences that user voted for before or have a score < -1 or 1 >
    if (!is.null(isolate(input$user_input)))  {
    applied_sequences$to.remove[grepl(isolate(input$user_input), applied_sequences$who.voted) | applied_sequences$score %in% c(-2,2)]<- " "

    values$number_to_evaluate(length(applied_sequences$to.remove[!grepl(isolate(input$user_input), applied_sequences$who.voted) & applied_sequences$score %in% c(1,0,-1)]))
    }

    rownames(applied_sequences) <-
      #add empty space as a row space for collapsing buttons
      paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <p data-placement="top" id="modify_',
        1:nrow(applied_sequences),
        '"></p>
        </div>
        '
      )
    colnames(applied_sequences)[1] <- " "
    values$datatable_applied_management_final(applied_sequences)
    datatable(
      isolate(values$datatable_applied_management_final()[-c(17:21)]),
      #render applied_sequences for the website
      class = "compact",
      extensions = c("Responsive"),
      escape = FALSE,
      selection = "none",
      style = "bootstrap4",
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 50, 100),
        search = list(regex = TRUE),
        ordering = FALSE
      )
    ) %>%

      #vertical align
      formatStyle(columns = colnames(isolate(
        values$datatable_applied_management_final()[-c(17:21)]
      )),
      `vertical-align` = 'middle', height = 30) %>%
      #colour whole row beside for the 1 column based on the scores (for some reason without name is ignored)
      formatStyle(
        columns = colnames(isolate(
          values$datatable_applied_management_final()[c(1:16)]
        )),
        valueColumns = colnames(isolate(
          values$datatable_applied_management_final()[16]
        )),
        backgroundColor = styleEqual(c(-2, -1, 1, 2), c("#ff9999", "#ffcccb", "#b7e2fc", "#86cefa"))
      ) %>%
      #color first column based on the scores
      formatStyle(
        columns = 1,
        valueColumns = colnames(isolate(
          values$datatable_applied_management_final()[16]
        )),
        backgroundColor = styleEqual(c(-2, -1, 1, 2), c("#ff9999", "#ffcccb", "#b7e2fc", "#86cefa"))
      ) %>%
      #styling of STATUS
      formatStyle(columns = "status",
                  `text-align` = 'center'
      ) %>%
      #width of Reference
      formatStyle(columns = "reference",
                  width = 200)

  })

  #save datatable of applied_sequences_management as proxy for later edit changes
  proxy_applied_sequences_management <-
    dataTableProxy("applied_sequences_management")

  #observe the replacement of the applied_sequences
  observe({
    applied_sequences <-
      values$applied_sequences() #upload reactive val with telomere_sequences_management df
    colnames(applied_sequences)[1] <- "to.remove"
    applied_sequences$to.remove <-
      #add edit button to every row of the df
      paste0(
        '
        <div class="btn-group" style="width: 102px" role="group" aria-label="Basic example">
        <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="up_',
        1:nrow(applied_sequences),
        '"><i class="fa fa-thumbs-o-up"></i></button>
        <button class="btn btn-warning btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="modify_',
        1:nrow(applied_sequences),
        '"><i class="fa fa-pencil-square-o"></i></button>
        <button class="btn btn-danger btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="down_',
        1:nrow(applied_sequences),
        '"><i class="fa fa-thumbs-o-down"></i></button>
        </div>
        '
      )

    #remove sequences that user voted for before or have a score < -1 or 1 >
    if (!is.null(input$user_input))  {
      applied_sequences$to.remove[grepl(input$user_input, applied_sequences$who.voted) | applied_sequences$score %in% c(-2,2)]<- " "

      values$number_to_evaluate(length(applied_sequences$to.remove[!grepl(input$user_input, applied_sequences$who.voted) & applied_sequences$score %in% c(1,0,-1)]))
    }

    rownames(applied_sequences) <-
      #add empty cell to each rowname so the button to expanse is alone
      paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <p data-placement="top" id="modify_',
        1:nrow(applied_sequences),
        '"></p>
        </div>
        '
      )
    colnames(applied_sequences)[1] <- " "
    replaceData(proxy_applied_sequences_management,
                applied_sequences[-c(17:21)],
                resetPaging = FALSE)
  })

  values$applied_sequences_row <-
    reactiveVal({
      NA
    }) #reactive value of the row that is later filled with the row number that should be edited by buttons

  #Actions of thumbs-up, thumbs-down and edit button for the applied_sequences datatable
  observeEvent(eventExpr = input$lastClick_appliedsequences,
               handlerExpr = {
                 if (input$lastClick_appliedsequencesId %like% "up_") {
                   #save row selected to upvote as a reactive value number for action button in modal to handle
                   row_to_upvote = as.numeric(gsub("up_", "", input$lastClick_appliedsequencesId))
                   values$applied_sequences_row(row_to_upvote)

                   #modal dialog for confirmation of the row to be upvoted
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = HTML(
                         '<div style="text-align: center;">
                         <span style="color:#3a88b6;font-size:200%;">
                         <i class="fa fa-thumbs-o-up"></i>
                         </span>
                         </div>'
                       ),
                       size = c("s"),
                       "Do you want to upvote this sequence?",
                       footer = tagList(
                         actionButton("upvote_sequence_row", "Yes"),
                         modalButton('Cancel')
                       )
                       )
                     )

                 } else if (input$lastClick_appliedsequencesId %like% "down_") {
                   #save row selected to upvote as a reactive value number for action button in modal to handle
                   row_to_downvote = as.numeric(gsub("down_", "", input$lastClick_appliedsequencesId))
                   values$applied_sequences_row(row_to_downvote)

                   #modal dialog for confirmation of the row to be upvoted
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = HTML(
                         '<div style="text-align: center;">
                         <span style="color:#dd4b39;font-size:200%;">
                         <i class="fa fa-thumbs-o-down"></i>
                         </span>
                         </div>'
                       ),
                       size = c("s"),
                       "Do you want to downvote this sequence?",
                       footer = tagList(
                         actionButton("downvote_sequence_row", "Yes"),
                         modalButton('Cancel')
                       )
                       )
                     )

                 } else if (input$lastClick_appliedsequencesId %like% "modify_") {
                   #save row selected to edit as a reactive value number for action button in modal to handle
                   row_to_edit = as.numeric(gsub("modify_", "", input$lastClick_appliedsequencesId))
                   values$applied_sequences_row(row_to_edit)

                   applied_sequences <-
                     values$applied_sequences() #upload reactive val with applied_sequences df
                   #modal dialog for edit of the row
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = "Edit row",
                       size = c("l"),
                       fluidPage(
                         column(
                           6,
                           #style for the selectInput
                           tags$head(
                             tags$style(".selectize-input {border-radius:0px}"),
                             tags$style(
                               ".selectize-input.dropdown-active {border-radius:0px}"
                             ),
                             tags$style(".selectize-dropdown {border-radius:0px}"),
                             tags$style(
                               HTML(
                                 '
                                 .selectize-input.focus {
                                 border-color:#3c8dbc;
                                 outline: 0;
                                 -webkit-box-shadow:none !important;
                                 box-shadow:none !important;}
                                 '
                               )
                               )
                               ),
                           textInput(
                             inputId = "name_row_applied",
                             label = "Name",
                             value = applied_sequences$name[row_to_edit]
                           ),
                           textInput(
                             inputId = "sequence_row_applied",
                             label = "Sequence",
                             value = applied_sequences$sequence[row_to_edit]
                           ),
                           textInput(
                             inputId = "location_row_applied",
                             label = "Location",
                             value = applied_sequences$location[row_to_edit]
                           ),
                           selectInput(
                             inputId = "status_row_applied",
                             label = "Status",
                             choices = c("MODEL", "EXPERIMENTAL"),
                             selected = applied_sequences$status[row_to_edit]
                           ),
                           textInput(
                             inputId = "reference_row_applied",
                             label = "Reference (e.g., Smith et al. 2020)",
                             value = gsub('.*>' ,"", gsub('</a>', "", applied_sequences$reference[row_to_edit]))
                           ),
                           textInput(
                             inputId = "doi_row_applied",
                             label = "DOI (https://doi.org/ included)",
                             value = gsub('<a href=\"' ,"", gsub('" target=\"_blank\">.*', "", applied_sequences$reference[row_to_edit]))
                           ),
                           textInput(
                             inputId = "species_row_applied",
                             label = "Species",
                             value = applied_sequences$species[row_to_edit]
                           )
                               ),

                         column(
                           6,
                           textInput(
                             inputId = "genus_row_applied",
                             label = "Genus",
                             value = applied_sequences$genus[row_to_edit]
                           ),
                           textInput(
                             inputId = "family_row_applied",
                             label = "Family",
                             value = applied_sequences$family[row_to_edit]
                           ),
                           textInput(
                             inputId = "order_row_applied",
                             label = "Order",
                             value = applied_sequences$order[row_to_edit]
                           ),
                           textInput(
                             inputId = "class_row_applied",
                             label = "Class",
                             value = applied_sequences$class[row_to_edit]
                           ),
                           textInput(
                             inputId = "phylum_row_applied",
                             label = "Phylum",
                             value = applied_sequences$phylum[row_to_edit]
                           ),
                           textInput(
                             inputId = "kingdom_row_applied",
                             label = "Kingdom",
                             value = applied_sequences$kingdom[row_to_edit]
                           )
                         )

                           ),
                       footer = tagList(
                         actionButton("applied_sequences_edit", "Edit"),
                         modalButton('Cancel')
                       )
                         )
                       )
                 }
               })

  #Action of the thumbs-up button in the confirmation modal dialog
  observeEvent(
    eventExpr = input$upvote_sequence_row,
    handlerExpr = {
      #rewrite telomere_sequences.csv with deleted value
      applied_sequences <-
        values$applied_sequences() #upload reactive val with accs df
      row_to_upvote <-
        values$applied_sequences_row() #upload which row is clicked

      applied_sequences$score[row_to_upvote] <-
        applied_sequences$score[row_to_upvote] + 1

      applied_sequences$who.voted[row_to_upvote] <-
        paste0(applied_sequences$who.voted[row_to_upvote],";", input$user_input)

      #when voted value will reach +2, move telomere sequence to the database
      if (applied_sequences$score[row_to_upvote] > 1) {
        telomere_sequences <-
          values$telomere_sequences_management() #upload reactive val with telomere sequences management df

        new_telomere_sequence_row<- applied_sequences[row_to_upvote,1:15]

        telomere_sequences_with_new_row <-
          rbind(telomere_sequences, new_telomere_sequence_row) #bind newly created user row with the telomere sequences loaded from the start

        #order telomere sequences
        reference_dates<- as.numeric(str_sub(telomere_sequences_with_new_row$reference,-8,-5)) #create vector with dates of published articles
        telomere_sequences_with_new_row<- cbind(telomere_sequences_with_new_row,reference_dates) #bind it to the dataframe

        telomere_sequences_with_new_row<-
          telomere_sequences_with_new_row[
            with(telomere_sequences_with_new_row, order(kingdom, phylum, class, order, family, genus, species, status, reference_dates, -as.numeric(FTELS))),
            ]
        telomere_sequences_with_new_row<- telomere_sequences_with_new_row[-ncol(telomere_sequences_with_new_row)] #remove the reference dates column

        #save modified values to telomere_sequences_management and update telomere_sequences.csv
        values$telomere_sequences_management(telomere_sequences_with_new_row)
        write.csv2(telomere_sequences_with_new_row[-1],
                   file = "./data/telomere_sequences.csv",
                   row.names = FALSE) #rewrite telomere_sequences.csv
      }

      #when voted value will reach +2, move user who sent the application to the accs and send him an email about his sucessful registration
      if (applied_sequences$score[row_to_upvote] > 1 & !is.na(applied_sequences$users[row_to_upvote])) {
        accs <-
          values$accs() #upload reactive val with accs df

        new_user_row<- applied_sequences[row_to_upvote,18:21]

        accs_with_new_row <-
          rbind(accs, new_user_row) #bind newly created user row with the accs loaded from the start

        #save modified values to accs and update accs.csv
        values$accs(accs_with_new_row)
        write.csv2(accs_with_new_row,
                   file = "./data/accs.csv",
                   row.names = FALSE) #rewrite accs.csv

        #remove from applied_sequences the e-mail adress
        applied_sequences[row_to_upvote,20]<- NA

        #send an email that user is registred
        email <- envelope(
          to = new_user_row[1,3],
          from = Sys.getenv("SMTP_USER"),
          subject = "Confirmation of your registration to the TeloBase",
          text = "Your sequence have been approved by the TeloBase community, hence your registration was sucessfully completed. Now you can sign in.
          Your TeloBase team"
        )

        smtp <- server(host = Sys.getenv("SMTP_HOST"),
                       port = as.integer(Sys.getenv("SMTP_PORT")),
                       username = Sys.getenv("SMTP_USER"),
                       password = Sys.getenv("SMTP_PASS"))
        smtp(email, verbose = TRUE)
      }

      #save modified values to applied_sequences and update applied_sequences.csv
      values$applied_sequences(applied_sequences)
      write.csv2(applied_sequences,
                 file = "./data/applied_sequences.csv",
                 row.names = FALSE) #rewrite applied_sequences.csv

      #close modal after upvoting and saving to applied_sequences.csv
      removeModal()
    }
  )

  #Action of the thumbs-down button in the confirmation modal dialog
  observeEvent(
    eventExpr = input$downvote_sequence_row,
    handlerExpr = {
      #rewrite telomere_sequences.csv with deleted value
      applied_sequences <-
        values$applied_sequences() #upload reactive val with applied_sequences df
      row_to_downvote <-
        values$applied_sequences_row() #upload which row is clicked

      applied_sequences$score[row_to_downvote] <-
        applied_sequences$score[row_to_downvote] - 1

      applied_sequences$who.voted[row_to_downvote] <-
        paste0(applied_sequences$who.voted[row_to_downvote],";", input$user_input)

      #when voted value will reach -2, remove the e-mail address of the user that applied for registration
      if (applied_sequences$score[row_to_downvote] < -1 & !is.na(applied_sequences$users[row_to_downvote])) {
        applied_sequences[row_to_downvote,20]<- NA
      }

      #save modified values to applied_sequences and update applied_sequences.csv
      values$applied_sequences(applied_sequences)
      write.csv2(applied_sequences,
                 file = "./data/applied_sequences.csv",
                 row.names = FALSE) #rewrite applied_sequences.csv

      #close modal after upvoting and saving to applied_sequences.csv
      removeModal()
    }
  )

  #Action of the edit button in the confirmation modal dialog
  observeEvent(
    eventExpr = input$applied_sequences_edit,
    handlerExpr = {
      applied_sequences <-
        values$applied_sequences() #upload reactive val with applied_sequences df
      row_to_edit <-
        values$applied_sequences_row() #upload which row is clicked

      applied_sequences$name[row_to_edit] <-
        input$name_row_applied
      applied_sequences$sequence[row_to_edit] <-
        input$sequence_row_applied
      applied_sequences$location[row_to_edit] <-
        input$location_row_applied
      applied_sequences$status[row_to_edit] <-
        input$status_row_applied
      applied_sequences$reference[row_to_edit] <-
        paste('<a href=\"',input$doi_row_applied,'" target=\"_blank\">',input$reference_row_applied,'</a>', sep = "")
      applied_sequences$species[row_to_edit] <-
        input$species_row_applied
      applied_sequences$genus[row_to_edit] <-
        input$genus_row_applied
      applied_sequences$family[row_to_edit] <-
        input$family_row_applied
      applied_sequences$order[row_to_edit] <-
        input$order_row_applied
      applied_sequences$class[row_to_edit] <-
        input$class_row_applied
      applied_sequences$phylum[row_to_edit] <-
        input$phylum_row_applied
      applied_sequences$kingdom[row_to_edit] <-
        input$kingdom_row_applied

      applied_sequences$score[row_to_edit] <- 0 #restart score
      applied_sequences$who.voted[row_to_edit] <- NA #restart users who have voted

      #save modified values to applied_sequences and update applied_sequences.csv
      values$applied_sequences(applied_sequences)
      write.csv2(applied_sequences,
                 file = "./data/applied_sequences.csv",
                 row.names = FALSE) #rewrite applied_sequences.csv
      removeModal()#close modal after editing and saving to applied_sequences.csv
    }
  )

  ## END - SUBMITTED SEQUENCES TAB ##



  #### START - REPORTS TAB ####

  output$reported_sequences_box <- renderUI({
    fluidPage(
      column(
        width = 9,
        box(
          width = NULL,
          title = "Reports",
          status = "danger",
          dataTableOutput("reported_sequences_management"),
          tags$script(
            "$(document).on('click', '#reported_sequences_management button', function () {
            Shiny.onInputChange('lastClick_reportedsequencesId',this.id);
            Shiny.onInputChange('lastClick_reportedsequences', Math.random())});"
        )
        )
          ),
      column(width = 3,
             box(width = NULL,
                 title = "Management",
                 "When you finish solving each report, please do validate that report was checked and solved by buttons (either approved and changed (check) or remained unchanged (cross).",
                 background = "red")
      ))
  })

  #Datatable of the reports
  output$reported_sequences_management <- renderDataTable({
    reported_sequences <-
      values$datatable_applied_reports_final() #upload reactive val with reports df
    colnames(reported_sequences)[1] <- "to.remove"
    reported_sequences$to.remove <-
      #add check and cross button to the every row of the df
      paste0(
        '
        <div class="btn-group" style="width: 102px" role="group" aria-label="Basic example">
        <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="up_',
        1:nrow(reported_sequences),
        '"><i class="fa fa-check"></i></button>
        <button class="btn btn-danger btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="down_',
        1:nrow(reported_sequences),
        '"><i class="fa fa-times"></i></button>
        </div>
        '
      )

    reported_sequences$comment <-
      #add check and cross button to the every row of the df
      paste0(
        '
        <div class="btn-group" style="width: 102px" role="group" aria-label="Basic example">
        <button class="btn btn-default btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="show_',
        1:nrow(reported_sequences),
        '">Show</button>
        </div>
        '
      )

    #remove sequences that have a score -1 or 1
    reported_sequences$to.remove[reported_sequences$score %in% c(-1,1)]<- " "

    values$number_to_evaluate_reports(length(reported_sequences$to.remove[reported_sequences$score %in% c(0)]))

    rownames(reported_sequences) <-
      #add empty space as a row space for collapsing buttons
      paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <p data-placement="top" id="modify_',
        1:nrow(reported_sequences),
        '"></p>
        </div>
        '
      )

    colnames(reported_sequences)[1] <- " "
    values$datatable_applied_reports_final(reported_sequences)
    datatable(
      isolate(values$datatable_applied_reports_final()),
      #render reported_sequences for the website
      class = "compact",
      extensions = c("Responsive"),
      escape = FALSE,
      selection = "none",
      style = "bootstrap4",
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 50, 100),
        search = list(regex = TRUE),
        ordering = FALSE
      )
    ) %>%

      #vertical align
      formatStyle(columns = colnames(isolate(
        values$datatable_applied_reports_final()
      )),
      `vertical-align` = 'middle', height = 30) %>%
      #colour whole row beside for the 1 column based on the scores (for some reason without name is ignored)
      formatStyle(
        columns = colnames(isolate(
          values$datatable_applied_reports_final()
        )),
        valueColumns = colnames(isolate(
          values$datatable_applied_reports_final()[6]
        )),
        backgroundColor = styleEqual(c(-1, 1), c("#ff9999", "#86cefa"))
      ) %>%
      #color first column based on the scores
      formatStyle(
        columns = 1,
        valueColumns = colnames(isolate(
          values$datatable_applied_reports_final()[6]
        )),
        backgroundColor = styleEqual(c(-1, 1), c("#ff9999", "#86cefa"))
      ) %>%
      #width of Reference
      formatStyle(columns = "reference",
                  width = 200)

  })

  #save datatable of reported_sequences_management as proxy for later edit changes
  proxy_reported_sequences_management <-
    dataTableProxy("reported_sequences_management")

  #observe the replacement of the reports
  observe({
    reported_sequences <-
      values$applied_reports() #upload reactive val with telomere_sequences_management df
    colnames(reported_sequences)[1] <- "to.remove"
    reported_sequences$to.remove <-
      #add edit button to every row of the df
      paste0(
        '
        <div class="btn-group" style="width: 102px" role="group" aria-label="Basic example">
        <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="up_',
        1:nrow(reported_sequences),
        '"><i class="fa fa-check"></i></button>
        <button class="btn btn-danger btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="down_',
        1:nrow(reported_sequences),
        '"><i class="fa fa-times"></i></button>
        </div>
        '
      )

    reported_sequences$comment <-
      #add check and cross button to the every row of the df
      paste0(
        '
        <div class="btn-group" style="width: 102px" role="group" aria-label="Basic example">
        <button class="btn btn-default btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="show_',
        1:nrow(reported_sequences),
        '">Show</button>
        </div>
        '
      )

    #remove sequences that have a score -1 or 1
    reported_sequences$to.remove[reported_sequences$score %in% c(-1,1)]<- " "

    values$number_to_evaluate_reports(length(reported_sequences$to.remove[reported_sequences$score %in% c(0)]))

    rownames(reported_sequences) <-
      #add empty cell to each rowname so the button to expanse is alone
      paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <p data-placement="top" id="modify_',
        1:nrow(reported_sequences),
        '"></p>
        </div>
        '
      )
    colnames(reported_sequences)[1] <- " "
    replaceData(proxy_reported_sequences_management,
                reported_sequences,
                resetPaging = FALSE)
  })

  values$reported_sequences_row <-
    reactiveVal({
      NA
    }) #reactive value of the row that is later filled with the row number that should be edited by buttons

  #Actions of check, cross and show button for the reported_sequences datatable
  observeEvent(eventExpr = input$lastClick_reportedsequences,
               handlerExpr = {
                 if (input$lastClick_reportedsequencesId %like% "up_") {
                   #save row selected to upvote as a reactive value number for action button in modal to handle
                   row_to_check = as.numeric(gsub("up_", "", input$lastClick_reportedsequencesId))
                   values$reported_sequences_row(row_to_check)

                   #modal dialog for confirmation of the row to be upvoted
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = HTML(
                         '<div style="text-align: center;">
                         <span style="color:#3a88b6;font-size:200%;">
                         <i class="fa fa-check"></i>
                         </span>
                         </div>'
                       ),
                       size = c("s"),
                       "Were the sequences changed?",
                       footer = tagList(
                         actionButton("check_sequence_row", "Yes"),
                         modalButton('Cancel')
                       )
                       )
                     )

                 } else if (input$lastClick_reportedsequencesId %like% "down_") {
                   #save row selected to upvote as a reactive value number for action button in modal to handle
                   row_to_cross = as.numeric(gsub("down_", "", input$lastClick_reportedsequencesId))
                   values$reported_sequences_row(row_to_cross)

                   #modal dialog for confirmation of the row to be upvoted
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = HTML(
                         '<div style="text-align: center;">
                         <span style="color:#dd4b39;font-size:200%;">
                         <i class="fa fa-times"></i>
                         </span>
                         </div>'
                       ),
                       size = c("s"),
                       "Were the changes ignored?",
                       footer = tagList(
                         actionButton("cross_sequence_row", "Yes"),
                         modalButton('Cancel')
                       )
                       )
                     )

                 } else if (input$lastClick_reportedsequencesId %like% "show_") {
                   #save row selected to upvote as a reactive value number for action button in modal to handle
                   row_to_show = as.numeric(gsub("show_", "", input$lastClick_reportedsequencesId))
                   values$reported_sequences_row(row_to_show)

                   #modal dialog for confirmation of the row to be upvoted
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = HTML(
                         '<div style="text-align: center;">
                         <span style="font-size:200%;">
                         Comment
                         </span>
                         </div>'
                       ),
                       size = c("s"),
                       paste(values$applied_reports()$comment[values$reported_sequences_row()]),
                       footer = tagList(
                         modalButton('Cancel')
                       )
                     )
                   )

                 }
               })

  #Action of the check button in the confirmation modal dialog
  observeEvent(
    eventExpr = input$check_sequence_row,
    handlerExpr = {

      reported_sequences <-
        values$applied_reports() #upload reactive val with reports df
      row_to_check <-
        values$reported_sequences_row() #upload which row is clicked

      reported_sequences$score[row_to_check] <-
        reported_sequences$score[row_to_check] + 1

      #save modified values to applied_reports and update reports.csv
      values$applied_reports(reported_sequences)
      write.csv2(reported_sequences,
                 file = "./data/reports.csv",
                 row.names = FALSE) #rewrite reports.csv

      #close modal after upvoting and saving to reports.csv
      removeModal()
    }
  )

  #Action of the cross button in the confirmation modal dialog
  observeEvent(
    eventExpr = input$cross_sequence_row,
    handlerExpr = {

      reported_sequences <-
        values$applied_reports() #upload reactive val with reports df
      row_to_cross <-
        values$reported_sequences_row() #upload which row is clicked

      reported_sequences$score[row_to_cross] <-
        reported_sequences$score[row_to_cross] - 1

      #save modified values to applied_reports and update reports.csv
      values$applied_reports(reported_sequences)
      write.csv2(reported_sequences,
                 file = "./data/reports.csv",
                 row.names = FALSE) #rewrite reports.csv

      #close modal after upvoting and saving to reports.csv
      removeModal()
    }
  )

  ## END - REPORTS TAB ##



  #### START - ADMIN TAB ####

  output$admin_box <- renderUI({
    column(width = 12,
           column(
             width = 9,
             tabBox(
               width = NULL,
               id = "admin_tab",
               tabPanel(
                 "Current users",
                 dataTableOutput("current_users"),
                 tags$script(
                   "$(document).on('click', '#current_users button', function () {
                   Shiny.onInputChange('lastClick_accsId',this.id);
                   Shiny.onInputChange('lastClick_accs', Math.random())});"
               ),
               actionLink(
                 inputId = "create_user",
                 label = "Create user",
                 icon = icon("plus")
               )
               ),

               tabPanel(
                 "Telomere sequences management",
                 dataTableOutput("sequences_management"),
                 tags$script(
                   "$(document).on('click', '#sequences_management button', function () {
                   Shiny.onInputChange('lastClick_telomeresId',this.id);
                   Shiny.onInputChange('lastClick_telomeres', Math.random())});"
               )
               ),
               tabPanel("Core files management",
                        fluidPage(column(
                          12,
                          box(
                            width = NULL,
                            title = "Accounts table",
                            status = "warning",
                            fileInput(
                              inputId = "accs_upload",
                              label = "Upload accs.csv",
                              accept = ".csv"
                            ),
                            uiOutput("ui.upload_accs"),
                            br(),
                            downloadButton(outputId = "download_accs", label = "Download accs.csv"),
                            downloadButton(outputId = "download_dump_accs", label = "Download dump_accs.csv")
                          ),

                          box(
                            width = NULL,
                            title = "Telomere sequences table",
                            status = "primary",
                            fileInput(
                              inputId = "telomere_sequences_upload",
                              label = "Upload telomere_sequences.csv",
                              accept = ".csv"
                            ),
                            uiOutput("ui.upload_telomere_sequences"),
                            br(),
                            actionButton(inputId = "reorder_sequences", label = "Reorder sequences"),
                            downloadButton(outputId = "download_telomere_sequences", label = "Download telomere_sequences.csv"),
                            downloadButton(outputId = "download_dump_telomere_sequences", label = "Download dump_telomere_sequences.csv")
                          ),

                          box(
                            width = NULL,
                            title = "Submitted sequences table",
                            status = "warning",
                            downloadButton(outputId = "download_applied_sequences", label = "Download applied_sequences.csv")
                          )
                        )))
                 )
           ),
           column(
             width = 3,
             box(
               width = NULL,
               title = "File Upload",
               background = "red",
               "When uploading new accounts or sequences as .csv file, old files are backed up on the server in old_file folder without direct access.
               Dump files store records that were interactively deleted through the database system."
             )
             ))

  })


  #Current users datatable #####


  output$current_users <- renderDataTable({
    accs <- values$accs_final() #upload reactive val with accs df
    rownames(accs) <-
      #add edit and delete button to every row of the df
      paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <button class="btn btn-warning btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="modify_',
        1:nrow(accs),
        '"><i class="fa fa-pencil-square-o"></i></button>
        <button class="btn btn-danger btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="delete_',
        1:nrow(accs),
        '"><i class="fa fa-trash-o"></i></button>
        </div>
        '
      )
    values$accs_final(accs)
    datatable(
      isolate(values$accs_final()[-4]),
      #render accs for the website
      escape = FALSE,
      selection = "none",
      style = "bootstrap4",
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 50, 100)
      )
    )%>%

      #vertical align
      formatStyle(columns = colnames(isolate(values$accs_final()[-4])),
      `vertical-align` = 'middle', height = 30)

  })

  #save datatable of accs as proxy for later delete and edit changes
  proxy_accs <- dataTableProxy("current_users")

  #observe the replacement of the accs using edit and delete button
  observe({
    accs <- values$accs() #upload reactive val with accs df
    rownames(accs) <-
      #add edit and delete button to every row of the df
      paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <button class="btn btn-warning btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="modify_',
        1:nrow(accs),
        '"><i class="fa fa-pencil-square-o"></i></button>
        <button class="btn btn-danger btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="delete_',
        1:nrow(accs),
        '"><i class="fa fa-trash-o"></i></button>
        </div>
        '
      )
    replaceData(proxy_accs, accs[-4], resetPaging = FALSE)
  })


  values$accs_row <-
    reactiveVal({
      NA
    }) #reactive value of the row that is later filled with the row number that should be edited or deleted by buttons

  #Actions of edit and delete button for the current users datatable
  observeEvent(eventExpr = input$lastClick_accs,
               handlerExpr = {
                 if (input$lastClick_accsId %like% "delete") {
                   #save row selected to delete as a reactive value number for action button in modal to handle
                   row_to_del = as.numeric(gsub("delete_", "", input$lastClick_accsId))
                   values$accs_row(row_to_del)

                   #modal dialog for confirmation of the row to be deleted
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = HTML(
                         '
                         <div style="text-align: center;">
                         <span style="color:#dd4b39;font-size:200%;">
                         <i class="fas fa-exclamation"></i>
                         </span>
                         </div>
                         '
                       ),
                       size = c("s"),
                       "Do you want to delete the record?",
                       footer = tagList(
                         actionButton("delete_accs_row", "Delete"),
                         modalButton('Cancel')
                       )
                       )
                     )

                 } else if (input$lastClick_accsId %like% "modify") {
                   #save row selected to edit as a reactive value number for action button in modal to handle
                   row_to_edit = as.numeric(gsub("modify_", "", input$lastClick_accsId))
                   values$accs_row(row_to_edit)

                   accs <-
                     values$accs() #upload reactive val with accs df
                   #modal dialog for edit of the row
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = "Edit row",
                       size = c("m"),
                       fluidPage(
                         column(
                           6,
                           useShinyFeedback(),
                           #style for the selectInput
                           tags$head(
                             tags$style(".selectize-input {border-radius:0px}"),
                             tags$style(".selectize-input.dropdown-active {border-radius:0px}"),
                             tags$style(".selectize-dropdown {border-radius:0px}"),
                             tags$style(
                               HTML(
                                 '
                                 .selectize-input.focus {
                                 border-color:#3c8dbc;
                                 outline: 0;
                                 -webkit-box-shadow:none !important;
                                 box-shadow:none !important;
                                 }
                                 '
                               )
                               )
                               ),
                           textInput(
                             inputId = "users_row_edit",
                             label = "User",
                             value = accs$users[row_to_edit]
                           ),


                           selectInput(
                             inputId = "rights_row_edit",
                             label = "Rights",
                             choices = c("admin", "user"),
                             selected = accs$rights[row_to_edit]
                           ),


                           textInput(
                             inputId = "contact_row_edit",
                             label = "Contact",
                             value = accs$contact[row_to_edit]
                           )
                               ),
                         column(
                           6,
                           passwordInput(inputId = "password_row_edit", label = "New password")
                         )

                           ),
                       footer = tagList(actionButton("accs_edit", "Edit"),
                                        modalButton('Cancel'))
                         )
                       )
                 }
               })

  #Action of the delete button in the confirmation modal dialog
  observeEvent(eventExpr = input$delete_accs_row,
               handlerExpr = {
                 #rewrite accs.csv with deleted value
                 accs <-
                   values$accs() #upload reactive val with accs df
                 row_to_del <-
                   values$accs_row() #upload which row is clicked
                 values$accs(accs[-row_to_del,])
                 write.csv2(accs[-row_to_del,], file = "./data/accs.csv", row.names = FALSE)

                 #save deleted value to the dump_accs.csv
                 dump_accs <-
                   read.csv2("./dump_data/dump_accs.csv", stringsAsFactors = FALSE)
                 new_dump_accs <-
                   rbind(dump_accs, accs[row_to_del,])
                 write.csv2(new_dump_accs, file = "./dump_data/dump_accs.csv", row.names = FALSE)

                 #close modal after deleting and saving to dump_accs.csv
                 removeModal()
               })

  #Action of the edit button in the confirmation modal dialog
  observeEvent(eventExpr = input$accs_edit,
               handlerExpr = {
                 hideFeedback("users_row_edit")
                 hideFeedback("contact_row_edit")

                 accs <-
                   values$accs() #upload reactive val with accs df
                 row_to_edit <-
                   values$accs_row() #upload which row is clicked

                 pass <-
                   TRUE #if TRUE, edit is executed, otherwise shinyFeedback is shown

                 if (input$users_row_edit != "") {
                   if (any(accs$users[-row_to_edit] == input$users_row_edit)) {
                     showFeedbackDanger(inputId = "users_row_edit", text = "Username already used.")
                     pass <- FALSE
                   }

                 } else {
                   showFeedbackDanger(inputId = "users_row_edit", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$contact_row_edit != "") {
                   if (!grepl(input$contact_row_edit,
                              pattern = "@",
                              fixed = TRUE) |
                       !grepl(input$contact_row_edit,
                              pattern = ".",
                              fixed = TRUE)) {
                     showFeedbackDanger(inputId = "contact_row_edit", text = "Not an e-mail format.")
                     pass <- FALSE
                   }

                 } else {
                   showFeedbackDanger(inputId = "contact_row_edit", text = "Empty")
                   pass <- FALSE
                 }

                 if (pass == TRUE) {
                   accs$users[row_to_edit] <- input$users_row_edit
                   accs$rights[row_to_edit] <- input$rights_row_edit
                   accs$contact[row_to_edit] <-
                     input$contact_row_edit
                   if (input$password_row_edit != "") {
                     accs$password[row_to_edit] <-
                       password_store(input$password_row_edit)
                   }

                   #save modified values to accs and update accs.csv
                   values$accs(accs)
                   write.csv2(accs, file = "./data/accs.csv", row.names = FALSE) #rewrite accs.csv
                   removeModal()#close modal after editing and saving to accs.csv
                 }
               })

  #Actions of Create user actionlink
  observeEvent(eventExpr = input$create_user,
               handlerExpr = {
                 accs <- values$accs() #upload reactive val with accs df
                 #modal dialog for edit of the row
                 showModal(
                   modalDialog(
                     easyClose = TRUE,
                     title = "Create user",
                     size = c("m"),
                     fluidPage(
                       column(
                         6,
                         useShinyFeedback(),
                         textInput(
                           inputId = "users_row_create",
                           label = "User",
                           value = ""
                         ),

                         #style for the selectInput
                         tags$head(
                           tags$style(".selectize-input {border-radius:0px}"),
                           tags$style(".selectize-input.dropdown-active {border-radius:0px}"),
                           tags$style(".selectize-dropdown {border-radius:0px}"),
                           tags$style(
                             HTML(
                               '
                               .selectize-input.focus {
                               border-color:#3c8dbc;
                               outline: 0;
                               -webkit-box-shadow:none !important;
                               box-shadow:none !important;
                               }
                               '
                             )
                             )
                             ),
                         selectInput(
                           inputId = "rights_row_create",
                           label = "Rights",
                           choices = c("admin", "user"),
                           selected = "user"
                         ),


                         textInput(
                           inputId = "contact_row_create",
                           label = "Contact",
                           value = ""
                         )
                             ),
                       column(
                         6,
                         passwordInput(
                           inputId = "password_row_first_create",
                           label = "Password",
                           value = ""
                         ),
                         passwordInput(
                           inputId = "password_row_second_create",
                           label = "Repeat Password",
                           value = ""
                         )
                       )

                         ),
                     footer = tagList(
                       actionButton("accs_create", "Create"),
                       modalButton('Cancel')
                     )
                    )
                  )
                }
               )

  #Action of the Create user button in the confirmation modal dialog
  observeEvent(eventExpr = input$accs_create,
               handlerExpr = {
                 hideFeedback("users_row_create")
                 hideFeedback("contact_row_create")
                 hideFeedback("password_row_first_create")
                 hideFeedback("password_row_second_create")

                 accs <-
                   values$accs() #upload reactive val with accs df

                 pass <-
                   TRUE #if TRUE, edit is executed, otherwise shinyFeedback is shown

                 if (input$users_row_create != "") {
                   if (any(accs$users == input$users_row_create)) {
                     showFeedbackDanger(inputId = "users_row_create", text = "Username already used.")
                     pass <- FALSE
                   }

                 } else {
                   showFeedbackDanger(inputId = "users_row_create", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$contact_row_create != "") {
                   if (!grepl(input$contact_row_create,
                              pattern = "@",
                              fixed = TRUE) |
                       !grepl(input$contact_row_create,
                              pattern = ".",
                              fixed = TRUE)) {
                     showFeedbackDanger(inputId = "contact_row_create", text = "Not an e-mail format.")
                     pass <- FALSE
                   }

                 } else {
                   showFeedbackDanger(inputId = "contact_row_create", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$password_row_first_create == "") {
                   showFeedbackDanger(inputId = "password_row_first_create", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$password_row_second_create == "") {
                   showFeedbackDanger(inputId = "password_row_second_create", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$password_row_first_create != input$password_row_second_create) {
                   showFeedbackDanger(inputId = "password_row_first_create", text = "")
                   showFeedbackDanger(inputId = "password_row_second_create", text = "Passwords do not match.")
                   pass <- FALSE
                 }

                 if (pass == TRUE) {
                   new_user_row <-
                     c(
                       input$users_row_create,
                       input$rights_row_create,
                       input$contact_row_create,
                       password_store(input$password_row_first_create)
                     )
                   accs_with_new_row <-
                     rbind(accs, new_user_row) #bind newly created user row with the accs loaded from the start

                   #save modified values to accs and update accs.csv
                   values$accs(accs_with_new_row)
                   write.csv2(accs_with_new_row,
                              file = "./data/accs.csv",
                              row.names = FALSE) #rewrite accs.csv
                   removeModal()#close modal after editing and saving to accs.csv
                 }
               })


  # Telomere sequences management ####

  output$sequences_management <- renderDataTable({
    telomere_sequences <-
      values$datatable_management_final() #upload reactive val with telomere_sequences df
    colnames(telomere_sequences)[1] <- "to.remove"
    telomere_sequences$to.remove <-
      #add edit and delete button to every row of the df
      paste0(
        '
        <div class="btn-group" style="width: 67px" role="group" aria-label="Basic example">
        <button class="btn btn-warning btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="modify_',
        1:nrow(telomere_sequences),
        '"><i class="fa fa-pencil-square-o"></i></button>
        <button class="btn btn-danger btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="delete_',
        1:nrow(telomere_sequences),
        '"><i class="fa fa-trash-o"></i></button>
        </div>
        '
      )
    rownames(telomere_sequences) <-
      #add empty cell to each rowname so the button to expanse is alone
      paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <p data-placement="top" id="modify_',
        1:nrow(telomere_sequences),
        '"></p>
        </div>
        '
      )
    colnames(telomere_sequences)[1] <- " "
    values$datatable_management_final(telomere_sequences)
    datatable(
      isolate(values$datatable_management_final()),
      #render telomere_sequences for the website
      class = "compact",
      extensions = c("RowGroup", "Responsive"),
      escape = FALSE,
      selection = "none",
      style = "bootstrap4",
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 50, 100),
        rowGroup = list(dataSrc = c(15,9)),
        search = list(regex = TRUE),
        ordering = FALSE
      )
    ) %>%

      #vertical align
      formatStyle(columns = colnames(isolate(
        values$datatable_management_final()
      )),
      `vertical-align` = 'middle', height = 30) %>%
      #color if EXPERIMENTAL or MODEL sequence
      formatStyle(
        columns = "status",
        backgroundColor = styleEqual(c("POTENTIAL","MODEL", "EXPERIMENTAL"), c("#FCF2BE","#ffcccb", "#b7e2fc")),
        `text-align` = 'center'
      ) %>%
      #FTANS (show only two digits after . + make the colour showing %)
      formatStyle(columns = "FTANS",
                  background = styleColorBar(range(0, 1), "#b7e2fc"),
                  backgroundSize = '100% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatPercentage('FTANS', 2) %>%
      #FTELS (show only two digits after . + make the colour showing %)
      formatStyle(columns = "FTELS",
                  background = styleColorBar(range(0, 1), "#ffcccb"),
                  backgroundSize = '100% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatPercentage('FTELS', 2)

  })

  #save datatable of telomere_sequences_management as proxy for later edit changes
  proxy_sequences_management <-
    dataTableProxy("sequences_management")

  #observe the replacement of the telomere_sequences
  observe({
    telomere_sequences <-
      values$telomere_sequences_management() #upload reactive val with telomere_sequences_management df
    colnames(telomere_sequences)[1] <- "to.remove"
    telomere_sequences$to.remove <-
      #add edit button to every row of the df
      paste0(
        '
        <div class="btn-group" style="width: 67px" role="group" aria-label="Basic example">
        <button class="btn btn-warning btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="modify_',
        1:nrow(telomere_sequences),
        '"><i class="fa fa-pencil-square-o"></i></button>
        <button class="btn btn-danger btn-sm edit_btn" data-toggle="tooltip" data-placement="top" id="delete_',
        1:nrow(telomere_sequences),
        '"><i class="fa fa-trash-o"></i></button>
        </div>
        '
      )
    rownames(telomere_sequences) <-
      #add empty space so the expanse button doesnt have rownames showing
      paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <p data-placement="top" id="modify_',
        1:nrow(telomere_sequences),
        '"></p>
        </div>
        '
      )
    colnames(telomere_sequences)[1] <- " "
    replaceData(proxy_sequences_management,
                telomere_sequences,
                resetPaging = FALSE)
  })

  values$telomere_sequences_row <-
    reactiveVal({
      NA
    }) #reactive value of the row that is later filled with the row number that should be edited by buttons

  #Actions of edit and delete button for the telomere_sequence datatable
  observeEvent(eventExpr = input$lastClick_telomeres,
               handlerExpr = {
                 if (input$lastClick_telomeresId %like% "delete_") {
                   #save row selected to delete as a reactive value number for action button in modal to handle
                   row_to_del = as.numeric(gsub("delete_", "", input$lastClick_telomeresId))
                   values$telomere_sequences_row(row_to_del)

                   #modal dialog for confirmation of the row to be deleted
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = HTML(
                         '<div style="text-align: center;">
                         <span style="color:#dd4b39;font-size:200%;">
                         <i class="fas fa-exclamation"></i>
                         </span>
                         </div>'
                       ),
                       size = c("s"),
                       "Do you want to delete the record?",
                       footer = tagList(
                         actionButton("delete_telomere_sequences_management_row", "Delete"),
                         modalButton('Cancel')
                       )
                       )
                     )

                 } else if (input$lastClick_telomeresId %like% "modify_") {
                   #save row selected to edit as a reactive value number for action button in modal to handle
                   row_to_edit = as.numeric(gsub("modify_", "", input$lastClick_telomeresId))
                   values$telomere_sequences_row(row_to_edit)

                   telomere_sequences <-
                     values$telomere_sequences_management() #upload reactive val with telomere_sequences df
                   #modal dialog for edit of the row
                   showModal(
                     modalDialog(
                       easyClose = TRUE,
                       title = "Edit row",
                       size = c("l"),
                       fluidPage(
                         column(
                           6,
                           textInput(
                             inputId = "name_row_edit",
                             label = "Name",
                             value = telomere_sequences$name[row_to_edit]
                           ),

                           #style for the selectInput
                           tags$head(
                             tags$style(".selectize-input {border-radius:0px}"),
                             tags$style(
                               ".selectize-input.dropdown-active {border-radius:0px}"
                             ),
                             tags$style(".selectize-dropdown {border-radius:0px}"),
                             tags$style(
                               HTML(
                                 '
                                 .selectize-input.focus {
                                 border-color:#3c8dbc;
                                 outline: 0;
                                 -webkit-box-shadow:none !important;
                                 box-shadow:none !important;}
                                 '
                               )
                               )
                               ),
                           textInput(
                             inputId = "sequence_row_edit",
                             label = "Sequence",
                             value = telomere_sequences$sequence[row_to_edit]
                           ),
                           textInput(
                             inputId = "location_row_edit",
                             label = "Location",
                             value = telomere_sequences$location[row_to_edit]
                           ),
                           selectInput(
                             inputId = "status_row_edit",
                             label = "Status",
                             choices = c("MODEL", "EXPERIMENTAL", "POTENTIAL"),
                             selected = telomere_sequences$status[row_to_edit]
                           ),
                           textInput(
                             inputId = "reference_row_edit",
                             label = "Reference (e.g., Smith et al. 2020)",
                             value = gsub('.*>' ,"", gsub('</a>', "", telomere_sequences$reference[row_to_edit]))
                           ),
                           textInput(
                             inputId = "doi_row_edit",
                             label = "DOI (https://doi.org/ included)",
                             value = gsub('<a href=\"' ,"", gsub('" target=\"_blank\">.*', "", telomere_sequences$reference[row_to_edit]))
                           ),
                           textInput(
                             inputId = "species_row_edit",
                             label = "Species",
                             value = telomere_sequences$species[row_to_edit]
                           )
                               ),

                         column(
                           6,
                           textInput(
                             inputId = "genus_row_edit",
                             label = "Genus",
                             value = telomere_sequences$genus[row_to_edit]
                           ),
                           textInput(
                             inputId = "family_row_edit",
                             label = "Family",
                             value = telomere_sequences$family[row_to_edit]
                           ),
                           textInput(
                             inputId = "order_row_edit",
                             label = "Order",
                             value = telomere_sequences$order[row_to_edit]
                           ),
                           textInput(
                             inputId = "class_row_edit",
                             label = "Class",
                             value = telomere_sequences$class[row_to_edit]
                           ),
                           textInput(
                             inputId = "phylum_row_edit",
                             label = "Phylum",
                             value = telomere_sequences$phylum[row_to_edit]
                           ),
                           textInput(
                             inputId = "kingdom_row_edit",
                             label = "Kingdom",
                             value = telomere_sequences$kingdom[row_to_edit]
                           )
                         )

                           ),
                       footer = tagList(
                         actionButton("telomere_sequences_management_edit", "Edit"),
                         modalButton('Cancel')
                       )
                       )
                   )
                 }
               })

  #Action of the delete button in the confirmation modal dialog
  observeEvent(
    eventExpr = input$delete_telomere_sequences_management_row,
    handlerExpr = {
      #rewrite telomere_sequences.csv with deleted value
      telomere_sequences <-
        values$telomere_sequences_management() #upload reactive val with telomere_sequences df
      row_to_del <-
        values$telomere_sequences_row() #upload which row is clicked
      values$telomere_sequences_management(telomere_sequences[-row_to_del,])
      write.csv2(telomere_sequences[-row_to_del,-1],
                 file = "./data/telomere_sequences.csv",
                 row.names = FALSE)

      #save deleted value to the dump_telomere_sequences.csv
      dump_telomere_sequences <-
        read.csv2("./dump_data/dump_telomere_sequences.csv",
                  stringsAsFactors = FALSE)
      new_dump_telomere_sequences <-
        rbind(dump_telomere_sequences, telomere_sequences[row_to_del,-1])
      write.csv2(new_dump_telomere_sequences,
                 file = "./dump_data/dump_telomere_sequences.csv",
                 row.names = FALSE)

      #close modal after deleting and saving to dump_accs.csv
      removeModal()
    }
  )

  #Action of the edit button in the confirmation modal dialog
  observeEvent(
    eventExpr = input$telomere_sequences_management_edit,
    handlerExpr = {
      telomere_sequences <-
        values$telomere_sequences_management() #upload reactive val with telomere_sequences df
      row_to_edit <-
        values$telomere_sequences_row() #upload which row is clicked

      telomere_sequences$name[row_to_edit] <-
        input$name_row_edit
      telomere_sequences$sequence[row_to_edit] <-
        input$sequence_row_edit
      telomere_sequences$location[row_to_edit] <-
        input$location_row_edit
      telomere_sequences$status[row_to_edit] <-
        input$status_row_edit
      telomere_sequences$reference[row_to_edit] <-
        paste('<a href=\"',input$doi_row_edit,'" target=\"_blank\">',input$reference_row_edit,'</a>', sep = "")
      telomere_sequences$species[row_to_edit] <-
        input$species_row_edit
      telomere_sequences$genus[row_to_edit] <-
        input$genus_row_edit
      telomere_sequences$family[row_to_edit] <-
        input$family_row_edit
      telomere_sequences$order[row_to_edit] <-
        input$order_row_edit
      telomere_sequences$class[row_to_edit] <-
        input$class_row_edit
      telomere_sequences$phylum[row_to_edit] <-
        input$phylum_row_edit
      telomere_sequences$kingdom[row_to_edit] <-
        input$kingdom_row_edit

      #save modified values to telomere_sequences_management and update telomere_sequences.csv
      values$telomere_sequences_management(telomere_sequences)
      write.csv2(telomere_sequences[-1],
                 file = "./data/telomere_sequences.csv",
                 row.names = FALSE) #rewrite telomere_sequences.csv
      removeModal()#close modal after editing and saving to telomere_sequences.csv
    }
  )

  # Core files management ####

  ## Upload new accs.csv button scripts and download current .csv

  #show Upload button when upload_accs is not empty
  output$ui.upload_accs <- renderUI({
    if (is.null(filedata_accs()))
      return()

    actionButton("upload_accs", "Upload")
  })

  filedata_accs <- reactive({
    infile <- input$accs_upload
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv2(infile$datapath, stringsAsFactors = FALSE)
  })

  #uploading new accs.csv by Upload button
  observeEvent(input$upload_accs, {
    accs_upload <- filedata_accs()
    file_name <-
      paste("./old_data/",
            Sys.Date(),
            "-",
            random(1),
            random(1),
            "-accs.csv",
            sep = "")
    write.csv2(values$accs_final(),
               file = file_name,
               row.names = FALSE)
    write.csv2(accs_upload, file = "./data/accs.csv", row.names = FALSE)
    showNotification(
      "New accs.csv was uploaded.",
      closeButton = FALSE,
      duration = 1,
      type = "warning"
    )
  })

  #download accs.csv
  output$download_accs <- downloadHandler(
    filename = function() {
      paste('accs-', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      accs <- read.csv2("./data/accs.csv", stringsAsFactors = FALSE)
      write.csv2(accs, con, row.names = FALSE)
    }
  )

  #download dump_accs.csv
  output$download_dump_accs <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), '-dump_accs.csv', sep = '')
    },
    content = function(con) {
      dump_accs <-
        read.csv2("./dump_data/dump_accs.csv", stringsAsFactors = FALSE)
      write.csv2(dump_accs, con, row.names = FALSE)
    }
  )

  ## Upload new telomere_sequences.csv button scripts and download current .csv

  #show Upload button when upload_telomere_sequences is not empty
  output$ui.upload_telomere_sequences <- renderUI({
    if (is.null(input$telomere_sequences_upload))
      return()
    actionButton("upload_telomere_sequences", "Upload")
  })

  filedata_telomere_sequences <- reactive({
    infile <- input$telomere_sequences_upload
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv2(infile$datapath, stringsAsFactors = FALSE)
  })

  #uploading new telomere_sequences.csv by Upload button
  observeEvent(input$upload_telomere_sequences, {
    telomere_sequences_upload <- filedata_telomere_sequences()
    file_name <-
      paste(
        "./old_data/",
        Sys.Date(),
        "-",
        random(1),
        random(1),
        "-telomere_sequences.csv",
        sep = ""
      )
    write.csv2(
      values$telomere_sequences_management(),
      file = file_name,
      row.names = FALSE
    )
    write.csv2(telomere_sequences_upload,
               file = "./data/telomere_sequences.csv",
               row.names = FALSE)
    showNotification(
      "New telomere_sequences.csv was uploaded.",
      closeButton = FALSE,
      duration = 1,
      type = "message"
    )
  })

  output$ui.upload_telomere_sequences <- renderUI({
    if (is.null(input$telomere_sequences_upload))
      return()
    actionButton("upload_telomere_sequences", "Upload")
  })

  #Action of Reorder sequences button
  observeEvent(eventExpr = input$reorder_sequences,
               handlerExpr = {
                 telomere_sequences<- read.csv2("./data/telomere_sequences.csv", stringsAsFactors = FALSE)

                 reference_dates<- as.numeric(str_sub(telomere_sequences$reference,-8,-5)) #create vector with dates of published articles
                 telomere_sequences<- cbind(telomere_sequences,reference_dates) #bind it to the dataframe

                 telomere_sequences<-
                   telomere_sequences[
                     with(telomere_sequences, order(kingdom, phylum, class, order, family, genus, species, status, reference_dates, -as.numeric(FTELS))),
                     ]
                 telomere_sequences<- telomere_sequences[-ncol(telomere_sequences)] #remove the reference_dates column

                 write.csv2(telomere_sequences,
                            file = "./data/telomere_sequences.csv",
                            row.names = FALSE) #rewrite telomere_sequences.csv
               })

  #download telomere_sequences.csv
  output$download_telomere_sequences <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), '-telomere_sequences.csv', sep = '')
    },
    content = function(con) {
      telomere_sequences <-
        read.csv2("./data/telomere_sequences.csv", stringsAsFactors = FALSE)
      write.csv2(telomere_sequences, con, row.names = FALSE)
    }
  )

  #download dump_telomere_sequences.csv
  output$download_dump_telomere_sequences <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), '-dump_telomere_sequences.csv', sep = '')
    },
    content = function(con) {
      dump_telomere_sequences <-
        read.csv2("./dump_data/dump_telomere_sequences.csv",
                  stringsAsFactors = FALSE)
      write.csv2(dump_telomere_sequences, con, row.names = FALSE)
    }
  )

  #download applied_sequences.csv
  output$download_applied_sequences <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), '-applied_sequences.csv', sep = '')
    },
    content = function(con) {
      applied_sequences <-
        read.csv2("./data/applied_sequences.csv",
                  stringsAsFactors = FALSE)
      write.csv2(applied_sequences, con, row.names = FALSE)
    }
  )

  ## END - ADMIN TAB ##



  #### START - USER MANAGEMENT TAB ####

  output$user_management_box <- renderUI({
    box(
      width = 9,
        title = "User management",
        status = "primary",
        fluidPage(
          box(
            width = 6,
            status = "warning",
          tags$head(
            tags$style(".selectize-input {border-radius:0px}"),
            tags$style(".selectize-input.dropdown-active {border-radius:0px}"),
            tags$style(".selectize-dropdown {border-radius:0px}"),
            tags$style(
              HTML(
                '
                .selectize-input.focus {
                border-color:#3c8dbc;
                outline: 0;
                -webkit-box-shadow:none !important;
                box-shadow:none !important;
                }
                '
              )
              )
              ),
          useShinyFeedback(),
          textInput(
            inputId = "contact_row_manage",
            label = "Contact",
            value = values$accs()$contact[values$accs()$users == input$user_input]
          ),
          useShinyFeedback(),
            actionButton(inputId = "change_contact", label = "Edit")),
          box(
            width = 6,
            status = "danger",
            passwordInput(
              inputId = "password_row_first_manage",
              label = "New Password",
              value = ""
            ),
            passwordInput(
              inputId = "password_row_second_manage",
              label = "Repeat Password",
              value = ""
            ),
            actionButton(inputId = "change_password", label = "Change password"))
        ))
  })

  #changing contact by user using Edit button next to the textInput
  observeEvent(eventExpr = input$change_contact,
               handlerExpr = {

                 hideFeedback("contact_row_manage")
                 pass<- TRUE
                 if (input$contact_row_manage != "") {
                   if (!grepl(input$contact_row_manage,
                              pattern = "@",
                              fixed = TRUE) |
                       !grepl(input$contact_row_manage,
                              pattern = ".",
                              fixed = TRUE)) {
                     showFeedbackDanger(inputId = "contact_row_manage", text = "Not an e-mail format.")
                     pass <- FALSE
                   }

                 } else {
                   showFeedbackDanger(inputId = "contact_row_manage", text = "Empty")
                   pass <- FALSE
                 }

                 if (pass == TRUE ) {
                 contact<- input$contact_row_manage #save change
                 accs <-
                   values$accs() #upload reactive val with accs df
                 accs$contact[accs$users == input$user_input]<- contact
                 values$accs(accs)
                 write.csv2(accs,
                            file = "./data/accs.csv",
                            row.names = FALSE)
                 showNotification(
                   "Contact was edited.",
                   closeButton = FALSE,
                   duration = 1,
                   type = "warning"
                 )
                 }

               })

  #changing password by user using Edit button next to the textInput
  observeEvent(eventExpr = input$change_password,
               handlerExpr = {

                 hideFeedback("password_row_first_manage")
                 hideFeedback("password_row_second_manage")

                 pass<- TRUE
                 if (input$password_row_first_manage == "") {
                   showFeedbackDanger(inputId = "password_row_first_manage", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$password_row_second_manage == "") {
                   showFeedbackDanger(inputId = "password_row_second_manage", text = "Empty")
                   pass <- FALSE
                 }

                 if (input$password_row_first_manage != input$password_row_second_manage) {
                   showFeedbackDanger(inputId = "password_row_first_manage", text = "")
                   showFeedbackDanger(inputId = "password_row_second_manage", text = "Passwords do not match.")
                   pass <- FALSE
                 }

                 if (pass == TRUE ) {
                   password<- password_store(input$password_row_first_manage) #save password in a hash format
                   accs <-
                     values$accs() #upload reactive val with accs df
                   accs$password[accs$users == input$user_input]<- password
                   values$accs(accs)
                   write.csv2(accs,
                              file = "./data/accs.csv",
                              row.names = FALSE)
                   showNotification(
                     "Password was edited.",
                     closeButton = FALSE,
                     duration = 1,
                     type = "warning"
                   )
                 }

               })

  ## END - USER MANAGEMENT TAB ##

})
