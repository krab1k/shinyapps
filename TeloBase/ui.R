library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFeedback)
library(plotly)
library(utf8)

dashboardPage(
  dashboardHeader(title = "TeloBase",
                  tags$li(
                    class = "dropdown", actionLink("login_logout", "Log in")
                  )
                  ),

  dashboardSidebar(sidebarMenuOutput(outputId = "sidebar_menu")),

  dashboardBody(fluidRow(
    #Style of the dashboard
    tags$head(tags$style(
      HTML(
        '
        /*logo*/
        .skin-blue .main-header .logo {
        background-color: black;
        }
        /*logo when hovered*/
        .skin-blue .main-header .logo:hover {
        background-color: black;
        }
        /*navbar*/
        .skin-blue .main-header .navbar {
        background-color: black;
        }
        /* toggle button when hovered  */
        .skin-blue .main-header .navbar .sidebar-toggle:hover{
        background-color: #1e282c;
        }
        .shiny-notification-warning {
        border-color:#F39C12;
        background-color:#F39C12;
        color:#FFFFFF;
        }
        .shiny-notification-message {
        border-color:#0073B7;
        background-color:#0073B7;
        color:#FFFFFF;
        }

        '
      )
      )),

    tabItems(
      ## ABOUT TAB ##

      tabItem(
        tabName = "about",
        column(
          width = 5,
          box(
            width = NULL,
            title = "About",
            status = "primary",
            "TeloBase is a curated database containing computationally predicted and experimentally confirmed telomere sequences of species across the whole tree of life.",
            br(),
            br(),
            htmlOutput("number_unique_species"),
            plotlyOutput("about_tab_plot", height = '100%'),
            br(),
            "It should be noted that in some species telomeres have a high degree of sequence heterogeneity which is not addressed in the database with a consensus sequence, instead all identified variations of these sequences are shown.",
            br(),
            br(),
            "The curation of new sequences is peer-reviewed by current members of the TeloBase community. To join the TeloBase community, you need to
            make a contribution to the database that is accepted by current members.",
            br(),
            br(),
            "If you are an author or co-author of a work that is
            part of our database and want to become a member of the TeloBase community, please do contact us."
          )
          ),

        column(
          width = 4,
          box(
            width = NULL,
            title = "Citation",
            background = "blue",
            HTML("If you used our database in the research, <b>please do cite</b>:"),
            br(),
            br(),
            "Martin Lyčka, Michal Bubeník, Michal Závodník, Vratislav Peska, Petr Fajkus, Martin Demko, Jiří Fajkus, Miloslava Fojtová, TeloBase: a community-curated database of telomere sequences across the tree of life, Nucleic Acids Research, Volume 52, Issue D1, 5 January 2024, Pages D311–D321,",
            a("https://doi.org/10.1093/nar/gkad672", href="https://doi.org/10.1093/nar/gkad672", target="_blank"),
          ),

          box(
            width = NULL,
            title = "Contact",
            background = "green",
            "Have you encountered any error? Do you have any suggestions or a feedback?",
            br(),
            br(),
            HTML("Do not hesitate to contact us: <b>telobase@email.cz</b>")
          ),

          box(
            width = NULL,
            title = "Have you predicited or confirmed telomere sequence?",
            background = "yellow",
            "Please, consider sharing your findings and apply them to the TeloBase."
          ),

          box(
            width = NULL,
            title = "Have you found an article confirming or predicting telomere sequence that is not yet in TeloBase?",
            background = "red",
            "Please, consider applying these findings to our database and become a member of the TeloBase community."
          )
        ),

        column(
          width = 3,
          box(
            width = NULL,
            title = "News",
            status = "danger",
            HTML("<b>01/02/23</b> - TeloBase was launched")
            ),

          box(
            width = NULL,
            title = "Acknowledgement",
            status = "primary",
            "TeloBase is running on the servers of Biological Data Management and Analysis Core Facility of CEITEC Masaryk University, Brno.",
            br(),
            "Development was supported by the Czech Science Foundation, project 20-01331X, and Ministry of Education, Youth and Sports of the Czech Republic, INTER-COST project LTC20003.",
            br(),
            br(),
            img(src = 'logo.png', width = "100%")
          )
        )
      ),

      ## REFERENCES ##

      tabItem(tabName = "references",
              column(
              width = 12,
                box(
                  width = NULL,
                  title = "Telobase is utilizing following packages:",
                  status = "primary",
                  "Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). _shiny: Web Application Framework for R_. R
  package version 1.7.2, <https://CRAN.R-project.org/package=shiny>.",
                  br(),
                  br(),
                  "Ooms J (2022). _sodium: A Modern and Easy-to-Use Crypto Library_. R package version 1.2.1, <https://CRAN.R-project.org/package=sodium>.",
                  br(),
                  br(),
                  "Dowle M, Srinivasan A (2021). _data.table: Extension of `data.frame`_. R package version 1.14.2, <https://CRAN.R-project.org/package=data.table>.",
                  br(),
                  br(),
                  "Xie Y, Cheng J, Tan X (2022). _DT: A Wrapper of the JavaScript Library 'DataTables'_. R package version 0.25, <https://CRAN.R-project.org/package=DT>.",
                  br(),
                  br(),
                  "Henry L, Wickham H (2022). _rlang: Functions for Base Types and Core R and 'Tidyverse' Features_. R package version 1.0.6,
  <https://CRAN.R-project.org/package=rlang>.",
                  br(),
                  br(),
                  "Wickham H (2019). _stringr: Simple, Consistent Wrappers for Common String Operations_. R package version 1.4.0, <https://CRAN.R-project.org/package=stringr>.",
                  br(),
                  br(),
                  "Wickham H, François R, Henry L, Müller K (2022). _dplyr: A Grammar of Data Manipulation_. R package version 1.0.9, <https://CRAN.R-project.org/package=dplyr>.",
                  br(),
                  br(),
                  "Perrier V, Meyer F, Granjon D (2022). _shinyWidgets: Custom Inputs Widgets for Shiny_. R package version 0.7.4,
  <https://CRAN.R-project.org/package=shinyWidgets>.",
                  br(),
                  br(),
                  "C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida, 2020.",
                  br(),
                  br(),
                  "Bailey E (2022). _shinyBS: Twitter Bootstrap Components for Shiny_. R package version 0.61.1, <https://CRAN.R-project.org/package=shinyBS>.",
                  br(),
                  br(),
                  "Scott Chamberlain and Eduard Szocs (2013). taxize - taxonomic search and retrieval in R. F1000Research, 2:191. URL:
  https://f1000research.com/articles/2-191/v2",
                  br(),
                  br(),
                  "Meyer F, Perrier V (2022). _shinybusy: Busy Indicators and Notifications for 'Shiny' Applications_. R package version 0.3.1,
  <https://CRAN.R-project.org/package=shinybusy>.",
                  br(),
                  br(),
                  "Collier AB (2022). _emayili: Send Email Messages_. R package version 0.7.11, <https://CRAN.R-project.org/package=emayili>.",
                  br(),
                  br(),
                  "Grothendieck G (2018). _gsubfn: Utilities for Strings and Function Arguments_. R package version 0.7, <https://CRAN.R-project.org/package=gsubfn>.",
                  br(),
                  br(),
                  "Gagolewski M (2022). “stringi: Fast and portable character string processing in R.” _Journal of Statistical Software_, *103*(2), 1-59.
  doi:10.18637/jss.v103.i02 <https://doi.org/10.18637/jss.v103.i02>.",
                  br(),
                  br(),
                  "Foster Z, Sharpton T, Grünwald N (2017). “Metacoder: An R package for visualization and manipulation of community taxonomic diversity data.” _PLOS
  Computational Biology_, *13*(2), 1-15. doi:10.1371/journal.pcbi.1005404 <https://doi.org/10.1371/journal.pcbi.1005404>,
  <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005404>.
",
                  br(),
                  br(),
                  "Temple Lang D (2022). _RCurl: General Network (HTTP/FTP/...) Client Interface for R_. R package version 1.98-1.9, <https://CRAN.R-project.org/package=RCurl>.",
                  br(),
                  br(),
                  "Chang W, Borges Ribeiro B (2021). _shinydashboard: Create Dashboards with 'Shiny'_. R package version 0.7.2,
  <https://CRAN.R-project.org/package=shinydashboard>.",
                  br(),
                  br(),
                  "Attali D (2021). _shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds_. R package version 2.1.0,
  <https://CRAN.R-project.org/package=shinyjs>.",
                  br(),
                  br(),
                  "Merlino A, Howard P (2021). _shinyFeedback: Display User Feedback in Shiny Apps_. R package version 0.4.0, <https://CRAN.R-project.org/package=shinyFeedback>.",
                  br(),
                  br(),
                  "Perry PO (2021). _utf8: Unicode Text Processing_. R package version 1.2.2, <https://CRAN.R-project.org/package=utf8>."
                )
              )
      ),

      ## TELOMERE SEQUENCES TAB ##

      tabItem(tabName = "telomere_sequences",
              uiOutput("telomere_sequences_box")),


      ## APPLY SEQUENCE TAB ##

      tabItem(tabName = "apply_sequence",
              uiOutput("apply_sequences_box")),


      ## REPORT SEQUENCE TAB ##

      tabItem(tabName = "report_sequence",
              uiOutput("report_sequences_box")),


      ## APPLIED SEQUENCES TAB ##

      tabItem(tabName = "sequences_to_approve",
              uiOutput("applied_sequences_box")),

      ## REPORTS TAB ##

      tabItem(tabName = "reports_to_check",
              uiOutput("reported_sequences_box")),

      ## USER MANAGEMENT TAB ##

      tabItem(tabName = "user_management",
              uiOutput("user_management_box")),

      ## ADMIN TAB ##

      tabItem(tabName = "admin_tools",
              uiOutput("admin_box"))

      )

      ))

    )
