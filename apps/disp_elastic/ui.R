ui <- dashboardPage(
  dashboardHeader(title = titleWarning("Shiny DISP (v0.20)"),
                  titleWidth = 700),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("database")),
      selectInput(inputId ="search_drug",
                     label = "Ingredient",
                     choices = c(ing_choices, "Start typing to search..." = ""),
                     multiple = F,
                     selected = ing_choices[1]),
      selectInput(inputId = "search_pt",
                     label = "Adverse Event Preferred Term",
                     choices = c(pt_choices,"Start typing to search..." = ""),
                     multiple = F,
                     selected= pt_choices[1]),
      
      tags$div(class="form-group shiny-input-container",
               actionButton(inputId = "search_button",
                            label = "Search",
                            width = '90%')
      ),
      tags$h3(strong("Current Query:")),
      tableOutput("current_search"),
      downloadButton(outputId = "pt_data_dl",
                     label = "Export PT data"),
      menuItem("Documentation", tabName = "Documentation", icon = icon("flag")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"), selected = TRUE)
    )
  ), 
  
  dashboardBody(
    customCSS(),
    tabItems(
      tabItem(tabName = "data",
              fluidRow(width=12,
                
                column(12,
                       h3(textOutput("current_pt_title")),
                       plotlyOutput( "timeplot_pt"))
                ),
              
               fluidRow(width=12,
                  tabBox(id="tabbox",width=12,
                  
                  tabPanel(
                    title="PRR",
                    DT::dataTableOutput("table_pt_prr")
                    #tags$p("NOTE: The above table is ranked decreasingly by PRR value. All drug*reactions pairs that have PRR value of infinity are added at the end of the table."),
                    ),
                  tabPanel(
                    title="BCPNN",
                    DT::dataTableOutput("table_pt_bcpnn")
                    #tags$p("NOTE: The above table is ranked decreasingly by PRR value. All drug*reactions pairs that have PRR value of infinity are added at the end of the table."),
                    ),
                  tabPanel(
                    title="RFET",
                    DT::dataTableOutput("table_pt_rfet")
                    #tags$p("NOTE: The above table is ranked decreasingly by PRR value. All drug*reactions pairs that have PRR value of infinity are added at the end of the table."),
                    )
                  ))
              
      ),
      
      tabItem(tabName = "Documentation",
              fluidRow(
                box(
                  tabPanel(
                    "Documentation",
                    withMathJax(includeMarkdown("/home/shared/DISP data/CopyOfDISP about/DISP_about.md")),
                    tags$br(),
                    width = 12),
                  width=12
                )
              )
      ),
      
      tabItem(tabName = "aboutinfo", box(
        width = 12,
        h2("About"),
        # using tags$p() and tags$a() inserts spaces between text and hyperlink...thanks R
        HTML(paste0(
          "<p>",
          "This app has been developed by the Data Sciences Unit of RMOD at Health Canada as part of the Open Data Initiative. ",
          "This is a prototyping platform to utilize open data sources (Canada Vigilance Adverse Reaction Online Database) ",
          "to conduct disproportionality analysis for safety signal detection. It provides visualizations in an interactive ",
          "format to demonstrate the results of multiple disproportionality analysis.",
          "</p>",
          "<p>",
          "DO NOT USE FOR REGULATORY DECISION MAKING! The methods described and results generated from the work presented herein ",
          "relate solely to the testing of methodologies and representations for the evaluation of drugs and AERs. This report ",
          "neither replaces nor is intended to replace or comment on any regulatory decisions made by Health Canada.",
          "</p>",
          "<p>",
          "Detailed documentation on all disproportionality analyses can be found in Documentation tab.",
          "</p>",
          "<br>",
          "<p>",
          "<strong>Data last updated: 2015-03-31</strong><br>",
          "<strong>MedDRA version: 19.0</strong><br>",
          "Data provided by the Canada Vigilance Adverse Reaction Online Database. The recency of the data is therefore ",
          "dependent on when the data source is updated, and is the responsibility of the Canada Vigilance Program. ",
          "For more information, please refer to ",
          "<a href = \"http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php\">",
          "http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php</a>.",
          "</p>")),
        aboutAuthors()
      ))
    )
  ), 
  
  skin = "blue"
)



