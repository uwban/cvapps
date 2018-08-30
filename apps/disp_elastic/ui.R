ui <- dashboardPage(
  dashboardHeader(title = titleWarning("Shiny DISP (v0.30)"),
                  titleWidth = 700),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
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
                            width = '90%')),
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("calculator")),
      menuItem("Dynamic PRR", tabName = "dnprr", icon = icon("equalizer",lib = "glyphicon")),
      menuItem("Change Point Analysis", tabName = "cpa", icon = icon("superscript")),
      #menuItem("Likelihood Analysis", tabName = "llr", icon = icon("industry")),
      tags$h3(strong("Current Query:")),
      tableOutput("current_search"),
      downloadButton(outputId = "pt_data_dl",
                     label = "Export PT data"),
      menuItem("Documentation", tabName = "Documentation", icon = icon("archive")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"), selected = TRUE)
    )
  ), 
  
  dashboardBody(
    customCSS(),
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                
                column(12,
                       h3(textOutput("current_pt_title")),
                       plotlyOutput( "timeplot_pt"))
                ),
              
               fluidRow(
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
      tabItem(tabName="dnprr",
              fluidRow(width=12,
              h3(textOutput("current_dpnrr_title")),
              plotlyOutput( "dpnrr_plot")
              ),
                
              fluidRow(width=12,
                       DT::dataTableOutput("dpnrr_table")
                       )
              ),
    
      tabItem(tabName="cpa",
              tabsetPanel(
                tabPanel("Change in Mean Analysis",
                         fluidRow(column(width=12,
                                         h3(textOutput("cpa_mean_title")),
                                         plotlyOutput('cpa_mean')),
                                  column(tableOutput('cpa_mean_txt'),width=6))),
                tabPanel("Change in Variance Analysis",
                         fluidRow(column(width=12,
                                         h3(textOutput("cpa_var_title")),
                                         plotlyOutput('cpa_variance'))),
                         fluidRow(column(tableOutput('cpa_variance_txt'),width=6))),
                tabPanel("Bayesian Analysis",
                         fluidRow(column(width=12,
                                         h3(textOutput("bcp_title")),
                                         plotOutput('bcpa'))),
                         fluidRow(column(
                           tableOutput('bcpa_txt'),width=6))
              ))),
      
      # tabItem(tabName="llr",
      #         fluidRow(width=12,
      #                  column(12,
      #                         h3(textOutput("llr_title")),
      #                         plotlyOutput('llr_plot'))),
      #         fluidRow(width=12,tableOutput('llr_txt'))),
      
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
          "<strong>Data last updated: 2018-03-31</strong><br>",
          "<strong>MedDRA version: 20.0</strong><br>",
          "Data provided by the Canada Vigilance Adverse Reaction Online Database. The recency of the data is therefore ",
          "dependent on when the data source is updated, and is the responsibility of the Canada Vigilance Program. ",
          "For more information, please refer to ",
          "<a href = \"https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database.html\">",
          "https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database.html</a>.",
          "</p>")),
        aboutAuthors()
      ))
    )
  ), 
  
  skin = "blue"
)



