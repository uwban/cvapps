ui <- dashboardPage(
  dashboardHeader(title = titleWarning("Shiny DISP (version 2.1)"),
                  titleWidth = 700),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      selectInput(inputId ="search_drug",
                  label = "Ingredient",
                  choices = c("Start typing to search..." = "",ing_choices),
                  multiple = F),
      
      selectInput(inputId = "search_pt",
                  label = "Adverse Event Preferred Term",
                  choices = c("Start typing to search..." = ""),
                  multiple = F),
      #checkboxGroupInput('stratify',label='Stratification',c('Gender','Age-Group','None'),selected='None'),
      
      tags$div(class="form-group shiny-input-container",
               actionButton(inputId = "search_button",
                            label = "Search",
                            width = '90%')),
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("calculator")),
      menuItem("Dynamic PRR", tabName = "dnprr", icon = icon("equalizer",lib = "glyphicon")),
      menuItem("Change Point Analysis", tabName = "cpa", icon = icon("superscript")),
      #menuItem("Likelihood Analysis", tabName = "llr", icon = icon("industry")),
      menuItem("Documentation", tabName = "Documentation", icon = icon("archive")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"), selected = TRUE),
      tags$h3(strong("Current Query:")),
      tableOutput("current_search"),
      downloadButton(outputId = "pt_data_dl",
                     label = "Export Raw Data"),
      selectizeInput('select_column',
                     "Select Columns",
                     choices=c('Select columns to download',report_col),
                     multiple=T)
    )
  ), 
  
  dashboardBody(
    customCSS(),
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
              box(htmlOutput("current_pt_title"),
                  plotlyOutput( "timeplot_pt"),
                  htmlOutput(outputId = "search_url"),
               width=12,solidHeader = TRUE
               )
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
              fluidRow(
              box(
              htmlOutput("current_dpnrr_title"),
              plotlyOutput( "dpnrr_plot"),
              width=12,solidHeader = TRUE
              )),
                
              fluidRow(
                box(DT::dataTableOutput("dpnrr_table"),
                    width=12)
                )),
    
      tabItem(tabName="cpa",
              tabsetPanel(
                tabPanel("Change in Mean Analysis",
                         fluidRow(box(width=12,solidHeader = TRUE,
                                         htmlOutput("cpa_mean_title"),
                                         plotlyOutput('cpa_mean'))),
                          fluidRow(column(tableOutput('cpa_mean_txt'),width=6))),
                tabPanel("Change in Variance Analysis",
                         fluidRow(box(width=12,solidHeader = TRUE,
                                         htmlOutput("cpa_var_title"),
                                         plotlyOutput('cpa_variance'))),
                         fluidRow(column(tableOutput('cpa_variance_txt'),width=6))),
                tabPanel("Bayesian Analysis",
                         fluidRow(column(width=12,solidHeader = TRUE,
                                         htmlOutput("bcp_title"),
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
              tabsetPanel(type='tabs',
                    tabPanel("DISP Intro",
                    withMathJax(includeMarkdown("/home/shared/DISP data/CopyOfDISP about/DISP_about.md"))),
                    tabPanel("CPA Intro",
                             withMathJax(includeMarkdown("cpa_about_v1.Rmd")))
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
          "<strong>Data last updated: ",max_date,"</strong><br>",
          "<strong>MedDRA version: 21.0</strong><br>",
          "Data provided by the Canada Vigilance Adverse Reaction Online Database. The recency of the data is therefore ",
          "dependent on when the data source is updated, and is the responsibility of the Canada Vigilance Program. ",
          "For more information, please refer to ",
          "<a href = \"https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database.html\" target='_blank'>",
          "https://www.canada.ca/en/health-canada/services/drugs-health-products/medeffect-canada/adverse-reaction-database.html</a>.",
          "</p>")),
        aboutAuthors()
      ))
    )
  ), 
  
  skin = "blue"
)



