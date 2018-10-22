library(plotly)
library(stringr)





shinyServer(function(input, output, session) {
  
  
  hide(id="loading-content",anim=TRUE,animType="fade")
  show("main-content")
  
  
  #Autocomplete suggestions are generated
  updateSelectizeInput(session, 'search_brand', choices = topbrands, server = TRUE)
  updateSelectizeInput(session, 'search_ing', choices = topings_cv, server = TRUE)
  updateSelectizeInput(session, 'search_rxn', choices = pt_choices, server = TRUE)
  updateSelectizeInput(session, 'search_soc', choices = soc_choices, server = TRUE)
  
  
  
  ##### Reactive data processing
  # Data structure to store current query info
  current_search <- reactiveValues()

  
  # We need to have a reactive structure here so that it activates upon loading
  reactiveSearchButton <- reactive(as.vector(input$searchButton))
  

  
  observeEvent(reactiveSearchButton(),
    withProgress(message = 'Calculation in progress', value = 0, {
      
      if (input$name_type == "brand") {
        name <- input$search_brand
      } else if (input$name_type == "ingredient") {
        name <- input$search_ing
      } else {
        name <- input$search_ing2
      }
      
      startDate <- input$daterange[1] %>% ymd(tz = 'EST')
      print(startDate)
      endDate <- input$daterange[2] %>% ymd(tz = 'EST')
      print(endDate)
      dateRange <- c(startDate, endDate)
      print(dateRange)
      
      #search variables
      
      
      current_search$name_type <- input$name_type
      
      current_search$name <- name
      
      current_search$drug_inv <- input$drug_inv
      
      current_search$seriousness_type <- input$seriousness_type
      
      current_search$rxn <- input$search_rxn
      
      
      current_search$gender <- input$search_gender
      
      current_search$soc <- input$search_soc
      
      current_search$age <- input$search_age
      
      current_search$date_range <- dateRange
      
      current_search$startDate <- startDate
      
      current_search$endDate <- endDate
      
      #current_search$age_estimate <- input$filter_estimates_age
      
      current_search$uri <- create_uri(current_search$startDate, current_search$endDate, current_search$gender, 
                                       current_search$age, current_search$rxn, current_search$soc, 
                                       current_search$drug_inv, current_search$name, current_search$seriousness_type, 
                                       current_search$name_type)
      incProgress(4/9, detail = 'Assembling query string')
      # api_key <- api_key
      
      n<-request(current_search$uri)
      
      if (n == 0) {
        setProgress(1)
        showModal(modalDialog(
          title = list(icon("exclamation-triangle"), "No results found!"),
          "There were no reports matching your query. Please make another selection",
          size = "s",
          easyClose = TRUE))
        
        current_search$name<-NULL
        
        current_search$uri <- create_uri(current_search$startDate, current_search$endDate, current_search$gender, 
                                         current_search$age, current_search$rxn, current_search$soc, 
                                         current_search$drug_inv, current_search$name, current_search$seriousness_type, 
                                         current_search$name_type)
        
      }


      
      option_list <- c(      
                             #current_search$age_estimate,
                             current_search$dateRange,
                             current_search$gender,
                             current_search$age,
                             current_search$rxn,
                             current_search$soc,
                             current_search$drug_inv,
                             current_search$seriousness_type)
      
      print(option_list)
      incProgress(7/9, detail = 'Querying results')

      
      
    })
  )
  
  # strtrans <- function(input)
  # {
  #   input <- as.character(input)
  #   input <- str_pad(input, 9, pad = "0")
  #   input <- paste0("\t",input)
  #   
  #   return(input)
  # }
  # 
  
  nreports<-reactive({
    n<-request(current_search$uri)
    return(n)
  })
    
 
  ##### Output ####
  ##### Construct Current_Query_Table for generic name, brand name, adverse reaction term & date range searched
  output$current_search <- renderTable({
    data <- current_search
    result <- data.frame(
      names = c("Name Type:",
                "Age Range:",
                "Gender:",
                "Name:",
                "Adverse Reaction Term:",
                "System Organ Class:",
                "Seriousness:",
                "Date Range:"),
      values = c(data$name_type %>% toupper(),
                                    paste(data$age[1], 'to', data$age[2], 'including', ifelse(data$filter_estimates_age, 'estimates', '')),
                                    data$gender,
                                    paste0(data$name, collapse = ", "),
                                    paste0(data$rxn, collapse = ", "),
                                    paste0(data$soc, collapse = ", "),
                                    paste0(data$seriousness_type, collapse = ", "),
                                    paste(data$date_range, collapse = " to ")),
                         stringsAsFactors = FALSE)
    
    result$values["" == result$values] <- "Not Specified"
    result
  },
  include.colnames = FALSE
  )

  
  ##### Create time plot  ###
  
  output$timeplot_title <- renderUI({
   
    nreports<-nreports()
    
    drug_name<-paste0(current_search$name,collapse=', ')
    rxn <- paste0(current_search$rxn, collapse = ", ")
    soc <- paste0(current_search$soc, collapse = ", ")
    # print(rxn)
    # print(soc)
    if ("" == drug_name) drug_name <- "All Drugs"
    if ("" == rxn) rxn_name <- "All Reactions"
    else rxn_name <- rxn
    if ("" != soc & rxn == ""){
      rxn_name <- soc
    }else if("" != soc & rxn != ""){
      rxn_name <- paste(rxn, ', ', soc)
    }

    plottitle <- paste0("Drug Adverse Event Reports for ", drug_name, " and ", rxn_name, " (", nreports, " reports) from ",input$daterange[1], " to ",input$daterange[2])
    h3(strong(plottitle))
    })
  
  output$search_url <- renderUI({
    url <- current_search$uri
    HTML(paste0('Search URL: <a href = ', url,' target="_blank">', url, '</a>'))
  })
  
  timeDataSelection <- reactive({

    print('time data')
    time_results
    
    
  })
  
  output$mychart <- renderLineChart({


    two_years <- 730

    if ((current_search$endDate - current_search$startDate) >= two_years) {
      time_period <- "year"
      time_function <- function(x) {years(x)}
    } else {
      time_period <- "month"
      time_function <- function(x) {months(x)}
    }
    
    dateSequence_start <- get_date_sequence_start(current_search$startDate, current_search$endDate, time_period)
    dateSequence_end <- get_date_sequence_end(current_search$startDate, current_search$endDate, time_period)
    
    data <- get_timechart_data(time_period, dateSequence_start,dateSequence_end, current_search$gender, current_search$age, current_search$rxn,
                               current_search$soc, current_search$drug_inv, current_search$name, current_search$seriousness_type, current_search$name_type)
    
    
    
    time_data <- lapply(data, request_listed)
    df <- as.data.frame(t(as.data.frame(time_data))) %>%
      plyr::rename(c('V1' = 'Serious(Excluding Death)', 'V2' = 'Death', 'V3' = 'Nonserious'))
    
    
    rownames(df) <- c()

    dateSequence <- dateSequence_start[1:(length(dateSequence_start) - 1)] 
    if(time_period == 'year'){
      dateSequence <- format(as.Date(dateSequence), "%Y")
    }
    else {
      dateSequence <- format(as.Date(dateSequence), "%Y-%m")
    }
    
    df$time_p <- dateSequence
    df <- df[,c(4,1,2,3)]
    transform(df, time_p = toString(time_p))
    print('help')
    df

  })
  
  
  ##### Data about Reports
  
  ### Reporterplot ###
  reportertable <- reactive({

    reporter_count <- counter(current_search$uri, 'reporter_type.keyword', api_key)
    reporter_count[nrow(reporter_count) + 1,] = list("Not Specified",nreports()-sum(reporter_count$doc_count))
    reporter_count$doc_count<-ifelse(reporter_count$doc_count<0,0,reporter_count$doc_count)
    return(reporter_count)
  })
  
  output$reporterchart <- renderGvis({
    x = "reporter_type_eng"
    y = "count"
    gvisPieChart_HCSC(as.data.frame(reportertable()),x,y)
  })
  
  output$reportertable    <- renderGvis({
    gvisTable(as.data.frame(reportertable()))
  })
  
  ### seriousplot ###
  seriousplot_data <- reactive({

    
    search_uri<-current_search$uri
    
    death_count <- counter(search_uri, 'outcome.keyword', api_key)
    death_count <- death_count[(death_count$category =="Death"),]
    seriousness_count <- counter(search_uri, 'seriousness.keyword', api_key)
    
    #remove the death counts and rename 
    if (length(death_count$category)!=0){
    seriousness_count[1,2] <- seriousness_count[1,2] - death_count[1,2]
    }else{
    seriousness_count[1,2] <- seriousness_count[1,2]
    }
    
    seriousness_count[1,1] <- 'Serious(excluding death)'
    seriousness_count[2,1] <- 'Non-Serious'

    totals <- rbind(death_count, seriousness_count)
    
    return(totals)
    # return(big_table)
  })
  
  output$seriouschart <- renderGvis({
    x = "category"
    y = "doc_count"
    gvisPieChart_HCSC(as.data.frame(seriousplot_data()),x,y)
  })
  
  output$serioustable    <- renderGvis({
    gvisTable(as.data.frame(seriousplot_data()))
  })
  
  
  
  ### seriousreasonplot ###
  output$seriousreasonsplot <- renderGvis({

    congenital_anomaly <- counter(current_search$uri, 'congenital_anomaly.keyword', api_key)
    congenital_anomaly <- congenital_anomaly[(congenital_anomaly$category =="true"),]
    congenital_anomaly[1,1] <- 'Congenital Anomaly'
    
    death <- counter(current_search$uri, 'outcome.keyword', api_key)
    death <- death[(death$category =="Death"),]
    death[1,1] <- 'Death'
    
    disability <- counter(current_search$uri, 'disability.keyword', api_key)
    disability <- disability[(disability$category =="true"),]
    disability[1,1] <- 'Disability'
    
    life_threatening <- counter(current_search$uri, 'life_threatening.keyword', api_key)
    life_threatening <- life_threatening[(life_threatening$category =="true"),]
    life_threatening[1,1] <- 'Life Threatening'
    
    hosp_required <- counter(current_search$uri, 'hosp_required.keyword', api_key)
    hosp_required <- hosp_required[(hosp_required$category =="true"),]
    hosp_required[1,1] <- 'Hospital Required'
    
    other_medically_imp_cond <- counter(current_search$uri, 'other_medically_imp_cond.keyword', api_key)
    other_medically_imp_cond <- other_medically_imp_cond[(other_medically_imp_cond$category =="true"),]
    other_medically_imp_cond[1,1] <- 'Other Medically Impaired Condition'

    serious_reasons <-do.call(rbind,list(congenital_anomaly,death,life_threatening,hosp_required,disability,other_medically_imp_cond))
    serious_reasons[nrow(serious_reasons) + 1,] = list("Not Specified",nreports()-sum(serious_reasons$doc_count))
    serious_reasons$doc_count<-ifelse(serious_reasons$doc_count<0,0,serious_reasons$doc_count)
    
    gvisBarChart(serious_reasons,
                 xvar = "category",
                 yvar = "doc_count",
                 options = list(
                   legend = "{position: 'none'}",
                   hAxis = "{title: 'Number of Reports'}",
                   chartArea = "{top: 0, height: '80%', left: 150, width: '60%'}",
                   bar = "{groupWidth: '90%'}",
                   colors = colorCodeToString(google_colors[5])
                 )
    )
  })
  
  ### Data about Patients
  sexplot_data <- reactive({

    sex_data <- counter(current_search$uri, 'patient_gender.keyword', api_key)
    if(nrow(sex_data[sex_data$category=='Not specified',])!=0){
    sex_data[sex_data$category=='Not specified','doc_count']<-nreports()-sum(sex_data$doc_count)+sex_data$doc_count[sex_data$category=='Not specified']
    }else{
    sex_data[nrow(sex_data) + 1,] = list("Not specified",nreports()-sum(sex_data$doc_count)) 
    }
    sex_data
  })
  
  output$sexchart <- renderGvis({
    x = "gender_eng"
    y = "n"
    gvisPieChart_HCSC(as.data.frame(sexplot_data()),x,y)
  })
  
  output$sextable    <- renderGvis({
    gvisTable(as.data.frame(sexplot_data()))
  })
  
  
  #count age groups
  agegroup_data <-reactive({
 
    age_count <- counter(current_search$uri, 'patient_age_y&limit=200', api_key) %>%
      mutate(age_group = ifelse(category <= 25/365, 'Neonate', ifelse(category <= 1, 'Infant', ifelse(category < 13, 'Child', ifelse(category < 18, 'Adolescent', ifelse(category <= 65, 'Adult', ifelse(category > 65, 'Elderly', 'Unknown')))))))
    
    age_count <- aggregate(age_count['doc_count'], by=age_count['age_group'], sum) 
    

    return(age_count)
  })
  output$agechart <- renderGvis({
    x = "age_group_clean"
    y = "n"
    gvisPieChart_HCSC(as.data.frame(agegroup_data()),x,y)
  })
  
  output$agetable    <- renderGvis({
    gvisTable(as.data.frame(agegroup_data()))
  })
  
  
  
  output$agehisttitle <- renderUI({
    # excluded_count <- mainDataSelection() %>%
    #   filter(age_group_clean != "Unknown", age_y > 100) %>%
    #   tally() %>% as.data.frame() %>% `$`(n)
    # HTML(paste0("<h3>Histogram of Patient Ages ",
    #             tipify(
    #               el = icon("info-circle"), trigger = "hover click",
    #               title = "Distribution of number of reports per age, colour-coded by age group. Each bin groups 2 years."),
    #             "<br>(", excluded_count, " reports with age greater than 100 excluded)", "</h3>"))
  })
  output$agehist <- renderPlotly({

    age_count <- counter(current_search$uri, 'patient_age_y&limit=200', api_key) %>%
      mutate(age_group = ifelse(category <= 25/365, 'Neonate', ifelse(category <= 1, 'Infant', ifelse(category < 13, 'Child', ifelse(category < 18, 'Adolescent', ifelse(category <= 65, 'Adult', ifelse(category > 65, 'Elderly', 'Unknown')))))))

    colours_df <- data.frame(
      age_group = c("Neonate", "Infant", "Child", "Adolescent", "Adult", "Elderly"),
      colours = google_colors[1:6],
      stringsAsFactors = FALSE) 
    
    colours_df <- left_join(colours_df, age_count, by = "age_group")
    
    hist <- ggplot(age_count, aes(x = category, fill = age_group, weight=doc_count)) +
      geom_histogram(breaks = seq(0, 100, by = 0.5)) +
      scale_fill_manual(values = colours_df$colours) +
      xlab("Age at onset (years)") +
      ylab("Number of Reports")
    

    the_graph <- ggplot(age_count, aes(x=category, fill=age_group, color=age_group, weight =doc_count, displayModeBar = F)) + geom_histogram(position="identity", binwidth =1)  +
      xlab("Age at onset (years)") +
      ylab("Number of Reports")
    # ggplotly(hist)
    
    final_graph <- ggplotly(the_graph) %>% plotly::config(displayModeBar = F)
    

    final_graph
  })
  

  
  ## indication ###
  indication_data <- reactive({

    data <- counter(current_search$uri,'report_indication_eng.keyword&limit=20', api_key)
    data
  })

  output$indicationchart <- renderGvis({
    x = "category"
    y = "doc_count"
    gvisBarChart_HCSC(indication_data(),x,y,color = google_colors[1])
  })

  output$indicationtable    <- renderGvis({
    gvisTable(as.data.frame(indication_data()))
  })
  

  ### suspected drug ###
  suspect_data <- reactive({    

    data <- counter(current_search$uri, 'report_drugname_suspect.keyword&limit=20', api_key)
    
    data

    
  })
  ### concomitant drug ###
  concomitant_data <- reactive({

    data <- counter(current_search$uri, 'report_drugname_concomitant.keyword&limit=20', api_key)
    data
  })
  
#There is a small chance that just adding the top 1000 from both might not give the correct top 20 (if one appears high on suspect, but not in top 1000 of concomitant for instance),
  #this possibility seems almost Infinitesimally small though
  all_data <- reactive({
    
    concomitant <- concomitant_data()
    suspect <- suspect_data()
    
    data <- rbind(concomitant, suspect)
    data <- aggregate(data['doc_count'], by=data['category'], sum) 
    data <- data[order(data$doc_count, decreasing = TRUE),] 
    #take the top 20
    data <- data[1:20,]
    
    data
  })
  
  output$alldrugchart <- renderGvis({
    x = "category"
    y = "doc_count"
    gvisBarChart_HCSC(as.data.frame(all_data())[1:20,],x,y,color = google_colors[2])
  })
  
  output$alldrugtable    <- renderGvis({
    gvisTable(as.data.frame(all_data())[1:20,])
  })
  


  
  output$suspecteddrugchart <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some report_id maybe duplicated due to multiple REPORT_DRUG_ID & drug_product_id which means that patient has diff dosage/freq)
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisBarChart_HCSC(suspect_data()[1:20,], "category", "doc_count", google_colors[3])
    
  })
  
  output$suspecteddrugtable <- renderGvis({
    gvisTable(suspect_data()[1:20,])
  })
  

  
  output$concomitantdrugchart <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some report_id maybe duplicated due to multiple REPORT_DRUG_ID & drug_product_id which means that patient has diff dosage/freq)
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    
    gvisBarChart_HCSC(concomitant_data()[1:20,], "category", "doc_count", google_colors[4])
  })
  
  output$concomitantdrugtable <- renderGvis({
    data <- concomitant_data()
    gvisTable(data[1:20,])
  })
  
  output$drugcounttitle <- renderUI({
    # excluded_count <- drugDataSelection() %>%
    #   count(report_id) %>%
    #   filter(n > 20) %>%
    #   count() %>% as.data.frame() %>% `$`('nn')
    HTML(paste0("<h3>Number of Drugs per Report ",
                tipify(
                  el = icon("info-circle"), trigger = "hover click",
                  title = paste0(
                    "This plot indicates the number of drugs (e.g. suspect, concomitant, past, treatment, etc) included in each report. ",
                    "The search query filters unique reports, which may have one or more drugs associated with them.")),
                "<br>(reports with more than 20 drugs excluded)", "</h3>"))
  })

  drugcount_data <- reactive({
    data <- counter(current_search$uri, 'n_drugs_per_report&limit=20', api_key)
    data
  })
  
  output$drugcount_plot <- renderGvis({
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    data<- drugcount_data()%>%filter(category<=20)%>%arrange(category)
    data$category <-paste("Number of Drugs:",data$category)


    gvisColumnChart(data, xvar="category", yvar="doc_count", options = list(
      legend = "{ position: 'none' }",
      height = 600,
      vAxis = "{title: 'Number of Reports'}",
      hAxis = "{title: 'Number of Drugs per Report'}",
      chartArea = "{top: 20, height: '75%', left: 80, width: '90%'}")
    )
  })
  
  
  #### Data about Reactions
  
  # rxnDataSelection <- reactive({
  #   n_ids <- selected_ids$ids %>% nrow()
  #   
  #   if (nrow(selected_ids$ids) > 0)
  #   {
  #     data <- cv_reactions %>% semi_join(selected_ids$ids, by = "report_id", copy = T) 
  #   }
  #   else
  #   {
  #     data <- NULL
  #     data <- cv_reactions
  #     current_search$name = ""
  #     current_search$rxn = ""
  #   }
  #   
  #   data
  # })
  
  ### toppt ###
  top_pt_data <- reactive({

    data <- counter(current_search$uri, 'reaction_pt.keyword&limit=20', api_key)
    data
  })
  
  output$topptchart <- renderGvis({
    x = "category"
    y = "doc_count"
    gvisBarChart_HCSC(as.data.frame(top_pt_data()),x,y,color = google_colors[1])
  })
  
  output$toppttable    <- renderGvis({
    gvisTable(as.data.frame(top_pt_data()))
  })
  
  ### tophlt ###
  top_hlt_data <- reactive({
    data <- counter(current_search$uri, 'reaction_pt.keyword&limit=50', api_key)
    
    
    meddra_pt<-sapply(data$category,function(x){sprintf('{
    "_source":"hlt_name","query":{"match":{"pt_name_eng":{"query":"%s","operator":"and"}}}}',x)})
    
    res_pt<-lapply(meddra_pt,function(x)Search(index ='meddra_pt',body=x,size=100))
    
    #extract hlt and count:
    res_hlt<-vector()
    for (i in seq_along(res_pt)){
      res_hlt[[i]]<-res_pt[[i]]$hits$hits[[1]]$`_source`[[1]]
    }
    
    data$hlt<-res_hlt
    
    data<-data%>%group_by(hlt)%>%
                 summarise(n=sum(doc_count))%>%
                 arrange(desc(n))%>%
                 slice(1:10)
    
    
    data
  })
  
  output$tophltchart <- renderGvis({
    x = "hlt"
    y = "n"
    gvisBarChart_HCSC(as.data.frame(top_hlt_data()),x,y,color = google_colors[2])
  })
  
  output$tophlttable    <- renderGvis({
    gvisTable(as.data.frame(top_hlt_data()))
  })
  
  ### outcome plot ###
  outcomeplot_data <- reactive({

    # search_uri <- create_uri(current_search$startDate, current_search$endDate, gender=current_search$gender, 
    #                          age=current_search$age, rxn=current_search$rxn, soc=current_search$soc, drug_inv=current_search$drug_inv, drugname=current_search$name, 
    #                          seriousness=current_search$seriousness_type, search_type=current_search$name_type)
    
    search_uri<-current_search$uri
    outcome_count <- counter(search_uri, 'outcome.keyword', api_key)
    return(outcome_count)
  })
  output$outcomechart <- renderGvis({
    x = "outcome"
    y = "n"
    gvisPieChart_HCSC(as.data.frame(outcomeplot_data()),x,y)
  })
  
  output$outcometable    <- renderGvis({
    gvisTable(as.data.frame(outcomeplot_data()))
  })
  
  
  
############# Download Tab
  observe({
    x<-input$search_dataset_type
    if(x=='Report Data'){
      choices<-c('report_id','report_no','disability',
                 'death','mah_no','source','outcome',
                 'version_no','datreceived','report_type','seriousness',
                 'hosp_required','reporter_type','datintreceived','patient_gender',
                 'patient_weight','life_threatening','congenital_anomaly','patient_weight_unit',
                 'report_drugname_suspect','report_ingredient_suspect','n_drugs_per_report',
                 'patient_age','patient_age_y','patient_age_unit','patient_height','patient_height_unit','patient_age_group')
      updateSelectizeInput(session,'select_column',choices=choices,server=T)
    }else if (x=='Drug Data'){
      choices<-c("report_id","druginvolv_code","dose_unit_fr","druginvolv_fr","seq_therapy","drug_product_id",
                 "freq_time_unit_eng","seq_product","frequency_time_fr","freq_time","routeadmin_code",
                 "ingredients","therapy_duration_unit_eng","freq_time_unit_fr","therapy_duration_unit_fr",
                 "unit_dose_qty","therapy_duration_unit_code","frequency_time_eng","dose_unit_eng","freq_time_unit_code",
                 "drugname","druginvolv_eng","routeadmin_fr","routeadmin_eng","indication_name_eng","indication_name_fr",
                 "therapy_duration","frequency","dosageform_eng","dosageform_fr")
      updateSelectizeInput(session,'select_column',choices=choices,server=T)
    }else{
      choices<-c('report_id','meddra_version','reaction_id','soc_name','pt_name')
      updateSelectizeInput(session,'select_column',choices=choices,server=T) 
      
    }
  })
  
  cv_download_reports <- reactive({
    
    withProgress(message = 'Reformating JSON files to CSV', value = 0, {
    
    incProgress(1/3)
      
    full_report<-parse_response(current_search$uri)%>%
                 select(`_source`)%>%
                 flatten()
    
    incProgress(3/4)
    
    colnames(full_report)<-gsub('_source.','',names(full_report))

    if(input$search_dataset_type == "Report Data"){
       report_data<-full_report%>%select(-reactions,-reaction_pt,-report_drug_detail,-report_links,-reaction_soc,
                                         -report_ingredient_concomitant,-report_drugname_concomitant,-report_indication_eng)
       
       report_data$report_drugname_suspect<-sapply(report_data$report_drugname_suspect,paste,collapse=',',USE.NAMES = F)
       report_data$report_ingredient_suspect<-sapply(report_data$report_ingredient_suspect,paste,collapse=',',USE.NAMES = F)
       
       if(!is.null(input$select_column)){
       report_data<-report_data%>%select(input$select_column)
       }else{
       report_data<-report_data
       }
       
       col_names<-colnames(report_data)
       
    }
    else if(input$search_dataset_type == "Drug Data"){
       report_data<-full_report%>%select(report_drug_detail)%>%
                                  unnest(report_drug_detail)
       
       if(!is.null(input$select_column)){
         report_data<-report_data%>%select(input$select_column)
       }else{
         report_data<-report_data
       }
       
       col_names<-colnames(report_data)
    }
    else if(input$search_dataset_type == "Reaction Data"){
       report_data<-full_report%>%select(report_id,reactions)%>%
                                  unnest(reactions)
       
       if(!is.null(input$select_column)){
         report_data<-report_data%>%select(input$select_column)
       }else{
         report_data<-report_data
       }
       
       col_names<-colnames(report_data)
    }

    return(list(report_data=report_data,
                col_names=col_names))
    
  })
    
})
  
  output$download_reports <- downloadHandler(
    filename = function() {
      current_rxn <- paste0(current_search$rxn, collapse = "+")
      if (current_rxn == "") current_rxn <- "all"
      current_drug <- paste0(current_search$name, collapse = "+")
      if (current_drug == "") current_drug <- "all"
      current_drug <- gsub(" ", "_", current_drug)
      paste0(input$search_dataset_type, '_', current_drug, '_', current_rxn, '.csv')
    },
    content = function(file){
      write.csv(cv_download_reports()$report_data,
                file,
                fileEncoding = "UTF-8",
                row.names = FALSE)
    }
  )
  
 
  
  # output$column_select_drug<-renderUI({
  #   selectizeInput("column_select_drug",
  #                   "Select Columns",
  #                   cv_download_reports()$col_names,
  #                   c("Start typing to select columns..." = ""),
  #                   multiple = TRUE)
  #   })
  # 
  # 
  # output$column_select_data<-renderUI({
  #   selectizeInput("column_select_report",
  #                "Select Columns",
  #                cv_download_reports()$col_names,
  #                c("Start typing to select columns..." = ""),
  #                multiple = TRUE)
  # 
  # })
  # 
  # output$column_select_reaction<-renderUI({
  #   selectizeInput("column_select_reaction",
  #                "Select Columns",
  #                cv_download_reports()$col_names,
  #                c("Start typing to select columns..." = ""),
  #                multiple = TRUE)
  # })
  
}
)