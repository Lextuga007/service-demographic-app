# data sourced from sourceData.R which runs automatically from folder R/

library(shiny)
library(shinycssloaders)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title -------------------------------------------------------
  
  
  titlePanel("Demographics"),
  
  sidebarPanel(
    
    # 1.1.1
    uiOutput("financialYearInput"),
    
    # 10.1.1
    uiOutput("genderInput"),
    
    # 9.1.1
    uiOutput("localAuthorityInput"),
    
    
    # Ethnicity
    radioButtons("ethnicityChoice", "Select how ethnicity is grouped",
                 choices = c("Grouped", "Detail"),
                 selected = "Grouped"),
    
    # 2.1.1
    conditionalPanel(condition = "input.ethnicityChoice == 'Grouped'",
                     uiOutput("ethnicityGroupInput")),
    
    # 3.1.1
    conditionalPanel(condition = "input.ethnicityChoice == 'Detail'",
                     uiOutput("ethnicityDetailInput")),
    
    # 6.1.1
    conditionalPanel(condition = "input.tabset == 'Age' & input.ethnicityComparisonChoice != 'Public'",
                     uiOutput("IMDInput")),
    
    
    # Comparison --------------------------------------------------------------
    
    hr(),
    
    h4("Comparison charts"),
    
    hr(),
    
    # Ethnicity
    
    radioButtons("ethnicityComparisonChoice", "Select how comparison ethnicity is grouped",
                 choices = c("Grouped", "Detail", "Public"),
                 selected = "Grouped"),

    hr(),    

    # 1.2.1
    conditionalPanel(condition = "input.ethnicityComparisonChoice != 'Public'",
                     uiOutput("financialYearInputComp")),
    
    # 10.2.1
    conditionalPanel(condition = "input.ethnicityComparisonChoice != 'Public'",
                     uiOutput("genderInputComp")),
    
    # 9.2.1
    conditionalPanel(condition = "input.ethnicityComparisonChoice != 'Public'",
                     uiOutput("localAuthorityCompInput")),
    
    # 10.1.1
    conditionalPanel(condition = "input.tabset == 'Age' & input.ethnicityComparisonChoice == 'Public'",
                     uiOutput("publicLocalAuthorityInput")),
    
    
    
    # 2.3.1
    conditionalPanel(condition = "input.ethnicityComparisonChoice == 'Grouped'",
                     uiOutput("ethnicityGroupInputComp"),
    ),
    
    # 3.3.1
    conditionalPanel(condition = "input.ethnicityComparisonChoice == 'Detail'",
                     uiOutput("ethnicityInputComp")),
    
    # 4.1.1
    conditionalPanel(condition = "input.tabset == 'Age' & input.ethnicityComparisonChoice == 'Public'",
                     uiOutput("publicEthnicityGroupInput")),
    
    # 5.1.1
    conditionalPanel(condition = "input.tabset == 'IMD' & input.ethnicityComparisonChoice == 'Public'",
                     uiOutput("publicIMDEthnicityGroupInput")),
    
    
    # IMD selection -----------------------------------------------------------
    # Disappears when Public selected
    
    # 6.2.1
    conditionalPanel(condition = "input.ethnicityComparisonChoice != 'Public'",
                     uiOutput("IMDDecileInputComp")),
    
    
    # adding the new div tag to the sidebar            
    tags$div(class="header", checked=NA,
             tags$p("To select more than one category click on the ones you want and they
             will move automatically to the selected box"),
             tags$br("All data is returned if nothing is selected"),
             tags$br("For best results expand the screen to full size"),
             tags$br("IMD Quintile/Decile: 1 Most deprived, 0 = Unknown or missing postcode")
             #tags$a(href="shiny.rstudio.com/tutorial", "Click Here!")
    )
    
    
    
  ),
  
  
  # Tabs --------------------------------------------------------------------
  
  mainPanel(
    tabsetPanel(id = 'tabset',
                tabPanel("Age",
                         withSpinner(plotlyOutput("distPlot")),
                         withSpinner(plotlyOutput("distPlotComp")),
                         value = 'Age'),
                tabPanel("IMD", 
                         withSpinner(plotlyOutput("distPlotIMD")), 
                         withSpinner(plotlyOutput("distComparisonPlotIMD")),
                         value = 'IMD')
    ),
    htmlOutput("NBText")
  ),
)

# Define server function
server <- function(input, output){
  
  # RENDERUI INPUT ------------------------------------------------------------
  
  # 1.1 Financial Year Input ----------------------------------------------------
  
  output$financialYearInput <- renderUI({
    
    selectInput("finYr",
                "Select Financial Year",
                choices = finYears,
                multiple = TRUE)
  })
  
  # 2.1 Grouped Ethnicity Input --------------------------------------------------
  
  output$ethnicityGroupInput <- renderUI({
    
    selectInput("ethInputGrouped",
                "Select Ethnicity Group",
                choices = ethnicGroupedList,
                multiple = TRUE)
  })
  
  # 3.1 Detail Ethnicity Input ---------------------------------------------------
  
  output$ethnicityDetailInput <- renderUI({
    
    selectInput("ethInputDetail", 
                "Select Ethnicity",
                choices = ethnicDetailList,
                multiple = TRUE)
  })  
  
  # 4.1 Comparison Estimate Age Ethnicity Input ---------------------------------------
  
  output$publicEthnicityGroupInput <- renderUI({
    
    selectInput("pubEstEthnicity",
                "Select Comparison Public Grouped Ethnicity (Population Estimates)",
                choices = ageEstimates,
                multiple = TRUE)
  })  
  
  # 5.1 Comparison IMD Public Ethnicity Input -----------------------------------
  
  output$publicIMDEthnicityGroupInput <- renderUI({
    
    selectInput("pubIMDEthnicity",
                "Select Comparison Public Grouped Ethnicity (IMD)",
                choices = IMDONSList,
                multiple = TRUE)
  })  
  
  # 6.1 IMD Decile --------------------------------------------------------------
  
  output$IMDDecileInput <- renderUI({
    
    selectInput("IMDDecile",
                "Select IMD Decile",
                choices = IMDDecileList,
                multiple = TRUE)
  })
  
  # 9.1 Local Authority Region ----------------------------------------------
  
  output$localAuthorityInput <- renderUI({
    
    selectInput("LARegion",
                "Select the Local Authority area (Local patient residence)",
                choices = LARegion,
                multiple = TRUE)
    
  })
  
  # 11.1 Gender ----------------------------------------------
  
  output$genderInput <- renderUI({
    
    selectInput("gender",
                "Select Sex",
                choices = genderList,
                multiple = TRUE)
    
  })
  

  # RENDERUI COMPARISON--------------------------------------------------------------
  
  # 1.1 Comparison Financial Year
  
  output$financialYearInputComp <- renderUI({
    
    selectInput("finYrComp",
                "Select Comparison Financial Year (Local data)",
                choices = finYears,
                multiple = TRUE)
  })
  
  # 2.3 Comparison Grouped Ethnicity Input --------------------------------------------------
  
  output$ethnicityGroupInputComp <- renderUI({
    
    selectInput("ethInputGroupedComp",
                "Select Comparison Ethnicity Group",
                choices = ethnicGroupedList,
                multiple = TRUE)
  })
  
  # 3.3 Comparison Detail Ethnicity Input ---------------------------------------------------
  
  output$ethnicityInputComp <- renderUI({
    
    selectInput("ethInputComp", 
                "Select Comparison Ethnicity",
                choices = ethnicDetailList,
                multiple = TRUE)
  })    
  
  # 6.2 Comparison IMD Input ----------------------------------------------------
  
  output$IMDInputComp <- renderUI({
    
    selectInput("IMDComp",
                "Select Comparison IMD (Local data)",
                choices = IMDDecileList,
                multiple = TRUE)
  })
  
  # 9.2 Comparison Local Authority Region ----------------------------------------------
  
  output$localAuthorityCompInput <- renderUI({
    
    selectInput("LACompRegion",
                "Select the Local Authority area (Local comparison patient residence)",
                choices = LARegion,
                multiple = TRUE)
    
  })
  
  # 10.1 Local Authority Region ----------------------------------------------
  
  output$publicLocalAuthorityInput <- renderUI({
    
    selectInput("pubLARegion",
                "Select the Local Authority area (Public)",
                choices = publicLARegion,
                multiple = TRUE)
    
  })
  
  # 11.2 Comparison Gender ----------------------------------------------
  
  output$genderInputComp <- renderUI({
    
    selectInput("genderComp",
                "Select Sex (Local data)",
                choices = genderList,
                multiple = TRUE)
    
  })
  
  
  # 13.1 Gender Public ----------------------------------------------
  
  output$genderPublicInputComp <- renderUI({
    
    selectInput("genderPublic",
                "Select Sex (public)",
                choices = genderPublicList,
                multiple = TRUE)
    
  })
  
  # REACTIVE ---------------------------------------------------------------
  
  # logic for returning all if nothing selected     -------------------------
  
  dfReturn <- reactive({
    
    if(input$ethnicityChoice %in% c('Grouped', 'Detail')){
      
      df_final <- df_synth
      
      # 1.1.0
      if(!is.null(input$finYr)){
        df_final <- df_final %>%
          filter(fin_year_name %in% input$finYr)
      }
      
      # 9.1.0
      if(!is.null(input$LARegion)){
        df_final <- df_final %>% 
          filter(local_authority_name %in% input$LARegion)
      }
      
      # 2.1.0
      if(!is.null(input$ethInputGrouped) & input$ethnicityChoice == 'Grouped'){
        df_final <- df_final %>%
          filter(ethnicity_category %in% input$ethInputGrouped)
      }
      
      # 3.1.0
      if(!is.null(input$ethInputDetail) & input$ethnicityChoice == 'Detail'){
        df_final <- df_final %>%
          filter(ethnicity_detail %in% input$ethInputDetail)
      }
      
      # 6.1.0
      if(!is.null(input$imd_decile)){
        df_final <- df_final %>% 
          filter(imd_decile %in% input$imd_decile)
        
      }
      
      # 11.1.0
      if(!is.null(input$gender)){
        df_final <- df_final %>% 
          filter(gender %in% input$gender)
      }
      
      return(df_final)
      
    }
    
    else if(input$ethnicityComparisonChoice == 'Public'){
      
      dfQfinal <- df_synth
      
      # 1.1.1
      if(!is.null(input$finYrComp)){
        dfQfinal <- dfQfinal %>%
          filter(fin_year_name %in% input$finYrComp)
      }
      
      # 9.1.1
      if(!is.null(input$LARegion)){
        dfQfinal <- dfQfinal %>% 
          filter(local_authority_name %in% input$LARegion)
      }
      
      # 2.1.1
      if(!is.null(input$ethInputGrouped) & input$ethnicityChoice == 'Grouped'){
        dfQfinal <- dfQfinal %>%
          filter(ethnicity_category %in% input$ethInputGrouped)
      }
      
      # 3.1.1
      if(!is.null(input$ethInputDetail) & input$ethnicityChoice == 'Detail'){
        dfQfinal <- dfQfinal %>%
          filter(ethnicity_detail %in% input$ethInputDetail)
      }
      
      # 7.1.1
      if(!is.null(input$IMDQuintileInput)){
        dfQfinal <- dfQfinal %>% 
          filter(imd_quintile %in% input$IMDQuintileInput)
      }
      
      return(dfQfinal)
      
    }
    
  })
  
  
  # Comparison if logic for returning all if nothing selected ---------------
  
  dfReturnComp <- reactive({
    
    if(input$ethnicityComparisonChoice %in% c('Grouped', 'Detail')){
      
      dfFinalComp <- df_synth 
      
      # 1.1.2
      if(!is.null(input$finYrComp)){
        dfFinalComp <- dfFinalComp %>%
          filter(fin_year_name %in% input$finYrComp)
      }
      
      # 9.2.1
      if(!is.null(input$LACompRegion)){
        dfFinalComp <- dfFinalComp %>% 
          filter(local_authority_name %in% input$LACompRegion)
      }
      
      # 2.3.1
      if(!is.null(input$ethInputGroupedComp) & input$ethnicityComparisonChoice == 'Grouped'){
        dfFinalComp <- dfFinalComp %>%
          filter(ethnicity_category %in% input$ethInputGroupedComp)
      }
      
      # 3.3.1
      if(!is.null(input$ethInputComp) & input$ethnicityComparisonChoice == 'Detail'){
        dfFinalComp <- dfFinalComp %>%
          filter(ethnicity_detail %in% input$ethInputComp)
      }
      
      ## IMD Selection
      
      # 6.2.1
      if(!is.null(input$IMDComp)){
        dfFinalComp <- dfFinalComp %>% 
          filter(imd_decile %in% input$IMDComp)
      }
      
      ## Comparison Gender
      
      # 11.2.1
      if(!is.null(input$genderComp)){
        dfFinalComp <- dfFinalComp %>% 
          filter(gender %in% input$genderComp)
      }
      
      return(dfFinalComp)
      
    }
    
    if(input$tabset == 'Age' & input$ethnicityComparisonChoice == 'Public'){
      
      dfPublicComp <- publicEthnicityEstimates
      
      # 4.1.0
      if(!is.null(input$pubEstEthnicity) & input$ethnicityComparisonChoice == 'Public'){
        dfPublicComp <- dfPublicComp %>%
          filter(Ethnicity %in% input$pubEstEthnicity)
      }
      
      # 10.1.0
      if(!is.null(input$pubLARegion) & input$ethnicityComparisonChoice == 'Public'){
        dfPublicComp <- dfPublicComp %>%
          filter(lad2014_name %in% input$pubLARegion)
      }
      
      # 13.1.0
      if(!is.null(input$genderInputComp) & input$ethnicityComparisonChoice == 'Public'){
        dfPublicComp <- dfPublicComp %>% 
          filter(Sex %in% input$genderInputComp)
      }
      
      return(dfPublicComp)
      
    }
    
    else if(input$tabset == 'IMD' & input$ethnicityComparisonChoice == 'Public'){
      
      dfIMDComp <- regionIMD
      
      # 5.1.0
      if(!is.null(input$pubIMDEthnicity) & input$ethnicityComparisonChoice == 'Public'){
        dfIMDComp <- dfIMDComp %>%
          filter(Ethnicity %in% input$pubIMDEthnicity)
      } 
      
      # 13.1.0
      if(!is.null(input$genderInputComp) & input$ethnicityComparisonChoice == 'Public'){
        dfIMDComp <- dfIMDComp %>% 
          filter(sex %in% input$genderInputComp)
      }
      
      return(dfIMDComp)
      
    }    
  })
  
  
  # PLOTS ---------------------------------------------------------------------
  
  
  # Age data and plot -------------------------------------------------------
  
  output$distPlot <- renderPlotly({
    
    ageFinal <- ageGroupFunction(dfReturn())
    
    ggplot(ageFinal, aes(x = age_at_referral, y = n)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Distribution of ages at referral of patients:", 
           y = "Number of distinct patients", x = "Age at Referral") + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 6),
            title = element_text(size = 10))  +
      scale_y_continuous(expand = c(0, 0),
                         breaks = function(x) unique(floor(pretty(x)))) +
      scale_x_continuous(breaks = seq(min(ageFinal$age_at_referral), max(ageFinal$age_at_referral), by = 1),
                         expand = c(0,0))
  })
  
  # Comparison Internal data Age plot -----------------------------------------------------
  
  output$distPlotComp <- renderPlotly({
    
    if(input$ethnicityComparisonChoice %in% c('Grouped', 'Detail')){
      
      ageFinalComp <- ageGroupFunction(dfReturnComp())
      
      ggplot(ageFinalComp, aes(x = age_at_referral, y = n)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Comparison distribution of ages at referral of patients:"),
             y = "Number of distinct patients", x = "Age at Referral") + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              # axis.text.x = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text = element_text(size = 6)) +
        scale_y_continuous(expand = c(0, 0),
                           breaks = function(x) unique(floor(pretty(x)))) +
        scale_x_continuous(breaks = seq(min(ageFinalComp$age_at_referral), max(ageFinalComp$age_at_referral), by = 1),
                           expand = c(0,0))
    }
    
    # Comparison Public Age data ----------------------------------------------
    
    else if(input$ethnicityComparisonChoice == 'Public'){
      
      ethnicityEstimates <- dfReturnComp() %>%
        group_by(age) %>%
        mutate(Totals = sum(Count)) 
      
      ggplot(ethnicityEstimates, aes(x = age, y = Totals)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Ethnicity Population Estimate by age and Local Authority 2017, Source: ONS",
             y = "mid year population estimated age 2017",
             x = "Population numbers") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text = element_text(size = 6)) +
        scale_y_continuous(expand = c(0, 0),
                           breaks = function(x) unique(floor(pretty(x))))
    }
  })
  
  # IMD data and plot -----------------------------------------------------
  
  output$distPlotIMD <- renderPlotly({
    
    if(input$ethnicityComparisonChoice == 'Public'){
      
      # shows by quintile if Public selected to aid comparison
      
      imdQuintileFinal <- imdGroupFunction(dfReturn(), "imd_quintile", 5)
      
      ggplot(imdQuintileFinal, aes(x = IMD, y = n)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Distribution of IMD Quintile at referral of patients to services",
             y = "Number of distinct patients", 
             x = "IMD Quintile") + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"),
              axis.text = element_text(size = 6)) +
        scale_y_continuous(expand = c(0, 0),
                           breaks = function(x) unique(floor(pretty(x)))) +
        scale_x_continuous(breaks = seq(min(imdQuintileFinal$IMD), max(imdQuintileFinal$IMD), by = 1),
                           expand = c(0,0))
    }
    
    
    
    else if(input$ethnicityChoice %in% c('Grouped', 'Detail')){
      
      imdFinal <- imdGroupFunction(dfReturn(), "imd_decile", 10)
      
      ggplot(imdFinal, aes(x = IMD, y = n)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Distribution of IMD Decile at referral of patients to services",
             y = "Number of distinct patients", 
             x = "IMD Decile") + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        scale_y_continuous(expand = c(0, 0),
                           breaks = function(x) unique(floor(pretty(x)))) +
        scale_x_continuous(breaks = seq(min(imdFinal$IMD), max(imdFinal$IMD), by = 1),
                           expand = c(0,0))
    }
  })
  
  
  # IMD Comparison data and plot --------------------------------------------
  
  output$distComparisonPlotIMD <- renderPlotly({
    
    if(input$ethnicityComparisonChoice %in% c('Grouped', 'Detail')){
      
      imdComparisonFinal <- imdGroupFunction(dfReturnComp(), "imd_decile", 10)
      
      ggplot(imdComparisonFinal, aes(x = IMD, y = n)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Comparison distribution of IMD Decile at referral of patients to services",
             y = "Number of distinct patients", 
             x = "IMD Decile") + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        scale_y_continuous(expand = c(0, 0),
                           breaks = function(x) unique(floor(pretty(x)))) +
        scale_x_continuous(breaks = seq(min(imdComparisonFinal$IMD), max(imdComparisonFinal$IMD), by = 1),
                           expand = c(0,0))
    }
    
    # Public IMD Comparison data ----------------------------------------------
    
    else if(input$ethnicityComparisonChoice == 'Public'){
      
      imdPublicFinal <- dfReturnComp() 
      
      ggplot(imdPublicFinal, aes(x = IMD.Quintile, y = Totals)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Distribution of East Midlands IMD Quintile 2013/14 (latest information)",
             y = "Number of people", 
             x = "IMD Quintile") + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        scale_y_continuous(expand = c(0, 0),
                           breaks = function(x) unique(floor(pretty(x)))) +
        scale_x_continuous(breaks = seq(min(imdPublicFinal$IMD.Quintile), max(imdPublicFinal$IMD.Quintile), by = 1),
                           expand = c(0,0))
    }
    
  })
  
  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)