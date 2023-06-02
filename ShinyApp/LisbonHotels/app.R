# https://www.sciencedirect.com/science/article/pii/S2352340918315191#s0005

# Import libraries
library(shiny)
library(readr)
library(plotly)
library(dplyr)
library(magrittr)
library(shinythemes)

# Import data
df <- readr::read_csv("H2.csv")

# Data Wrangling
df %<>% dplyr::select(c("IsCanceled", "LeadTime","ArrivalDateWeekNumber","Children",	"Babies", "DistributionChannel",
                            "ADR", "Country")) %>%
            dplyr::filter(IsCanceled == 0) %>%
            dplyr::select(-"IsCanceled")

df$LeadTime %<>% as.integer
df$ArrivalDateWeekNumber %<>% as.integer
df$Children %<>% as.integer
df$Babies %<>% as.integer
df$DistributionChannel %<>% as.factor
df$Country %<>% as.factor

df %<>%  mutate(Family = ifelse(Children + Babies > 0, TRUE, FALSE)) %>%
        dplyr::select(-c(Children, Babies))
df %<>%  mutate(Domestic = ifelse(Country == "PRT", TRUE, FALSE)) %>%
  dplyr::select(-"Country")

df$Family %<>% as.logical
df$Domestic %<>% as.logical

df %<>%  mutate(DistributionChannel = recode(DistributionChannel, "GDS" = "TA/TO"))
levels(df$DistributionChannel) <- c("Corporate", "Direct", "Travel Agent")
df %<>%  mutate(Corporate = ifelse(DistributionChannel == "Corporate", TRUE, FALSE))

df %<>% dplyr::rename(DailyRate = ADR)


# Define UI for application
ui <- navbarPage(theme = shinytheme("flatly"),
        title = "DATA 608",
        tabPanel(title = "Introduction",
          h4(strong("Project Description")),
          "This project utilizes publicly available hotel booking data compiled by researchers from the University Institute of Lisbon. It is available at:", 
          a(href = "https://www.sciencedirect.com/science/article/pii/S2352340918315191",
            "Science Direct. "),
          "The researchers sanitized actual data from two hotels SQL databases, sanitized customer and hotel identities and released it under a creative common license. This data set is particularly useful for visualizations as well as possible modeling scenarios.",
          br(),
          br(), 
          "In this project I aim to create useful visualizations representing changes over time of key variables for the hotel in Lisbon. This could be useful for both a business reporting scenario – local hotel management trying to address monthly or weekly trends. As well as possible traveler research scenarios.",
          br(),
          br(), 
          "My visualizations show trends in the hotel pricing, booking lead times and guest demographic trends. Reactive filters for all visualizations allow the data to be filtered by traveler place of origin, family travel status and business travel status. 
          The code for this application is available on"),
          a(href = "https://github.com/adavidowitz100/DATA608Visualization/tree/main/FinalProject/LisbonHotels",
          "Github."),
        tabPanel(title = "Visualizations", 
          fluidPage(
          
          # Application title
          titlePanel("Traveler Hotel Data in Lisbon Portugal"),
      
          # Sidebar with a slider input
          sidebarLayout(
              sidebarPanel(
                  sliderInput(inputId  = "weeks",
                              label  = "Week:",
                              min = 1,
                              max = 53,
                              value = c(1, 53)),
                  checkboxGroupInput(inputId = "family", 
                              label = "Include by Family Type:", 
                              choices = c("Family", "No Children")),
                  checkboxGroupInput(inputId = "domestic", 
                              label = "Include by Traveler Origin:", 
                              choices = c("Domestic", "International")),
                  checkboxGroupInput(inputId = "corporate", 
                              label = "Include by Corporate Type:", 
                              choices = c("Business", "Leisure")),
                  actionButton("clear_button", "Clear All Selections")
              ),
      
              # Show plots
              mainPanel(
                tabsetPanel(
                  br(),
                  tabPanel(title = "Rates",plotlyOutput(outputId ="ratePlot")),
                  tabPanel(title = "Lead Times",plotlyOutput(outputId ="leadPlot")),
                  tabPanel(title = "Guests",plotlyOutput(outputId ="ratioPlot"))
                )
              )
          )
      )
)
)
# Define server logic required for plots
server <- function(input, output, session) {
  
  observeEvent(input$clear_button, {
    updateSelectInput(session, "family", selected = "")
    updateSelectInput(session, "domestic", selected = "")
    updateSelectInput(session, "corporate", selected = "")
    updateSliderInput(session, "weeks", value = c(1, 53))
    
  })
  
   filtered_data <- reactive({
        # Date slider filter
          filtered <- df %>% filter(ArrivalDateWeekNumber >= input$weeks[1] & ArrivalDateWeekNumber <= input$weeks[2])
    
        # Check box filter logic
        if (is.null(input$family) || input$family == "" || ("Family" %in% input$family & "No Children" %in% input$family)){
          filter_family <- c(TRUE, FALSE)
        }
        else { if ("Family" %in% input$family) {
          filter_family <- c(TRUE)
        }
          else filter_family <- FALSE
        }
        
        if (is.null(input$domestic) || input$domestic == "" || ("Domestic" %in% input$domestic & "International" %in% input$domestic)){
          filter_domestic <- c(TRUE, FALSE)
        }
        else { if ("Domestic" %in% input$domestic) {
          filter_domestic <- c(TRUE)
        }
          else filter_domestic <- FALSE
        }  
          
        if (is.null(input$corporate) || input$corporate == "" || ("Business" %in% input$corporate & "Leisure" %in% input$corporate)){
          filter_corporate <- c(TRUE, FALSE)
        }
        else { if ("Business" %in% input$corporate) {
          filter_corporate <- c(TRUE)
        }
          else filter_corporate <- FALSE
        }      
          
        filtered %<>% filter(Family %in% filter_family & Domestic %in% filter_domestic & Corporate %in% filter_corporate)
        
    })

    output$ratePlot <- renderPlotly({
        filtered_rate <- filtered_data()
        filtered_rate %<>% group_by(ArrivalDateWeekNumber) %>%
          summarise(Rate = mean(DailyRate, na.rm = TRUE)) 
        plot <- plot_ly(filtered_rate, x = ~ArrivalDateWeekNumber, y = ~Rate, type = "scatter", mode="lines" , line = list(shape = "spline"))
        
        plot <- plot %>% layout(
          title = "Average Daily Rate",
          xaxis = list(title = "Week"),
          yaxis = list(title = "Rate", range = c(0, 180))
        )
    })
    
    output$leadPlot <- renderPlotly({
      filtered_lead <- filtered_data()
      filtered_lead %<>% group_by(ArrivalDateWeekNumber) %>%
        summarise(Days = mean(LeadTime, na.rm = TRUE)) 

      plot <- plot_ly(filtered_lead, x = ~ArrivalDateWeekNumber, y = ~Days, type = "scatter", mode="lines", line = list(shape = "spline"))

      plot <- plot %>% layout(
        title = "Average Lead Booking",
        xaxis = list(title = "Week"),
        yaxis = list(title = "Days", range = c(0, 150))
      )

    })
    
    output$ratioPlot <- renderPlotly({
      filtered_ratio_family <- filtered_data()
      filtered_ratio_domestic <- filtered_data()
      filtered_ratio_corporate <- filtered_data()
      filtered_ratio_family <- filtered_ratio_family %>% group_by(ArrivalDateWeekNumber) %>%
        summarise(family_percentage = mean(Family) * 100)
      filtered_ratio_domestic <- filtered_ratio_domestic %>% group_by(ArrivalDateWeekNumber) %>%
        summarise(domestic_percentage = mean(Domestic) * 100) 
      filtered_ratio_corporate <- filtered_ratio_corporate %>% group_by(ArrivalDateWeekNumber) %>%
        summarise(corporate_percentage = mean(Corporate) * 100) 
      
      plot <- plot_ly()
      plot <- plot %>%
        add_trace(data = filtered_ratio_family, x = ~ArrivalDateWeekNumber, y = ~family_percentage, type = "scatter", mode="lines", line = list(shape = "spline"), name="Families") %>%
        add_trace(data=filtered_ratio_domestic, x = ~ArrivalDateWeekNumber, y = ~domestic_percentage, type = "scatter", mode="lines", line = list(shape = "spline"), name="Domestic") %>%
        add_trace(data=filtered_ratio_corporate, x = ~ArrivalDateWeekNumber, y = ~corporate_percentage, type = "scatter", mode="lines", line = list(shape = "spline"), name="Corporate")
        
      plot <- plot %>% layout(
        title = "Guest Distribution Over Time",
        xaxis = list(title = "Week"),
        yaxis = list(title = "Percent", range = c(0, 100))
      )
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
