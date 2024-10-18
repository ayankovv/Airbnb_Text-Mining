library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Airbnb Descriptions of Postings Analysis", titleWidth = 400),
  dashboardSidebar(
    width = 280,
    selectInput("countrySelect", "Choose Country:", 
                choices = c("United States", "Turkey", "Brazil", "Australia", "Canada", "China", "Hong Kong", "Portugal"),
                selected = "United States", multiple = FALSE),
    sliderInput("freqSlider", "Score rating of the property:",
                min = 20, max = 100, value = 20, step = 1),
    selectInput("themeSelector", "Theme",
                choices = c("Light" = "light", "Dark" = "dark"), selected = "light"),
    actionButton("resetButton", "Reset Filters", icon = icon("refresh")),
    actionButton("aboutBtn", "About", icon = icon("info-circle"))
  ),
  dashboardBody(
    useShinyjs(),  # Ensure shinyjs is initialized
    tags$head(
      tags$style(HTML('
                /* Custom styles for the tab navigation */
                .nav-tabs a {
                    font-size: 18px;  /* Increase font size */
                    font-weight: bold; /* Make font bold */
                    color: #337ab7; /* Set a specific font color */
                }
                /* Change background and text color when hovering over the tab */
                .nav-tabs a:hover {
                    background-color: #f8f9fa; /* Light grey background on hover */
                    color: #0056b3; /* Darker blue text on hover */
                }
                /* Active tab style */
                .nav-tabs .active a {
                    background-color: #007bff; /* Blue background for active tab */
                    color: white; /* White text for active tab */
                }
            '))
    ),
    tabsetPanel(id = "tabs",
                tabPanel("Analysis", 
                         fluidRow(
                           box(title = "Key Phrase Patterns in Airbnb Listings", status = "primary", solidHeader = TRUE, 
                               plotOutput("bigramGraph", height = "300px")),
                           box(title = "Sentiments", status = "primary", solidHeader = TRUE, 
                               plotOutput("positiveWordCloud", height = "300px")),
                           box(title = "TF-IDG Score By Bigrams", status = "primary", solidHeader = TRUE, 
                               plotOutput("negativeWordCloud", height = "300px")),
                           box(title = "Top Terms in Each Topic using LDA model", status = "primary", solidHeader = TRUE, 
                               plotOutput("topTermsTopics", height = "300px"))
                         )),
                tabPanel("Map", 
                         leafletOutput("map", height = "600px")
                )
    )
  )
)