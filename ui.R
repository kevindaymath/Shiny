library(shiny)
library(leaflet)
states = read.csv("data/USStateOrder.csv")
states <- states[order(states$NAME),] #order data via region name
newnames2 <- c("African American", "Medicaid Eligibles", "< 65 W/o Health Insurance",
              "Persons in Poverty", "Urban Population", "High Education", "Workers")

# Define UI for dataset viewer application
fluidPage(
  
  # Application title
  titlePanel("Public Health Regions and ARHF Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Regions"),
      selectInput("state",label="Choose a state",
                  choices=states$NAME, selected="Alabama"),
      br(),
      uiOutput("countySelector")
      
    ),
    
      

    mainPanel(
      verbatimTextOutput("region"),
      tabsetPanel(
        tabPanel("County", DT::dataTableOutput("Info")), 
        tabPanel("Region", DT::dataTableOutput("RegionInfo"),verbatimTextOutput("regiontotal")),
        tabPanel("Map", leafletOutput("Map")),
        tabPanel("SS Participant # Map", leafletOutput("FullMap")),
        tabPanel("Percent Variables Map", selectInput("variable",label="Choose a Variable",
                                     choices=newnames2, selected="African American"),
                 leafletOutput("PercentMap"))
      )
      
      # verbatimTextOutput("region"),
      # actionButton("renderMatrix","Region Info"),
      # dataTableOutput("Info")
      
      # plotOutput("plot_map")
    )
)

)