# note use shinyapps.io to share app online
# devtools::install_github("rstudio/shinyapps")
# shiny-server

library(shiny)
library(shinydashboard)

# Make navigation pane
sidebar <- dashboardSidebar(
    width = 250,
    hr(),
    sidebarMenu(id="tabs",
        menuItem("Maps", tabName="maps", icon=icon("globe"), selected=TRUE,
            menuSubItem("By Year", tabName = "by_year", icon = icon("angle-right")),
            menuSubItem("By Country", tabName = "by_country", icon = icon("angle-right"))
        ),
        menuItem("Gifs",  tabName = "gifs", icon = icon("play"),
            menuSubItem("All Countries", tabName = "gif100", icon = icon("angle-right")),
            menuSubItem("High Impact Countries", tabName = "gif80", icon = icon("angle-right"))
        )
    ),
    hr()
)

# Make 
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "by_year",
            fluidRow( # split panel into 2 columns: 1 for year slider input, 1 for map output
                column(width = 4,
#                    tabBox( width = NULL,
#                        tabPanel(h5("Select a Year"),
                            sliderInput("year", "Select a year:", value=1, min=1990, max = 2014, step=1)
#                        )
#                    )
                ),
                column(width = 8,
                    box( width = NULL, plotOutput("plot",height="500px"), collapsible = TRUE,
                    title = "Map", status = "primary", solidHeader = TRUE),
                    downloadButton('downloadTable', 'Download'),
                    br(),br()
                )
            )
        ),
        tabItem(tabName = "by_country",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Select a Country",                
                downloadButton('downloadTable', 'Download'),
                br(),br()
#                tableOutput("table")
            )
        ),
        tabItem(tabName = "gif100",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="History of Global Electricity Access",
                     downloadButton('downloadTable', 'Download'),
                     br(),br()
#                     tableOutput("table")
            )
        ),
        tabItem(tabName = "gif80",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="History of Electricity Access in High Impact Countries",                
                 downloadButton('downloadTable', 'Download'),
                 br(),br()
#                 tableOutput("table")
            )
        )
    )
)

ui <- dashboardPage(
  dashboardHeader(title = "Global Electricity Access", titleWidth = 250),
  sidebar,
  body
)



############################



server <- function(input, output) ({
    
    output$maps
})

shinyApp(ui = ui, server = server)

