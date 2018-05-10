# note use shinyapps.io to share app online
# devtools::install_github("rstudio/shinyapps")
# shiny-server


# Load necessary packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(maps)


# Load world map and fix virgin islands labels
worldmap <- map_data(map="world")
worldmap$region[worldmap$region == 'Virgin Islands'][1:23] <- "British Virgin Islands"
worldmap$region[worldmap$region == 'Virgin Islands'] <- "United States Virgin Islands"


# Get list of countries
countrylist <- unique(worldmap$region)


# Load access data
access_data <- read.csv('access_data.csv', header=TRUE, stringsAsFactors = F, na.strings="NA")


# Define data processing function with year, h, as input
mapDataFxn = function(h){
    # Get access data for year h
    access_df <- access_data[access_data$Year == as.numeric(h),]
    access_df <- cbind.data.frame(access_df$country, as.numeric(access_df$Nat.Elec.Rate), stringsAsFactors=FALSE)
    names(access_df) <- c("Country","Nat.Elec.Rate")
    # Replace electrification rates over 80% with NA
    access_df$Nat.Elec.Rate[access_df$Nat.Elec.Rate >= 60] <- NA
    # Merge access data with map.world by country
    df <- merge(worldmap, access_df, by.x = "region", by.y = "Country", sort = F, all.x=T)
    df2 <- df[order(df$order),]
    # Append column to map data and rename
    worldmap2 <- cbind.data.frame(worldmap, as.numeric(df2$Nat.Elec.Rate), stringsAsFactors=FALSE)
    names(worldmap2)[names(worldmap2) == "as.numeric(df2$Nat.Elec.Rate)"] <- "access"
    return(worldmap2)
}

# Get rid of axes for plot
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)




# Make navigation pane
sidebar <- dashboardSidebar(
    width = 250,
    hr(),
    sidebarMenu(id="tabs",
        menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
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
                    box( width = NULL, title = "Select a year:",
                        sliderInput("year"," ", value=1, min=1990, max = 2014, step=1)
                    )
                ),
                column(width = 8,
                    box(
                      width = NULL, 
                      plotOutput("plot",height="500px"),
                      collapsible = TRUE, 
                      title = "Map", 
                      status = "primary",
                      solidHeader = TRUE
                    ),
                    downloadButton('downloadTable', 'Download'),
                    br(),br()
                )
            )
        ),
        tabItem(tabName = "by_country",
            # split panel into 2 columns: 1 for year slider input, 1 for map output
            fluidRow( 
                # column(width = 4,
                #     box( width = NULL, title = "Select a country",
                #         dropdownMenu(type = "Country", .list = countrylist2)
                #     )
                # ),
                column(width = 8,
#                    box( width = NULL, plotOutput("plot",height="500px"), collapsible = TRUE, title = "Map", status = "primary", solidHeader = TRUE),
                    downloadButton('downloadTable', 'Download'),
                    br(),br()
                )
            )
        ),
        tabItem(tabName = "gif100",
            box( width = NULL, status = "primary", solidHeader = TRUE, 
                title="History of Global Electricity Access",
                downloadButton('downloadTable', 'Download'),
                br(),br()
#               tableOutput("table")
            )
        ),
        tabItem(tabName = "gif80",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="History of Electricity Access in High Impact Countries",                
                 downloadButton('downloadHtml', 'Download'),
                 br(),br()
#                 htmlOutput("picture")
#                 gif(src = "https://drive.google.com/open?id=1jy04HJ-Vrk-S-NXokb5xFc7YG1Lb9S2E")
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

    output$plot <- renderPlot({
        worldmap2 <- mapDataFxn(input$year)
        gg = ggplot() + 
            ggtitle(as.character(h)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_map(
                data=worldmap2, 
                map=worldmap2,
                aes(x=long, y=lat, map_id=region, fill=access)
            ) + 
            scale_fill_gradient(low = "orange", high = "blue", guide = "colourbar") + 
            coord_equal() +
            ditch_the_axes +
            annotate("text",x=160, y=66.5,label = "\U00A9 K. Ramirez-Meyers",col="white", cex=1,alpha = 0.8)
        print(gg)
    })
  
})

shinyApp(ui = ui, server = server)

