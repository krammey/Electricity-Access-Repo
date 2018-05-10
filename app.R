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
YearMapDataFxn = function(h,MaxPerc=100){
  
     # Get access data for year h
     access_df <- access_data[access_data$Year == as.numeric(h),]
     access_df <- cbind.data.frame(access_df$country, as.numeric(access_df$Nat.Elec.Rate), stringsAsFactors=FALSE)
     names(access_df) <- c("Country","Nat.Elec.Rate")
     # Replace electrification rates over 80% with NA
     access_df$Nat.Elec.Rate[access_df$Nat.Elec.Rate >= MaxPerc] <- NA
     # Merge access data with map.world by country
     df <- merge(worldmap, access_df, by.x = "region", by.y = "Country", sort = F, all.x=T)
     df2 <- df[order(df$order),]
     # Append column to map data and rename
     worldmap2 <- cbind.data.frame(worldmap, as.numeric(df2$Nat.Elec.Rate), stringsAsFactors=FALSE)
     names(worldmap2)[names(worldmap2) == "as.numeric(df2$Nat.Elec.Rate)"] <- "access"
     return(worldmap2)
}

### COMMENT OUT
# Generate images to make GIFs
for(h in 1990:2014){
    worldmap2 <- YearMapDataFxn(h,50)
    # Plot
    gg <- ggplot() +
        ggtitle(as.character(h)) +
        theme(plot.title = element_text(hjust = 0.5, size = 30)) +
        geom_map(
            data=worldmap2,
            map=worldmap2,
            aes(x=long, y=lat, map_id=region, fill=access)
        ) +
        scale_fill_gradient(low = "orange", high = "blue", guide = "colourbar") +
        coord_equal() +
        ditch_the_axes +
        annotate("text",x=160, y=66.5,label = "\U00A9 K. Ramirez-Meyers",col="white", cex=2,alpha = 0.8)
    gg
    column <- paste0(h,"_Access")
    ggsave(paste0(column,"-max50.jpg"), dpi = 72)
}
  
  
# Get rid of axes for plot
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_blank()
)



ui <- dashboardPage(
  
    dashboardHeader(title = "Global Electricity Access", titleWidth = 250),
    
    
    dashboardSidebar(
      width = 250,
      hr(),
      sidebarMenu(id="tabs",
                  #        menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
                  menuItem("Maps", tabName="maps", icon=icon("globe"), selected=TRUE,
                           menuSubItem("By Year", tabName = "by_year", icon = icon("angle-right"))
                           # menuSubItem("By Country", tabName = "by_country", icon = icon("angle-right"))
                  ),
                  menuItem("Gifs",  tabName = "gifs", icon = icon("play"),
                      menuSubItem("All Countries", tabName = "gif100", icon = icon("angle-right"))
                  #     menuSubItem("High Impact Countries", tabName = "gif80", icon = icon("angle-right"))
                  )
      ),
      hr()
    ),
    
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "by_year",
                fluidRow( # split panel into 2 columns: 1 for year slider input, 1 for map output
                    column(width = 4,
                        box( width = NULL, title = "Select a year:",solidHeader = TRUE,status = "primary", 
                            sliderInput(inputId="inputyear",label="", value=1990, min=1990, max = 2014, step=1, sep ="")
                        )
                    ),
                    column(width = 8,
                         box(
                           height = NULL, width = NULL, solidHeader = TRUE,title = textOutput("MapTitle"), status = "primary", 
                           plotOutput("YearMap")
                         )
                         # box(
                         #   width = NULL, collapsible = TRUE, title = "Map", status = "primary", solidHeader = TRUE,
                         #   plotOutput("plot",height="500px")
                         # ),
                         # downloadButton('downloadTable', 'Download'),
                         # br(),br()
                    )
                )
            ),
#             tabItem(tabName = "by_country",
#                     fluidRow( 
#                       # column(width = 4,
#                       #     box( width = NULL, title = "Select a country",
#                       #         dropdownMenu(type = "Country", .list = countrylist2)
#                       #     )
#                       # ),
#                       column(width = 8,
# #                    box( width = NULL, plotOutput("plot",height="500px"), collapsible = TRUE, title = "Map", status = "primary", solidHeader = TRUE),
#                              downloadButton('downloadTable', 'Download'),
#                              br(),br()
#                       )
#                     )
#             ),
            tabItem(tabName = "gif100",
                    box( width = NULL, status = "primary", solidHeader = TRUE,
                         title="History of Global Electricity Access",
                         imageOutput("gif100")
                         # downloadButton('downloadTable', 'Download'),
                         # br(),br()
                    )
            )
#             tabItem(tabName = "gif80",
#                     box( width = NULL, status = "primary", solidHeader = TRUE, title="History of Electricity Access in High Impact Countries",                
#                          downloadButton('downloadHtml', 'Download'),
#                          br(),br()
#                          #                 htmlOutput("picture")
#                          #                 gif(src = "https://drive.google.com/open?id=1jy04HJ-Vrk-S-NXokb5xFc7YG1Lb9S2E")
#                     )
#             )
        )
    )
)


############################



server <- function(input, output) ({
    
    
    output$MapTitle <- renderText(paste0("Map of Electricity Access in ",input$inputyear))
    
    
    output$YearMap <- renderPlot({
        
        # Get input year
        h <- input$inputyear
        
        # Get mapping data
        worldmap2 <- YearMapDataFxn(h)
        
        # Plot
        gg = ggplot() + 
            # ggtitle(as.character(h)) +
            # theme(plot.title = element_text(hjust = 0.5)) +
            geom_map(
                data=worldmap2, 
                map=worldmap2,
                aes(x=long, y=lat, map_id=region, fill=access)
            ) + 
            scale_fill_gradient(low = "orange", high = "blue", guide = "colourbar") + 
            coord_equal() +
            ditch_the_axes +
            annotate("text",x=160, y=66.5,label = "\U00A9 K. Ramirez-Meyers",col="white", cex=2,alpha = 0.8)
        print(gg)
        
    })
    
    
    output$gif100 <- renderImage({
      
        tmpfile <- image_read("gif100.gif") %>% 
            image_animate(fps=4) %>%
            image_write(tempfile(fileext='gif'), format = 'gif')
        
        list(src = tmpfile, contentType = "image/gif")
      
        # worldmapALL <- YearMapDataFxn(1990)
        # worldmapALL$year <- 1990
        # for(y in 1991:2014){
        #    tempmap <- YearMapDataFxn(y)
        #    tempmap$year <- y
        #    worldmapALL <- rbind.data.frame(worldmapALL,tempmap,stringsAsFactors=FALSE)
        # }
        # 
        # 
        # p <- ggplot() + 
        #   # ggtitle(as.character(h)) +
        #   # theme(plot.title = element_text(hjust = 0.5)) +
        #   geom_map(
        #     data=worldmapALL, 
        #     map=worldmapALL,
        #     aes(x=long, y=lat, map_id=region, fill=access,frame = year)
        #   ) + 
        #   scale_fill_gradient(low = "orange", high = "blue", guide = "colourbar") + 
        #   coord_equal() +
        #   ditch_the_axes +
        #   annotate("text",x=160, y=66.5,label = "\U00A9 K. Ramirez-Meyers",col="white", cex=2,alpha = 0.8)
        # gganimate(p)
    })
  
})

shinyApp(ui = ui, server = server)



