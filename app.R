


# Load necessary packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(maps)
library(magick)

# Load world map and fix virgin islands labels
worldmap <- map_data(map="world")
worldmap$region[worldmap$region == 'Virgin Islands'][1:23] <- "British Virgin Islands"
worldmap$region[worldmap$region == 'Virgin Islands'] <- "United States Virgin Islands"


# Get list of countries
countrylist <- data.frame(unique(worldmap$region))
countrylist <- countrylist[order(countrylist$unique.worldmap.region),]

# Load access data
access_data <- read.csv('access_data.csv', header=TRUE, stringsAsFactors = F, na.strings="NA")

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

# Define data processing function with year, h, as input
YearMapDataFxn = function(h,MaxPerc=100){
  
     # Get access data for year h
     access_df <- access_data[access_data$Year == as.numeric(h),]
     access_df <- cbind.data.frame(access_df$country, as.numeric(access_df$Nat.Elec.Rate), stringsAsFactors=FALSE)
     names(access_df) <- c("Country","Nat.Elec.Rate")
     # Replace electrification rates over 80% with NA
     access_df$Nat.Elec.Rate[access_df$Nat.Elec.Rate > MaxPerc] <- NA
     # Merge access data with map.world by country
     df <- merge(worldmap, access_df, by.x = "region", by.y = "Country", sort = F, all.x=T)
     df2 <- df[order(df$order),]
     # Append column to map data and rename
     worldmap2 <- cbind.data.frame(worldmap, as.numeric(df2$Nat.Elec.Rate), stringsAsFactors=FALSE)
     names(worldmap2)[names(worldmap2) == "as.numeric(df2$Nat.Elec.Rate)"] <- "access"
     return(worldmap2)
}

# Define data processing function with year, h, as input
CountryMapDataFxn = function(c,h){
  
    # Get access data for year h, country c
    access_df <- access_data[access_data$country == c,]
    access_df <- access_df[access_df$Year == h,]
    # Merge access data with worldmap by country
    countrymap <- worldmap[worldmap$region == c,]
    df <- merge(countrymap, access_df, by.x = "region", by.y = "country")
    df2 <- df[order(df$Year),]
    names(df2)[8:10] <- c("National","Rural","Urban")
    return(df2)
  
}

### COMMENT OUT
##  Generate images to make GIFs
# for(h in 1990:2014){
#     worldmap2 <- YearMapDataFxn(h,50)
#     # Plot
#     gg <- ggplot() +
        # ggtitle(as.character(h)) +
        # theme(plot.title = element_text(hjust = 0.5, size = 30)) +
#         # theme(plot.margin=unit(c(0,0,0,0),"mm")) +
#         geom_map(
#             data=worldmap2,
#             map=worldmap2,
#             aes(x=long, y=lat, map_id=region, fill=access)
#         ) +
#         scale_fill_gradient(low = "orange", high = "blue", guide = "colourbar", limits=c(0,100)) +
#         coord_equal() +
#         ditch_the_axes +
#         annotate("text",x=160, y=66.5,label = "\U00A9 K. Ramirez-Meyers",col="white", cex=2,alpha = 0.8)
#     gg
#     column <- paste0(h,"_Access")
#     ggsave(paste0(column,"-max50.jpg"), dpi = 72)
# }



ui <- dashboardPage(
  
    dashboardHeader(title = "Global Electricity Access", titleWidth = 300),
    
    dashboardSidebar(
      width = 300,
      hr(),
      sidebarMenu(id="tabs",
                  menuItem("ReadMe", tabName = "readme", icon=icon("info")),
                  menuItem("Global Access By Year", tabName="by_year", icon=icon("globe"), selected=TRUE),
                  menuItem("Rural Versus Urban Access By Country", tabName="by_country", icon=icon("adjust"), selected=TRUE),
                  menuItem("Gifs",  tabName = "gifs", icon = icon("play"),
                      menuSubItem("All Countries", tabName = "gif100", icon = icon("angle-right")),
                      menuSubItem("High Impact Countries", tabName = "gif50", icon = icon("angle-right"))
                  )
      ),
      hr()
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "by_year",
                fluidRow( # split panel into 2 columns: 1 for year slider input, 1 for map output
                    column(width = 2,
                        box( width = NULL, title = "Select a year:",solidHeader = TRUE,status = "primary", 
                            sliderInput(inputId="inputyear",label="", value=1990, min=1990, max = 2014, step=1, sep ="")
                        )
                    ),
                    column(width = 10,
                         box(
                           height = NULL, 
                           width = NULL, solidHeader = TRUE,
                           title = textOutput("MapTitle"), status = "primary", 
                           plotOutput("YearMap")
                         )
                    )
                )
            ),
            tabItem(tabName = "by_country",
                fluidRow(
                    column(width = 4,
                        box( width = NULL, title = "Select a country:",solidHeader = TRUE,status = "primary",
                            selectizeInput(inputId="inputcountry", h4(""),
                                options = list(dropdownParent = 'body'),
                                choices = countrylist
                            )
                            # tags$head(tags$style(".selectize-control.single { width: 150px; z-index: 1; }"))
                        ),
                        box( width = NULL, title = "Choose a region:",solidHeader = TRUE,status = "primary",
                             radioButtons(inputId="inputregion", "", 
                                          c("National"="National","Urban"="Urban","Rural"="Rural"))
                        ),
                        box( width = NULL, title = "Select a year:",solidHeader = TRUE,status = "primary", 
                             sliderInput(inputId="inputyear_c",label="", value=2014, min=1990, max = 2014, step=1, sep ="")
                        )
                    ),
                    column(width = 8,
                        box(
                            height = NULL, width = NULL, solidHeader = TRUE,
                            title = textOutput("CountryMapTitle"), status = "primary",
                            plotOutput("CountryMap")
                        )
                    )
                )
            ),
            tabItem(tabName = "gif100",
                    box( width = NULL, status = "primary", solidHeader = TRUE,
                         title="History of Global Electricity Access",
                         imageOutput("gif100"),
                         downloadButton('downloadGif100', 'Download')
                    )
            ),
            tabItem(tabName = "gif50",
                    box( width = NULL, status = "primary", solidHeader = TRUE, 
                         title="History of Electricity Access in High Impact Countries",
                         imageOutput("gif50"),
                         downloadButton('downloadGif50', 'Download')
                    )
            )
        )
    )
)



server <- function(input, output) ({
    
    
    output$MapTitle <- renderText(paste0("Map of Electricity Access in ",input$inputyear))
    
    
    output$YearMap <- renderPlot({
        
        # Get input year
        h <- input$inputyear
        
        # Get mapping data
        worldmap2 <- YearMapDataFxn(h)
        
        # Plot
        gg = ggplot() + 
            geom_map(
                data=worldmap2, 
                map=worldmap2,
                aes(x=long, y=lat, map_id=region, fill=access)
            ) + 
            scale_fill_gradient(low = "orange", high = "blue", guide = "colourbar", limits=c(0,100)) + 
            coord_equal() +
            ditch_the_axes +
            annotate("text",x=160, y=66.5,label = "\U00A9 K. Ramirez-Meyers",col="white", cex=2,alpha = 0.8)
        gg
        
    })
    
    output$CountryMapTitle <- renderText({
      
        c <- input$inputcountry
        r <- input$inputregion
        if(r == "National"){r <- ""}
        paste0("Map of Electricity Access in ",r," ",c," in ",input$inputyear_c)
    })
    
    output$CountryMap <- renderPlot({
      
        # Get input year and country
        h <- input$inputyear_c
        c <- input$inputcountry
        r <- input$inputregion
        
        # Get mapping data
        map2 <- CountryMapDataFxn(c,h)
        map <- map2[,1:6]
        map$access <- map2[names(map2) == r]
        names(map)[ncol(map)] <- "access"
        
        # Plot
        gg = ggplot() +
            ggtitle(paste0(as.character(map$access[1,1]),"%")) +
            theme(plot.title = element_text(hjust = 0.5, size = 30)) +
            geom_map(
                data=map, 
                map=map,
                aes(x=long, y=lat, map_id=region, fill=access)
            ) + 
            scale_fill_gradient(low = "orange", high = "blue", guide = "colourbar", limits=c(0,100)) + 
            coord_equal()
        # Ammend gg for countries that do not have data
        if(dim(map$access)[1]==0){
            map <- worldmap[worldmap$region==c,]
            gg = ggplot() +
                ggtitle(paste0("Data is unavailable for \n",as.character(c))) +
                geom_map(
                    data=map, 
                    map=map,
                    aes(x=long, y=lat, map_id=region),fill = "gray"
                ) +
                theme(plot.title = element_text(hjust = 0.5, size = 30))
        }else if(is.na(map$access[1,1]) == T){
            gg = ggplot() +
                ggtitle(paste0("Data is unavailable for \n", c)) +
                geom_map(
                    data=map, 
                    map=map,
                    aes(x=long, y=lat, map_id=region),fill = "#ffffff"
            ) +
            coord_equal()
            theme(plot.title = element_text(hjust = 0.5, size = 30))
        }else{
            gg = gg
        }
        gg
      
    })
    
    output$gif100 <- renderImage({
      
        tmpfile <- image_read("gif100.gif") %>% 
            image_resize("90%") %>%
            image_animate(fps=4) %>%
            image_write(tempfile(fileext='gif'), format = 'gif')
        
        list(src = tmpfile, contentType = "image/gif")

    })
    
    output$gif50 <- renderImage({
      
      tmpfile <- image_read("gif50.gif") %>% 
        image_resize("90%") %>%
        image_animate(fps=4) %>%
        image_write(tempfile(fileext='gif'), format = 'gif')
      
      list(src = tmpfile, contentType = "image/gif")
      
    })
    
    
    output$downloadGif100 <- downloadHandler(
      filename = "gif100.gif",
      content = function(file){
        image_write(im = image_read("gif100.gif"), path = file)
      }
    )
    
    
    output$downloadGif50 <- downloadHandler(
      filename = "gif50.gif",
      content = function(file){
          image_write(im = image_read("gif50.gif"), path = file)
      }
    )
  
})



shinyApp(ui = ui, server = server)



