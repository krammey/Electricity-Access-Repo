
# load necessary libraries
library(pdftools)
library(xml2)
library(rvest)

# Get full list of countries
url <- "http://gtf.esmap.org/countries"
main_list <- read_html(url)
main_list2 <- html_attr(html_nodes(main_list, "a"), "href")
main_list3 <- main_list2[grep("/country/", main_list2)]
COUNTRIES <- unique(main_list3)

x <- matrix(vector(), 0, 5, dimnames=list(c(), c('Year', 'Nat.Elec.Rate', 'Rural.Elec.Rate', 'Urban.Elec.Rate','Country')))

# download all pdfs
for(k in 1:length(COUNTRIES)){ # looping through countries to download each report
    country <- gsub("/country/","",COUNTRIES[k]) # get name of country
    location <- paste0("http://gtf.esmap.org",COUNTRIES[k]) # navigate to country page
    page <- read_html(location)
    page2 <- html_attr(html_nodes(page, "a"), "href") 
    pdf_url <- page2[grep("/export/pdf/", page2)] # extract export link for report
    pdf_url2 <- paste0("http://gtf.esmap.org",pdf_url)
    pdf_name <- paste0(country,".pdf")
    # download.file(url = pdf_url2, destfile = pdf_name, mode = 'wb')
    txt <- pdf_text(pdf_name)
    txt2 <- strsplit(txt, "\n")
    pages <- length(txt2)
    for(i in 1:pages){ # loop through pages of report
      y <- matrix(vector(), 0, 4, dimnames=list(c(), c('Year', 'Nat.Elec.Rate', 'Rural.Elec.Rate', 'Urban.Elec.Rate')))
      page_lines <- length(txt2[[i]])
      for(j in 1:page_lines){ # loop through lines on each page
        line <- strsplit(txt2[[i]][j], split = "  ")
        line <- line[[1]][!line[[1]] == ""]
        if(length(line) == 4){ # identify lines we want
          y <- rbind(y, line)
        }# end if
      } # end j
      # make the matrix y a dataframe
      y <- data.frame(y,stringsAsFactors=FALSE)
      country <- gsub("-"," ",country) # Capitalize country name
      y$country <- sapply(country, simpleCap) # append country to y
      x <- rbind(x, y) # append y (a page-worth of lines to final dataframe, x)
    } # end i
} # end k

# Save access data to csv
write.csv(x, file = 'AccessData_1990_to_2014.csv', row.names = F)


#-----------------------------
#            Mapping
#-----------------------------

# load packages
library(ggplot2)
library(ggmap)
library(maps)

# Load access data create above
access_data <- read.csv('AccessData_1990_to_2014.csv', header=TRUE, stringsAsFactors = F, na.strings="NA")
access_data$country[access_data$country == "C%C3%B4te Divoire"] <- "Ivory Coast"
access_data$country[access_data$country == "United States America"] <- "USA"
access_data$country[access_data$country == "United Kingdom"] <- "UK"

# load world map
map.world <- map_data(map="world")

# compare the 2 lists
countrylist2 <- unique(access_data$country) # GTF country list
countrylist <- unique(map.world$region) # 'Maps' country list
countrylist2[!(countrylist2 %in% countrylist)]
countrylist[!(countrylist %in% countrylist2)]

# Fix the GTF country list
access_data <- rbind.data.frame(access_data,access_data[access_data$country == "Antigua And Barbuda",])
access_data$country[5826:5850] <- "Barbuda"
access_data <- rbind.data.frame(access_data,access_data[access_data$country == "Saint Kitts And Nevis",])
access_data$country[5851:5875] <- "Nevis"
access_data$country[access_data$country == "Saint Kitts And Nevis"] <- "Saint Kitts"
access_data <- rbind.data.frame(access_data,access_data[access_data$country == "Trinidad And Tobago",])
access_data$country[5876:5900] <- "Tobago"
access_data$country[access_data$country == "Trinidad And Tobago"] <- "Trinidad"
access_data <- rbind.data.frame(access_data,access_data[access_data$country == "Saint Vincent And Grenadines",])
access_data$country[5901:5925] <- "Grenadines"
access_data$country[access_data$country == "Saint Vincent And Grenadines"] <- "Saint Vincent"
access_data$country[access_data$country == "Antigua And Barbuda"] <- "Antigua"
access_data$country[access_data$country == "Bosnia And Herzegovina"] <- "Bosnia and Herzegovina"
access_data$country[access_data$country == "Bolivia Plurinational State"] <- "Bolivia"
access_data$country[access_data$country == "Brunei Darussalam"] <- "Brunei"
access_data$country[access_data$country == "Cabo Verde"] <- "Cape Verde"
access_data$country[access_data$country == "Chinese Taipei"] <- "Taiwan"
access_data$country[access_data$country == "Congo Dem Rep"] <- "Democratic Republic of the Congo"
access_data$country[access_data$country == "Congo Rep"] <- "Republic of Congo"
access_data$country[access_data$country == "Faeroe Islands"] <- "Faroe Islands"
access_data$country[access_data$country == "Falkland Islands Malvinas"] <- "Falkland Islands"
access_data$country[access_data$country == "Guinea Bissau"] <- "Guinea-Bissau"
access_data$country[access_data$country == "Iran Islamic Rep"] <- "Iran"
access_data$country[access_data$country == "Isle Man"] <- "Isle of Man"
access_data$country[access_data$country == "Korea Dem Peoples Republic"] <- "North Korea"
access_data$country[access_data$country == "Korea Rep"] <- "South Korea"
access_data$country[access_data$country == "Lao Pdr"] <- "Laos"
access_data$country[access_data$country == "Macedonia Former Yugoslavia Rep"] <- "Macedonia"
access_data$country[access_data$country == "Micronesia Federated States"] <- "Micronesia"
access_data$country[access_data$country == "Moldova Rep"] <- "Moldova"
access_data$country[access_data$country == "Palestine State"] <- "Palestine"
access_data$country[access_data$country == "Russian Federation"] <- "Russia"
access_data$country[access_data$country == "Saint Martin French Part"] <- "Saint Martin"
access_data$country[access_data$country == "Saint Pierre And Miquelon"] <- "Saint Pierre and Miquelon"
access_data$country[access_data$country == "Sao Tome And Principe"] <- "Sao Tome and Principe"
access_data$country[access_data$country == "Sint Maarten Dutch Part"] <- "Sint Maarten"
access_data$country[access_data$country == "Syrian Arab Republic"] <- "Syria"
access_data$country[access_data$country == "Tanzania United Rep"] <- "Tanzania"
access_data$country[access_data$country == "Timor Leste"] <- "Timor-Leste"
access_data$country[access_data$country == "Turks And Caicos Islands"] <- "Turks and Caicos Islands"
access_data$country[access_data$country == "Wallis And Futuna Islands"] <- "Wallis and Futuna"

map.world$region[map.world$region == 'Virgin Islands'][1:23] <- "British Virgin Islands"
map.world$region[map.world$region == 'Virgin Islands'] <- "United States Virgin Islands"

# compare the 2 lists
countrylist2 <- unique(access_data$country) # GTF country list
countrylist <- unique(map.world$region) # 'Maps' country list
countrylist2[!(countrylist2 %in% countrylist)]
countrylist[!(countrylist %in% countrylist2)]

# make Year column numeric
access_data$Year <- as.numeric(access_data$Year)
access_data <- as.data.frame(access_data,stringsAsFactors=FALSE)

# Make map for each year
for(h in 1990:2014){
  
  # Get access data for year h
  access_df <- access_data[access_data$Year == as.numeric(h),]
  access_df <- cbind.data.frame(access_df$country, as.numeric(access_df$Nat.Elec.Rate), stringsAsFactors=FALSE)
  names(access_df) <- c("Country","Nat.Elec.Rate")
  
  # Merge access data with map.world by country
  df <- merge(map.world, access_df, by.x = "region", by.y = "Country", sort = F, all.x=T)
  df2 <- df[order(df$order),]
  
  
  # Append column to map data and rename
  map.world2 <- cbind.data.frame(map.world, as.numeric(df2$Nat.Elec.Rate), stringsAsFactors=FALSE)
  
  # Rename column
  column <- paste0(h,"_Access")
  names(map.world2)[names(map.world2) == "as.numeric(df2$Nat.Elec.Rate)"] <- "access"
  
  # Plot
  ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
#    panel.background = element_rect(fill = "white", colour = "white"),
    axis.title = element_blank()
  )

  gg <- ggplot() + 
    geom_map(data=map.world2, map=map.world2, aes(x=long, y=lat, map_id=region, fill=access)) + 
    scale_fill_gradient(low = "orange", high = "blue", guide = "colourbar") + 
    coord_equal() +
    ditch_the_axes
  gg
  
  # Save plot
  ggsave(paste0(column,".pdf"), height = 6, width = 9, units = "in")
  
  names(map.world2)[ncol(map.world2)] <- column
}


