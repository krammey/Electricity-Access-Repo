
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
    } # end i
} # end k


