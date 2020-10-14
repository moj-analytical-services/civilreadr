#' Starts from root directory to find prior civils pages
#' @name prior_source
#' @importFrom magrittr %>%
#' @param origin whether CSV comes from mortgage and landlord "mlp" or civil statistics "civil".
#' @param year_quarter string of year and quarter of interest, in the format "YYYY-Q"
#' @title Find a specified recent weblink to download zip files from
#'
prior_source <- function(origin, year_quarter){

  #Set up temporary file
  temp <- tempfile()
  #Choose starting point based on origin
  if(origin == "mortgage"){
    download.file("https://www.gov.uk/government/collections/mortgage-and-landlord-possession-statistics", destfile = temp, quiet=TRUE)
  } else if(origin =="civil"){
    download.file("https://www.gov.uk/government/collections/civil-justice-statistics-quarterly", destfile = temp, quiet=TRUE)
  }
  page <- xml2::read_html(temp)
  page <- page %>%
    rvest::html_nodes("a") %>%       # find all links
    rvest::html_attr("href")    # get the url
  page <- page[grepl(paste0("*", origin, "*"), page)]  # find the mortgage and landlord ones

  #Extract years as numerics and find the highest one
  year <- stringr::str_extract(string = year_quarter, pattern = "20[:digit:][:digit:]") %>%
    as.numeric() %>%
    na.omit() %>%
    max()

  page <- page[grepl(year, page)] #Find the ones with the highest year

  #Extract higher of the two months
  month <- stringr::str_extract(string = year_quarter, pattern = "_[[:digit:]]")
  month <-  gsub("_", "", month)
  month <- as.numeric(month)
  #Use month abbreviations to figure out which month is the highest and take that
  month_choose <- month_from_quarter(month)

  page <- unique(page[grepl(month_choose, page, ignore.case = T)]) #Find the ones with the highest year

  #Join page to root name of gov.uk and download this page to temp
  latest_page <- paste0("www.gov.uk", page)
  download.file(latest_page, destfile = temp, quiet=TRUE)

  #Read and find the zip file
  page <- xml2::read_html(temp)
  page <- page %>%
    rvest::html_nodes("a") %>%       # find all links
    rvest::html_attr("href")    # get the url
  unique(page[grepl("\\.zip", page)])  # find the zip file

}
