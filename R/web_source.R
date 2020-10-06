web_source <- function(){
#Set up temporary file
  temp <- tempfile()
  download.file("https://www.gov.uk/government/collections/mortgage-and-landlord-possession-statistics", destfile = temp, quiet=TRUE)
  page <- xml2::read_html(temp)
  page <- page %>%
    rvest::html_nodes("a") %>%       # find all links
    rvest::html_attr("href")    # get the url
  page <- page[grepl("mortgage", page)]  # find the mortgage and landlord ones

  #Extract years as numerics and find the highest one
  year <- stringr::str_extract(string = page, pattern = "20[:digit:][:digit:]") %>%
    as.numeric() %>%
    na.omit() %>%
    max()

  page <- page[grepl(year, page)] #Find the ones with the highest year

  #Extract higher of the two months
  month <- stringr::str_extract(string = page, pattern = "to-[[:lower:]][[:lower:]][[:lower:]]")
  month <-  gsub("to-", "", month)
  #Use month abbreviations to figure out which month is the highest and take that
  month_choose <- month.abb[max(grep(paste(month, collapse = "|"), tolower(month.abb)))]

  page <- page[grepl(month_choose, page, ignore.case = T)] #Find the ones with the highest year

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
