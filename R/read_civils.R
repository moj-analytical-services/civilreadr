#' Allows reading in of the most recent published Civil Justice data CSVs in an automated way
#' @param csv_name csv name as a string, choose from "court", "la", "map", and "monthly"
#' @export
#' @name read_civils
#' @title Read in CSV of interest as a data.table
read_civils <- function(csv_name){
  #Create temporary file
  temp <- tempfile()
  download.file(web_source(), temp)

  zip_files <- unzip(temp,list=TRUE)[,1]
  zip_files <- zip_files[grepl(csv_name, zip_files, ignore.case = T)] #Search for file based on name

  data <- data.table::fread(unzip(temp, zip_files)) #Unzip and read file of interest
  file.remove(zip_files) #Remove file after use
  data
}

