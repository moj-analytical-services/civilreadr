#' Allows reading in of the published Civil Justice and Mortgage and Landlord Possession data CSVs for a specific date in an automated way
#' @param csv_name csv name as a string, choose from "court", "la", "map", and "monthly" for mortgage and landlord possession, and "workload", "jr", "timeliness" and "progression" for civil
#' @param data_type whether CSV comes from mortgage and landlord "mortgage" or civil statistics "civil".
#' @param year_quarter string of year and quarter of interest, in the format "YYYY-Q"
#' @export
#' @name read_previous
#' @title Read in CSV of interest as a data.table
read_previous <- function(csv_name, data_type, year_quarter){
  #Create temporary file
  temp <- tempfile()
  download.file(prior_source(origin = data_type, year_quarter = year_quarter), temp)

  zip_files <- unzip(temp,list=TRUE)[,1]
  zip_files <- zip_files[grepl(csv_name, zip_files, ignore.case = T)] #Search for file based on name

  data <- data.table::fread(unzip(temp, zip_files)) #Unzip and read file of interest
  file.remove(zip_files) #Remove file after use
  data
}

