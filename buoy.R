library(dplyr)
library(data.table)
buoy_data <- function(start_year = 1985, end_year = 2023) {
file_root<-"https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail<- ".txt.gz&dir=data/historical/stdmet/"
for (year in start_year:end_year) {
  path<-paste0(file_root,year,tail)
  preview_data <- fread(path, nrows = 2, header = FALSE)
  if (all(sapply(preview_data[2,], is.character))) {
    skip_value <- 2  
  } else if (all(sapply(preview_data[2,], is.numeric))) {
    skip_value <- 1  
  }
  header <- scan(path, what = 'character', nlines = 1, quiet = TRUE)
  if (year >= 1985 && year <= 1999) {
    fill_value <- 16
  } else if (year == 2000) {
    fill_value <- 17
  } else if (year >= 2001 && year <= 2023) {
    fill_value <- 17
  }
  
  buoy <- fread(path, header = FALSE, skip = skip_value, fill = fill_value)
  if (ncol(buoy) < fill_value) {
    buoy[, paste0("V", (ncol(buoy) + 1):fill_value) := NA]
  }
  #header=scan(path,what= 'character',nlines=1)
  #buoy<-fread(path,header=FALSE,skip_value)
  colnames(buoy)<-header
}
}
buoy_data(1985)
