library(dplyr)
library(data.table)
buoy_data <- function(start_year = 1985, end_year = 2023) {
  file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
  tail <- ".txt.gz&dir=data/historical/stdmet/"
  
  all_data <- list()  
  
  for (year in start_year:end_year) {
    path <- paste0(file_root, year, tail)
    preview_data <- tryCatch({
      fread(path, nrows = 2, header = FALSE)
    })

    if (all(sapply(preview_data[2,], is.character))) {
      skip_value <- 2  
    } else if (all(sapply(preview_data[2,], is.numeric))) {
      skip_value <- 1 
    }
    
    header <- tryCatch({
      scan(path, what = 'character', nlines = 1, quiet = TRUE)
    }, error = function(e) {
      message(paste("Error reading header for year", year, ": ", e))
      return(NULL)
    })
    
    if (year >= 1985 && year <= 1999) {
      fill_value <- 16
    } else if (year >= 2000) {
      fill_value <- 16
    }
      else if (year >= 2001 && year <= 2023) {
      fill_value <- 17
      }
    
    buoy <- tryCatch({
      fread(path, header = FALSE, skip = skip_value, fill = Inf)
    })
    
    if (ncol(buoy) < fill_value) {
      buoy[, paste0("V", (ncol(buoy) + 1):fill_value) := NA]
    }
    colnames(buoy) <- header
    buoy$Year <- year
    all_data[[length(all_data) + 1]] <- buoy
  }
  combined_data <- rbindlist(all_data, fill = TRUE)
  combined_data <- combined_data %>%
    select(Year, everything()) %>%  
    select(-one_of(c("YY", "YYYY", "#YY"))) 
  return(combined_data)
}
buoy_data_1985_2023 <- buoy_data(1985)
if (!is.null(buoy_data_1985_2023)) {
  print(head(buoy_data_1985_2023))
}