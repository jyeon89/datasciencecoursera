setwd("C:/Users/Jae/Desktop/JHU - Data Science/2 _ R Programming")

pollutantmean <- function(directory, pollutant, id = 1:332){
      
      mean_vector <- c()      # initial empty vector
      
      # all files in the 'specdata' folder
      all_files <- as.character( list.files(directory) )
      file_paths <- paste("./",directory,"/", all_files, sep="")
      
      for(i in id) {
            current_file <- read.csv(file_paths[i], header=T, sep=",") # read files
            
            # remove NA
            na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
            
            mean_vector <- c(mean_vector, na_removed)
      }
      result <- mean(mean_vector)
      
      return(result) 
      
}


pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)