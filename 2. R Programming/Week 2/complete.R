setwd("C:/Users/Jae/Desktop/JHU - Data Science/2 _ R Programming")

complete <- function(directory, id = 1:332){
      
      nob <- c() # empty vector
      
      # all files in the 'specdata' folder
      all_files <- as.character( list.files(directory) )
      file_paths <- paste("./",directory,"/", all_files, sep="")
      
      for(i in id){
            current_file <- read.csv(file_paths[i], header=T, sep=",") # read files
            
            nob <- c(nob, sum(complete.cases(current_file)))
      }
      
      
      return (data.frame("id"=id, "nobs"=nob))
}
