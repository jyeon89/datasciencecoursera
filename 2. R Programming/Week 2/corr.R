setwd("C:/Users/Jae/Desktop/JHU - Data Science/2 _ R Programming")

corr <- function(directory, threshold = 0){
      
      nob <- c() # empty vector
      
      # all files in the 'specdata' folder
      all_files <- as.character( list.files(directory) )
      file_paths <- paste("./",directory,"/", all_files, sep="")
      
      # get nobs using complete.R
      comp_data <- complete("specdata", 1:332)
      nobs <- comp_data$nobs
      
      # valid ids using threshold
      ids <- comp_data$id[nobs > threshold]
      
      # set up initial corr vector
      len_ids <- length(ids)
      corr_vec <- rep(0,len_ids)
      
      all_files <- as.character( list.files(directory) )
      file_paths <- paste("./",directory,"/", all_files, sep="")
      
      j <- 1 # initial array number
      
      for(i in ids){
            current_file <- read.csv(file_paths[i], header=T, sep=",") # read files
            
            corr_vec[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
            
            j <- j + 1
      }
      
      return(corr_vec)
}
