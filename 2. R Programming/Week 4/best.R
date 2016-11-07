#####################################################################
## A function to find the best hospital in a selected state
#####################################################################

# Set working directory
setwd("C:/Users/Jae/Desktop/JHU - Data Science/2 _ R Programming/Week4/Assignment")

best <- function(state, outcome) {
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character")
      
      
      # Re-arrange the table with necessary information
      ndata <- as.data.frame(cbind( data[,2],    # Hospital Name
                                    data[,7],    # State
                                    data[, 11],  # Heart Attack
                                    data[, 17],  # Heart Failure
                                    data[, 23]), # Pneumonia
                             stringsAsFactors = FALSE)
      
      # Set the Column names
      colnames(ndata) <- c("hospital name", 
                           "state", 
                           "heart attack", 
                           "heart failure", 
                           "pneumonia")
      
      
      ## Check that state and outcome are valid
      if (!state %in% ndata[, 'state']){
            print('invalid state')
            stop()
      } else if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')){
            print('invalid outcome')
            stop()
      } else{
            # Select a state
            ss <- which(ndata[,'state']==state)
            
            # display information regarding selected state
            di <- ndata[ss,]
            
            # Numeric values of mortality rates from selected outcome
            mr <- suppressWarnings(as.numeric(di[,outcome]))
            
            # Minimum rates
            minR <- min(mr, na.rm = TRUE)
            
            result <- di[, 'hospital name'][which(mr==minR)]
            
      }
      
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      return(result)
}