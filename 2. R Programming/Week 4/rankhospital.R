#####################################################################
## A function to rank the hospitals in a selected state
#####################################################################

# Set working directory
setwd("C:/Users/Jae/Desktop/JHU - Data Science/2 _ R Programming/Week4/Assignment")

rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character")
      
      
      ndata <- as.data.frame(cbind( data[,2],    # Hospital Name
                                    data[,7],    # State
                                    data[, 11],  # Heart Attack
                                    data[, 17],  # Heart Failure
                                    data[, 23]), # Pneumonia
                             stringsAsFactors = FALSE)
      
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
      } else if (is.numeric(num) || !is.numeric(num)){
      
            # Select a state
            ss <- which(ndata[,'state']==state)
            
            # Display data with the selected state
            dd <- ndata[ss,]
            
            # Replace selected outcome to numeric values
            dd[,outcome] <- suppressWarnings(as.numeric(dd[,outcome]))
            
            # Set an order from lowest outcome rate to higest
            ord <- dd[order(dd[,outcome], dd[,'hospital name']),]
            
            if (is.numeric(num) > 0){
                  result <- ord[,'hospital name'][num]
            } else if (num == 'best'){
                  source('C:/Users/Jae/Desktop/datasciencecoursera/2. R Programming/Week 4/best.R')
                  result <- best(state, outcome)
            
            } else if (num == 'worst'){
                  # Set an order from highest outcome rate to lowest
                  ord <- dd[order(dd[,outcome], dd[,'hospital name'], decreasing = TRUE),]
                  
                  result <- ord[,'hospital name'][1]
            } else{
                  print('invalid rank')
                  stop()
            }
      } 
      
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      
      return(result)
}