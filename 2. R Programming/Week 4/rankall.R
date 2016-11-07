#####################################################################
## A function to rank all the hospitals in every state
#####################################################################

# Set working directory
setwd("C:/Users/Jae/Desktop/JHU - Data Science/2 _ R Programming/Week4/Assignment")

rankall <- function(outcome, num = "best") {
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
      ## For each state, find the hospital of the given rank
      if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')){
            stop('invalid outcome')
            
      } else if (is.numeric(num) || !is.numeric(num)){
            
            # Select an outcome and convert to numeric values
            ndata[,outcome] <- suppressWarnings(as.numeric(ndata[,outcome]))
            
            # split the data by states
            sd <- split(ndata, ndata$state)
            sd1 <- sd
            rank <- list()
            best <- list()
            worst <- list()
            
            for(i in seq_along(sd)){
                  # Rank from the lowest rate to the highest rate
                  sd[[i]] <- sd[[i]][order(sd[[i]][, outcome], 
                                           sd[[i]][,'hospital name']), ]
                  
                  # Rank from the highest rate to the lowest rate
                  sd1[[i]] <- sd[[i]][order(sd[[i]][, outcome],
                                            sd[[i]][,'hospital name'],
                                            decreasing = TRUE), ]
                  
                  # Name of hospitals and states at a desired rank
                  rank[[i]] <- c(sd[[i]][num, 'hospital name'],
                                 sd[[i]][,'state'][1])
                  
                  # Best rate
                  best[[i]] <- c(sd[[i]][1, c('hospital name', 
                                              'state')])
                  
                  # Worst rate
                  worst[[i]] <- c(sd1[[i]][1, c('hospital name',
                                                'state')])
                  
            }

            if (is.numeric(num)>0){
                  f_list <- do.call(rbind, rank)
                  output <- as.data.frame(f_list, 
                                          row.names = f_list[, 2],
                                          stringsAsFactors = FALSE)
                  
                  colnames(output) <- c('hospital name', 'state')  
            } else if (!is.numeric(num)){
                  if (num == 'best'){
                        f_list <- do.call(rbind, best)
                        output <- as.data.frame(f_list, 
                                                stringsAsFactors = FALSE)
                        rownames(output) = f_list[, 2]
                        colnames(output) <- c('hospital name', 'state')
                  } else if (num == 'worst'){
                        f_list <- do.call(rbind, worst)
                        output <- as.data.frame(f_list, 
                                                stringsAsFactors = FALSE)
                        
                        rownames(output) = f_list[, 2]
                        colnames(output) <- c('hospital name', 'state')
                  }
            }
      }
      
   
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
      return(output)
}
