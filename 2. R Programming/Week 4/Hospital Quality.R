### Data Science - R Programming Assignment
### Hospital Quality

# Set working directory
setwd("C:/Users/Jae/Desktop/JHU - Data Science/2 _ R Programming/Week4/Assignment")

# read the file
outcome <- read.csv("outcome-of-care-measures.csv", 
                    colClasses = "character")

#####################################################################
## Plot the 30-day mortality rates for heart attack

# Get the righ column (30-day mortality rates for heart attack)
outcome[, 11] <- as.numeric(outcome[, 11])

# There might be some missing information that will cause an error
# but you can disregard it

hist(outcome[, 11])     # plot histogram


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
