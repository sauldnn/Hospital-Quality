rankhospital <-function(state, outname, num="best"){
  #first capital letter and concatenate with a dot.
  col_outname <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", perl=TRUE) %>% 
    gsub(" ", ".") %>% 
    paste("Hospital.30.Day.Death..Mortality..Rates.from", sep=".")
  
  #check validation
  X_full <-read.csv("outcome-of-care-measures.csv")
  
  if ((state %in% X_full[[7]])==FALSE){
    stop("invalid state")
  }
  else if((col_outname %in% colnames(X_full)) ==F)
    stop("invalid outcome")
  
  #building
  X <- X_full[X_full$State == State, ]
  X <- X[with(X, order(X[[col_outname]]))]
  
  #take cases and calculate the ratio...
  if (num=="best"){
    ratio <-X[col_outname, 1]  #like random element of "num" rank position 
    x <- sort(X[which(X$Hospital.Name == ratio), ])  #then sort (alpha) and ...
    x[1]                       #take the first element.
  }
  else if (num="worst"){     #Repeat for the "num" rank
    ratio <-X[col_outname, -1]
    x <- sort(X[which(X$Hospital.Name == ratio), ])
    x[-1]
  }
  else if (is.numeric(num)){
    if(length(X$Hospital)<num){
      stop(NA)
    }
    else{
      ratio <-X[col_outname, num]
      x <- sort(X[which(X$Hospital.Name == ratio), ])
      x[num]
    }
  }
  else{
    stop("Error")
  }
  
}