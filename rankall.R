rankall <-function(outname, num="best"){
  #first capital letter and concatenate with a dot.
  col_outname <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", perl=TRUE) %>% 
    gsub(" ", ".") %>% 
    paste("Hospital.30.Day.Death..Mortality..Rates.from", sep=".")
  
  #check validation
  X_full <-read.csv("outcome-of-care-measures.csv")
  if ((state %in% X_full[[7]])==FALSE)
    stop("invalid state")
  else if((col_outname %in% colnames(X_full)) ==F)
    stop("invalid outcome")
  
  states <- unique(X_full[[State]])
  Ret <- data.frame()
   for (state in states){
    X <- X_full[X_full$State == State, ]
    X <- X[with(X, order(X[[col_outname]]))]
    #take cases and calculate the ratio...
    if (num=="best"){
      num <- 1                  #take the first element.
    }
    else if (num="worst"){     #Repeat for the "num" rank
      num <- length(X[[col_outname]])
    }
    else (is.numeric(num)){
      if(length(X$Hospital)<num)
        stop(NA)}
    ratio <-X[num, col_outname]  #like random element of "num" rank position 
    X <- X[which(X[[col_outname]] == ratio), ]  
    X <- sort(X$Hospital.Name) #then sort (alpha) and ...
    Ret <- rbind(Ret, data.frame(hospital = X[1],state = state))
   }
  Ret
}