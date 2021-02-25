best <- function(state, outname){
  
  #first capital letter and concatenate with a dot.
  outname <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", outname, perl=TRUE) 
  outname <- gsub(" ", ".", outname)
  col_outname <- paste("Hospital.30.Day.Death..Mortality..Rates.from", outname, sep=".")
  
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
  
  ratio <-X[col_outname, 1]
  x <- sort(X[which(X$Hospital.Name == ratio), ])
  x[1]
}