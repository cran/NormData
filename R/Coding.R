Coding <- function(x, verbose=TRUE){
if (inherits(x, what = "numeric")==TRUE){
  if (verbose==TRUE) cat("The specified variable is a numeric variable that will not be dummy-coded.\n")
  }

if (inherits(x, what = "factor")==TRUE & length(unique(na.exclude(x)))==2){
  if (verbose==TRUE) cat("The specified variable is a binary variable (R class: factor) \nthat will be dummy-coded as:\n\n")
  nam <- (deparse(substitute(x)))
  nam <- sub('.*\\$(.*)','\\1', nam)
  if (nam=="x"){nam<-""}
  temp <- contr.treatment(Levels(x))
  colnames(temp) <- paste(nam, colnames(temp), sep="")
  if (verbose==TRUE) print(temp)
}
 
  if (inherits(x, what = "factor")==TRUE & length(unique(na.exclude(x)))>2){
    if (verbose==TRUE) cat("The specified variable is a non-binary qualitative variable \n(R class: factor) that will be dummy-coded as:\n\n")
    nam <- (deparse(substitute(x)))
    nam <- sub('.*\\$(.*)','\\1', nam)
    if (nam=="x"){nam<-""}
    temp <- contr.treatment(Levels(x))
    colnames(temp) <- paste(nam, colnames(temp), sep="")
    if (verbose==TRUE) print(temp)
  }   
  if (inherits(x, what = "character")==TRUE){
    if (verbose==TRUE) cat("The specified variable is of class character. Such variables can \ngive issues when fitting a regression model, consider re-coding \nthe variable as a factor.")
  }
}

Levels <- function(x){
  levels(x)
}