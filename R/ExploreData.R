ExploreData <- function(Dataset, Model, CI=.99, Digits=6){

  old.options <- options()
  on.exit(options(old.options))
  options(digits = Digits, scipen=999)  
  
# rename input
Data <- Dataset
Formula <- Model
Data <- Data[, (names(Data) %in% all.vars(Formula))]  

# only do this check if NOT formula with dot
if (all.vars(Formula)[2]!="."){
N1 <- dim(Data)[1]
Data <- na.exclude(Data)
N2 <- dim(Data)[1]
Miss <- N1 - N2

# Stop analysis if one of the IV is numeric
Variable.Classes <- NULL
Fit <- lm(Model, data = Dataset)
for (i in 2: dim(Fit$model)[2]){
  Variable.Classes <- c(Variable.Classes, class(Fit$model[,i]))
}
Contains.Numeric.Pred <- ((sum(grepl(pattern = "numeric", x = Variable.Classes))>0)) 
if (Contains.Numeric.Pred==TRUE){
  stop("\nSome of the independent variables are numeric variables. \nAll independent variables should be coded as factors.")
}

}

# only do this check if formula with dot
if (all.vars(Formula)[2]=="."){
  N1 <- length(Data)
  Data <- na.exclude(Data)
  N2 <- length(Data)
  Miss <- N1 - N2  
}

Std.Error.Mean <- function(x){sd(x)/sqrt(length(x))}  
Lower.CI.Mean <- function(x){mean(x) - (qt(c((1-CI)/2), df=length(x)-1, lower.tail=FALSE) * (sd(x)/sqrt(length(x))))}  
Upper.CI.Mean <- function(x){mean(x) + (qt(c((1-CI)/2), df=length(x)-1, lower.tail=FALSE) * (sd(x)/sqrt(length(x))))}    

old.options <- options()
on.exit(options(old.options))
options(digits=Digits)  

# only compute if NOT formula with dot
if (all.vars(Formula)[2]!="."){
Res <- doBy::summaryBy(formula = Formula, data = Data, FUN = c(mean, sd, length, Std.Error.Mean, 
                                                               Lower.CI.Mean, Upper.CI.Mean))  
Dims <- dim(Res)
Res[,(Dims[2]-5):(Dims[2]-4)] <- round(Res[,(Dims[2]-5):(Dims[2]-4)], digits = Digits) 
Res[,(Dims[2]-2):(Dims[2])] <- round(Res[,(Dims[2]-2):(Dims[2])], digits = Digits) 
Res <- data.frame(Res)
names(Res)[(Dims[2]-5):(Dims[2])] <- c("Mean", "SD", "N", "Std.Err", "CI.Low", "CI.High")
}

# only compute if formula with dot
if (all.vars(Formula)[2]=="."){
  Data <- cbind(Data, 1)
  Res <- NULL
  Res[1] <- tapply(Data[,1], INDEX = Data[,2], FUN = c("mean"))   
  Res[2] <- tapply(Data[,1], INDEX = Data[,2], FUN = c("sd"))   
  Res[3] <- tapply(Data[,1], INDEX = Data[,2], FUN = c("length"))   
  Res[4] <- tapply(Data[,1], INDEX = Data[,2], FUN = c("Std.Error.Mean"))   
  Res[5] <- tapply(Data[,1], INDEX = Data[,2], FUN = c("Lower.CI.Mean"))   
  Res[6] <- tapply(Data[,1], INDEX = Data[,2], FUN = c("Upper.CI.Mean"))   
  
  Dims <- length(Res)
  Res[(Dims-5):(Dims-4)] <- round(Res[(Dims-5):(Dims-4)], digits = Digits) 
  Res[(Dims-2):(Dims)] <- round(Res[(Dims-2):(Dims)], digits = Digits) 
  names(Res)[(Dims-5):(Dims)] <- c("Mean", "SD", "N", "Std.Err", "CI.Low", "CI.High")
  Res <- data.frame(t((Res)))
    }

try(if (min(Res$N)<10){
warning("For (some of) the different combinations of the levels \nof the independent variables, there are less than 10 observations. \nMake sure that all independent variables are factors!\n")
}, silent = TRUE)

fit <- list(Results=Res, Miss=Miss, Dataset=Dataset, Model=Model, CI=CI, N=N1, Call=match.call())
class(fit) <- "ExploreData"
fit
}

# Summary function
summary.ExploreData <- function(object, ..., Object){
 
  old.options <- options()
  on.exit(options(old.options))
  options(digits = 8, scipen=999)
  
  if (missing(Object)){Object <- object}
  print(Object$Results, row.names=FALSE)
  
  if (Object$Miss > 0){
    cat("\nWarning: a total of ", Object$Miss, " observations were discarded \nfrom the analysis due to missing values. The analysis \nwas conducted using the data of ", (Object$N - Object$Miss), " observations.", sep="")
      }
  }