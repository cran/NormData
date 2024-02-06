CheckFit <- function(Stage.1.Model, Means, CI=.99, Digits=6){

Model <- Means
old.options <- options()
on.exit(options(old.options))
options(digits = Digits, scipen=999)  
  
CI.High <- CI.Low <- N <- SD <- Std.Err <- NULL # LS for R CDM check  
# rename input
Data <- Stage.1.Model$HomoNorm$Table.Obs.Pred.Res.Delta
Formula <- formula(Model)
Formula.Stage.1 <- formula(Stage.1.Model$HomoNorm$Reg.Model)

Data <- data.frame(Data$Predicted, Data[, names(Data) %in% all.vars(Formula)])

# only do this check if NOT formula with dot
if (all.vars(Formula)[2]!="."){
N1 <- dim(Data)[1]
Data <- na.exclude(Data)
N2 <- dim(Data)[1]
Miss <- N1 - N2
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

# old.options <- options()
# on.exit(options(old.options))
# options(digits=Digits)  

# Computations for raw test score

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
Res.Observed <- Res

# Computations for predicted test scores
Formula.Stage.1 <- update(Formula, Data.Predicted ~ .)

# only compute if NOT formula with dot
if (all.vars(Formula.Stage.1)[2]!="."){
  Res <- doBy::summaryBy(formula = Formula.Stage.1, data = Data, FUN = c(mean, sd, length, Std.Error.Mean, 
                                                                 Lower.CI.Mean, Upper.CI.Mean))  
  Dims <- dim(Res)
  Res[,(Dims[2]-5):(Dims[2]-4)] <- round(Res[,(Dims[2]-5):(Dims[2]-4)], digits = Digits) 
  Res[,(Dims[2]-2):(Dims[2])] <- round(Res[,(Dims[2]-2):(Dims[2])], digits = Digits) 
  Res <- data.frame(Res)
  names(Res)[(Dims[2]-5):(Dims[2])] <- c("Mean", "SD", "N", "Std.Err", "CI.Low", "CI.High")
}

# only compute if formula with dot
if (all.vars(Formula.Stage.1)[2]=="."){
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

Res.Predicted <- Res

if (min(Res$N)<10){
message("Warning: For (some of) the different combinations of the levels \nof the independent variables, there are less than 10 observations. \nMake sure that all independent variables are factors!\n")
}

# Check for saturated model 
Saturated <- FALSE
if(max(Res.Observed$Mean - Res.Predicted$Mean) < 0.00001){
  Saturated <- TRUE
  }


fit <- list(Results.Observed=Res.Observed, 
            Results.Predicted=subset(Res.Predicted, select=-c(SD, N, Std.Err, CI.Low, CI.High)), 
            Miss=Miss, Dataset=Stage.1.Model$HomoNorm$Table.Obs.Pred.Res.Delta, 
            Model=Model, 
            CI=CI, N=N1, Stage.1.Model=Stage.1.Model, Saturated=Saturated, Call=match.call())

class(fit) <- "CheckFit"
fit
}

# Summary function
summary.CheckFit <- function(object, ..., Object){

  old.options <- options()
  on.exit(options(old.options)) 
  options(digits = 8, scipen=999)
  
  if (missing(Object)){Object <- object}
  cat("Sample means of the test score:\n")
  cat("-------------------------------\n")
  print(Object$Results.Observed, row.names=FALSE)

  cat("\nMean model-predicted test scores:\n")
  cat("---------------------------------\n")
  print(Object$Results.Predicted, row.names=FALSE)
  
    
  if (Object$Miss > 0){
    cat("\nWarning: a total of ", Object$Miss, 
        " observations were discarded \nfrom the analysis due to missing values. The analysis \nwas conducted using the data of ", (Object$N - Object$Miss), " observations.", sep="")
      }

  # Give note if saturated model is fitted
  if (Object$Saturated==TRUE){
  cat("\n\nNote. The predicted and observed means are identical \nbecause a saturated model is used.")
  }
  
  }
