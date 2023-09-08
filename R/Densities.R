Densities <- function(Dataset, Test.Score, IV, Color=TRUE, Size.Legend=1, 
                      xlab="Test score", main, ...){
  
  if (missing(main)){main <- ""}
  Outcome <- Dataset[,paste(substitute(Test.Score))]
  if (missing(IV)){
  plot(density(na.exclude(Outcome), from=min(na.exclude(Outcome)), to=max(na.exclude(Outcome))), 
       main = main, xlab = xlab, ...)  
  }
  
  if (missing(IV)==FALSE){
  IV <- Dataset[,paste(substitute(IV))]
  
  nums <- 1:length(Outcome)
  Data <- data.frame(na.exclude(cbind(nums, Outcome, IV)))
  Data$Outcome <- (Outcome[as.numeric(as.character(Data$nums))])
  
  Num <- length(unique(Data$IV))
  Vals_IV <- unique(Data$IV)
  
  # determine max value for density
  Max_all <- NULL
  for (i in 1: Num){
    Data_hier <- subset(Data, IV==Vals_IV[i])
    Max_here <- max(density(Data_hier$Outcome)$y)
    Max_all <- c(Max_all, Max_here)
  }
  Max_val <- max(Max_all)*1.05
  
  # density plots
  plot(density(Data$Outcome), col=0, ylim=c(0, Max_val), main=main, xlab = xlab, ...)
  for (i in 1: Num){
    Data_hier <- subset(Data, IV==Vals_IV[i])
    
    if (Color==TRUE){lines(density(Data_hier$Outcome, from=min(na.exclude(Outcome)), to=max(na.exclude(Outcome))), lty=i, col=i)}
    if (Color!=TRUE){lines(density(Data_hier$Outcome, from=min(na.exclude(Outcome)), to=max(na.exclude(Outcome))), lty=i)}
  }
  
  # add legend
  if (Color==TRUE){
  legend("topright", legend = Vals_IV, lty = 1:Num, col=1:Num, cex=Size.Legend)}  
  
  if (Color!=TRUE){
    legend("topright", legend = Vals_IV, lty = 1:Num, cex=Size.Legend)}

   } 
  }
