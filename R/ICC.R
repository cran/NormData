# The function ICC is a wrapper for the function ICCest from the ICC package (v2.3.0) with some small changes 
# for details of the original function, see https://cran.r-project.org/web/packages/ICC/ICC.pdf
# The author of this function is Matthew Wolak

ICC <- function(Cluster, Test.Score, Dataset, CI=0.95){
 
  old.options <- options()
  on.exit(options(old.options))
  options(digits = 8, scipen=999)
  Data <- Dataset
  
  if (inherits(Data, what = "data.frame")==FALSE){
    stop("\nThe specified dataset should be a data.frame.\n")}
  
  
  Outcome <- Data[,paste(substitute(Test.Score))]
  Cluster <- Data[,paste(substitute(Cluster))]
  
  
  data <- data.frame(Outcome, Cluster)
  type = "THD" # see manual ICC package for details
  Total_N <- dim(data)[1]
  alpha <- 1 - CI
  
  if (alpha<0){stop("The CI should be within the 0-1 interval.")}
  if (alpha>1){stop("The CI should be within the 0-1 interval.")}
  
    # remove missing
  data <- na.exclude(data)
  y <- data[,1]
  x <- data[,2]

  if (inherits(x, what = "factor")==FALSE){
    if (inherits(x, what = "numeric")==FALSE){
      stop("The Cluster variable should be coded as factor or as numeric.\n")
    }
  }
  
  Removed_N <- Total_N - length(x)
  
  Labels.Cluster <- x
  
  # Start computation ICC
  square <- function(z) {
    z^2
  }
  icall <- list(y = substitute(y), x = substitute(x))
  if (is.character(icall$y)) {
    warning("passing a character string to 'y' is deprecated since ICC version 2.3.0 and will not be supported in future versions. The argument to 'y' should either be an unquoted column name of 'data' or an object")
    if (missing(data)) 
      stop("Supply either the unquoted name of the object containing 'y' or supply both 'data' and then 'y' as an unquoted column name to 'data'")
    icall$y <- eval(as.name(y), data, parent.frame())
  }
  if (is.name(icall$y)) 
    icall$y <- eval(icall$y, data, parent.frame())
  if (is.call(icall$y)) 
    icall$y <- eval(icall$y, data, parent.frame())
  if (is.character(icall$y)) 
    icall$y <- eval(as.name(icall$y), data, parent.frame())
  if (is.character(icall$x)) {
    warning("passing a character string to 'x' is deprecated since ICC vesion 2.3.0 and will not be supported in future versions. The argument to 'x' should either be an unquoted column name of 'data' or an object")
    if (missing(data)) 
      stop("Supply either the unquoted name of the object containing 'x' or supply both 'data' and then 'x' as an unquoted column name to 'data'")
    icall$x <- eval(as.name(x), data, parent.frame())
  }
  if (is.name(icall$x)) 
    icall$x <- eval(icall$x, data, parent.frame())
  if (is.call(icall$x)) 
    icall$x <- eval(icall$x, data, parent.frame())
  if (is.character(icall$x) && length(icall$x) == 1) 
    icall$x <- eval(as.name(icall$x), data, parent.frame())
  tdata <- data.frame(icall)
  tdata <- na.omit(tdata)
  a <- length(unique(tdata$x))
  if (!is.null(attributes(tdata)$na.action)) {
    warning("NAs removed from rows:\n", unclass(attributes(tdata)$na.action), 
                "\n")
  }
  if (!is.factor(tdata$x)) {
  #  warning("'x' has been coerced to a factor")
    tdata$x <- as.factor(tdata$x)
  }
  else {
    if (length(levels(tdata$x)) > a) {
      tdata$x <- factor(as.character(tdata$x), levels = unique(tdata$x))
      warning("Missing levels of 'x' have been removed")
    }
  }
  tmpbb <- anova(aov(y ~ x, data = tdata))
  num.df <- tmpbb$Df[1]
  denom.df <- tmpbb$Df[2]
  MSa <- tmpbb$"Mean Sq"[1]
  MSw <- var.w <- tmpbb$"Mean Sq"[2]
  tmp.outj <- aggregate(y ~ x, data = tdata, FUN = length)$y
  k <- (1/(a - 1)) * (sum(tmp.outj) - (sum(square(tmp.outj))/sum(tmp.outj)))
  var.a <- (MSa - MSw)/k
  r <- var.a/(var.w + var.a)
  low.F <- qf(alpha/2, num.df, denom.df, lower.tail = FALSE)
  N <- nrow(tdata)
  n.bar <- N/a
  n.not <- n.bar - sum(square(tmp.outj - n.bar)/((a - 1) * 
                                                   N))
  #type <- match.arg(CI.type)
  if (type == "THD") {
    up.F <- qf(alpha/2, denom.df, num.df, lower.tail = FALSE)
    FL <- (MSa/MSw)/low.F
    FU <- (MSa/MSw) * up.F
    low.CI <- (FL - 1)/(FL + n.not - 1)
    up.CI <- (FU - 1)/(FU + n.not - 1)
  }
  if (type == "Smith") {
    z.not <- qnorm(alpha/2)
    Vr <- (2 * square(1 - r)/square(n.not)) * ((square((1 + 
     r * (n.not - 1)))/(N - a)) + ((a - 1) * (1 - r) * 
     (1 + r * (2 * n.not - 1)) + square(r) * (sum(square(tmp.outj)) - 
     2 * (1/N) * sum((tmp.outj^3)) + (1/square(N)) * square(sum(square(tmp.outj)))))/square(a - 
     1))
    low.CI <- r + z.not * sqrt(Vr)
    up.CI <- r - z.not * sqrt(Vr)
  }
  
  r[r<0] <- 0
  low.CI[low.CI<0] <- 0
  up.CI[up.CI<0] <- 0
  
  fit <- list(ICC = r, LowerCI = low.CI, UpperCI = up.CI, Num.Clusters = a, Mean.Cluster.Size = k, 
       #varw = var.w, vara = var.a
       Data=data, N.Dataset=Total_N, N.Removed=Removed_N, alpha=alpha, Labels.Cluster=Labels.Cluster)

  class(fit) <- "ICC"
  fit
  
  }





# Plot function
plot.ICC <- function(x, X.Lab="Cluster", Y.Lab="Test score", Main="", Add.Jitter=0.2, 
                     Size.Points=1, Size.Labels=1, Add.Mean.Per.Cluster=TRUE, Col.Mean.Symbol="red", Seed=123, ...){

old.options <- options()
on.exit(options(old.options))
options(digits = 8, scipen=999)  
Add.Jitter <- c(0 - Add.Jitter, 0 + Add.Jitter)
Object <- x 
Data_Plot <- Object$Data[order(Object$Data[,2]),]
Cluster <- Data_Plot[,2]
Outcome <- Data_Plot[,1]
X_length <- length(unique(Cluster))
plot(y=Outcome, x=as.numeric(Cluster), col=0, xaxt="no", xlab=X.Lab, ylab=Y.Lab, main=Main, 
     xlim=c(0.5, X_length+0.5), ...)
abline(v=1:(length(unique(Cluster))), col="grey")   
set.seed(Seed); Add_jit_vals <- runif(min = Add.Jitter[1], max=Add.Jitter[2], n = length(Outcome))

# LS!
Cluster_temp <- NA
for (i in 1: length(unique(Cluster))){
  Cluster_temp[Cluster==unique(Cluster)[i]] <- i
}
Cluster <- Cluster_temp

points(y=Outcome, x=(Add_jit_vals + as.numeric((Cluster))), cex=Size.Points)
axis(1, at = 1:length(unique(as.numeric(Cluster))), 
     labels = as.character(unique(Object$Labels.Cluster)), cex.axis=Size.Labels)

if (Add.Mean.Per.Cluster==TRUE){
 Means <- tapply(X = Data_Plot[,1], INDEX = Data_Plot[,2], FUN = mean, na.rm=TRUE)
 points(y=Means, x=unique(as.numeric(Cluster)), cex=2, pch="-", col=Col.Mean.Symbol)
}
}


# Summary function
summary.ICC <- function(object, ..., Object){
  old.options <- options()
  on.exit(options(old.options))
  options(digits = 8, scipen=999)
  if (missing(Object)){Object <- object} 
  cat("\nNote: In the specified dataset, N = ", Object$N.Dataset, sep="")

  if (Object$N.Removed==0){
  cat("\nAll observations were used in the analysis (no missing data).")
}

  if (Object$N.Removed>0){
  cat("\nWarning: a total of ", Object$N.Removed, 
      " observations were discarded \nfrom the analysis due to missing values.", sep="")}
cat("\n\nThere were ", Object$Num.Clusters, " clusters in the dataset (mean cluster size = ", Object$Mean.Cluster.Size, ")", sep="")

cat("\n\nResults ICC analyses")
cat("\n~~~~~~~~~~~~~~~~~~~~")  
cat("\nICC = ", round(Object$ICC, digits = 6), sep="")
cat("\n", (1-Object$alpha)*100, "% CI = [", round(Object$LowerCI, digits = 6), ", ", 
    round(Object$UpperCI, digits = 6), "]", sep="")
}
