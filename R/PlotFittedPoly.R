PlotFittedPoly <- function(Dataset, Test.Score, IV, Center.Value.IV=0,
               Order.Polynomial=3, Confidence.Band.Poly=FALSE, Alpha=.01, 
               EMF = TRUE, Confidence.Band.EMF=TRUE, xlab, ylab,
               Color = "red", Black.white=FALSE,
               Legend.Location="topright", Legend.text.size=1, Add.Jitter=0, 
               Seed=123, cex.axis=1, cex.main=1, cex.lab=1, 
               Loess.Span=0.75, ...){

if (length(Order.Polynomial)>1){stop("Only one value should be specified for Order.Polynomial=\n")}

if (missing(xlab)){xlab <- "IV"}
if (missing(ylab)){ylab = "Test score"}
Test.Score <- Dataset[,paste(substitute(Test.Score))] 
IV <- Dataset[,paste(substitute(IV))]

if ((inherits(IV, what = "factor")==TRUE) | (inherits(IV, what = "character")==TRUE)){stop("\nThe specified IV must be a numeric variable.\n")}
if ((inherits(Test.Score, what = "factor")==TRUE)  | inherits(Test.Score, what = "character")==TRUE){stop("\nThe specified Test.Score must be a numeric variable.\n")}
if (Order.Polynomial[1] != "None"){if (min(Order.Polynomial) < 1){stop("\nThe specified Lowest.Order should be at least 1.\n")}}
if(Black.white==TRUE){Color <- rep("black", times=100)}

# Add jitter if specified
set.seed(Seed); Test.Score <- 
  Test.Score + runif(min = 0-Add.Jitter, max=Add.Jitter, n = length(Test.Score))

IV_orig <- IV
IV <- IV - Center.Value.IV # Center the IV if needed

Data <- data.frame(na.exclude(cbind(Test.Score, IV, IV_orig)))

# oldpar <- par(no.readonly=TRUE) 
# on.exit(par(oldpar)) 
# par(mar=Mar)
plot(x = Data$IV, y=Data$Test.Score, col="grey", xlab=xlab, ylab=ylab, 
     cex.axis=cex.axis, cex.main=cex.main, cex.lab=cex.lab, ...)
if (Center.Value.IV==0){
newdata <- data.frame(seq(from=min(Data$IV), to=max(Data$IV), length.out = 1000)) 
xvals <- (seq(from=min(Data$IV), to=max(Data$IV), length.out = 1000))
}
if (Center.Value.IV!=0){
  newdata <- data.frame(seq(from=min(Data$IV_orig), to=max(Data$IV_orig), length.out = 1000)) - Center.Value.IV 
  xvals <- (seq(from=min(Data$IV_orig), to=max(Data$IV_orig), length.out = 1000))-Center.Value.IV
}

names(newdata) <- "IV"
Legend_text <- NULL
Num_order <- length(Order.Polynomial)
if (Order.Polynomial[1] != "None"){
Num_order <- length(Order.Polynomial)
for (i in 1: Num_order){
name_here <- paste("Order", Order.Polynomial[i], sep=" ")
Fit <- lm(data = Data, Test.Score ~ poly(IV, Order.Polynomial[i], raw=TRUE))
# Mean prediction function for linear/quadratic/cubic/... model
lines(x = xvals, y=predict(Fit, newdata), col="black", lty=i, lwd=2)
Legend_text <- c(Legend_text, name_here)

if (Confidence.Band.Poly==TRUE){
  Conf.Band <-predict(Fit, newdata, interval = "confidence", level = 1-Alpha)
  lines(x = xvals, y=Conf.Band[,2], col=Color[i], lty=i, lwd=2)
  lines(x = xvals, y=Conf.Band[,3], col=Color[i], lty=i, lwd=2)
  }

}
}

if (EMF==TRUE){
  if (Order.Polynomial[1] != "None"){
  EMF_fit <- loess(data = Data, formula = Test.Score ~ IV, span = Loess.Span)
  Pred_EMF <- predict(EMF_fit, newdata, se = TRUE)
  lines(x = xvals, y = Pred_EMF$fit, col=Color, lty=2, lwd=3)
  if (Confidence.Band.EMF==TRUE){
  lines(x = xvals, y = Pred_EMF$fit-qt(1-Alpha/2, Pred_EMF$df)*Pred_EMF$se, col=Color, lty=3, lwd=2)
  lines(x = xvals, y = Pred_EMF$fit+qt(1-Alpha/2, Pred_EMF$df)*Pred_EMF$se, col=Color, lty=3, lwd=2)  
  }
  Legend_text <- c(Legend_text, "Empirical Mean Function (EMF)")
  Num_order <- Num_order + 1 #LS voor legend}
  }
  
  if (Order.Polynomial[1] == "None"){
    EMF_fit <- loess(data = Data, formula = Test.Score ~ IV, span = Loess.Span)
    Pred_EMF <- predict(EMF_fit, newdata, se = TRUE)
    lines(x = xvals, y = Pred_EMF$fit, col=Color, lty=2, lwd=3)
    if (Confidence.Band.EMF==TRUE){
    lines(x = xvals, y = Pred_EMF$fit-qt(1-Alpha/2, Pred_EMF$df)*Pred_EMF$se, col=Color, lty=3, lwd=2)
    lines(x = xvals, y = Pred_EMF$fit+qt(1-Alpha/2, Pred_EMF$df)*Pred_EMF$se, col=Color, lty=3, lwd=2)
    }
    Legend_text <- c(Legend_text, "Empirical mean function (EMF)")
    Num_order <- Num_order 
  }
}

if (Legend.Location != "None"){
  
  if (length(Legend_text)>=1){
  Color.Legend <- NULL
  Lty.Legend <- NULL
  if (grepl(Legend_text[1], pattern = "Order")){Color.Legend[1]<-"Black"; Lty.Legend[1]<-1}
  if (grepl(Legend_text[1], pattern = "EMF")){Color.Legend[1]<-"Red"; Lty.Legend[1]<-2}
  if (grepl(Legend_text[2], pattern = "Order")){Color.Legend[2]<-"Black"; Lty.Legend[2]<-2}
  if (grepl(Legend_text[2], pattern = "EMF")){Color.Legend[2]<-"Red"; Lty.Legend[2]<-2}
  
legend(Legend.Location, legend = Legend_text, col = Color.Legend, 
       lty=Lty.Legend, cex=Legend.text.size, lwd=2)
  }
}

}
