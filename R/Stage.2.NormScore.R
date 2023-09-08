Stage.2.NormScore <- function(Stage.1.Model, 
                              Assume.Homoscedasticity, 
                              Assume.Normality, 
                              Score, 
                              Rounded=TRUE){ 

Fitted.Model <- Stage.1.Model

ifelse(test = Rounded==TRUE, yes = Digits.Percentile<-0, no = Digits.Percentile<-1)

if (inherits(Fitted.Model, what = "Stage.1")==FALSE){stop("The argument Stage.1.Model=... should specify an object of class Stage.1\n")}

# Determine which assumptions are used to derive the normative data (homosced and/or normality)
if (Fitted.Model$HomoNorm$Num.Preds > 1){
  Check.Assump <- Check.Assum(Fitted.Model)
  if(missing(Assume.Homoscedasticity)){Group.Specific.SD.Resid <- !Check.Assump$Assume.Homo.S2}
  if(missing(Assume.Normality)){Empirical.Dist.Delta <- !Check.Assump$Assume.Normality.S2}
}
# for intercept-only model, homoscedasticity not relevant
if (Fitted.Model$HomoNorm$Num.Preds == 1){
  # LS!
  # levene not significant here
  Group.Specific.SD.Resid<-FALSE
  Check.Assump <- Check.Assum(Fitted.Model)
  if(missing(Assume.Normality)){Empirical.Dist.Delta <- !Check.Assump$Assume.Normality.S2}
}
# if Assume.Homoscedasticity or Assume.Normality specified in function call, use these
if (missing(Assume.Homoscedasticity)==FALSE){Group.Specific.SD.Resid <- !Assume.Homoscedasticity}
if (missing(Assume.Normality)==FALSE){Empirical.Dist.Delta <- !Assume.Normality}

# Start computations. Select type of model (homosced/normality assumed yes/no based on fitted model) 
if (Group.Specific.SD.Resid==FALSE & Empirical.Dist.Delta==FALSE){Fitted.Model <- Fitted.Model$HomoNorm; 
    Assume.Homoscedasticity<-TRUE; Assume.Normality<-TRUE}

if (Group.Specific.SD.Resid==TRUE & Empirical.Dist.Delta==FALSE){Fitted.Model <- Fitted.Model$NoHomoNorm; 
    Assume.Homoscedasticity<-FALSE; Assume.Normality<-TRUE}

if (Group.Specific.SD.Resid==FALSE & Empirical.Dist.Delta==TRUE){Fitted.Model <- Fitted.Model$HomoNoNorm; 
    Assume.Homoscedasticity<-TRUE; Assume.Normality<-FALSE}

if (Group.Specific.SD.Resid==TRUE & Empirical.Dist.Delta==TRUE){Fitted.Model <- Fitted.Model$NoHomoNoNorm; 
    Assume.Homoscedasticity<-FALSE; Assume.Normality<-FALSE}

old.options <- options()
on.exit(options(old.options))
options(digits = 8, scipen=999)    
  
Results <- NULL
Y <- Score[1]  
Object <- Fitted.Model
Pred.Score <- round(predict(Object$Reg.Model, newdata = Score), digits=8)

# if intercept only model:
if (Object$Num.Preds == 1){
  Pred.Score <- round(unique(predict(Object$Reg.Model)), digits=8)
  }
Results <- cbind(Results, Y, Pred.Score)

# If same SD(residual) has to be used for all predicted scores
if (Object$Group.Specific.SD.Resid==FALSE){
  SD.Resid <- Object$SD.Resid
  Results <- cbind(Results, SD.Resid)
}

# If different SD(residual)s have to be used 
if (Object$Group.Specific.SD.Resid==TRUE  & Object$Use.Variance.Function==FALSE){
  SD.Resid <- Object$SD.Resid
  SD.Resid$Abs.Diff <- abs(as.numeric(as.character(SD.Resid$`Predicted test score`)) - Pred.Score)
  temp <- SD.Resid[order(SD.Resid$Abs.Diff),]
  SD.Resid <- temp$`Residual Std.Error`[1]
  Results <- cbind(Results, SD.Resid)
}

  # If continuous variance function residuals has to be used
if (Object$Group.Specific.SD.Resid==TRUE & Object$Use.Variance.Function==TRUE){
    Predicted <- data.frame(Results[,2])
    names(Predicted) <- "Predicted"
    suppressWarnings(SD.Resid <- try(sqrt(predict(Object$Fit.Var.Function, newdata = Predicted)), silent=TRUE))
    Results <- cbind(Results, SD.Resid)
  }

Results <- data.frame(Results) #LS
Residual_i <- (as.numeric(Results$Y) - as.numeric(Results$Pred.Score))  
Delta_i <- (as.numeric(Results$Y) - as.numeric(Results$Pred.Score)) / as.numeric(Results$SD.Resid)

# If normality not assumed
if (Empirical.Dist.Delta==TRUE){
Percentiles <- cbind(seq(from=0, to=1, by=.001)*100, Fitted.Model$Percentiles.Delta)
Percentiles <- data.frame(cbind(Percentiles, Delta_i, abs(Percentiles[,2]-Delta_i)))
Sorted <- Percentiles[order(Percentiles[,4]),]
Percentile_here <- Sorted[1,1]
}

# If normality assumed
if (Empirical.Dist.Delta==FALSE){
Percentile_here <- round(pnorm(c(Delta_i), mean=0, sd=1, lower.tail=TRUE)*100, digits = 1) 
}

# Round percentiles
Percentile_here <- round(Percentile_here, digits = Digits.Percentile)

Results <- cbind(Results, Residual_i, Delta_i, Percentile_here)
row.names(Results) <- NULL
names(Results)[6] <- "Percentile"
names(Results)[1] <- "Y_i"

fit<-list(Fitted.Model=Fitted.Model, Results=Results,
          Assume.Homoscedasticity=Assume.Homoscedasticity, 
          Assume.Normality=Assume.Normality, 
          Score=Score, Stage.1.Model=Stage.1.Model,
          Call=match.call())

class(fit) <- "Stage.2.NormScore"
fit
}

# Summary function
summary.Stage.2.NormScore <- function(object, ..., Object){

old.options <- options()
on.exit(options(old.options))
options(digits = 8, scipen=999)

if (missing(Object)){Object <- object} 

if (Object$Fitted.Model$Higher.Score.Is.Better==TRUE){
cat("----------------------------------------------")
cat("\nResults Stage 2, four-step normative procedure")
cat("\n----------------------------------------------")
cat("\nStep 1: Compute the predicted mean test score (Y-hat): ", as.numeric(Object$Results$Pred.Score))
cat("\nStep 2: Compute the residual (varepsilon): ", as.numeric(Object$Results$Y_i), "-", as.numeric(Object$Results$Pred.Score), "=", 
    as.numeric(Object$Results$Y_i) - as.numeric(Object$Results$Pred.Score))
cat("\nStep 3: Compute the standardized residual (delta): ", 
    as.numeric(Object$Results$Y_i) - as.numeric(Object$Results$Pred.Score), "/", as.numeric(Object$Results$SD.Resid), "=",
    as.numeric(Object$Results$Delta_i))
cat("\nStep 4: Convert the standardized residual into an estimated percentile rank: ", as.numeric(Object$Results$Percentile), sep="")
}

if (Object$Fitted.Model$Higher.Score.Is.Better==FALSE){
  cat("\n\nResults Stage 2, four-step normative procedure:")
  cat("\n-----------------------------------------------")
  cat("\nStep 1: Compute model-predicted test score (Y-hat): ", as.numeric(Object$Results$Pred.Score))
  cat("\nStep 2: Compute residual (varepsilon): ", "-(", as.numeric(Object$Results$Y_i), "-", 
      as.numeric(Object$Results$Pred.Score), ") = ", 
      -(as.numeric(Object$Results$Y_i) - as.numeric(Object$Results$Pred.Score)), sep="")
  cat("\nNote: sign reversed because higher test score corresponds with less desirable outcome.")
  cat("\nStep 3: Compute standardized residual (delta): ", 
      -(as.numeric(Object$Results$Y_i) - as.numeric(Object$Results$Pred.Score)), "/", as.numeric(Object$Results$SD.Resid), "=",
      as.numeric(Object$Results$Delta_i))
  cat("\nStep 4: Convert standardized residual into an estimated percentile rank: ", as.numeric(Object$Results$Percentile), sep="")
}

cat("\n\n------------------------------------")
cat("\nAssumptions made in the computations")
cat("\n------------------------------------")
Homo.Assumed <- ifelse(Object$Fitted.Model$Group.Specific.SD.Resid==FALSE, yes = TRUE, no = FALSE)
Norm.Assumed <- ifelse(Object$Fitted.Model$Empirical.Dist.Delta==TRUE, yes=FALSE, no = TRUE)
cat("\nHomoscedasticity assumed?", Homo.Assumed)
cat("\nNormality of standardized residuals assumed?", Norm.Assumed)
}



# plot function
plot.Stage.2.NormScore <- function(x, Main=" ", Both.CDFs=FALSE, xlim, 
                                   cex.axis=1, cex.main=1, cex.lab=1, ...){
Object <- x
old.options <- options()
on.exit(options(old.options))
options(digits = 8, scipen=999)  
if (missing(xlim)){xlim <- c(-4,4)}

# Empirical distribution delta
if (Object$Fitted.Model$Empirical.Dist.Delta == TRUE){
plot(density(Object$Fitted.Model$Delta), xlab=expression(paste("Standardized residual ", hat(delta))), ylab="",
     main=Main, xlim=xlim, cex.axis=cex.axis, cex.main=cex.main, cex.lab=cex.lab, ...)
abline(v=Object$Results$Delta_i, lty=2)
dens <- density(Object$Fitted.Model$Delta)
try(x1 <- min(which(dens$x >= -100)), silent = TRUE)  
try((x2 <- max(which(dens$x < Object$Results$Delta_i))), silent = TRUE)

if (inherits(x2, what = "try-error")==TRUE){x2 <- 1}
try(with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray")), silent=TRUE)
}

# True standard normal
if (Object$Fitted.Model$Empirical.Dist.Delta == FALSE){
vals_x <- seq(-50, 50, length.out=5000)  
vals_y <- dnorm(vals_x, mean = 0, sd = 1)

min_val <- abs(Object$Results$Delta_i) * -1.1
max_val <- abs(Object$Results$Delta_i) * 1.1
plot(x=vals_x, y=vals_y, type="l", xlab=expression(paste("Standardized residual ", hat(delta))), 
     xlim=xlim, ylab=" ", main=Main, cex.axis=cex.axis, cex.main=cex.main, cex.lab=cex.lab, ...)
abline(v=Object$Results$Delta_i, lty=2)

cut_off <- Object$Results$Delta_i
cord.x <- c(-50,seq(-50,cut_off,0.01),cut_off) 
cord.y <- c(0,dnorm(seq(-50,cut_off,0.01)),0) 
polygon(cord.x,cord.y,col="grey")
}

if (Both.CDFs==TRUE){
vals_x <- seq(-50, 50, length.out=5000)  
vals_y <- dnorm(vals_x, mean = 0, sd = 1)
   # max x vals
min_val <- abs(Object$Results$Delta_i) * -1.1
max_val <- abs(Object$Results$Delta_i) * 1.1
   # max y
max_y <- max(vals_y, max(density(Object$Fitted.Model$Delta)$y))*1.1

  # plot theoretical CDF delta_i
plot(x=vals_x, y=vals_y, type="l", xlab=expression(paste("Standardized residual ", hat(delta))), 
     xlim=xlim, col="grey", ylim <- c(0, max_y), ylab=" ", main=Main, cex.axis=cex.axis, cex.main=cex.main, cex.lab=cex.lab, ...)
lines(density(Object$Fitted.Model$Delta))
abline(v=Object$Results$Delta_i, lty=2)
legend("topright", c("Theoretical density", "Empirical density"), lty=c(1,1), col=c("grey", "black"), cex=.9)
}

}

