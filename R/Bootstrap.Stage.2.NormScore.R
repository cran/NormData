Bootstrap.Stage.2.NormScore <- function(Stage.2.NormScore, 
        CI=.99, Number.Bootstraps=2000, 
        Seed=123, Rounded=FALSE, Show.Fitted.Boot=FALSE, verbose=TRUE){  

ifelse(test = Rounded==TRUE, yes = Digits.Percentile<-0, no = Digits.Percentile<-1)
  
Assume.Homoscedasticity <- Stage.2.NormScore$Assume.Homoscedasticity    
Assume.Normality=Stage.2.NormScore$Assume.Normality  

if (inherits(Stage.2.NormScore, what = "Stage.2.NormScore")==FALSE){
  stop("The specified Stage.2.NormScore should be an object of class Stage.2.NormScore\n")
}

if (Number.Bootstraps < 100){
    message("\nNOTE: the number of bootstrap samples is lower than 100. It is recommended to use at least 100 bootstrap samples\n")
  }  
  
All.Percentiles <- NULL
if (verbose==TRUE) cat("\nBootstrapping... \n")
for (i in 1: Number.Bootstraps){
  
  if (Number.Bootstraps>50){
  if (i%%round(Number.Bootstraps/10, digits = 0) == 0){
    if (verbose==TRUE) cat(paste(round((i/Number.Bootstraps)*100, digits = 0), "% done... ", sep=""))
  }}
  
  data_here <- Stage.2.NormScore$Fitted.Model$Dataset
  N <- dim(data_here)[1]
  set.seed(Seed*i); Data_Boot <- data_here[sample(x = 1:N, size = N, replace = TRUE),]
  
  Model <- formula(Stage.2.NormScore$Fitted.Model$Reg.Model)
  
  # If homoscedasticity is valid
  if (Assume.Homoscedasticity==TRUE){
    Fit <- Stage.1(Dataset = Data_Boot, Model = Model, Alpha.Homosc = 0) # always accept homoscedasticity
  }
  
  # If homoscedasticity is invalid and model contains no numeric predictor in the mean structure, use step function of std resid
  if (Assume.Homoscedasticity==FALSE & Stage.2.NormScore$Stage.1.Model$NoHomoNoNorm$Contains.Numeric.Pred==FALSE){
    Fit <- Stage.1(Dataset = Data_Boot, Model = Model, 
                   Alpha.Homosc = 1) # always reject homoscedasticity
  }
  
  # If homoscedasticity is invalid and model contains numeric predictor in the mean structure, use polynomial var function
  if (Assume.Homoscedasticity==FALSE & Stage.2.NormScore$Stage.1.Model$NoHomoNoNorm$Contains.Numeric.Pred==TRUE){
    Fit <- Stage.1(Dataset = Data_Boot, Model = Model, Order.Poly.Var = Stage.2.NormScore$Stage.1.Model$Order.Poly.Var,
                   Alpha.Homosc = 1) # always reject homoscedasticity
  }
  
  if (Show.Fitted.Boot==TRUE){
    message(paste0("\n\n*** Bootstrap sample ", i, " ***\n\n"))
    summary(Fit)}
  
  Percentile.Here <- Stage.2.NormScore(Stage.1.Model = Fit, Score = Stage.2.NormScore$Score, 
                                       Rounded=FALSE,  # LS FALSE!
                                       Assume.Normality = Stage.2.NormScore$Assume.Normality, 
                                       Assume.Homoscedasticity = Stage.2.NormScore$Assume.Homoscedasticity)$Results$Perc
  All.Percentiles <- c(All.Percentiles, Percentile.Here)
}
if (verbose==TRUE) cat("\n\nAll done.\n")
alpha <- 1-CI
CI.Percentile <- round(quantile(x = All.Percentiles, c(alpha/2, 1-(alpha/2))), digits = Digits.Percentile)

# Point estimate based on original dataset (no bootstrap sample)
Dataset.Orig <- Stage.2.NormScore$Fitted.Model$Dataset
Model <- formula(Stage.2.NormScore$Fitted.Model$Reg.Model)

# If homoscedasticity is valid
if (Assume.Homoscedasticity==TRUE){
  Fit <- Stage.1(Dataset = Dataset.Orig, Model = Model, Alpha.Homosc = 0) # always accept homoscedasticity
}

# If homoscedasticity is invalid and model contains no numeric predictor in the mean structure, use step function of std resid
if (Assume.Homoscedasticity==FALSE & Stage.2.NormScore$Stage.1.Model$NoHomoNoNorm$Contains.Numeric.Pred==FALSE){
  Fit <- Stage.1(Dataset = Dataset.Orig, Model = Model, Alpha.Homosc = 1) # always reject homoscedasticity
}

# If homoscedasticity is invalid and model contains numeric predictor in the mean structure, use polynomial var function
if (Assume.Homoscedasticity==FALSE & Stage.2.NormScore$Stage.1.Model$NoHomoNoNorm$Contains.Numeric.Pred==TRUE){
  Fit <- Stage.1(Dataset = Dataset.Orig, Model = Model, Order.Poly.Var = Stage.2.NormScore$Stage.1.Model$Order.Poly.Var,
                 Alpha.Homosc = 1) # always reject homoscedasticity
}

Percentile.Point.Estimate <- Stage.2.NormScore(Stage.1.Model = Fit, Score = Stage.2.NormScore$Score, 
                                               Rounded=Rounded,
                                               Assume.Normality = Stage.2.NormScore$Assume.Normality, 
                                               Assume.Homoscedasticity = Stage.2.NormScore$Assume.Homoscedasticity)$Results$Perc

fit<-list(CI.Percentile=CI.Percentile, CI=CI, All.Percentiles=All.Percentiles, 
          Assume.Homoscedasticity=Stage.2.NormScore$Assume.Homoscedasticity, 
          Assume.Normality=Stage.2.NormScore$Assume.Homoscedasticity, 
          Stage.2.NormScore=Stage.2.NormScore,
          Percentile.Point.Estimate=Percentile.Point.Estimate,
          Call=match.call())

class(fit) <- "Bootstrap.Stage.2.NormScore"
fit

}


# Summary function
summary.Bootstrap.Stage.2.NormScore <- function(object, ..., Object){
  
  old.options <- options()
  on.exit(options(old.options))
  options(digits = 8, scipen=999)
  
  
  if (missing(Object)){Object <- object} 
  
  # Confidence interval
  cat("-----------------------------------")
  cat("\nBootstrap-based confidence interval")
  cat("\n-----------------------------------")
  cat("\nPoint estimate percentile rank: ", Object$Percentile.Point.Estimate, sep="")
  cat(paste0("\n", Object$CI*100, "% CI: [", Object$CI.Percentile[1], ", ", Object$CI.Percentile[2], "]"))
  
  cat("\n\n------------------------------------")
  cat("\nAssumptions made in the computations")
  cat("\n------------------------------------")
  Homo.Assumed <- ifelse(Object$Stage.2.NormScore$Fitted.Model$Group.Specific.SD.Resid==FALSE, yes = TRUE, no = FALSE)
  Norm.Assumed <- ifelse(Object$Stage.2.NormScore$Fitted.Model$Empirical.Dist.Delta==TRUE, yes=FALSE, no = TRUE)
  cat("\nHomoscedasticity assumed?", Homo.Assumed)
  cat("\nNormality of standardized residuals assumed?", Norm.Assumed)
}


# Plot function
plot.Bootstrap.Stage.2.NormScore <- function(x, cex.axis=1, cex.main=1, cex.lab=1, ...){
  
  Object <- x
  CI.Val <- Object$CI*100
  plot(density(Object$All.Percentiles), xlab=expression(paste("Estimated percentile ", hat(pi)[0])), lwd=2, 
       main=expression(paste("Bootstrap distribution ", hat(pi)[0])), 
       cex.axis=cex.axis, cex.main=cex.main, cex.lab=cex.lab, ...) 
  abline(v=Object$CI.Percentile, lty=2)

}



