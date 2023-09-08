GLT <- function(Dataset, Unrestricted.Model, Restricted.Model, Alpha=0.05, Alpha.Homosc=0.05, 
                Assume.Homoscedasticity=NULL){
  
  Alpha.Homoscedasticity <-  Alpha.Homosc  
  
  if (is.null(Assume.Homoscedasticity)==FALSE){
    if (Assume.Homoscedasticity==TRUE){Alpha.Levene <- Alpha.BP <- Alpha.Homoscedasticity <-0 } 
    if (Assume.Homoscedasticity==FALSE){Alpha.Levene <- Alpha.BP <- Alpha.Homoscedasticity <-1 }
  }
  
  # LS FitNormModel for $included.data further on
Fit.Unrestricted.Model <- .FitNormModel(Dataset = Dataset, Model = Unrestricted.Model, 
                                       Test.Assumptions = T, Alpha.Reg = Alpha, Alpha.Levene = Alpha.Homoscedasticity, 
                                       Alpha.BP = Alpha.Homoscedasticity)
Data_here <- data.frame(Fit.Unrestricted.Model$Included.Data)
Fit.Restricted.Model <- lm(data = Data_here, formula = Restricted.Model)

F.Test.Stat.Results <- anova(Fit.Unrestricted.Model$Reg.Model, Fit.Restricted.Model)
F.Test.Hetero.Robust <- lmtest::waldtest(Fit.Restricted.Model, Fit.Unrestricted.Model$Reg.Model, test="F",
                   vcov = sandwich::vcovHC(Fit.Unrestricted.Model$Reg.Model, type = "HC0"))

# Is homoscedasticity assumption valid for the unrestricted model?
  # p-value levene if no quantitative IV in unrestricted model
if (Fit.Unrestricted.Model$Contains.Numeric.Pred==FALSE){
  p.val.homoscedasticity <- Fit.Unrestricted.Model$Levene$`Pr(>F)`[1]}
# p-value BP test if quantitative IV in unrestricted model
if (Fit.Unrestricted.Model$Contains.Numeric.Pred==TRUE){
p.val.homoscedasticity <- Fit.Unrestricted.Model$Breusch.Pagan$p.value 
}

fit <- list(F.Test.Stat.Results=F.Test.Stat.Results, Fit.Unrestricted.Model=Fit.Unrestricted.Model, 
            Fit.Restricted.Model=Fit.Restricted.Model, Alpha=Alpha, 
            p.val.homoscedasticity=p.val.homoscedasticity,
            F.Test.Hetero.Robust=F.Test.Hetero.Robust, Alpha.Homoscedasticity=Alpha.Homoscedasticity, 
            Call=match.call())
class(fit) <- "GLT"
fit

}


summary.GLT <- function(object, ..., Object, Assume.Homoscedasticity){
  old.options <- options()
  on.exit(options(old.options))
  options(digits = 12, scipen=999)
  if (missing(Object)){Object <- object} 
  
  cat("Note: In the specified dataset, N = ", Object$Fit.Unrestricted.Model$N.Dataset, sep="")
  
  if (Object$Fit.Unrestricted.Model$N.Dataset == length(Object$Fit.Unrestricted.Model$Reg.Model$fitted.values)){
    cat("\nAll observations were used in the analysis (no missing data).")
  }
  
  # Missing data?
  if (Object$Fit.Unrestricted.Model$N.Dataset != length(Object$Fit.Unrestricted.Model$Reg.Model$fitted.values)){
    
    cat("\n\nWarning: a total of ", (Object$Fit.Unrestricted.Model$N.Dataset - length(Object$Fit.Unrestricted.Model$Reg.Model$fitted.values)), 
        " observations were discarded \nfrom the analyses due to missing values (for the unrestricted model). \nThe analyses were conducted using ", length(Object$Fit.Unrestricted.Model$Reg.Model$fitted.values), " observations (for both models).", sep="")
  }
  
  cat("\n\n-----------------------------\n")
  cat("General linear test procedure\n")
  cat("-----------------------------\n")
  Main.Results.F.test <- data.frame(Object$F.Test.Stat.Results, row.names = NULL)
  Main.Results.F.test <- cbind(Main.Results.F.test[,2], Main.Results.F.test[,1], Main.Results.F.test[,c(5:6)])
  Main.Results.F.test <- round(Main.Results.F.test, digits=6)
  row.names(Main.Results.F.test) <- c("Unrestricted model", "Restricted model")
  Sig <- Sig_Level <- Main.Results.F.test[,4] <= Object$Alpha
  Sig_Level[Sig==TRUE] <- as.character("*")
  Sig_Level[Sig!=TRUE] <- NA
  Main.Results.F.test <- data.frame(cbind(Main.Results.F.test, as.character(Sig_Level)))
  names(Main.Results.F.test) <- c("SSE", "DF", "F* value", "Pr(>F*)", "")
  Main.Results.F.test[1,c(3:4)] <- c(" ", "") # LS!
  print(Main.Results.F.test, na.print="", digits = 16)  # LS!
  cat("\n*: p-value <=", Object$Alpha)
  
  # Give note if homoscedasticity assumption of unrestricted model is violated, Levene test
  if (Object$Fit.Unrestricted.Model$Contains.Numeric.Pred==FALSE){
    if (Object$Fit.Unrestricted.Model$Levene$Pr[1] <= Object$Fit.Unrestricted.Model$Alpha.Levene){
      cat("\n\nNote: The homoscedasticity assumption is invalid for the \nunrestricted fitted model (see below).")
    }
    if (Object$Fit.Unrestricted.Model$Levene$Pr[1] > Object$Fit.Unrestricted.Model$Alpha.Levene){
      cat("\n\nNote: The homoscedasticity assumption is valid for the \nunrestricted fitted model (see below).")
    }
  }   # end Levene
  # for BP test
  if (Object$Fit.Unrestricted.Model$Contains.Numeric.Pred==TRUE){
    if (Object$Fit.Unrestricted.Model$Breusch.Pagan$p.value <= Object$Fit.Unrestricted.Model$Alpha.BP){
      cat("\n\nNote: The homoscedasticity assumption is invalid for the \nunrestricted fitted model (see below).")
    }
    if (Object$Fit.Unrestricted.Model$Breusch.Pagan$p.value > Object$Fit.Unrestricted.Model$Alpha.BP){
      cat("\n\nNote: The homoscedasticity assumption is valid for the \nunrestricted fitted model (see below).")
    }
  } 
  
  # Heteroscedasticity consistent results F-test
  if(missing(Assume.Homoscedasticity)){
    if (Object$Fit.Unrestricted.Model$Contains.Numeric.Pred==TRUE){
    Assume.Homoscedasticity <- Object$Fit.Unrestricted.Model$Breusch.Pagan$p.value > Object$Fit.Unrestricted.Model$Alpha.BP}
    if (Object$Fit.Unrestricted.Model$Contains.Numeric.Pred==FALSE){
      Assume.Homoscedasticity <- Object$Fit.Unrestricted.Model$Levene$Pr[1] > Object$Fit.Unrestricted.Model$Alpha.Levene}
    }
  
  if (Assume.Homoscedasticity==FALSE){
    
    cat("\n\nHeteroscedasticity-robust results: \n")
    Robust <- data.frame(Object$F.Test.Hetero.Robust, row.names = NULL)
    Heterosced.Robust.F <- round(Robust$F[2], digits=6)
    Heterosced.Robust.F.p.value <- round(Robust$Pr..F.[2], digits=6)
    Sig <- Sig_Level <- Heterosced.Robust.F.p.value <= Object$Alpha
    Sig_Level[Sig==TRUE] <- as.character(" *")
    Sig_Level[Sig!=TRUE] <- as.character(" ")
    temp <- data.frame(Heterosced.Robust.F, Heterosced.Robust.F.p.value, Sig_Level)
    cat(paste("F* value = ", Heterosced.Robust.F, ", Pr(>F*) = ", Heterosced.Robust.F.p.value, Sig_Level, sep=""))
    cat("\n\n*: p-value <=", Object$Alpha)
    }
  
  cat("\n\n\nHomoscedasticity assumption unrestricted model\n")
  cat("----------------------------------------------")
  
  # Check homoscedasticity for unrestricted model, Levene test
  if (Object$Fit.Unrestricted.Model$Contains.Numeric.Pred==FALSE){
    cat("\nLevene test of homoscedasticity:\n")
    temp0 <- Object$Fit.Unrestricted.Model$Group.Spec.SD.Resid.Values
    temp0$`Residual Var.` <- temp0$`Residual Std.Error`**2
    cols.here <- dim(temp0)[2]
    temp0 <- temp0[,c(1, cols.here, 2:(cols.here-1))]
    temp0[,c(1:3)] <- round(temp0[,c(1:3)], digits = 6)
    cat("L* test-statistic: ", round(Object$Fit.Unrestricted.Model$Levene$`F value`[1], digits=6), 
        ". DF = (", Object$Fit.Unrestricted.Model$Levene$Df[1], ", ", Object$Fit.Unrestricted.Model$Levene$Df[2], 
        "), p-value = ", round(Object$Fit.Unrestricted.Model$Levene$`Pr(>F)`[1], digits=6), sep="")
    cat(paste("\n\nUsing alpha =", Object$Fit.Unrestricted.Model$Alpha.Levene))
    if (Object$Fit.Unrestricted.Model$Levene$Pr[1] <= Object$Fit.Unrestricted.Model$Alpha.Levene){
      cat(", the null hypothesis of homoscedasticity \nis rejected. \n")
    }
    
    if (Object$Fit.Unrestricted.Model$Levene$Pr[1] > Object$Fit.Unrestricted.Model$Alpha.Levene){
      cat(", the null hypothesis of homoscedasticity is \nnot rejected. \n")}
  }   # end Levene
  
  # Check homoscedasticity for unrestricted model, BP test
  if (Object$Fit.Unrestricted.Model$Contains.Numeric.Pred==TRUE){
    cat("\nBreusch-Pagan test of homoscedasticity: ")
    cat("\nChi2_BP* test-statistic: ", round(Object$Fit.Unrestricted.Model$Breusch.Pagan$statistic[1], digits=6), 
        " (DF = ", Object$Fit.Unrestricted.Model$Breusch.Pagan$parameter, "), p-value = ", 
        round(Object$Fit.Unrestricted.Model$Breusch.Pagan$p.value, digits=6), sep="")
    cat(paste("\n\nUsing alpha =", Object$Fit.Unrestricted.Model$Alpha.BP))
    if (Object$Fit.Unrestricted.Model$Breusch.Pagan$p.value <= Object$Fit.Unrestricted.Model$Alpha.BP){
      cat(", the null hypothesis of homoscedasticity \nis rejected.") 
    }
    if (Object$Fit.Unrestricted.Model$Breusch.Pagan$p.value > Object$Fit.Unrestricted.Model$Alpha.BP){
      cat(", the null hypothesis of homoscedasticity is \nnot rejected.  \n")   }
  } 
    
  }