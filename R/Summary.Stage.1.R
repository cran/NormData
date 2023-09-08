summary.Stage.1 <- function(object, ..., Object){
  
  old.options <- options()
  on.exit(options(old.options))
  options(digits = 8, scipen=999)
  if (missing(Object)){Object <- object} 
  Object_orig <- x <- Object
  
  if (Object$HomoNorm$Num.Preds != 1){  # for intercept-only not relevant
  Check.Assump <- Check.Assum(x)
  Assume.Homoscedasticity <- Check.Assump$Assume.Homo.S2
  }

  # Give warning if continuous variance prediction function yields negative vales 
  if (Object$HomoNorm$Num.Preds != 1){  # for intercept-only not relevant
  if (Assume.Homoscedasticity==FALSE & x$NoHomoNoNorm$Give.Warning.Neg.Variances){
     warning("\nThe variance prediction function yielded negative values. \nExamine the variance function plot to understand the cause of this issue. \n\nPotential remedial action that can be taken: \n- Try using a different order of the polynomial for the variance prediction function \n  (see argument Order.Poly.Var=... of the Stage.1() function call)\n- Remove potential outliers\n\n")
    }
  }
  
  # Show output of analysis assuming homoscedasticity and normality in Stage 1 
  Object <- Object_orig$HomoNorm
  Object_NoHomo <- Object_orig$NoHomoNorm   # for delta not assuming homoscedasticity. Could also use NoHomoNoNorm$Delta, same
  
  if (Object$Num.Preds == 1){  # for intercept-only homoscedasticity not relevant
    Assume.Homoscedasticity<-FALSE
  }
  
  # List of variables in the model
  
  cat("-----------------------------\n")
  cat("Variables in the fitted model")
  cat("\n-----------------------------\n")
  print(Object$Variables, row.names = FALSE)  
  
  cat("\nNote: In the specified dataset, N = ", Object$N.Dataset, sep="")
  
  if (dim(Object$Dataset)[1] ==length(Object$Reg.Model$fitted.values)){
    cat("\nAll observations were used in the analysis (no missing data).")
  }
  
  # Missing data?
  if (dim(Object$Dataset)[1] !=length(Object$Reg.Model$fitted.values)){
    cat("\n\nWarning: a total of ", (dim(Object$Dataset)[1] - length(Object$Reg.Model$fitted.values)), 
        " observations were discarded \nfrom the analysis due to missing values. The analysis \nwas conducted using ", length(Object$Reg.Model$fitted.values), " observations.", sep="")
  }
  
  # Give warning if variables of class character are used
  if (sum(Object$Variables$`Type variable`=="character")>0){
    cat("\n\nNote: Variables of class character are used. Such variables can \ngive issues when fitting a regression model, consider re-coding \nthe variable as a factor.\n")
  }
  
  cat("\n\n------------------------\n")
  cat("Mean prediction function\n")
  cat("------------------------\n")
  Main.Results <- (round(summary(Object$Reg.Model)$coef, digits=6))
  Sig <- Sig_Level <- Main.Results[,4] <= Object$Alpha.Reg
  Sig_Level[Sig==TRUE] <- as.character("*")
  Sig_Level[Sig!=TRUE] <- c(" ")
  Main.Results <- data.frame(cbind(Main.Results, as.character(Sig_Level)))
  names(Main.Results) <- c("Estimate", "Std. Error",  "T* value", "Pr(>|T*|)", " ")
  
  if (Assume.Homoscedasticity==TRUE){
  print(Main.Results)
  cat("\n*: p-value <=", Object$Alpha.Reg)
  if (Object$Test.Assumptions==TRUE){
    cat("\n\nNote: OLS-estimators of the standard errors are used \nbecause the homoscedasticity assumption is valid (see below).")
  }
}
  
  if (Assume.Homoscedasticity==FALSE){
    summary(Sandwich(x, Type = x$Sandwich.Type))
    cat("\n\nNote: Sandwich-estimators of the standard errors are used \nbecause the homoscedasticity assumption is invalid (see below).")
  
    if (length(Object$Reg.Model$fitted.values) < 250){
      cat("\n\nNote 2: The HC0 standard errors are used by default but the sample size is \nbelow 250. It is recommended to use the HC3 standard errors (by adding \nthe argument Sandwich.Type = 'HC3' in the Stage.1() function call).")
    }
    
}
  
  
  # Assumptions of fitted model for mean structure
 
  # If assumptions tested
  if (Object$Test.Assumptions == TRUE){
    
    cat("\n\n\nModel assumptions:\n")
    cat("------------------\n")
    
    if (Object$Num.Preds > 1){ # for model with more than one predictor 
      
      # if model does not contain numeric predictors, use levene
      if (Object$Contains.Numeric.Pred==FALSE){
        cat("\nLevene test of homoscedasticity:\n")
        cat("--------------------------------\n")
        temp0 <- Object$Group.Spec.SD.Resid.Values
        temp0$`Residual Var.` <- temp0$`Residual Std.Error`**2
        cols.here <- dim(temp0)[2]
        temp0 <- 
          temp0[,c(1, cols.here, 2:(cols.here-1))]
        temp0[,c(1:3)] <- round(temp0[,c(1:3)], digits = 6)
        cat("L* test-statistic: ", round(Object$Levene$`F value`[1], digits=6), ". DF = (", Object$Levene$Df[1], ", ", Object$Levene$Df[2], "), p-value = ", round(Object$Levene$`Pr(>F)`[1], digits=6), sep="")
        cat(paste("\n\nUsing alpha =", Object$Alpha.Levene))
        if (Object$Levene$Pr[1] <= Object$Alpha.Levene){
          cat(", the null hypothesis of homoscedasticity \nis rejected.")
        }
        if (Object$Levene$Pr[1] > Object$Alpha.Levene){
          cat(", the null hypothesis of homoscedasticity \nis not rejected.")
        }
      }
      
      # if model contains numeric predictors, use Breusch-Pagan
      if (Object$Contains.Numeric.Pred==TRUE){
        cat("\nBreusch-Pagan test of homoscedasticity:\n")
        cat("---------------------------------------\n")
        cat("Chi2_BP* test-statistic: ", round(Object$Breusch.Pagan$statistic[1], digits=6), 
            " (DF = ", Object$Breusch.Pagan$parameter, "), p-value = ", round(Object$Breusch.Pagan$p.value, digits=6), sep="")
        cat(paste("\n\nUsing alpha =", Object$Alpha.BP))
        if (Object$Breusch.Pagan$p.value <= Object$Alpha.BP){
          cat(", the null hypothesis of homoscedasticity \nis rejected.") 
        }
        if (Object$Breusch.Pagan$p.value > Object$Alpha.BP){
          cat(", the null hypothesis of homoscedasticity \nis not rejected.") 
        }
      }
    }
    
    if (Object$Num.Preds == 1){ # for intercept-only model 
      cat("\nLevene test of homoscedasticity:\n")
      cat("--------------------------------\n")
      cat("Test not conducted (not meaningful for intercept-only model)")
    }
    
    cat("\n\n\nShapiro-Wilk test of normality:\n")
    cat("-------------------------------")
    
    # delta i = residual/overall SD(residual)    (assuming homoscedasticity)
    if (Assume.Homoscedasticity==TRUE){ 
      cat("\nW* test-statistic: ", round(Object$Shapiro.Wilk$statistic, digits = 6), ", p-value = ", round(Object$Shapiro.Wilk$p.value, digits=6), sep="")
      cat(paste("\n\nUsing alpha =", Object$Alpha.Shapiro))
      if (Object$Shapiro.Wilk$p.value <= Object$Alpha.Shapiro){
        cat(", the null hypothesis of normality \nis rejected.") #If residual plots confirm this problem, consider using the empirical CDF \nof the observed standardized residuals to convert raw test scores \ninto percentile values (using the argument Empirical.Dist.Delta=TRUE \nin the FitNormModel function call).")
      }
      if (Object$Shapiro.Wilk$p.value > Object$Alpha.Shapiro){
        cat(", the null hypothesis of normality \nis not rejected.")
      }
    }
    
    # delta i = residual/SD(residual) per group    (assuming no homoscedasticity)
    if (Assume.Homoscedasticity==FALSE){
      cat("\nW* test-statistic: ", round(Object_NoHomo$Shapiro.Wilk$statistic, digits = 6), ", p-value = ", 
          round(Object_NoHomo$Shapiro.Wilk$p.value, digits=6), sep="")
      cat(paste("\n\nUsing alpha =", Object_NoHomo$Alpha.Shapiro))
      if (Object_NoHomo$Shapiro.Wilk$p.value <= Object_NoHomo$Alpha.Shapiro){
        cat(", the null hypothesis of normality \nis rejected.") #If residual plots confirm this problem, consider using the empirical CDF \nof the observed standardized residuals to convert raw test scores \ninto percentile values (using the argument Empirical.Dist.Delta=TRUE \nin the FitNormModel function call).")
      }
      if (Object_NoHomo$Shapiro.Wilk$p.value > Object_NoHomo$Alpha.Shapiro){
        cat(", the null hypothesis of normality \nis not rejected.")
      }
    }
    
    # outliers
    cat("\n\n\nOutliers:\n")
    cat("---------")
    Cut_off <- Object$Outlier.Cut.Off
    
    # use delta based on overall SD(resid) or prediction-specific SD(resid)
    Rows.Dataset.Full <- as.numeric(as.character(names(predict(Object$Reg.Model))))
    if (Assume.Homoscedasticity==TRUE){
      Data_temp <- data.frame(cbind(Rows.Dataset.Full, Object$Delta))
    }
    if (Assume.Homoscedasticity==FALSE){
      Data_temp <- data.frame(cbind(Rows.Dataset.Full, Object_NoHomo$Delta))
    }
    names(Data_temp) <- c("Row in dataset", "Delta_i")
    
    #  highest |delta_i| values
    Data_temp2 <- na.exclude(Data_temp)
    Highest.Delta <- Data_temp2[order(-abs(Data_temp2$Delta_i)),][c(1:5),]
    Number_boven_cutoff <- sum(abs(na.exclude(Data_temp$Delta_i)) > Cut_off)
    Obs_above_cut_off <- data.frame(Data_temp[(abs(Data_temp$Delta_i) > Cut_off),])
    names(Obs_above_cut_off) <- c("Observation", "|Std. residual|")
    if (Number_boven_cutoff == 0){cat(paste("\nNo outliers were detected (using |Std. residual| > ", 
                                            Object$Outlier.Cut.Off,")\n", sep=""))}
    if (Number_boven_cutoff == 1){
      cat(paste("\nThere is ", Number_boven_cutoff, " outlier in the dataset (using |Std. residual| > ", 
                Object$Outlier.Cut.Off,"): \n\n", sep=""))
      Obs_above_cut_off <- data.frame(Obs_above_cut_off, row.names = NULL)
      colnames(Obs_above_cut_off) <- c("Observation", "|Std. residual|")
      print(na.exclude(Obs_above_cut_off))
    }
    if (Number_boven_cutoff > 1){
      cat(paste("\nThere are ", Number_boven_cutoff, " outliers in the dataset (using |Std. residual| > ", 
                Object$Outlier.Cut.Off,"): \n\n", sep=""))
      Obs_above_cut_off <- data.frame(Obs_above_cut_off, row.names = NULL)
      colnames(Obs_above_cut_off) <- c("Observation", "|Std. residual|")
      print(na.exclude(Obs_above_cut_off))
    }
    
    # list of observations with highest |delta_i| values
    cat("\nObservations with highest |delta_i| values:\n")
    Highest.Delta <- data.frame(Highest.Delta, row.names = NULL)
    colnames(Highest.Delta) <- c("Observation", "|Std. residual|")
    print(Highest.Delta[c(1:3),])
    
    if (Object$Num.Preds != 1){  # for intercept-only not relevant
    if (Assume.Homoscedasticity==TRUE){
      cat("\nNote: the std. residuals are computed using the overall residual \nstandard error because the homoscedasticity assumption is valid \n(see above).\n")
    }
    if (Assume.Homoscedasticity==FALSE){
      cat("\nNote: the std. residuals are computed using prediction-specific \nresidual standard errors because the homoscedasticity assumption \nis invalid (see above).\n")
    }
      }
    
    if (Object$Num.Preds == 1){  # for intercept-only not relevant
      # print how delta i is computed
      cat("\nNote: the std. residuals are computed using the overall \nresidual standard error (homoscedasticity assumption \nnot relevant for intercept-only model)\n")
    } 
    
    
    if (Object$Show.VIF==TRUE){
      
      if (is.na(Object$VIF.Original[1]) == FALSE){
        
        Names.Variables <- names(coef(Object$Reg.Model))
        Names.Variables <- Names.Variables[grepl("(Intercept)", Names.Variables)==FALSE]
        
        # If all variable names in the model contain Age (e.g., Age, Age.2 and Age.3), some higher polynomial
        # is fitted. For such models, multicollinearity diagnostics are essentially
        # irrelevant and this is suppressed in the output
        if (all(grepl("Age", Names.Variables))==FALSE){
          cat("\n\nGeneralized VIF (GVIF):\n")
          cat("-----------------------\n")
          
          if (is.null(names(Object$VIF.Original))==TRUE){
            names_temp <- row.names(Object$VIF.Original)
            temp <- round(as.data.frame(Object$VIF.Original)[,1], digits = 6) # GVIF^(1/(2*Df)) is SQRT of GVIF. GVIF is compared to 10 threshold value
          }
          if (is.null(names(Object$VIF.Original))==FALSE){
            names_temp <- names(Object$VIF.Original)
            temp <- round(as.data.frame(Object$VIF.Original)[,1], digits = 6) # GVIF^(1/(2*Df)) is SQRT of GVIF. GVIF is compared to 10 threshold value
          }
          
          Above.Threshold <- temp > Object$GVIF.Threshold
          temp <- data.frame(cbind(names_temp, temp, Object$GVIF.Threshold, Above.Threshold))
          names(temp) <- c("Variable", "GVIF", "Threshold", "Above Threshold?")
          print(temp)
          cat(paste("\nThe maximum GVIF = ", max(temp$GVIF), "\n", sep=""))   
          if ((max(temp$GVIF) >= Object$GVIF.Threshold)){ 
             cat(paste("\nThe maximum observed GVIF is above the threshold value = ", 
             Object$GVIF.Threshold, ", \nwhich can be indicative of severe multicollinearity.", sep=""))}
        }
      }
    }
  }
  
  if (Object$Test.Assumptions == FALSE){
    cat("\n\nNote: The standard errors are derived under the assumption \nthat the homoscedasticity assumption is valid.")
  }
  
  if (Object$Num.Preds != 1){  # for intercept-only not relevant
  cat("\n\n----------------------------\n")
  cat("Variance prediction function\n")
  cat("----------------------------\n")
  }
    
  # If assumptions not tested
  if (Object$Test.Assumptions == FALSE){   # | Assume.Homoscedasticity==TRUE
    SD_h <- round(Object$SD.Resid, digits=6)
    Var_h <- round(SD_h**2, digits=6)
    cat("Residual variance:", Var_h)
    cat("\nResidual std. error:", SD_h)
    cat("\n\nNote: The variance prediction function is derived under the \nassumption that the homoscedasticity assumption is valid.")
  }
  
  # If assumptions tested
  if (Object$Test.Assumptions == TRUE){
    
    if (Object$Num.Preds > 1){ # for model with more than one predictor 
      
      # if model does not contain numeric predictors, use levene
      if (Object$Contains.Numeric.Pred==FALSE){
    
        if (Object$Levene$Pr[1] <= Object$Alpha.Levene){
          cat("The homoscedasticity assumption is invalid (see above). \n\nThe variance prediction function corresponds to:\n")
          temp0 <- Object$Group.Spec.SD.Resid.Values
          temp0$`Residual Var.` <- temp0$`Residual Std.Error`**2
          cols.here <- dim(temp0)[2]
          temp0 <- 
            temp0[,c(1, cols.here, 2:(cols.here-1))]
          temp0[,c(1:3)] <- round(temp0[,c(1:3)], digits = 6)
          print(temp0)       
           }

        if (Object$Levene$Pr[1] > Object$Alpha.Levene){
          cat("The homoscedasticity assumption is valid (see above). \n\nThe variance prediction function corresponds to the \noverall residual variance: \n")
          SD_h <- round(Object$SD.Resid, digits=6)
          Var_h <- round(SD_h**2, digits=6)
          cat("Residual variance:", Var_h)
          cat("\nResidual std. error:", SD_h)
          }
        }
      
      # if model contains numeric predictors, use Breusch-Pagan
      if (Object$Contains.Numeric.Pred==TRUE){
        if (Object$Breusch.Pagan$p.value <= Object$Alpha.BP){
          cat("The homoscedasticity assumption is invalid (see above). \n\nThe variance prediction function corresponds to:\n") 
        }
        if (Object$Breusch.Pagan$p.value > Object$Alpha.BP){
          cat("The homoscedasticity assumption is valid (see above). \n\nThe variance prediction function corresponds to the \noverall residual variance: \n")
          SD_h <- round(Object$SD.Resid, digits=6)
          Var_h <- round(SD_h**2, digits=6)
          cat("Residual variance:", Var_h)
          cat("\nResidual std. error:", SD_h)
        }
      
        }
     }
    
    # If continuous variance prediction function residuals has to be used
    if (Assume.Homoscedasticity==FALSE & Object$Use.Variance.Function==TRUE){
      
      Main.Results.Var.Function <- (round(summary(Object$Fit.Var.Function)$coef, digits=6))
      Main.Results.Var.Function <- data.frame(Main.Results.Var.Function[,c(1)])
      names(Main.Results.Var.Function) <- c("Estimate") #, "Std. Error",  "T* value", "Pr(>|T*|)", " ")
      labels <- paste("Pred.Y.", 2:100, sep="")
      if (length(row.names(Main.Results.Var.Function))==2){
        row.names(Main.Results.Var.Function) <- c("(Intercept)", "Pred.Y")}
      if (length(row.names(Main.Results.Var.Function))>2){
        row.names(Main.Results.Var.Function) <- c("(Intercept)", "Pred.Y", labels[1:c(length(row.names(Main.Results.Var.Function))-2)])}
      print(Main.Results.Var.Function)
      cat("\nAIC:", AIC(Object$Fit.Var.Function))
     }
  }
}

