Stage.2.NormTable <- function(Stage.1.Model,
  Assume.Homoscedasticity, Assume.Normality, 
  Grid.Norm.Table, Test.Scores, Digits=6, 
  Rounded=TRUE){ 
  
  ifelse(test = Rounded==TRUE, yes = Digits.Percentile<-0, no = Digits.Percentile<-1)
  
  Fitted.Model <- Stage.1.Model
  # Overall norms, no stratification
  if (missing(Grid.Norm.Table)){Grid.Norm.Table = expand.grid("All")}
  
  if (inherits(Fitted.Model, what = "Stage.1")==FALSE){stop("The argument Stage.1.Model=... should specify an object of class Stage.1\n")}
  
  # Determine which assumptions are used to derive the normative data (homoseced and/or normality)
  # if model contains predictors
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
    if(missing(Assume.Normality)){Empirical.Dist.Delta <- !Check.Assump$Assume.Normality.S2}
  }
  # if Assume.Homoscedasticity or Assume.Normality specified in function call, use these
  if (missing(Assume.Homoscedasticity)==FALSE){Group.Specific.SD.Resid <- !Assume.Homoscedasticity}
  if (missing(Assume.Normality)==FALSE){Empirical.Dist.Delta <- !Assume.Normality}
  
  # Start computations. Select type of model (homosced/normality assumed yes/no based on fitted model) 
  if (Group.Specific.SD.Resid==FALSE & Empirical.Dist.Delta==FALSE){Object <- Fitted.Model$HomoNorm}
  if (Group.Specific.SD.Resid==TRUE & Empirical.Dist.Delta==FALSE){Object <- Fitted.Model$NoHomoNorm}
  if (Group.Specific.SD.Resid==FALSE & Empirical.Dist.Delta==TRUE){Object <- Fitted.Model$HomoNoNorm}
  if (Group.Specific.SD.Resid==TRUE & Empirical.Dist.Delta==TRUE){Object <- Fitted.Model$NoHomoNoNorm}
  
  if (missing(Test.Scores)){stop("The test scores for which percentile values have to be computed need to be specified.")}
  
  old.options <- options()
  on.exit(options(old.options))
  options(digits=8)
  Pred.Means <- round(predict(Object$Reg.Model, newdata = Grid.Norm.Table), digits=Digits)
  temp <- cbind(Grid.Norm.Table, Pred.Means)
  
  # ----------------------------------------------------------
  # If same SD(residual) has to be used for all predicted scores
  # ----------------------------------------------------------
  if (Object$Group.Specific.SD.Resid==FALSE){
    SD.Resid <- Object$SD.Resid
    temp <- cbind(temp, SD.Resid)
  }

  # ----------------------------------------------------------  
  # If different SD_res to be used, depends on predicted score
  # ----------------------------------------------------------
  if (Object$Group.Specific.SD.Resid==TRUE){
    
    # setting with no numeric IVs in model
    # ------------------------------------
    if (Object$Use.Variance.Function==FALSE){
    SD.Resid <- Object$SD.Resid[,c(1:2)]
    
    # look for group-specific SD.Res for given predicted value and add these to the table
    Approx.Equal <- function(Target, Look.For.Value, Allowed.Abs.Diff=0.0001){
      Close.To.Target <- abs(Target-Look.For.Value)<=Allowed.Abs.Diff
      return(Close.To.Target)
    }
    temp <- data.frame(temp); temp$SD.Resid <- NULL
    # check for different groups
    for (l in 1: length(temp$Pred.Means)){
      Select.Here <- Approx.Equal(Target = temp$Pred.Means, 
                                  Look.For.Value = SD.Resid$`Predicted test score`[l])
      temp$SD.Resid[Select.Here] <- SD.Resid$`Residual Std.Error`[l]
    }
    } # end als geen variance functie
    
    # setting with numeric IVs in model
    # ------------------------------------
  if (Object$Use.Variance.Function==TRUE){
    Predicted <- data.frame(temp$Pred.Means)
    names(Predicted) <- "Predicted"
    suppressWarnings(Res.Std.Err.Temp <- try(sqrt(predict(Object$Fit.Var.Function, newdata = Predicted)), silent=TRUE))
    temp$SD.Resid <- Res.Std.Err.Temp
   } # end als geen variance functie
  }  # End group-spec SD resids
  
  # Compute percentiles corresponding with raw scores
  if ((inherits(Test.Scores, what = "numeric")==FALSE) & inherits(Test.Scores, what = "integer")==FALSE){warning("Test.Scores should be a vector of numeric values.")}  
    
  all_percentiles <- NULL
  all_perc_total <- matrix(NA, nrow = length(temp$Pred.Means))
  
  # Normality not assumed
  if (Object$Empirical.Dist.Delta==TRUE){
  for (i in 1: length(Test.Scores)){
      score_here <- Test.Scores[i]
      delta_here <- (score_here - temp$Pred.Means) / temp$SD.Resid
      
      pc_here_all <- NULL
      for (k in 1: length(delta_here)){
        Percentiles <- data.frame(cbind(seq(from=0, to=1, by=.001)*100, 
                                        Object$Percentiles.Delta, delta_here[k], 
                                        abs(Object$Percentiles.Delta-delta_here[k])))
        Sorted <- Percentiles[order(Percentiles[,4]),]
        Percentile_here <- Sorted[1,1]
        pc_here_all <- c(pc_here_all, Percentile_here)
      }
      all_percentiles <- data.frame(pc_here_all, row.names = NULL)
      all_perc_total <- cbind(all_perc_total, all_percentiles)
    }
    all_perc_total <- all_perc_total[,-1]
    names(all_perc_total) <- Test.Scores
    Norm.Table <- cbind(temp, round(all_perc_total, digits = Digits.Percentile))
    }
    
  # If normality assumed
  if (Object$Empirical.Dist.Delta==FALSE){
    for (i in 1: length(Test.Scores)){
      score_here <- Test.Scores[i]
      delta_here <- (score_here - temp$Pred.Means) / temp$SD.Resid
      pc_here_all <- round(pnorm(c(delta_here), mean=0, sd=1, lower.tail=TRUE)*100, digits = Digits.Percentile) 
      all_percentiles <- data.frame(pc_here_all, row.names = NULL)
      all_perc_total <- cbind(all_perc_total, all_percentiles)
    }
    all_perc_total <- all_perc_total[,-1]
    names(all_perc_total) <- Test.Scores
    Norm.Table <- cbind(temp, all_perc_total)
  }
  
  # finish table
  Norm.Table$Pred.Means <- round(Norm.Table$Pred.Means, digits = Digits)
  Norm.Table$SD.Resid <- round(Norm.Table$SD.Resid, digits = Digits)
  names(Norm.Table)[grep(names(Norm.Table), pattern = "SD.Resid")] <- "Res.Std.Err"
  row.names(Norm.Table) <- NULL
  Assume.Homoscedasticity <- ifelse(Object$Group.Specific.SD.Resid==FALSE, yes = TRUE, no = FALSE)
  Assume.Normality <- ifelse(Object$Empirical.Dist.Delta==TRUE, yes=FALSE, no = TRUE)
  
  fit<-list(Norm.Table=Norm.Table, 
            Group.Specific.SD.Resid=Object$Group.Specific.SD.Resid,
            Empirical.Dist.Delta=Object$Empirical.Dist.Delta,
            N.Analysis=length(na.exclude(Object$Predicted)),
            Test.Scores=Test.Scores,
            Assume.Homoscedasticity=Assume.Homoscedasticity, 
            Assume.Normality=Assume.Normality, Stage.1.Model=Stage.1.Model, 
            Grid.Norm.Table=Grid.Norm.Table,
            Digits.Percentile=Digits.Percentile, 
            #Digits.Mean.Res.Std.Error=Digits, 
            Call=match.call())
  
  class(fit) <- "Stage.2.NormTable"
  fit
  
}

# Summary function
summary.Stage.2.NormTable <-  function(object, ..., Object){
  
  if (missing(Object)){Object <- object} 
  
  cat("------------------------\n")
  cat("Obtained normative table")
  cat("\n------------------------\n")
  print(Object$Norm.Table)
  cat("\n------------------------------------")
  cat("\nAssumptions made in the computations")
  cat("\n------------------------------------")
  Homo.Assumed <- ifelse(Object$Group.Specific.SD.Resid==FALSE, yes = TRUE, no = FALSE)
  Norm.Assumed <- ifelse(Object$Empirical.Dist.Delta==TRUE, yes=FALSE, no = TRUE)
  
  if (length(Homo.Assumed)==0) {Homo.Assumed <- Object$Assume.Homoscedasticity}  
  if (length(Norm.Assumed)==0) {Norm.Assumed <- Object$Assume.Normality}    
  
  cat("\nHomoscedasticity assumed?", Homo.Assumed)
  cat("\nNormality of standardized residuals assumed?", Norm.Assumed)
}