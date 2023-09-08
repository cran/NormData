Bootstrap.Stage.2.NormTable <- function(Stage.2.NormTable,
     CI=.99, Number.Bootstraps=2000, 
     Seed=123, Rounded=FALSE, Show.Fitted.Boot=FALSE, verbose=TRUE){
  
  ifelse(test = Rounded==TRUE, yes = Digits.Percentile<-0, no = Digits.Percentile<-1)
  
  if (inherits(Stage.2.NormTable, what = "Stage.2.NormTable")==FALSE){
  stop("The specified Stage.2.NormTable should be an object of class Stage.2.NormTable\n")
  }
  
  if (Number.Bootstraps<100){
    message("\n\nNOTE: the number of bootstrap samples is lower than 100. It is recommended to use at least 100 bootstrap samples\n\n")
  }
  
  Dataset <- Stage.2.NormTable$Stage.1.Model$HomoNorm$Dataset
  Model <- formula(Stage.2.NormTable$Stage.1.Model$HomoNorm$Reg.Model)
  Higher.Score.Is.Better <- TRUE 
  Test.Scores <- Stage.2.NormTable$Test.Scores
  Assume.Homoscedasticity=Stage.2.NormTable$Assume.Homoscedasticity  
  Assume.Normality=Stage.2.NormTable$Assume.Normality
  Grid.Norm.Table = Stage.2.NormTable$Grid.Norm.Table

  All.Boot.Tables <- NULL
  if (verbose==TRUE) cat("\nBootstrapping... \n")
  for (i in 1:Number.Bootstraps){
    suppressWarnings({
    if (Number.Bootstraps > 50){
    if (i%%round(Number.Bootstraps/10, digits = 0) == 0){
      if (verbose==TRUE) cat(paste(round((i/Number.Bootstraps)*100, digits = 0), "% done... ", sep=""))
    }}
    
    # Bootstrap samples
    data_here <- Dataset
    N <- dim(data_here)[1]
    set.seed(Seed*i); Data_Boot <- data_here[sample(x = 1:N, size = N, replace = TRUE),]

    # If homoscedasticity is valid
    if (Assume.Homoscedasticity==TRUE){
    Model_Boot <- Stage.1(Dataset = Data_Boot, Model = Model, Alpha.Homosc = 0) # always accept homoscedasticity
    }
    
    # If homoscedasticity is invalid and model contains no numeric predictor in the mean structure, use step function of std resid
    if (Assume.Homoscedasticity==FALSE & Stage.2.NormTable$Stage.1.Model$NoHomoNoNorm$Contains.Numeric.Pred==FALSE){
      Model_Boot <- Stage.1(Dataset = Data_Boot, Model = Model, 
                            Alpha.Homosc = 1) # always reject homoscedasticity
    }
    
    # If homoscedasticity is invalid and model contains numeric predictor in the mean structure, use polynomial var function
    if (Assume.Homoscedasticity==FALSE & Stage.2.NormTable$Stage.1.Model$NoHomoNoNorm$Contains.Numeric.Pred==TRUE){
      Model_Boot <- Stage.1(Dataset = Data_Boot, Model = Model, Order.Poly.Var = Stage.2.NormTable$Stage.1.Model$Order.Poly.Var,
        Alpha.Homosc = 1) # always reject homoscedasticity
    }
    
    
    if (Show.Fitted.Boot==TRUE){
      message(paste0("\n\n*** Bootstrap sample ", i, " ***\n\n"))
      summary(Model_Boot)}
    
    Norm_Table_Boot <- try(Stage.2.NormTable(Stage.1.Model = Model_Boot,  
                                         Rounded=FALSE, # LS FALSE!!!
                                         Test.Scores = Test.Scores,
                                         Assume.Homoscedasticity=Assume.Homoscedasticity, 
                                         Assume.Normality=Assume.Normality,
                                         Grid.Norm.Table = Grid.Norm.Table)$Norm.Table, silent = TRUE)
    })
     covars <- dim(Grid.Norm.Table)[2]
    All.Boot.Tables[[i]] <- Norm_Table_Boot[,-c(1:c(covars+2))]
    
  }
  NormTable.With.CI <- NormTable.With.CI.Min <- NormTable.With.CI.Max <- Norm_Table_Boot[,-c(1:c(covars+2))]
  Cols.Normtable <- colnames(NormTable.With.CI)
  
  # Fit on regular dataset to get point estimates
  Model_Dataset <- Stage.1(Dataset = Dataset, Model = Model)
  
  # Point estimate 
  Stage.2.NormTable.For.PE <- Stage.2.NormTable(Stage.1.Model=Stage.2.NormTable$Stage.1.Model,
     Assume.Homoscedasticity=Stage.2.NormTable$Assume.Homoscedasticity, 
     Assume.Normality=Stage.2.NormTable$Assume.Normality, 
     Grid.Norm.Table=Stage.2.NormTable$Grid.Norm.Table, 
     Test.Scores=Stage.2.NormTable$Test.Scores, Digits=6, 
     Rounded=Rounded)
  
  Norm_Table_Point_Estimates <- Stage.2.NormTable.For.PE$Norm.Table[,-c(1:c(covars+2))]
  Norm_Table_Headers <- Stage.2.NormTable.For.PE$Norm.Table
  heading <- Norm_Table_Headers[,1:c(covars+2)]
  
  # Compute CIs
  if (verbose==TRUE) cat("\n\nCompute CI's... \n")
  
  count <- 0
  count.pc <- 1
  
  for (a in 1: dim(All.Boot.Tables[[1]])[1]){
    a_hier <- a

    for (b in 1: dim(All.Boot.Tables[[1]])[2]){
      b_hier <- b
      count <- count + 1
      
      combins <- dim(All.Boot.Tables[[1]])[1] * dim(All.Boot.Tables[[1]])[2]

      pc.done <- seq(from=.1, to=1, by=.1) 
       if (((count%%(round(combins/10, digits = 0)) == 0)|(count==combins)) & (count!=combins)){
         
         if (verbose==TRUE) cat(paste((pc.done[count.pc]*100)), "% done... ", sep="")
         count.pc <- count.pc+1
       }
      
      quantiles <- round(quantile(data.frame(lapply(All.Boot.Tables, "[", a_hier, b_hier)), 
                                  probs = c((1-CI)/2, 1-(1-CI)/2), na.rm=TRUE), digits = Digits.Percentile)
      
      Min.Max.quantiles <- round(quantile(data.frame(lapply(All.Boot.Tables, "[", a_hier, b_hier)), 
                                  probs = c((1-CI)/2, 1-(1-CI)/2), na.rm=TRUE), digits = Digits.Percentile)
      point_estim <- round(Norm_Table_Point_Estimates[a_hier, b_hier], digits = Digits.Percentile)
      
      result <- paste(point_estim, " [", quantiles[1], ", ", quantiles[2], "]", sep="")
      
      NormTable.With.CI[a_hier, b_hier] <- result
      
      NormTable.With.CI.Min[a_hier, b_hier] <- Min.Max.quantiles[1]
      NormTable.With.CI.Max[a_hier, b_hier] <- Min.Max.quantiles[2]
      
    }
    
  }
  if (verbose==TRUE) cat("\n\nAll done.\n")
  NormTable.With.CI <- data.frame(heading, NormTable.With.CI)
  colnames(NormTable.With.CI)[-c(1:c(covars+2))] <- Cols.Normtable
  
  NormTable.With.CI.Min <- data.frame(heading, NormTable.With.CI.Min)
  colnames(NormTable.With.CI.Min)[-c(1:c(covars+2))] <- Cols.Normtable
  NormTable.With.CI.Max <- data.frame(heading, NormTable.With.CI.Max)
  colnames(NormTable.With.CI.Max)[-c(1:c(covars+2))] <- Cols.Normtable
  
  fit <- list(Norm.Table=NormTable.With.CI, CI=CI, 
              Assume.Homoscedasticity=Assume.Homoscedasticity, 
              Assume.Normality=Assume.Normality, 
   NormTable.With.CI.Min=NormTable.With.CI.Min, NormTable.With.CI.Max=NormTable.With.CI.Max, 
              Call=match.call())
  class(fit) <- "Stage.2.NormTable"
  fit
}

