.FitNormModel <- function(Dataset, Model, 
  Group.Specific.SD.Resid=FALSE, Order.Polynomial.Var=3, Empirical.Dist.Delta=FALSE, 
  Alpha.Reg=.05, Alpha.Levene=0.05, Alpha.BP=0.05, Alpha.Shapiro=0.05, 
  Higher.Score.Is.Better=TRUE, Test.Assumptions=TRUE, Outlier.Cut.Off=4, 
  Show.VIF=FALSE, GVIF.Threshold=10, Alpha.CI.Group.Spec.SD.Resid=0.01, Seed=123){
  
  Give.Warning.Neg.Variances <- 0
  
  old.options <- options()
  on.exit(options(old.options))
  options(digits = 10, max.print = 100000)
  
  Total_N_Orig_Dataset <- dim(Dataset)[1]  
  row.names(Dataset) <- 1:Total_N_Orig_Dataset # LS for outliers!
  Fit.Temp <- lm(Model, data = Dataset) # Temp Fit, only used to delete missing cases in subsequent analyses
  if (is.null(Fit.Temp$na.action)==TRUE){Dataset.No.Miss <- Dataset}   # If no missing
  if (is.null(Fit.Temp$na.action)==FALSE){Dataset.No.Miss <- Dataset[-as.numeric(Fit.Temp$na.action),]}  # Dataset with observations with missing test score/IV deleted
  Total_N_No_Miss <- dim(Dataset.No.Miss)[1] 
  
  Fit <- lm(Model, data = Dataset.No.Miss)
  Design.Matrix.Mean <- model.matrix(Fit)
  Test.Scores <- Fit$model[,1]
  
  # Make table with variables specified in model (test score and IVs)
  Variable.Classes <- Num.Vals.All <- NULL
  for (i in 1: dim(Fit$model)[2]){
    Variable.Classes <- c(Variable.Classes, class(Fit$model[,i])[1])
    Num.Vals <- length(unique(na.exclude(Fit$model[,i])))
    Num.Vals.All <- c(Num.Vals.All, Num.Vals)
  }
  Variables <- Variables.Orig <- data.frame(data.frame(names(Fit$model)), data.frame(Variable.Classes), 
                                            data.frame(Num.Vals.All))
  Variables$Type <- Variables$Variable.Classes
  try(Variables[Variables$Type=="numeric",]$Type <- "quantitative", silent=TRUE)
  try(Variables[Variables$Type=="integer",]$Type <- "quantitative", silent=TRUE)
  try(Variables[Variables$Type=="factor" & Variables$Num.Vals.All==2,]$Type <- "binary", silent=TRUE)
  try(Variables[Variables$Type=="factor" & Variables$Num.Vals.All>2,]$Type <- "non-binary qualitative", silent=TRUE)
  Variables <- Variables[,-3]
  Variables$Variable.Classes <- gsub("\\b([a-z])", "\\U\\1", Variables$Variable.Classes, perl=TRUE)
  Variables <- Variables[,-2]
  Variables <- data.frame(Variables, Variables.Orig$Variable.Classes)
  names(Variables) <- c("Name variable", "Type variable", "R class")
  
  # Homoscedasticity test
  Breusch.Pagan <- NA #LS
  try(Breusch.Pagan <- lmtest::bptest(Fit, studentize = FALSE), silent = TRUE) 
  Num.Preds <- length(Fit$coefficients)
  Predicted <- round(predict(Fit), digits=6)
  Residuals <- round(resid(Fit), digits = 6)
  Overall.Var.Resid <- summary(Fit)$sigma**2
  Overall.SD.Resid <- summary(Fit)$sigma
  Dataset.Extra <- data.frame(Dataset.No.Miss, Predicted, Residuals)
  
  # If model contains at least one continuous variable   
  # use variance function for SD(residuals) (instead of SR_Res per level of S) 
  # and Breusch-pagan test (instead of level). Check is done here
  Variable.Classes <- NULL
  if (Num.Preds > 1){
  for (i in 2: dim(Fit$model)[2]){
    Variable.Classes <- c(Variable.Classes, class(Fit$model[,i]))
  }
  }
  Check.OK <- ((sum(grepl(pattern = "numeric", x = Variable.Classes))>0)) | ((sum(grepl(pattern = "integer", x = Variable.Classes))>0))
  ifelse(test = Check.OK==TRUE, yes = Use.Variance.Function <- TRUE, no = Use.Variance.Function <- FALSE)
  Contains.Numeric.Pred <- Check.OK  
# Make warning indicator if one of X is numeric but less than 15 predicted test scores. Variables are probably not appropriately coded then
  Warning.Coding <- 0
  if (Contains.Numeric.Pred==TRUE & length(unique(Predicted)) < 15){
  Warning.Coding <- 1
  }
  
  # if model contains at least 1 predictor (i.e., Num.Preds > 1 if no intercept model) 
if (Num.Preds > 1){
  
  ExploreData.Mod <- function(Dataset, Model, Fit, Digits=8){
    old.options <- options()
    on.exit(options(old.options))
    options(digits = Digits, scipen=999)  
    Data <- Dataset
    Data <- data.frame(Data[, (names(Data) %in% all.vars(Model))], Data$Predicted, Data$Residuals)
    Formula <- Model
    Temp.Formula.Pred.Y <- update(Formula, new = Data.Predicted~.)
    Temp.Formula.SD.Res <- update(Formula, new = Data.Residuals~.)
    SD_Res <- function(x){x <- na.exclude(x); sqrt(sum(x**2)/((length(x)-1)))}
    Res.Pred <- data.frame(doBy::summaryBy(formula = Temp.Formula.Pred.Y, data = Data, FUN = c(mean)))
    Cols.For.Labels <- 1:(grep(x = names(Res.Pred), pattern = "mean")-1)  # contains IVs
    Res.Pt0 <- Res.Pred[,Cols.For.Labels]
    if (max(Cols.For.Labels)==1){Res.Pt0 <- data.frame(Res.Pt0); names(Res.Pt0) <- as.character(Formula[[3]])}
    Res.Pt1 <- Res.Pred[,-Cols.For.Labels]
    Res.Pt1 <- round(Res.Pt1, digits = Digits)
    Res.SD.Res <- data.frame(doBy::summaryBy(formula = Temp.Formula.SD.Res, data = Data, FUN = c(SD_Res, length)))
    Cols.For.Labels <- 1:(grep(x = names(Res.SD.Res), pattern = "SD_Res")-1)  # contains IVs
    Res.Pt2 <- Res.SD.Res[,-Cols.For.Labels]
    Res.Pt2[,1] <- round(Res.Pt2[,1], digits = Digits)
    Res.Pts.1.2 <- data.frame(Res.Pt1, Res.Pt2)
    Res <- data.frame(Res.Pts.1.2, Res.Pt0)
    names(Res)[c(1:3)] <- c("Predicted test score", "Residual Std.Error", "Count")
    fit <- list(Results=Res, Call=match.call())
    class(fit) <- "ExploreData"
    fit
  }

  # Make temp table that will contain standardized residuals (Use group-specific SD(residuals) 
  # and overall SD(residuals)), and the delta's based on group-specific SD(residuals) and overall
  temp <- data.frame(cbind(Fit$model[,1], Residuals, Predicted))
  names(temp)[1] <- "Y"
  temp$Group_Specific_SD_Residual <- NA; temp$Delta_Group_Specific_SD_Residual <- NA 
  temp$Overall_SD_Residual <- NA; temp$Delta_Overall_SD_Residual <- NA 
  
  # Compute group-specific SD(residual) and conduct Levene test, if all variables in model are FACTORS
  Group.Spec.SD.Resid.Values.CI <- Group.Spec.SD.Resid.Values.Boot.CI <- NA  # LS
  if (Contains.Numeric.Pred==FALSE){  
    Group.Spec.SD.Resid.Values <- ExploreData.Mod(Dataset = Dataset.Extra, Model = Model, Fit = Fit, Digits = 8)$Res
    CI.Low.Res.Std.Error <- sqrt(((Group.Spec.SD.Resid.Values$Count-1)*(Group.Spec.SD.Resid.Values$`Residual Std.Error`**2))/
                                   (qchisq(p = Alpha.CI.Group.Spec.SD.Resid/2, df = Group.Spec.SD.Resid.Values$Count-1, lower.tail = FALSE)))
    CI.High.Res.Std.Error <- sqrt(((Group.Spec.SD.Resid.Values$Count-1)*(Group.Spec.SD.Resid.Values$`Residual Std.Error`**2))/
                                    (qchisq(p = Alpha.CI.Group.Spec.SD.Resid/2, df = Group.Spec.SD.Resid.Values$Count-1, lower.tail = TRUE)))
    Group.Spec.SD.Resid.Values <- data.frame(Group.Spec.SD.Resid.Values, CI.Low.Res.Std.Error, CI.High.Res.Std.Error)
    names(Group.Spec.SD.Resid.Values)[1:3] <- c("Predicted test score", "Residual Std.Error", "Count")
    
    # Levene test
    Levene <- car::leveneTest(Dataset.Extra$Residuals~as.factor(Dataset.Extra$Predicted))
    
    # look for group-specific SD.Res for given predicted value and add these to the table
    Approx.Equal <- function(Target, Look.For.Value, Allowed.Abs.Diff=0.0001){
      Close.To.Target <- abs(Target-Look.For.Value)<=Allowed.Abs.Diff
      return(Close.To.Target)
    }
    # check for different groups
    for (l in 1: length(Group.Spec.SD.Resid.Values$`Predicted test score`)){
    Select.Here <- Approx.Equal(Target = temp$Predicted, Look.For.Value = Group.Spec.SD.Resid.Values$`Predicted test score`[l])
    temp$Group_Specific_SD_Residual[Select.Here] <- Group.Spec.SD.Resid.Values$`Residual Std.Error`[l]
    }
    
    if (sum(is.na(temp$Group_Specific_SD_Residual))>0){
      warning("\n\nNot all predicted values could be assigned a SD.Resid ...\n\n")
    }
    temp$Delta_Group_Specific_SD_Residual <- temp$Residuals / temp$Group_Specific_SD_Residual
    temp$Overall_SD_Residual <- round(Overall.SD.Resid, digits=6)
    temp$Delta_Overall_SD_Residual <- temp$Residuals / temp$Overall_SD_Residual
    
    # Variance function
    Fit.Var.Function <- NA # variance function only computed if at least one IV in model is numeric
    Design.Matrix.Var <- model.matrix(lm(Residuals**2 ~ as.factor(Predicted)))
    }  # end if contains no numeric
    
  
# use SD function instead of SD(residuals) if at least one IV is NUMERIC
# Delta_SD_Residual_Function is delta computed based on the variance function 
if (Contains.Numeric.Pred==TRUE){
    Levene <- Predicted_Groups <- NA   # Levene not used, Predicted_Groups and Number.Groups.SD.Resid not relevant here
    Fit.Var.Function <- NULL
    temp$Group_Specific_SD_Residual <- NA  # Group-specific is here more prediction-specific
 if (Order.Polynomial.Var>=1){    
 Fit.Var.Function <- lm(data=temp, Residuals**2 ~ poly(Predicted, Order.Polynomial.Var, raw=TRUE))
 }
 if (Order.Polynomial.Var==0){    
      Fit.Var.Function <- lm(data=temp, Residuals**2 ~ 1)
    }
 Design.Matrix.Var <- model.matrix(Fit.Var.Function)
  # Gives warning if var pred function yields negative values
 suppressWarnings(Use.SD <- temp$Group_Specific_SD_Residual <- try(sqrt(predict(object = Fit.Var.Function)), silent = TRUE))
 temp$Delta_Group_Specific_SD_Residual <- temp$Residuals / temp$Group_Specific_SD_Residual
 temp$Overall_SD_Residual <- round(Overall.SD.Resid, digits=6)
 temp$Delta_Overall_SD_Residual <- temp$Residuals / temp$Overall_SD_Residual
 Group.Spec.SD.Resid.Values <- Fit.Var.Function
 
 # Gives warning if var pred function yields negative values
 suppressWarnings({
 if (sum(is.na(sqrt(predict(object = Fit.Var.Function))))>=1){
   Give.Warning.Neg.Variances <- 1}})
 
 }
 
# Make table for output
Table.Obs.Pred.Res.Delta <- temp  
Table.Obs.Pred.Res.Delta <- round(Table.Obs.Pred.Res.Delta[,c(2:7)], digits=6)

# Use overall or group-specific SD(residual) in output list?  
if (Group.Specific.SD.Resid==TRUE){
  SD.Resid <- Group.Spec.SD.Resid.Values
  Delta <- Table.Obs.Pred.Res.Delta$Delta_Group_Specific_SD_Residual
  }
if (Group.Specific.SD.Resid==FALSE){
  SD.Resid <- Overall.SD.Resid
  Delta <- Table.Obs.Pred.Res.Delta$Delta_Overall_SD_Residual
  }

  # use standard normal or empirical distribution of the residuals?
if (Group.Specific.SD.Resid==TRUE){Delta <- Table.Obs.Pred.Res.Delta$Delta_Group_Specific_SD_Residual}  
if (Group.Specific.SD.Resid==FALSE){Delta <- Table.Obs.Pred.Res.Delta$Delta_Overall_SD_Residual}  

if (Empirical.Dist.Delta==TRUE){ # 
  Percentiles.Delta <- quantile(x = Delta, probs = seq(from=0, to=1, by=.001), na.rm = TRUE)
  }  
if (Empirical.Dist.Delta==FALSE){ 
  Percentiles.Delta <- qnorm(seq(from=0, to=1, by=.001), mean=0, sd=1, lower.tail=TRUE)
  Percentiles.Delta[1] <- 
    qnorm(0.000001, mean=0, sd=1, lower.tail=TRUE) # use 0.000001 as lowest because 0 is undefined for normal (i.e., -Inf)
  Percentiles.Delta[length(Percentiles.Delta)] <- 
    qnorm(0.999999, mean=0, sd=1, lower.tail=TRUE) # use 0.999999 as lowest because 1 is undefined for normal (i.e., Inf)
  }  
# Test Normality
if (length(Delta) <= 5000){
set.seed(Seed); Shapiro.Wilk <- shapiro.test(x=Delta)
}

if (length(Delta) > 5000){ # sample needs to be < 5000
set.seed(Seed); Delta_random_sample <- sample(x = Delta, size = 5000, replace = F)  
Shapiro.Wilk <- shapiro.test(x=Delta_random_sample)
message("\n\nNote. N should be <= 5000 to be able to conduct the Shapiro-Wilk test. \nA random sample of N=5000 delta values is drawn (using set.seed(Seed))\nto conduct the Shapiro-Wilk test. \n\n")
}
  
# Cook's distance
D_i <- (cooks.distance(Fit))
Cut_off_D_i <- qf(c(.5), df1=length(coef(Fit)), df2=(length(D_i) - length(coef(Fit))), lower.tail=TRUE)

# VIF 
VIF <- VIF.Trad <- NA # LS   
suppressMessages(try(VIF.Trad <- vif(Fit), silent=TRUE))

# Are all data included in the analysis?
  Notal_N_Used_Model <- length(Fit$fitted.values)
  if (Total_N_Orig_Dataset!=Notal_N_Used_Model){Excluded.Data <- Dataset[as.numeric(Fit$na.action),]}
  if (Total_N_Orig_Dataset!=Notal_N_Used_Model){Included.Data <- Dataset[-as.numeric(Fit$na.action),]}
  if (Total_N_Orig_Dataset==Notal_N_Used_Model){Excluded.Data <- NA; Included.Data <- Dataset}
}  
  

# If intercept-only model  
if (Num.Preds == 1){  
  
  #Hetero.Consist.Errs <- 
  Design.Matrix.Var <- NA
  SD_Res <- function(x){x <- na.exclude(x); sqrt(sum(x**2)/((length(x)-(summary(Fit)$df[1]))))}
    
    Predicted_Groups <- 1  
    Levene <- NA
    Data_temp <- data.frame(na.exclude(cbind(Residuals, Predicted)))
    Count <- dim(Data_temp)[1]
    SD.Resid <- SD_Res(Residuals)
    Delta <- Residuals / SD.Resid
    if (Higher.Score.Is.Better==FALSE){Delta <- -Delta}
    if (Empirical.Dist.Delta==TRUE){
      Percentiles.Delta <- quantile(x = Delta, probs = seq(from=0, to=1, by=.001), na.rm = TRUE)}  
    if (Empirical.Dist.Delta==FALSE){
      Percentiles.Delta <- qnorm(seq(from=0, to=1, by=.001), mean=0, sd=1, lower.tail=TRUE)}  
    
    # Test Normality
    if (length(Delta) <= 5000){
      set.seed(Seed);  Shapiro.Wilk <- shapiro.test(x=Delta)
    }
    
    if (length(Delta) > 5000){ # sample needs to be < 5000
      Delta_random_sample <- sample(x = Delta, size = 5000, replace = F)  
      set.seed(Seed); Shapiro.Wilk <- shapiro.test(x=Delta_random_sample)
      message("\n\nNote. N should be <= 5000 to be able to conduct the Shapiro-Wilk test. \nA random sample of N=5000 delta values is drawn (using set.seed(123))\nto conduct the Shapiro-Wilk test. \n\n")
    }
    
    # Cook's distance
    D_i <- (cooks.distance(Fit))
    Cut_off_D_i <- qf(c(.5), df1=length(coef(Fit))+1, df2=(length(D_i) - length(coef(Fit))+1), lower.tail=TRUE)
    
    # VIF 
    VIF.Trad <- VIF.Original <- VIF <- NA # LS   
    VIF <- VIF.Trad <- NA # LS   
    
    # Are all data included in the analysis?
    Notal_N_Used_Model <- length(Fit$fitted.values)
    if (Total_N_Orig_Dataset!=Notal_N_Used_Model){Excluded.Data <- Dataset[as.numeric(Fit$na.action),]}
    if (Total_N_Orig_Dataset!=Notal_N_Used_Model){Included.Data <- Dataset[-as.numeric(Fit$na.action),]}
    if (Total_N_Orig_Dataset==Notal_N_Used_Model){Excluded.Data <- NA; Included.Data <- Dataset}
    
    # Set some output NA, not relevant for intercept-only model
    Number.Groups.SD.Resid <- Cut.Points <- NA
    Group.Specific.SD.Resid <- FALSE
    Table.Obs.Pred.Res.Delta <- data.frame(Predicted, Residuals)
    Fit.Var.Function <- NA
    Group.Spec.SD.Resid.Values.CI <- Group.Spec.SD.Resid.Values.Boot.CI <- NA 
  }  
  
  if (exists("Group.Spec.SD.Resid.Values")==FALSE){Group.Spec.SD.Resid.Values <-NA}

# table Group.Spec.SD.Resid.Values
if (Contains.Numeric.Pred==FALSE & Num.Preds>1){  
Max.Cols.Group.Spec.SD.Resid.Values <- dim(Group.Spec.SD.Resid.Values)[2]  
Group.Spec.SD.Resid.Values.CI <- Group.Spec.SD.Resid.Values  
Group.Spec.SD.Resid.Values <- 
  Group.Spec.SD.Resid.Values[,-c(Max.Cols.Group.Spec.SD.Resid.Values-1, Max.Cols.Group.Spec.SD.Resid.Values)]  
}  

old.options <- options()
on.exit(options(old.options))
options(digits=15)

fit <- list(Reg.Model=Fit, Predicted=Predicted, Residuals=as.numeric(Residuals),
            SD.Resid=SD.Resid, Delta=Delta, Percentiles.Delta=Percentiles.Delta,
            Levene=Levene, Shapiro.Wilk=Shapiro.Wilk, 
            D_i=D_i, Cut_off_D_i=Cut_off_D_i, VIF=VIF, Dataset=Dataset, Excluded.Data=Excluded.Data, 
            Included.Data=Dataset.No.Miss,
            Alpha.Reg=Alpha.Reg, Alpha.Levene=Alpha.Levene, Alpha.BP=Alpha.BP, Alpha.Shapiro=Alpha.Shapiro, 
            Group.Specific.SD.Resid=Group.Specific.SD.Resid,  
            Empirical.Dist.Delta=Empirical.Dist.Delta, 
            N.Dataset=Total_N_Orig_Dataset, N.Dataset.No.Miss=Total_N_No_Miss, 
            Higher.Score.Is.Better=Higher.Score.Is.Better, Test.Assumptions=Test.Assumptions,
            Outlier.Cut.Off=Outlier.Cut.Off, Num.Preds=Num.Preds, 
            Group.Spec.SD.Resid.Values=Group.Spec.SD.Resid.Values, 
            Show.VIF=Show.VIF, GVIF.Threshold=GVIF.Threshold,
            Table.Obs.Pred.Res.Delta=data.frame(Dataset.No.Miss, Table.Obs.Pred.Res.Delta), 
            VIF.Original=VIF.Trad, Fit.Var.Function=Fit.Var.Function,
            Use.Variance.Function=Use.Variance.Function,
            Contains.Numeric.Pred=Contains.Numeric.Pred, Breusch.Pagan=Breusch.Pagan,
            Variables=Variables, Warning.Coding=Warning.Coding, Give.Warning.Neg.Variances=Give.Warning.Neg.Variances,
         Group.Spec.SD.Resid.Values.CI=Group.Spec.SD.Resid.Values.CI, 
         Design.Matrix.Mean=Design.Matrix.Mean, Design.Matrix.Var=Design.Matrix.Var, 
         Test.Scores=Test.Scores, 
         Call=match.call())
  class(fit) <- "FitNormModel"
  fit
  
}  
