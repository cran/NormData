Stage.2.AutoScore <- function(Stage.1.Model, Assume.Homoscedasticity, 
  Assume.Normality, Folder, NameFile="NormSheet.xlsx", verbose=TRUE){ 

Fitted.Model <- Stage.1.Model

# If the user did not specify a Folder in the function call, ask here for entering a 
# folder. Instead of specifying a folder, it is also possible to enter WD. Then the file is written to the working directory 
# If nothing is entered by the user, the function stops
if (missing(Folder)){
  message(paste("Please specify the folder where the spreadsheet file should be saved, or \nenter WD if you want to write the file to the current working directory \n(", getwd(), "):", sep=""))
  Folder.Input <- readline(prompt = "")
  if (Folder.Input==""){stop("Folder is not provided, the spreadsheet file is not written")}
  ifelse(Folder.Input=="WD", yes = Folder <- getwd(), no = Folder<-Folder.Input)
  }

if (inherits(Fitted.Model, what="Stage.1")==FALSE){stop("The argument Stage.1.Model=... should specify an object of class Stage.1\n")}
  # check that last character of specified folder is /. If missing, add it
last <- substr(Folder, nchar(Folder), nchar(Folder)) 
if (last!="/"){Folder <- paste(Folder, "/", sep="")}

# Check whether dir exists and create one if it does not exist
if (!dir.exists(Folder)){
  dir.create(Folder)
}  

# Determine which assumptions are used to derive the normative data (homoseced and/or normality)
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

old.options <- options()
on.exit(options(old.options))
options(digits = 8, scipen=999)    

# Homoscedasticity and normality assumed
if (Group.Specific.SD.Resid==FALSE & Empirical.Dist.Delta==FALSE){
Fitted.Model <- Stage.1.Model
Fitted.Model <- Fitted.Model$HomoNorm
Fitted.Reg.Model <- Fitted.Model$Reg.Model
Resid.Std.Errors <- data.frame(Fitted.Model$SD.Resid)
Resid.Std.Errors <- cbind(Resid.Std.Errors)
names(Resid.Std.Errors) <- c("Residual Std.Error")
Resid.Std.Errors <- round(Resid.Std.Errors, digits=6)
Resid.Std.Errors$`Residual variance` <- Resid.Std.Errors$`Residual Std.Error`**2 
Resid.Std.Errors <- Resid.Std.Errors[,c(2, 1)]
Resid.Std.Errors <- Resid.Std.Errors[-2]
Percentiles.Delta <- data.frame(c(0, seq(from=0+.001, to=1-.001, by=.001), 1)*100,
                                Fitted.Model$Percentiles.Delta)
names(Percentiles.Delta) <- c("Percentile.Rank", "Delta")
}

# No homoscedasticity, normality assumed
if (Group.Specific.SD.Resid==TRUE & Empirical.Dist.Delta==FALSE){
Fitted.Model <- Stage.1.Model
Fitted.Model <- Fitted.Model$NoHomoNorm
Fitted.Reg.Model <- Fitted.Model$Reg.Model
  # If there are no numeric predictors of the model: sd(residual) per group
if (Fitted.Model$Contains.Numeric.Pred==FALSE){
  temp0 <- Fitted.Model$Group.Spec.SD.Resid.Values
  temp0$`Residual Variance` <- temp0$`Residual Std.Error`**2
  cols.here <- dim(temp0)[2]
  temp0 <- temp0[,c(1, cols.here, 2:(cols.here-1))]
  temp0[,c(1:3)] <- round(temp0[,c(1:3)], digits = 6)
  Resid.Std.Errors <- temp0[,c(1:2)]
}

# If there are  numeric predictors of the model: sd(residual) as continuous function
if (Fitted.Model$Contains.Numeric.Pred==TRUE){
  Resid.Std.Errors <- Fitted.Model$Fit.Var.Function
  }

Percentiles.Delta <- data.frame(c(0, seq(from=0+.001, to=1-.001, by=.001), 1)*100,
                                Fitted.Model$Percentiles.Delta)
names(Percentiles.Delta) <- c("Percentile.Rank", "Delta")
}

# Homoscedasticity assumed, normality not assumed
if (Group.Specific.SD.Resid==FALSE & Empirical.Dist.Delta==TRUE){
Fitted.Model <- Stage.1.Model
Fitted.Model <- Fitted.Model$HomoNoNorm
Fitted.Reg.Model <- Fitted.Model$Reg.Model
Resid.Std.Errors <- data.frame(Fitted.Model$SD.Resid)
Resid.Std.Errors <- cbind(Resid.Std.Errors)
names(Resid.Std.Errors) <- c("Residual Std.Error")
Resid.Std.Errors <- round(Resid.Std.Errors, digits=6)
Resid.Std.Errors$`Residual variance` <- Resid.Std.Errors$`Residual Std.Error`**2 
Resid.Std.Errors <- Resid.Std.Errors[,c(2, 1)]
Resid.Std.Errors <- Resid.Std.Errors[-2]

Percentiles.Delta <- data.frame(seq(from=0, to=1, by=.001)*100, Fitted.Model$Percentiles.Delta)
names(Percentiles.Delta) <- c("Percentile.Rank", "Delta")
}

# Homoscedasticity not assumed, normality not assumed
if (Group.Specific.SD.Resid==TRUE & Empirical.Dist.Delta==TRUE){
Fitted.Model <- Stage.1.Model
Fitted.Model <- Fitted.Model$NoHomoNoNorm
Fitted.Reg.Model <- Fitted.Model$Reg.Model
# If there are no numeric predictors of the model: sd(residual) per group
if (Fitted.Model$Contains.Numeric.Pred==FALSE){
  temp0 <- Fitted.Model$Group.Spec.SD.Resid.Values
  temp0$`Residual Variance` <- temp0$`Residual Std.Error`**2
  cols.here <- dim(temp0)[2]
  temp0 <- temp0[,c(1, cols.here, 2:(cols.here-1))]
  temp0[,c(1:3)] <- round(temp0[,c(1:3)], digits = 6)
  Resid.Std.Errors <- temp0[,c(1:2)]       
  }
# If there are  numeric predictors of the model: sd(residual) as continuous function
if (Fitted.Model$Contains.Numeric.Pred==TRUE){
  Resid.Std.Errors <- Fitted.Model$Fit.Var.Function}
Percentiles.Delta <- data.frame(seq(from=0, to=1, by=.001)*100, Fitted.Model$Percentiles.Delta)
names(Percentiles.Delta) <- c("Percentile.Rank", "Delta")
}

# If there are numeric predictors of the model: sd(residual) as continuous function
if (Fitted.Model$Contains.Numeric.Pred==TRUE){
  labels <- paste("Pred.Y.", 2:100, sep="")
  num.pars <- length(Resid.Std.Errors$coefficients)
  if (num.pars==2){labels_here <- c("(Intercept)", "Pred.Y")}
  if (num.pars>2){labels_here <- c("(Intercept)", "Pred.Y", labels[1:c(num.pars-2)])}
  try(names(Resid.Std.Errors$coefficients) <- labels_here, silent=TRUE)
  Resid.Std.Errors <- Resid.Std.Errors
}

Percentiles.Delta <- Percentiles.Delta[,c(2,1)]

# Assumptions 
Contains.Numeric.Pred <- Fitted.Model$Contains.Numeric.Pred
if (is.null(Contains.Numeric.Pred)){Contains.Numeric.Pred <- FALSE}

res <- c(Group.Specific.SD.Resid, Contains.Numeric.Pred, Empirical.Dist.Delta)
res.labs <- c("Homoscedasticity assumed?", "Variance function used?", "Normality of standardized residuals assumed?")
Assumptions <- data.frame(res.labs, res)
names(Assumptions) <- c("Assumption", "Status")

# If continuous variance function, only keep estimated regression parameters in table
# std error, p values etc are not correct
if (inherits(Resid.Std.Errors, what = "lm")==TRUE){
  Resid.Std.Errors <- data.frame(coef(Resid.Std.Errors))
  Resid.Std.Errors <- data.frame(rownames(Resid.Std.Errors), Resid.Std.Errors)
  names(Resid.Std.Errors) <- c("", "Estimate")
}

# Only keep 2 columns for model mean structure
Fitted.Reg.Model <- 
  data.frame(data.frame(names(coef(Fitted.Reg.Model))), 
             data.frame(Fitted.Reg.Model$coefficients), row.names = NULL)
#Fitted.Reg.Model <- Fitted.Reg.Model[,c(1:2)]
names(Fitted.Reg.Model) <- c("", "Estimate")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Mean.Structure")
openxlsx::addWorksheet(wb, "Residual.Structure")
openxlsx::addWorksheet(wb, "Percentiles.Delta")
#openxlsx::addWorksheet(wb, "Assumptions")
openxlsx::writeData(wb, "Mean.Structure", Fitted.Reg.Model, 
                    startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::writeData(wb, "Residual.Structure", Resid.Std.Errors, startRow = 1, startCol = 1)
openxlsx::writeData(wb, "Percentiles.Delta", Percentiles.Delta, startRow = 1, startCol = 1)
#openxlsx::writeData(wb, "Assumptions", Assumptions, startRow = 1, startCol = 1)
openxlsx::saveWorkbook(wb, file = paste(Folder, NameFile, sep=""), overwrite = TRUE)

if (verbose==TRUE) cat("The file was successfully written.\n\n")
if (verbose==TRUE) cat("--------------------------------------------")
if (verbose==TRUE) cat("\nAssumptions made in writing the output file:")
if (verbose==TRUE) cat("\n--------------------------------------------")
Homo.Assumed <- ifelse(Fitted.Model$Group.Specific.SD.Resid==FALSE, yes = TRUE, no = FALSE)
Norm.Assumed <- ifelse(Fitted.Model$Empirical.Dist.Delta==TRUE, yes=FALSE, no = TRUE)
if (verbose==TRUE) cat("\nHomoscedasticity assumed?", Homo.Assumed)
if (verbose==TRUE) cat("\nNormality of standardized residuals assumed?", Norm.Assumed)

fit<-list(Mean.Structure=Fitted.Model, Residual.Structure=Resid.Std.Errors, Percentiles.Delta=Percentiles.Delta,
          Call=match.call())

class(fit) <- "Stage.2.AutoScore"
}

