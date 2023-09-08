Check.Assum <- function(Stage.1.Model){

  # If no assumptions tested in Stage 1, assume that normality and homoscedasticity hold by default
if (Stage.1.Model$HomoNorm$Test.Assumptions==FALSE){Assume.Normality.S2 <- Assume.Homo.S2 <- TRUE}
# Alternative: use Levene/Breusch-pagan and Shapiro-Wilk tests
if (Stage.1.Model$HomoNorm$Test.Assumptions==TRUE){

    # Check homoscedasticity 
            # Only conduct checks if there are predictors included in model, not relevant for intercept-only model
    if (Stage.1.Model$HomoNorm$Num.Preds > 1){
    if (Stage.1.Model$HomoNorm$Contains.Numeric.Pred==FALSE){  # if no continuous predictors, use levene test homo
    ifelse(test = Stage.1.Model$HomoNorm$Levene$`Pr(>F)`[1] <= Stage.1.Model$HomoNorm$Alpha.Levene, yes = Assume.Homo.S2<-FALSE, 
           no = Assume.Homo.S2<-TRUE)} 
    if (Stage.1.Model$HomoNorm$Contains.Numeric.Pred==TRUE){  # if continuous predictors, use Breusch-Pagan test homo
      ifelse(test = Stage.1.Model$HomoNorm$Breusch.Pagan$p.value <= Stage.1.Model$HomoNorm$Alpha.BP, yes = Assume.Homo.S2<-FALSE, 
             no = Assume.Homo.S2<-TRUE)}
    }
        # If no predictors included in model, irrelevant
  if (Stage.1.Model$HomoNorm$Num.Preds ==1){
    Assume.Homo.S2<-TRUE #LS 
  }
  
  
  # Check normality 
  if (Assume.Homo.S2==TRUE){   # for delta computed under assumption homoscedasticity
    ifelse(test = Stage.1.Model$HomoNorm$Shapiro.Wilk$p.value <= Stage.1.Model$HomoNorm$Alpha.Shapiro, yes = Assume.Normality.S2<-FALSE,
           no = Assume.Normality.S2<-TRUE)
  }
  if (Assume.Homo.S2==FALSE){  # for delta computed not assuming homoscedasticity
    ifelse(test = Stage.1.Model$NoHomoNorm$Shapiro.Wilk$p.value <= Stage.1.Model$NoHomoNorm$Alpha.Shapiro, yes = Assume.Normality.S2<-FALSE,
           no = Assume.Normality.S2<-TRUE)
  }

}
  fit <- list(Assume.Homo.S2=Assume.Homo.S2, Assume.Normality.S2=Assume.Normality.S2, 
              Call=match.call())
  class(fit) <- "Check.Assum.S2"
  fit
  
  }
