Stage.1 <- function(Dataset, Model,  
                    Order.Poly.Var=3,
                    Alpha=0.05, Alpha.Homosc=0.05, Alpha.Norm = .05, 
                    Assume.Homoscedasticity=NULL,     
                    Test.Assumptions=TRUE, 
                    Outlier.Cut.Off=4, 
                    Show.VIF=TRUE, GVIF.Threshold=10, 
                    Sandwich.Type="HC0", Alpha.CI.Group.Spec.SD.Resid=0.01){
  
  Alpha.Levene <- Alpha.BP <- Alpha.Homosc
  Alpha.Shapiro <- Alpha.Norm
  
  if (is.null(Assume.Homoscedasticity)==FALSE){
    
    if (Assume.Homoscedasticity==TRUE){Alpha.Levene <- Alpha.BP <- Alpha.Homosc <- 0} 
    if (Assume.Homoscedasticity==FALSE){Alpha.Levene <- Alpha.BP <- Alpha.Homosc <- 1}
      }
  
  Order.Polynomial.Var <- Order.Poly.Var
  Higher.Score.Is.Better=TRUE  # LS

  # Default. Homoscedasticity and Normality assumed
  HomoNorm <- .FitNormModel(Dataset=Dataset, Model=Model, Order.Polynomial.Var=Order.Polynomial.Var,
                           Alpha.Reg=Alpha, Alpha.Levene=Alpha.Levene, Alpha.BP=Alpha.BP, Alpha.Shapiro=Alpha.Shapiro, 
                           Higher.Score.Is.Better=Higher.Score.Is.Better, Test.Assumptions=Test.Assumptions, 
                           Outlier.Cut.Off=Outlier.Cut.Off, 
                           Show.VIF=Show.VIF, GVIF.Threshold=GVIF.Threshold, 
                           Group.Specific.SD.Resid=FALSE, 
                           Empirical.Dist.Delta=FALSE, 
                           Alpha.CI.Group.Spec.SD.Resid=Alpha.CI.Group.Spec.SD.Resid)
  
  # No Homoscedasticity, Normality 
  NoHomoNorm <- .FitNormModel(Dataset=Dataset, Model=Model, Order.Polynomial.Var=Order.Polynomial.Var,
                             Alpha.Reg=Alpha, Alpha.Levene=Alpha.Levene, Alpha.BP=Alpha.BP, Alpha.Shapiro=Alpha.Shapiro, 
                             Higher.Score.Is.Better=Higher.Score.Is.Better, Test.Assumptions=Test.Assumptions, 
                             Outlier.Cut.Off=Outlier.Cut.Off, 
                             Show.VIF=Show.VIF, GVIF.Threshold=GVIF.Threshold, 
                             Group.Specific.SD.Resid=TRUE, 
                             Empirical.Dist.Delta=FALSE, 
                             Alpha.CI.Group.Spec.SD.Resid=Alpha.CI.Group.Spec.SD.Resid)
  
  # Homoscedasticity, No Normality 
  HomoNoNorm <- .FitNormModel(Dataset=Dataset, Model=Model, Order.Polynomial.Var=Order.Polynomial.Var,
                             Alpha.Reg=Alpha, Alpha.Levene=Alpha.Levene, Alpha.BP=Alpha.BP,Alpha.Shapiro=Alpha.Shapiro, 
                             Higher.Score.Is.Better=Higher.Score.Is.Better, Test.Assumptions=Test.Assumptions, 
                            Outlier.Cut.Off=Outlier.Cut.Off, 
                             Show.VIF=Show.VIF, GVIF.Threshold=GVIF.Threshold, 
                             Group.Specific.SD.Resid=FALSE, 
                             Empirical.Dist.Delta=TRUE, 
                             Alpha.CI.Group.Spec.SD.Resid=Alpha.CI.Group.Spec.SD.Resid)
  
  # NoHomoscedasticity, No Normality 
  NoHomoNoNorm <- .FitNormModel(Dataset=Dataset, Model=Model, Order.Polynomial.Var=Order.Polynomial.Var, 
                               Alpha.Reg=Alpha, Alpha.Levene=Alpha.Levene, Alpha.BP=Alpha.BP,Alpha.Shapiro=Alpha.Shapiro, 
                               Higher.Score.Is.Better=Higher.Score.Is.Better, Test.Assumptions=Test.Assumptions, 
                               Outlier.Cut.Off=Outlier.Cut.Off, 
                               Show.VIF=Show.VIF, GVIF.Threshold=GVIF.Threshold, 
                               Group.Specific.SD.Resid=TRUE, 
                               Empirical.Dist.Delta=TRUE, 
                               Alpha.CI.Group.Spec.SD.Resid=Alpha.CI.Group.Spec.SD.Resid)
  
   # give if warning if one of the independent variables is possible not properly coded as numeric
   if (NoHomoNoNorm$Warning.Coding==1){
   warning("Some of the independent variables are coded as a numeric variables but the number of predicted test scores \nis low (less than 15). Make sure that all independent variables are properly coded!")
   }
   
  fit <- list(HomoNorm=HomoNorm, NoHomoNorm=NoHomoNorm, HomoNoNorm=HomoNoNorm, NoHomoNoNorm=NoHomoNoNorm,
              Predicted=NoHomoNoNorm$Predicted, Sandwich.Type=Sandwich.Type, Order.Poly.Var=Order.Poly.Var,
              Call=match.call())
  class(fit) <- "Stage.1"
  fit
  
}


