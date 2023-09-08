Sandwich <- function(Stage.1.Model, Type="HC0"){
  if (inherits(Stage.1.Model, what = "Stage.1")==FALSE){
    stop("Fitted.Model should be an object of class Stage.1\n\n")
  }

model <- Stage.1.Model$HomoNorm$Reg.Model
robust <- lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type=Type))
not.robust <- coef(summary(model))

old.options <- options()
on.exit(options(old.options))
options(digits = 16)
Main.Results <- (round(robust, digits=8))
Sig <- Sig_Level <- Main.Results[,4] <= Stage.1.Model$HomoNorm$Alpha.Reg
Sig_Level[Sig==TRUE] <- as.character("*")
Sig_Level[Sig!=TRUE] <- c(" ")
Main.Results <- data.frame(cbind(Main.Results, as.character(Sig_Level)))
names(Main.Results) <- c("Estimate", "Std. Error",  "T* value", "Pr(>|T*|)", " ")
Main.Results[,1] <- round(as.numeric(as.character(Main.Results[,1])), digits = 6)
Main.Results[,2] <- round(as.numeric(as.character(Main.Results[,2])), digits = 6)
Main.Results[,3] <- round(as.numeric(as.character(Main.Results[,3])), digits = 6)
Main.Results[,4] <- round(as.numeric(as.character(Main.Results[,4])), digits = 6)

fit <- list(Sandwich=Main.Results, Alpha=Stage.1.Model$HomoNorm$Alpha.Reg, Call=match.call())
class(fit) <- "Sandwich"
fit

}

summary.Sandwich <- function(object, ..., Object){
  if (missing(Object)){Object <- object}
  old.options <- options()
  on.exit(options(old.options))
  options(digits = 8)
  print(Object$Sandwich)
  cat("\n*: p-value <=", Object$Alpha)
  }
