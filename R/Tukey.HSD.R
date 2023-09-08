Tukey.HSD <- function(Stage.1.Model, ...){
  if (inherits(Stage.1.Model, what = "Stage.1")==FALSE){stop("A fitted object of class Stage.1 should be provided.\n")}
  Tukey <- TukeyHSD(aov(Stage.1.Model$NoHomoNoNorm$Reg.Model))
  
  fit <- list(Tukey.HSD=Tukey, Call=match.call())
  class(fit) <- "Tukey.HSD"
  fit
  }

summary.Tukey.HSD <- function(object, ..., Object){
  if (missing(Object)){Object <- object} 
  print(Object$Tukey.HSD, digits=6)
}


plot.Tukey.HSD <- function(x, ...){
  plot(x$Tukey.HSD, las=1, ...)
}
