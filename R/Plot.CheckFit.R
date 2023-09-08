plot.CheckFit <- function(x, Color, pch, lty, 
          Width.CI.Lines=.125, Size.symbol = 1, No.Overlap.X.Axis=TRUE,
          xlab, ylab="Test score", main = " ", 
          Legend.text.size=1, Connect.Means, 
          cex.axis=1, cex.main=1.5, cex.lab=1.5, ...){

  if (missing(Color)){
    Color <- palette()
    Color[2:3] <- c("red", "blue")
  }
  if (missing(pch)){pch <- rep(1, times=1000)}
  if (missing(lty)){lty <- rep(1, times=1000)}
  
    if (missing(Connect.Means)){
    # If model contains no numeric predictors, do not connect the means by default
    if (x$Stage.1.Model$HomoNorm$Contains.Numeric.Pred==FALSE){Connect.Means <- FALSE}
    # If model contains numeric predictors, connect the means by default
    if (x$Stage.1.Model$HomoNorm$Contains.Numeric.Pred==TRUE){Connect.Means <- TRUE}
  }
  
  # This function is largely based on the plotMeans function of the RcmdrMisc package authored
  # by John Fox. See https://cran.r-project.org/web/packages/RcmdrMisc/index.html
  Object <- x 
  Data <- Object$Dataset
  Formula <- Object$Model
  CI <- Object$CI
  if (missing(xlab)){
    xlab <- names(Object$Results.Observed)[1]}
  Xlab <- xlab
  Ylab <- ylab
  Dataset <- Data # LS!
  # If no IVs, stop
  if (all.vars(Formula)[2]=="."){
    stop("\n\nNo IVs were specified in the model, so \nno plots showing the mean (CI) of Y as a function of the IVs can be made.")
  }
  
  # make vars
  name_outcome <- all.vars(Formula)[1]
  outcome <- Dataset[,name_outcome]
  predicted <- Dataset[,"Predicted"]
  
  if (length(all.vars(Formula))==2){
    name_AV1 <- all.vars(Formula)[2]
    factor1 <- Dataset[,name_AV1]
    # check whether is factor. 
    if ((class(factor1)!="factor")==TRUE & length(unique(factor1))>10){stop("\n\nIV1 should be a factor!")}
    factor1 <- as.factor(factor1)
    factor2 <- "None"
    factor3 <- "None"
    }
  if (length(all.vars(Formula))==3){
    name_AV1 <- all.vars(Formula)[2]
    name_AV2 <- all.vars(Formula)[3]
    factor1 <- Dataset[,name_AV1]
    factor2 <- Dataset[,name_AV2]
    if ((class(factor1)!="factor")==TRUE & length(unique(factor1))>10){stop("\n\nIV1 should be a factor!")}
    factor1 <- as.factor(factor1)
    factor2 <- Dataset[,name_AV2]
    if ((class(factor2)!="factor")==TRUE & length(unique(factor2))>10){stop("\n\nIV2 should be a factor!")}
    factor2 <- as.factor(factor2)
    factor3 <- "None"
  }
  if (length(all.vars(Formula))==4){
    name_AV1 <- all.vars(Formula)[2]
    name_AV2 <- all.vars(Formula)[3]
    name_AV3 <- all.vars(Formula)[4]
    factor1 <- Dataset[,name_AV1]
    factor2 <- Dataset[,name_AV2]
    factor3 <- Dataset[,name_AV3]
    # check whether is factor. 
    if ((class(factor1)!="factor")==TRUE){stop("\n\nIV1 should be a factor!")}
    factor1 <- as.factor(factor1)
    factor2 <- Dataset[,name_AV2]
    if ((class(factor2)!="factor")==TRUE & length(unique(factor2))>10){stop("\n\nIV2 should be a factor!")}
    factor2 <- as.factor(factor2)
    if ((class(factor3)!="factor")==TRUE & length(unique(factor3))>10){stop("\n\nIV3 should be a factor!")}
    factor3 <- Dataset[,name_AV3]
    factor3 <- as.factor(factor3)
    }
  if (length(all.vars(Formula))>4){
    warning("\nMore than 4 IVs are specified, but the plot function can depict 3 IVs at most.\nOnly the first three specified IVs are used.\n")
  }
  
  error.bars <- "conf.int" # LS!!!
  level <- CI
  legend.text.size <- Legend.text.size
  connect <- Connect.Means
  size.symbol <- Size.symbol
  xlab <- Xlab; ylab <- Ylab
  col <- Color
  
  if (!is.numeric(outcome)) stop("\n\nThe test score must be numeric!\n")
  # If 1 factor specified
  if (factor2[1]=="None" & factor3[1]=="None"){
    valid <- complete.cases(factor1, outcome)
    factor1 <- factor1[valid]
    outcome <- outcome[valid]
    means <- tapply(outcome, factor1, mean)
    means.predicted <- tapply(predicted, factor1, mean)
    sds <- tapply(outcome, factor1, sd)
    ns <- tapply(outcome, factor1, length)
    if (error.bars == "se"){sds <- sds/sqrt(ns)}
    if (error.bars == "conf.int") 
      sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * sds/sqrt(ns)
      sds[is.na(sds)] <- 0
      yrange <- if (error.bars != "none") 
      c(min(means - sds, na.rm = TRUE), max(means + sds, na.rm = TRUE))
    else range(means, na.rm = TRUE)
    levs <- levels(factor1)
    n.levs <- length(levs)
    plot(c(0.5, n.levs+0.5), yrange, type = "n", xlab = xlab, ylab = ylab, axes = FALSE, main = main, cex.main=cex.main, cex.lab=cex.lab, ...)
   
    if (connect==TRUE){points(1:n.levs, means, lwd=2, cex=size.symbol, type = "b")} 
    if (connect==FALSE){points(1:n.levs, means, lwd=2, cex=size.symbol, type = "p")} 
  
    # predicted means
    if (connect==TRUE){points(1:n.levs, lty=2, means.predicted, lwd=2, cex=size.symbol, type = "b", col=col[1], pch=4)} 
    if (connect==FALSE){points(1:n.levs, lty=2, means.predicted, lwd=2, cex=size.symbol, type = "p", col=col[1], pch=4)} 
    box()
    axis(2, cex.axis=cex.axis)
    
    axis(1, at = 1:n.levs, labels = levs, cex.axis=cex.axis)
    
    if (missing(Width.CI.Lines)){Width.CI.Lines <- .125} 
    
    if (error.bars != "none") 
      arrows(1:n.levs, means - sds, 1:n.levs, means + sds, 
             angle = 90, lty = 1, code = 3, length = Width.CI.Lines)
  }

  # If 2 factors specified
  if (factor2[1]!="None" & factor3[1]=="None")  {
    valid <- complete.cases(factor1, factor2, outcome)
    factor1 <- factor1[valid]
    factor2 <- factor2[valid]
    outcome <- outcome[valid]
    
    temp <- data.frame(outcome, factor1, factor2, predicted)    
    means <- tapply(temp$outcome, list(temp$factor1, temp$factor2), mean)
    means.predicted <- tapply(temp$predicted, list(temp$factor1, temp$factor2), mean)
    sds <- tapply(temp$outcome, list(temp$factor1, temp$factor2), sd)
    ns <- tapply(temp$outcome, list(temp$factor1, temp$factor2), length)
      if (error.bars == "se") 
        sds <- sds/sqrt(ns)
      if (error.bars == "conf.int") 
        sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * 
        sds/sqrt(ns)
      sds[is.na(sds)] <- 0
      yrange <- if (error.bars != "none") 
        c(min(means - sds, na.rm = TRUE), max(means + sds, na.rm = TRUE))
      else range(means, na.rm = TRUE)
      levs.1 <- levels(factor1)
      levs.2 <- levels(factor2)
      n.levs.1 <- length(levs.1)
      n.levs.2 <- length(levs.2)
      
      if (missing(Width.CI.Lines)){
        if (n.levs.2==2) {Width.CI.Lines <- .125}
        if (n.levs.2==3) {Width.CI.Lines <- .10}
        if (n.levs.2==4) {Width.CI.Lines <- .075}
        if (n.levs.2>=5) {Width.CI.Lines <- .05}
      } 
      
      plot(c(0.5, n.levs.1 * 1.4)*n.levs.2, yrange, type = "n", xlab = xlab, 
           ylab = ylab, axes = FALSE, main = main, cex.main=cex.main, cex.lab=cex.lab, ...) 
      
      box()
      axis(2, cex.axis=cex.axis)
      axis(1, at = n.levs.2*(1:n.levs.1), labels = levs.1, cex.axis=cex.axis)
      
      
      if (No.Overlap.X.Axis==TRUE){
        Correct <- seq(from=(-.1 * n.levs.2), to=(.1*n.levs.2), length.out = n.levs.2)}
      
      if (No.Overlap.X.Axis!=TRUE){
        Correct <- rep(c(0), times = n.levs.2)
      }
      
      for (i in 1:n.levs.2) {
        points(((1:n.levs.1)*n.levs.2)+Correct[i], means[, i], lwd=2, cex=size.symbol, type = if (connect) 
          "b" 
          else "p", pch = pch[i], 
          col = col[i], lty = lty[i])
        if (error.bars != "none") 
          arrows(((1:n.levs.1)*n.levs.2)+Correct[i], means[, i] - sds[, i], ((1:n.levs.1)*n.levs.2)+Correct[i], 
                 means[, i] + sds[, i], angle = 90, code = 3, 
                 col = col[i], lty = lty[i], length = Width.CI.Lines)
      }
      
      # predicted means
      for (i in 1:n.levs.2) {
        points(((1:n.levs.1)*n.levs.2)+Correct[i], means.predicted[, i], lwd=2, cex=size.symbol, type = if (connect) 
          "b" 
          else "p",  
          col=col[i],
          lty = 2, pch=4)
      }
      
      x.posn <- n.levs.1 * 1.1
      y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3, 4)])
      legend("topright", levs.2, #pch = pch, 
             col = col, cex = legend.text.size,  
             lty = lty, lwd=2)
  }
  
  # If 3 IV's specified
  if (factor2[1]!="None" & factor3[1]!="None")  {
    valid <- complete.cases(factor1, factor2, factor3, outcome)
    factor1 <- factor1[valid]
    factor2 <- factor2[valid]
    factor3 <- factor3[valid]
    outcome <- outcome[valid]
    
    # loop over levels of factor3
    for (s in 1: length(unique(factor3))){
      factor3_here <- unique(factor3)[s]
    
    temp <- data.frame(outcome, factor1, factor2,factor3, predicted)    
    temp <- temp[temp$factor3==factor3_here,]
    means <- tapply(temp$outcome, list(temp$factor1, temp$factor2), mean)
    means.predicted <- tapply(temp$predicted, list(temp$factor1, temp$factor2), mean)
    sds <- tapply(temp$outcome, list(temp$factor1, temp$factor2), sd)
    ns <- tapply(temp$outcome, list(temp$factor1, temp$factor2), length)
    if (error.bars == "se") 
      sds <- sds/sqrt(ns)
    if (error.bars == "conf.int") 
      sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * 
      sds/sqrt(ns)
    sds[is.na(sds)] <- 0
    yrange <- if (error.bars != "none") 
      c(min(means - sds, na.rm = TRUE), max(means + sds, na.rm = TRUE))
    else range(means, na.rm = TRUE)
    levs.1 <- levels(factor1)
    levs.2 <- levels(factor2)
    n.levs.1 <- length(levs.1)
    n.levs.2 <- length(levs.2)

   if (missing(Width.CI.Lines)){
     if (n.levs.2==2) {Width.CI.Lines <- .125}
     if (n.levs.2==3) {Width.CI.Lines <- .10}
     if (n.levs.2==4) {Width.CI.Lines <- .075}
     if (n.levs.2>=5) {Width.CI.Lines <- .05}
     } 

    plot(c(0.5, n.levs.1 * 1.4)*n.levs.2, yrange, type = "n", xlab = xlab, 
         ylab = ylab, axes = FALSE, cex.main=cex.main, cex.lab=cex.lab, main = as.character(factor3_here), ...) 
    
    box()
    axis(2, cex.axis=cex.axis)
    axis(1, at = n.levs.2*(1:n.levs.1), labels = levs.1, cex.axis=cex.axis)
    
    
    if (No.Overlap.X.Axis==TRUE){
    Correct <- seq(from=(-.1 * n.levs.2), to=(.1*n.levs.2), length.out = n.levs.2)}
    
    if (No.Overlap.X.Axis!=TRUE){
      Correct <- rep(c(0), times = n.levs.2)
      }
    
    for (i in 1:n.levs.2) {
      points(((1:n.levs.1)*n.levs.2)+Correct[i], means[, i], lwd=2, cex=size.symbol, type = if (connect) 
        "b" 
        else "p", pch = pch[i], 
        col = col[i], lty = lty[i])
      if (error.bars != "none") 
        arrows(((1:n.levs.1)*n.levs.2)+Correct[i], means[, i] - sds[, i], ((1:n.levs.1)*n.levs.2)+Correct[i], 
               means[, i] + sds[, i], angle = 90, code = 3, 
               col = col[i], lty = lty[i], length = Width.CI.Lines)
    }
    
    # predicted means
    for (i in 1:n.levs.2) {
      points(((1:n.levs.1)*n.levs.2)+Correct[i], means.predicted[, i], lwd=2, cex=size.symbol, type = if (connect) 
        "b" 
        else "p", pch = 4, 
        col=col[i],
        lty = 2)
      }
    
    x.posn <- n.levs.1 * 1.1
    y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3, 4)])
    legend("topright", levs.2, col = col, cex = legend.text.size,  
           lty = lty, lwd=2)
  }
  }
  
   invisible(NULL)
   
   # Give note if saturated model is fitted
   if (x$Saturated==TRUE){
     message("Note. The predicted and observed means in the plot \nare identical because a saturated model is used.")
   }
}
