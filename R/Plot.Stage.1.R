plot.Stage.1 <- function(x, Homoscedasticity=TRUE, Normality=TRUE, Outliers=TRUE, 
    Assume.Homoscedasticity, Add.Jitter=0, Seed=123, Confidence.QQ.Normality=.99, Plots.Together=TRUE, 
    Y.Lim.ResVarFunction, Group.Spec.Densities.Delta=FALSE, Main.Homosced.1,
    Main.Homosced.2, Main.Norm.1, Main.Norm.2, Main.Norm.3, Main.Outliers, 
    cex.axis.homo=1, cex.main.homo=1, cex.lab.homo=1, 
    cex.axis.norm=1.6, cex.main.norm=1.5, cex.lab.norm=1.5,  
    cex.axis.outl=1, cex.main.outl=1, cex.lab.outl=1, Color="red", 
    Loess.Span=0.75, verbose=TRUE, ...){
  
  # Slightly modified version from qqPlot function of car package of J. Fox 
  qqPlotMod <- function(x, distribution = "norm", groups, layout, ylim = range(x, 
            na.rm = TRUE), ylab = deparse(substitute(x)), xlab = paste(distribution, 
            "quantiles"), glab = deparse(substitute(groups)), main = NULL, 
                         las = par("las"), envelope = TRUE, col = carPalette()[1], 
                         col.lines = carPalette()[2], lwd = 2, pch = 1, cex = par("cex"), 
                         line = c("quartiles", "robust", "none"), id = TRUE, grid = TRUE, 
                         cex.axis=cex.axis.norm, cex.main=cex.main.norm, cex.lab=cex.lab.norm, ...) 
  {
    carPalette <- showLabels1 <- mfrow <- qqPlot.default <- cex.axis <- NULL
    
    applyDefaults <- function(args, defaults, type = "") 
    {
      if (isFALSE(args)) 
        return(FALSE)
      names <- names(args)
      names <- names[names != ""]
      if (!isTRUE(args) && !is.null(args) && length(names) != length(args)) 
        warning("unnamed ", type, " arguments, will be ignored")
      if (isTRUE(args) || is.null(names)) 
        defaults
      else defaults[names] <- args[names]
      as.list(defaults)
    }
    
    showLabels <- function(x, y, labels = NULL, method = "identify", n = length(x), 
                           cex = 1, col = carPalette()[1], location = c("lr", "ab", 
                                                                        "avoid"), ...) 
    {
      location <- match.arg(location)
      res <- NULL
      method <- if (is.list(method)) 
        method
      else list(method)
      for (meth in method) {
        if (length(meth) == 1 && is.character(meth) && meth == 
            "none") 
          next
        res <- c(res, showLabels1(x, y, labels, meth, n, cex, 
                                  col, location, ...))
      }
      return(if (is.null(res)) invisible(res) else res)
    }
    
    if (!missing(groups)) {
      if (isTRUE(id)) 
        id <- list(n = 2)
      if (is.null(id$labels)) 
        id$labels <- seq(along = x)
      grps <- levels(as.factor(groups))
      if (missing(layout)) 
        layout <- mfrow(length(grps), max.plots = 12)
      if (prod(layout) < length(grps)) 
        stop("layout cannot accomodate ", length(grps), " plots")
      oldpar <- par(mfrow = layout)
      on.exit(par(oldpar))
      for (group in grps) {
        id.gr <- id
        if (!isFALSE(id)) 
          id.gr$labels <- id$labels[groups == group]
        qqPlot.default(x[groups == group], distribution = distribution, 
                       ylim = ylim, ylab = ylab, xlab = xlab, main = paste(glab, 
                                                                           "=", group), las = las, envelope = envelope, 
                       col = col, col.lines = col.lines, pch = pch, 
                       cex = cex, line = line, id = id.gr, grid = grid, 
                       ...)
      }
      return(invisible(NULL))
    }
    if (!is.list(envelope) && length(envelope == 1) && is.numeric(envelope)) {
      envelope <- list(level = envelope)
    }
    if (!isFALSE(envelope)) {
      envelope <- applyDefaults(envelope, defaults = list(level = 0.95, 
                                                          style = "filled", col = col.lines, alpha = 0.15, 
                                                          border = TRUE))
      style <- match.arg(envelope$style, c("filled", "lines", 
                                           "none"))
      col.envelope <- envelope$col
      conf <- envelope$level
      alpha <- envelope$alpha
      border <- envelope$border
      if (style == "none") 
        envelope <- FALSE
    }
    id <- applyDefaults(id, defaults = list(method = "y", n = 2, 
                                            cex = 1, col = carPalette()[1], location = "lr"), type = "id")
    if (isFALSE(id)) {
      id.n <- 0
      id.method <- "none"
      labels <- id.cex <- id.col <- id.location <- NULL
    }
    else {
      labels <- id$labels
      if (is.null(labels)) 
        labels <- if (!is.null(names(x))) 
          names(x)
      else seq(along = x)
      id.method <- id$method
      id.n <- if ("identify" %in% id.method) 
        Inf
      else id$n
      id.cex <- id$cex
      id.col <- id$col
      id.location <- id$location
    }
    line <- match.arg(line)
    index <- seq(along = x)
    good <- !is.na(x)
    ord <- order(x[good])
    if (length(col) == length(x)) 
      col <- col[good][ord]
    if (length(pch) == length(x)) 
      pch <- pch[good][ord]
    if (length(cex) == length(x)) 
      cex <- cex[good][ord]
    ord.x <- x[good][ord]
    ord.lab <- labels[good][ord]
    q.function <- eval(parse(text = paste("q", distribution, 
                                          sep = "")))
    d.function <- eval(parse(text = paste("d", distribution, 
                                          sep = "")))
    n <- length(ord.x)
    P <- ppoints(n)
    z <- q.function(P, ...)
    plot(z, ord.x, type = "n", xlab = xlab, ylab = ylab, main = main, 
         las = las, ylim = ylim, cex.axis=cex.axis.norm, cex.main=cex.main.norm, cex.lab=cex.lab.norm)
    if (grid) {
      grid(lty = 1, equilogs = FALSE)
      box()
    }
    points(z, ord.x, col = col, pch = pch, cex = cex)
    if (line == "quartiles" || line == "none") {
      Q.x <- quantile(ord.x, c(0.25, 0.75))
      Q.z <- q.function(c(0.25, 0.75), ...)
      b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
      a <- Q.x[1] - b * Q.z[1]
      if (line == "quartiles") 
        abline(a, b, col = col.lines, lwd = lwd)
    }
    if (line == "robust") {
      coef <- coef(MASS::rlm(ord.x ~ z))
      a <- coef[1]
      b <- coef[2]
      abline(a, b, col = col.lines, lwd = lwd)
    }
    if (!isFALSE(envelope)) {
      zz <- qnorm(1 - (1 - conf)/2)
      SE <- (b/d.function(z, ...)) * sqrt(P * (1 - P)/n)
      fit.value <- a + b * z
      upper <- fit.value + zz * SE
      lower <- fit.value - zz * SE
      if (style == "filled") {
        envelope(z, z, lower, upper, col = col.envelope, 
                 alpha = alpha, border = border)
      }
      else {
        lines(z, upper, lty = 2, lwd = lwd, col = col.lines)
        lines(z, lower, lty = 2, lwd = lwd, col = col.lines)
      }
    }
    extreme <- showLabels(z, ord.x, labels = ord.lab, method = id.method, 
                          n = id.n, cex = id.cex, col = id.col, location = id.location)
    if (is.numeric(extreme)) {
      nms <- names(extreme)
      extreme <- index[good][ord][extreme]
      if (!all(as.character(extreme) == nms)) 
        names(extreme) <- nms
    }
    if (length(extreme) > 0) 
      extreme
    else invisible(NULL)
  }
  Object_orig <- x
  
  # Show output of analysis assuming homoscedasticity and normality in Stage 1 
  Object <- x$HomoNorm
  Object_Delta_NoHomo <- x$NoHomoNorm   # for delta not assuming homoscedasticity. Could also use NoHomoNoNorm$Delta, same
  
  if (Object$Num.Preds != 1){  # for intercept-only not relevant
  Check.Assump <- Check.Assum(x)
  }
  
  if (Object$Num.Preds == 1){  # for intercept-only not relevant
    Homoscedasticity <- FALSE   # Do not request homoscedasticity plot, makes no sense
    Assume.Homoscedasticity=TRUE
  }
  
  if(missing(Assume.Homoscedasticity) & Object$Num.Preds != 1){
    Assume.Homoscedasticity <- Check.Assump$Assume.Homo.S2}

    # Indicator: is numeric predictor in model  and more than 15 unique predicted values
  Num.Pred.In.Model <- x$HomoNorm$Contains.Numeric.Pred   # 
  
  # Add jitter if specified
  set.seed(Seed); Add_jit_vals <- runif(min = 0-Add.Jitter, 
                                         max=Add.Jitter, n = length(Object$Predicted))

  
  # Give warning if continuous variance prediction function yields negative vales 
  if (Assume.Homoscedasticity==FALSE & x$NoHomoNoNorm$Give.Warning.Neg.Variances){
    warning("\nThe variance prediction function yielded negative values. \nPotential remedial action that can be taken: \n- Try using a different order of the polynomial for the variance prediction function \n  (see argument Order.Poly.Var=... of the Stage.1() function call)\n- Remove potential outliers\n\n")
  }
  
  # H O M O S C E D A S T I C I T Y
  if ((Homoscedasticity==TRUE) & (Object$Num.Preds>1)){
    if (Plots.Together==TRUE){
      dev.new(width=11, height=4.5, noRStudioGD = TRUE); 
      oldpar <- par(no.readonly=TRUE) 
      on.exit(par(oldpar)) 
      par(mfrow=c(1,2))
    }
    
    lim_here <- ceiling(max(abs(Object$Residuals)))
    Y.Lim.Homoscedasticity <- c(-lim_here, lim_here)
    Predicted_plus_jitter <- Object$Predicted + Add_jit_vals
    temp <- data.frame(cbind(Object$Predicted, Predicted_plus_jitter, Object$Residuals)) #, Object$Predicted.Groups))
    names(temp) <- c("Predicted.Values", "Predicted_plus_jitter", "Residuals") #, "Predicted.Groups")  
  
    oldpar <- par(no.readonly=TRUE) 
    on.exit(par(oldpar)) 
    par(mar=c(4.3,5,4.1,2.1))
    if (missing(Main.Homosced.1)){Main <- "Residuals against predicted \nmean test scores"}
    if (!missing(Main.Homosced.1)){Main <- Main.Homosced.1}
    plot(x=temp$Predicted_plus_jitter, y = temp$Residuals, 
         xlab=expression(paste("Predicted mean test scores ", hat(Y))), 
         ylab=expression(paste("Residuals ", hat(epsilon))),
         main=Main, ylim=Y.Lim.Homoscedasticity, cex.axis=cex.axis.homo, cex.main=cex.main.homo, cex.lab=cex.lab.homo,
         col="black") 
    mar=c(5.1, 4.1, 4.1, 2.1)
    # einde homoscedasticity
    
    # variance function
       # with loess
    if (Num.Pred.In.Model==TRUE){
      Loess_Object <- loess(formula = (Object$Residuals**2)~Object$Predicted, span = Loess.Span)
      pred_ordered <- Object$Predicted[order(Object$Predicted)]
      Pred_Loess <- predict(Loess_Object, pred_ordered, se = TRUE)
      if (missing(Y.Lim.ResVarFunction)){Y.Lim.ResVarFunction <- c(0, max(Pred_Loess$fit)*2) # c(0, max(Object$Residuals**2))
      }
      oldpar <- par(no.readonly=TRUE) 
      on.exit(par(oldpar)) 
      par(mar=c(4.3,5,4.1,2.1))
      if (missing(Main.Homosced.2)){Main <- "Variance function"}
      if (!missing(Main.Homosced.2)){Main <- Main.Homosced.2}
      plot(y=(Object$Residuals**2), x=Object$Predicted, col="grey", main=Main, 
           xlab=expression(paste("Predicted mean test scores ", hat(Y))),
           cex.axis=cex.axis.homo, cex.main=cex.main.homo, cex.lab=cex.lab.homo,
           ylab= expression(paste("Squared residuals ", hat(epsilon)^2)), ylim=Y.Lim.ResVarFunction)
      # Add Stage 1 variance function 
      if (Assume.Homoscedasticity==FALSE){  
      Pred.vals <- data.frame(seq(from=min(Object$Predicted), to=max(Object$Predicted), length.out=1000)) 
      names(Pred.vals) <- c("Predicted")
      Pred.Poly <- predict(object = Object$Fit.Var.Function, newdata = Pred.vals, se.fit = TRUE)
      lines(x = Pred.vals$Predicted, y=Pred.Poly$fit, lwd=2, lty=1)
      }
      
      if (Assume.Homoscedasticity == TRUE){
        Rest.std.err <- Object_orig$HomoNorm$SD.Resid
        segments(x0 = min(Object$Predicted),  y0 = Rest.std.err**2,  x1 = max(Object$Predicted),  y1 = Rest.std.err**2, lwd=2,  lty=1)
      }
    
      lines(x = pred_ordered, y=Pred_Loess$fit, lwd=3, lty=2, col=Color)
      lines(x = pred_ordered, y = Pred_Loess$fit-qt(1-.01/2, Pred_Loess$df)*Pred_Loess$se, lty=3, lwd=2, col=Color)
      lines(x = pred_ordered, y = Pred_Loess$fit+qt(1-.01/2, Pred_Loess$df)*Pred_Loess$se, lty=3, lwd=2, col=Color) 
      
      mar=c(5.1, 4.1, 4.1, 2.1)
       }
    
    
    
    # if no numeric predictors in model, no loess line but just add residual standard errors**2 per predicted value
    if (Num.Pred.In.Model==FALSE){
      For.var <- Object_orig$NoHomoNoNorm$Group.Spec.SD.Resid.Values.CI
      For.var <- For.var[order(For.var$`Predicted test score`),]
      means.here <- For.var[,2]**2
      Cols.Table <- dim(For.var)[2]
      CI_Low <- For.var[,c(Cols.Table-1)]**2
      CI_High <- For.var[,c(Cols.Table)]**2
      if (missing(Y.Lim.ResVarFunction)){
        Y.Lim.ResVarFunction <- c(0, max(means.here*2))
      }
      
      oldpar <- par(no.readonly=TRUE) 
      on.exit(par(oldpar)) 
      par(mar=c(4.3,5,4.1,2.1))  
      if (missing(Main.Homosced.2)){Main <- "Variance function"}
      if (!missing(Main.Homosced.2)){Main <- Main.Homosced.2}
      plot(y=(Object$Residuals**2), x=Object$Predicted+Add_jit_vals, col="grey", 
           main=Main, cex.axis=cex.axis.homo, cex.main=cex.main.homo, cex.lab=cex.lab.homo, 
           xlab=expression(paste("Predicted mean test scores ", hat(Y))),
           ylab= expression(paste("Squared residuals ", hat(epsilon)^2)), 
           ylim=Y.Lim.ResVarFunction) #c(0, max(means.here)*2))
  
      # add  Stage 1 variance function. 
      if (Assume.Homoscedasticity == TRUE){
        Rest.std.err <- rep(Object_orig$HomoNorm$SD.Resid, length(For.var$`Predicted test score`))
        lines(x=For.var$`Predicted test score`, Rest.std.err**2, lwd=2, lty=1)
        }
      if (Assume.Homoscedasticity == FALSE){
        lines(x=For.var$`Predicted test score`, means.here, lwd=2, lty=1)
      }
      # Add EVF
      points(x=For.var$`Predicted test score`, means.here, lwd=3, col=Color)
      lines(x=For.var$`Predicted test score`, means.here, lwd=3, lty=2, col=Color)
      arrows(For.var$`Predicted test score`, CI_Low, For.var$`Predicted test score`, CI_High, 
             angle = 90, lty = 2, code = 3, length = .1, lwd=2, col=Color)
      
      
       mar=c(5.1, 4.1, 4.1, 2.1)
    }
    
    
  } # end homoscedasticity
  
  # N O R M A L I T Y
  
  if (Normality == TRUE){
    if (Plots.Together==TRUE){  
      dev.new(width=11, height=4.5, noRStudioGD = TRUE); 
      oldpar <- par(no.readonly=TRUE) 
      on.exit(par(oldpar)) 
      par(mfrow=c(1,3))
    }
    
    if (Assume.Homoscedasticity==TRUE){
      Residual_here <- Object$Delta
    }
    # delta i = residual/SD(residual) per group    (not assuming homoscedasticity)
    if (Assume.Homoscedasticity==FALSE){
      Residual_here <- Object_Delta_NoHomo$Delta
    } 
    
    # Histogram
    if (missing(Main.Norm.1)){Main <- "Histogram standardized \nresiduals"}
    if (!missing(Main.Norm.1)){Main <- Main.Norm.1}
    oldpar <- par(no.readonly=TRUE) 
    on.exit(par(oldpar)) 
    par(mar=c(5.1,5,4.1,2.1))
    hist(Residual_here, col="grey", xlab=expression(paste("Standardized residual ", hat(delta))), 
         main=Main, breaks=15, cex.axis=cex.axis.norm, cex.main=cex.main.norm, cex.lab=cex.lab.norm
         )  
    # Density
    Density_delta_observed <- density(na.exclude(Residual_here))
    # For density of a true standard normal with M = 1 and SD = 1 
    mean_for_true <- 0 
    sd_for_true <- 1 
    x_for_true <- seq(from = (2*min(Residual_here, na.rm = T)), to = (2*max(Residual_here, na.rm = T)), length.out=1000)  
    y_for_true <- dnorm(x_for_true, mean=mean_for_true, sd=sd_for_true)
    # make plot
    Max_plot <- max(max(Density_delta_observed$y), max(y_for_true))
    if (missing(Main.Norm.2)){Main <- "Density of \nstandardized residuals"}
    if (!missing(Main.Norm.2)){Main <- Main.Norm.2}
    oldpar <- par(no.readonly=TRUE) 
    on.exit(par(oldpar)) 
    par(mar=c(5.1,5,4.1,2.1))
    plot(Density_delta_observed, xlab=expression(paste("Standardized residual ", hat(delta))), 
         main=Main, ylim=c(0, Max_plot*1.1), lwd=2, cex.axis=cex.axis.norm, cex.main=cex.main.norm, cex.lab=cex.lab.norm)  
    lines(x = x_for_true, y=y_for_true, lwd=3, lty=2, col=Color)  
    # QQ plot
    if (missing(Main.Norm.3)){Main <- "QQ-plot"}
    if (!missing(Main.Norm.3)){Main <- Main.Norm.3}
    oldpar <- par(no.readonly=TRUE) 
    on.exit(par(oldpar)) 
    par(mar=c(5.1,5,4.1,2.1))
    qqPlotMod(Residual_here, grid = FALSE, xlab=expression(paste("Theoretical quantiles")), 
                ylab=expression(paste("Sample quantiles")),
                main=Main, col="grey", col.lines="black", 
              cex.axis=cex.axis.norm, cex.main=cex.main.norm, cex.lab=cex.lab.norm,
                envelope=list(level=Confidence.QQ.Normality, style="lines"), id=FALSE)

    }  # End normality
  
  # O U T L I E R S  
  if (Outliers==TRUE){
    if (Plots.Together==TRUE){
      dev.new(width=5.5, height=4.5, noRStudioGD = TRUE); 
      oldpar <- par(no.readonly=TRUE) 
      on.exit(par(oldpar)) 
      par(mfrow=c(1,1))
    }
    
    # Delta_i computed using homoscedasticity
    # delta i = residual/overall SD(residual)    
    if (Assume.Homoscedasticity==TRUE){
    max_plot <- max(max(na.exclude(Object$Delta)), Object$Outlier.Cut.Off)
    min_plot <- min(min(na.exclude(Object$Delta)), -Object$Outlier.Cut.Off)
    oldpar <- par(no.readonly=TRUE) 
    on.exit(par(oldpar)) 
    par(mar=c(4.3,5,4.1,2.1))
    if (missing(Main.Outliers)){Main <- "Outliers"}
    if (!missing(Main.Outliers)){Main <- Main.Outliers}
    plot(y=Object$Delta, x=1:length(Object$Delta), xlab=expression(paste("Observation")), 
         ylab=expression(paste("Standardized residual ", hat(delta))), 
         main=Main, cex.axis=cex.axis.outl, cex.main=cex.main.outl, cex.lab=cex.lab.outl,
         xaxt="no", col="black", ylim=c(min_plot*1.1, max_plot*1.1))
    axis(1, at = 1:length(Object$Delta), tick = F, cex.axis=cex.axis.outl)
    abline(h=c(Object$Outlier.Cut.Off, -Object$Outlier.Cut.Off), lty=2)
    mar=c(5.1, 4.1, 4.1, 2.1)
    }
    
    # Delta_i computed not assuming homoscedasticity
    if (Assume.Homoscedasticity==FALSE){
    max_plot <- max(max(na.exclude(Object_Delta_NoHomo$Delta)), Object_Delta_NoHomo$Outlier.Cut.Off)
    min_plot <- min(min(na.exclude(Object_Delta_NoHomo$Delta)), -Object_Delta_NoHomo$Outlier.Cut.Off)
    oldpar <- par(no.readonly=TRUE) 
    on.exit(par(oldpar)) 
    par(mar=c(4.3,5,4.1,2.1))
    if (missing(Main.Outliers)){Main <- "Outliers"}
    if (!missing(Main.Outliers)){Main <- Main.Outliers}
    plot(y=Object_Delta_NoHomo$Delta, x=1:length(Object_Delta_NoHomo$Delta), 
         xlab=expression(paste("Observation")),
         ylab=expression(paste("Standardized residual ", hat(delta))),
         main=Main, cex.axis=cex.axis.outl, cex.main=cex.main.outl, cex.lab=cex.lab.outl,
         xaxt="no", col="black", ylim=c(min_plot*1.1, max_plot*1.1))
    axis(1, at = 1:length(Object_Delta_NoHomo$Delta), tick = F, cex.axis=cex.axis.outl)
    abline(h=c(Object_Delta_NoHomo$Outlier.Cut.Off, -Object_Delta_NoHomo$Outlier.Cut.Off), lty=2)
    mar=c(5.1, 4.1, 4.1, 2.1)
        }

    
  } # einde outliers


  # G R O U P - S P E C I F I C   D E N S I T I E S      D E L T A    
  if (Group.Spec.Densities.Delta==TRUE){
    
    if (Plots.Together==TRUE){
      dev.new(width=5.5, height=4.5, noRStudioGD = TRUE); 
      oldpar <- par(no.readonly=TRUE) 
      on.exit(par(oldpar)) 
      par(mfrow=c(1,1))
    }

    # Delta_i computed using homoscedasticity
    # delta i = residual/overall SD(residual)    (assuming homoscedasticity)
    if (Assume.Homoscedasticity==TRUE){
      Delta_Overall_SD_Residual <- Groups_Num <- NULL # LS! to pass R CMD check 
      Densities(Object_orig$HomoNorm$Table.Obs.Pred.Res.Delta, Color = FALSE, Size.Legend = .8, 
                Test.Score = Delta_Overall_SD_Residual, 
                IV =  Groups_Num, main="Group-specific densities of\nstandardized residuals")
          }
    
    # # Delta_i not computed using homoscedasticity
    if (Assume.Homoscedasticity==FALSE){
      Delta_Group_Specific_SD_Residual <- Groups_Num <- NULL # LS! to pass R CMD check
      Densities(Object_orig$NoHomoNoNorm$Table.Obs.Pred.Res.Delta, Color = FALSE,Size.Legend = .8, 
                Test.Score = Delta_Group_Specific_SD_Residual, 
                IV =  Groups_Num, main="")
          }
  } # einde group-specific densities delta

  oldpar <- par(no.readonly=TRUE) 
  on.exit(par(oldpar)) 
  par(mfrow=c(1,1)) # LS!!!
  
  if (Object$Num.Preds != 1){  # for intercept-only not relevant
  # print how delta i is computed
  if (Assume.Homoscedasticity==TRUE){
    if (verbose==TRUE) cat("Note: the std. residuals are computed using the overall \nresidual standard error, i.e., assuming homoscedasticity\n")
  }
  if (Assume.Homoscedasticity==FALSE){
    if (verbose==TRUE) cat("Note: the std. residuals are computed using prediction-specific \nresidual standard errors, i.e., not assuming homoscedasticity\n")
  }
  }
  
   if (Object$Num.Preds == 1){  # for intercept-only not relevant
     # print how delta i is computed
     if (verbose==TRUE) cat("Note: the std. residuals are computed using the overall \nresidual standard error (homoscedasticity assumption \nnot relevant for intercept-only model)\n")
     
   } 
  
  
} 
