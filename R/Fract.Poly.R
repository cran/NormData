Fract.Poly <- function(IV, Outcome, 
  S=c(-3, -2.5, -2.0, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3), Max.M=3){
  S <- S[S!=0] # exclude zero
  if (Max.M > 5){
    message("The maximum allowed Max.M is 5. The requested Max.M is replaced by Max.M=5\n")
    Max.M <- c(5)
  }
  Dataset <- data.frame(IV, Outcome)
  n=length(Outcome)
  Results_m1 <- Results_m2 <- Results_m3 <- Results_m4 <- Results_m5 <- NULL
 
  for (j in 1: Max.M){
  r <- length(S)  #aantal in S
  n <- j    #aantal k
  aantal <- factorial(n+r-1)/
    (factorial(n) * factorial(r-1)) 
  if (aantal > 5000){
  warning("A total of ", aantal, " models of degree ", i, " can be fitted. Consider using \na more restricted set S or a lower Max.M \n\n", sep= "")
  }
  }
IV[IV==0] <- 0.00000001
  
Lowest.AIC.Value <- 10**100

#m1  
if (Max.M>=1){
Results_m1 <- NULL
for (i in 1:length(S)){
  if  (S[i] != 0){term1 <- IV ** S[i]} 
  if  (S[i] == 0){term1 <- log(IV)} 
  fit <- lm(Outcome~term1, data=Dataset)
  AIC.Value <- AIC(fit)
  if (AIC.Value < Lowest.AIC.Value){Lowest.AIC.Value <- AIC.Value; Best.Model <- fit}
  Results_m1 <- rbind(Results_m1, cbind(1, S[i], NA, NA, NA, NA, AIC.Value, coef(fit)[1], coef(fit)[2], NA, NA, NA, NA))
 }
}

# m=2
if (Max.M>=2){
Results_m2 <- NULL
for (i in 1:length(S)){
  for (j in 1:length(S)){
    if  (S[i] != 0){term1 <- IV ** S[i]} 
    if  (S[i] == 0){term1 <- log(IV)} 
    if  (S[j] != 0){term2 <- IV ** S[j]}
    if  (S[j] == 0){term2 <- log(IV)}
    if  (S[i] == S[j]){term2 <- (IV ** S[j]) * log(IV)} 
    fit <- 
      lm(Outcome~term1+term2, data=Dataset)
    AIC.Value <- AIC(fit)
    if (AIC.Value < Lowest.AIC.Value){Lowest.AIC.Value <- AIC.Value; Best.Model <- fit}
    Results_m2 <- rbind(Results_m2, cbind(2, S[i], S[j], NA, NA, NA, AIC.Value, coef(fit)[1], coef(fit)[2], coef(fit)[3], NA, NA, NA))
  }
}
Results_m2 <- Results_m2[apply(Results_m2[,c(1:2)], 1, function(x)length(unique(x)))==2,] # delete solutions with same power, like p1 = -3 and p2=-"
}
#m=3
if (Max.M>=3){  
Results_m3 <- NULL
for (i in 1:length(S)){
  for (j in 1:length(S)){
    for (k in 1: length(S)){
      if  (S[i] != 0){term1 <- IV ** S[i]} 
      if  (S[i] == 0){term1 <- log(IV)} 
      if  (S[j] != 0){term2 <- IV ** S[j]}
      if  (S[j] == 0){term2 <- log(IV)}
      if  (S[k] != 0){term3 <- IV ** S[k]} 
      if  (S[k] == 0){term3 <- log(IV)}    
      if  (S[i] == S[j]){term2 <- (IV ** S[j]) * log(IV)} 
      if  (S[i] == S[k]){term2 <- (IV ** S[k]) * log(IV)} 
      if  (S[j] == S[k]){term2 <- (IV ** S[k]) * log(IV)} 
      fit <- lm(Outcome~term1+term2+term3, data=Dataset)
      if (NA %in% fit$coeff == FALSE){
      AIC.Value <- AIC(fit)
      if (AIC.Value < Lowest.AIC.Value){Lowest.AIC.Value <- AIC.Value; Best.Model <- fit}
      Results_m3 <- rbind(Results_m3, cbind(3, S[i], S[j], S[k], NA, NA, AIC.Value, coef(fit)[1], coef(fit)[2], coef(fit)[3], coef(fit)[4], NA, NA))}
    }
  }
}
Results_m3 <- Results_m3[apply(Results_m3[,c(1:3)], 1, function(x)length(unique(x)))==3,]
}

# m=4
if (Max.M>=4){
  Results_m4 <- NULL
for (i in 1:length(S)){
  for (j in 1:length(S)){
    for (k in 1: length(S)){
      for (l in 1: length(S)){      
        if  (S[i] != 0){term1 <- IV ** S[i]} 
        if  (S[i] == 0){term1 <- log(IV)} 
        if  (S[j] != 0){term2 <- IV ** S[j]}
        if  (S[j] == 0){term2 <- log(IV)}
        if  (S[k] != 0){term3 <- IV ** S[k]} 
        if  (S[k] == 0){term3 <- log(IV)}    
        if  (S[l] != 0){term4 <- IV ** S[l]} 
        if  (S[l] == 0){term4 <- log(IV)}    
        if  (S[i] == S[j]){term2 <- (IV ** S[j]) * log(IV)} 
        if  (S[i] == S[k]){term2 <- (IV ** S[k]) * log(IV)} 
        if  (S[i] == S[l]){term2 <- (IV ** S[l]) * log(IV)} 
        if  (S[j] == S[k]){term2 <- (IV ** S[k]) * log(IV)} 
        if  (S[j] == S[l]){term2 <- (IV ** S[l]) * log(IV)} 
        if  (S[k] == S[l]){term2 <- (IV ** S[l]) * log(IV)} 
        fit <- lm(Outcome~term1+term2+term3+term4, data=Dataset)
        
        if (NA %in% fit$coeff == FALSE){
        AIC.Value <- AIC(fit)
        if (AIC.Value < Lowest.AIC.Value){Lowest.AIC.Value <- AIC.Value; Best.Model <- fit}
        Results_m4 <- rbind(Results_m4, cbind(4, S[i], S[j], S[k], S[l], NA, AIC.Value, 
                                              coef(fit)[1], coef(fit)[2], coef(fit)[3], coef(fit)[4], coef(fit)[5], NA))}
      }
    }
  }
}
Results_m4 <- Results_m4[apply(Results_m4[,c(1:4)], 1, function(x)length(unique(x)))==4,]
}

if (Max.M>=5){  
Results_m5 <- NULL
for (i in 1:length(S)){
  for (j in 1:length(S)){
    for (k in 1: length(S)){
      for (l in 1: length(S)){      
        for (m in 1: length(S)){      
          if  (S[i] != 0){term1 <- IV ** S[i]} 
          if  (S[i] == 0){term1 <- log(IV)} 
          if  (S[j] != 0){term2 <- IV ** S[j]}
          if  (S[j] == 0){term2 <- log(IV)}
          if  (S[k] != 0){term3 <- IV ** S[k]} 
          if  (S[k] == 0){term3 <- log(IV)}    
          if  (S[l] != 0){term4 <- IV ** S[l]} 
          if  (S[l] == 0){term4 <- log(IV)}    
          if  (S[m] != 0){term5 <- IV ** S[m]} 
          if  (S[m] == 0){term5 <- log(IV)}    
          if  (S[i] == S[j]){term2 <- (IV ** S[j]) * log(IV)} 
          if  (S[i] == S[k]){term2 <- (IV ** S[k]) * log(IV)} 
          if  (S[i] == S[l]){term2 <- (IV ** S[l]) * log(IV)} 
          if  (S[i] == S[m]){term2 <- (IV ** S[m]) * log(IV)} 
          if  (S[j] == S[k]){term2 <- (IV ** S[k]) * log(IV)} 
          if  (S[j] == S[l]){term2 <- (IV ** S[l]) * log(IV)} 
          if  (S[j] == S[m]){term2 <- (IV ** S[m]) * log(IV)} 
          if  (S[k] == S[l]){term2 <- (IV ** S[l]) * log(IV)} 
          if  (S[k] == S[m]){term2 <- (IV ** S[m]) * log(IV)} 
          if  (S[l] == S[m]){term2 <- (IV ** S[m]) * log(IV)} 
          fit <- lm(Outcome~term1+term2+term3+term4+term5, data=Dataset)

          if (NA %in% fit$coeff == FALSE){
            AIC.Value <- AIC(fit)
            if (AIC.Value < Lowest.AIC.Value){Lowest.AIC.Value <- AIC.Value; Best.Model <- fit}
            Results_m5 <- rbind(Results_m5, cbind(5, S[i], S[j], S[k], S[l], S[m], 
     AIC.Value, coef(fit)[1], coef(fit)[2], coef(fit)[3], coef(fit)[4], coef(fit)[5], coef(fit)[6]))}
          
        }
      }
    }
  }
}
Results_m5 <- Results_m5[apply(Results_m5[,c(1:5)], 1, function(x)length(unique(x)))==5,]
}

All.Results <- data.frame(rbind(Results_m1, Results_m2, Results_m3, Results_m4, Results_m5), row.names = NULL)
names(All.Results) <- c("M", "power1","power2","power3","power4","power5", "AIC.Value", 
                           "beta_0", "beta_1", "beta_2", "beta_3", "beta_4", "beta_5")
Best <- All.Results[order(All.Results$AIC.Value),][1,]  # Select model with lowest AIC.Value
Best <- Best[,colSums(is.na(Best))<nrow(Best)]  # Remove columns with NA 

fit <- list(All.Results=All.Results, Lowest.AIC.Value=Best, Best.Model=Best.Model, 
            IV=IV, Outcome=Outcome)   

class(fit) <- "Fract.Poly"
fit
}

