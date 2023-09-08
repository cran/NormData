Plot.Scatterplot.Matrix <- function(Dataset, Variables, Add.Jitter=0.1, Seed=123, ...){
Data.Plot <- Dataset[Variables]
for (i in 1: dim(Data.Plot)[2]){
  set.seed(Seed+i)
  Data.Plot[,i] <- as.numeric(Data.Plot[,i]) + runif(n=dim(Data.Plot)[1], min = -Add.Jitter, max = Add.Jitter)
}
pairs(x = Data.Plot, upper.panel = NULL) 
}