WriteNormTable <- function(NormTable, Folder, NameFile="NormTable.xlsx", verbose=TRUE){

if (inherits(NormTable, what = "Stage.2.NormTable")==FALSE){  
  stop("An object of class Stage.2.NormTable should be specified in the NormTable= argument\n")
} 

Is.Excel <- grepl(NameFile, pattern = "xlsx")

# If the user did not specify a Folder in the function call, ask here for entering a 
# folder. Instead of specifying a folder, it is also possible to enter WD. Then the file is written to the working directory 
# If nothing is entered by the user, the function stops
if (missing(Folder)){
  message(paste("Please specify the folder where the normative table should be saved, or \nenter WD if you want to write the file to the current working directory \n(", getwd(), "):", sep=""))
  Folder.Input <- readline(prompt = "")
  if (Folder.Input==""){stop("Folder is not provided, the normative table is not written")}
  ifelse(Folder.Input=="WD", yes = Folder <- getwd(), no = Folder<-Folder.Input)
}


# check that last character of specified folder is /. If missing, add it
last <- substr(Folder, nchar(Folder), nchar(Folder)) 
if (last!="/"){Folder <- paste(Folder, "/", sep="")}

# Check whether dir exists and create one if it does not exist
if (!dir.exists(Folder)){
    dir.create(Folder)
  }  
    
Object <- NormTable$Norm.Table
if (Is.Excel==FALSE){
  write.table(x = Object, 
  file = paste(Folder, NameFile, sep=""), sep = " ", row.names = FALSE, dec=".")
  if (verbose==TRUE) cat("The normative table was successfully written.\n")
  }

if (Is.Excel==TRUE){
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Normative data")
  openxlsx::writeData(wb, "Normative data", Object, startRow = 1, startCol = 1)
  openxlsx::saveWorkbook(wb, file = paste(Folder, NameFile, sep=""), overwrite = TRUE)
  if (verbose==TRUE) cat("The normative table was successfully written.\n")
  }

}