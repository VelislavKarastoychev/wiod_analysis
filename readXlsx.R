# This file read the xlsx files
# of wiod database that is downloaded
# in the folder wiod2013 and changes
# them to RData files that is more 
# usefull in R ...
if (!require("readxl")){
  install.packages("readxl", dep = TRUE)
  if (!require("readxl")) stop("The package readxl not found!")
}
library("readxl")
getWiodFromExcel <- function (year = 1995, dataset = 2013) {
  
  if (dataset == "2013" || dataset == 2013)fex <- paste("./wiod2013/wiot",as.character(year),".xlsx", sep = "")
  else fex <- fex <- paste("./wiod2016/WIOT",as.character(year),".xlsb", sep = "") 
  year <- as.numeric(year)
  if (dataset == "2013" || dataset == 2013) {
  countriesAsDataFrame <- unique(read_excel(fex, sheet = 1, range = "C7:C1441"))
  industries <- unique(read_excel(fex, sheet = 1, range = "B7:B1441"))
  final <- as.matrix(read_excel(fex, sheet = 1, range = "BCJ6:BKF1441"))
  cnf <- paste(
    names(read_excel(fex, sheet = 1, range = "BCJ4:BKF4")),
    names(read_excel(fex, sheet = 1, range = "BCJ5:BKF5")), 
    sep = ' of ')
  colnames(final) <- as.vector(cnf)
  rnc<- as.vector(
    paste(
      read_excel(fex, sheet = 1, range = "B6:B1441")$X__1,
      read_excel(fex, sheet = 1, range = "C6:C1441")$X__1,
      sep = ' of '))
  rownames(final) <- rnc
  inter <- as.matrix(read_excel(fex, sheet = 1, range = "E6:BCI1441"))
  colnames(inter) <- as.vector(names(read_excel(fex, sheet = 1, range = "E4:BCI4")))
  rownames(inter) <- rnc
  output <- as.matrix(read_excel(fex, sheet = 1, range = "BKG6:BKG1441")) 
  rownames(output) <- rnc
  valueAdded <- as.matrix(read_excel(fex, sheet = 1, range = "E1447:BCI1447"))
  #colnames(valueAdded) <- rnc
  total <- as.matrix(read_excel(fex, sheet = 1, range = "E1441:BCI1448"))
  rnt <- as.vector(
    read_excel(fex, sheet = 1, range = "B1441:B1448")$"Private Households with Employed Persons"
  )
  rownames(total) <- rnt
  colnames(total) <- rnc
  totalFinal <- as.matrix(read_excel(fex, sheet = 1, range = "BCJ1441:BKF1448"))
  rownames(totalFinal) <- rnt
  colnames(totalFinal) <- cnf
  valueAddedFinal <- as.matrix(read_excel(fex, sheet = 1, range = "BCJ1447:BKF1447"), nrow = 1, ncol = 1435)
  #colnames(valueAddedFinal) <- cnf
  #rownames(valueAddedFinal) <- "value added"
  iot <- list(year = year,
              countries = countriesAsDataFrame$AUS, 
              industries = industries$`Agriculture, Hunting, Forestry and Fishing`, 
              final = final,
              inter = inter, 
              output = output, 
              valueAdded = valueAdded,
              valueAddedFinal = valueAddedFinal,
              total = total,
              totalFinal = totalFinal)
  class(iot) = "iot"
  # if the file do not exst
  # save it in the folder 
  # wiod-2013/filename.RData ...
  fileLocation <- paste(c('wiod-2013/wiod_',as.character(year), '.RData'), collapse = "")
  if (isTRUE(file.exists(fileLocation))) return (iot)
  else {
    save(iot, file = fileLocation)
    print(paste(c('A new file was created at ', getwd(),'/',fileLocation), collapse = ""))
    return(iot)
  } 
  }
  else {
    countriesAsDataFrame <- unique(read_excel(fex, sheet = 1, range = "C7:C2470"))
    industries <- unique(read_excel(fex, sheet = 1, range = "B7:B2470"))
    final <- as.matrix(read_excel(fex, sheet = 1, range = "CPY6:CYJ2470"))
    cnf <- paste(
      names(read_excel(fex, sheet = 1, range = "CPY4:CYJ4")),
      names(read_excel(fex, sheet = 1, range = "CPY:CYJ5")), 
      sep = ' of ')
    colnames(final) <- as.vector(cnf)
    rnc<- as.vector(
      paste(
        read_excel(fex, sheet = 1, range = "B6:B2470")$X__1,
        read_excel(fex, sheet = 1, range = "C6:C2470")$X__1,
        sep = ' of '))
    rownames(final) <- rnc
    inter <- as.matrix(read_excel(fex, sheet = 1, range = "E6:CPX2470"))
    colnames(inter) <- as.vector(names(read_excel(fex, sheet = 1, range = "E4:CPX4")))
    rownames(inter) <- rnc
    output <- as.matrix(read_excel(fex, sheet = 1, range = "CYK6:CYK2470")) 
    rownames(output) <- rnc
    valueAdded <- as.matrix(read_excel(fex, sheet = 1, range = "E2476:CPX2476"))
    #colnames(valueAdded) <- rnc
    total <- as.matrix(read_excel(fex, sheet = 1, range = "E2470:CPX2477"))
    rnt <- as.vector(
      read_excel(fex, sheet = 1, range = "B2470:B2477")$"Activities of extraterritorial organizations and bodies"
    )
    #rownames(total) <- rnt
    #colnames(total) <- rnc
    totalFinal <- as.matrix(read_excel(fex, sheet = 1, range = "CPY2470:CYJ2477"))
    #rownames(totalFinal) <- rnt
    #colnames(totalFinal) <- cnf
    valueAddedFinal <- as.matrix(read_excel(fex, sheet = 1, range = "CPY2476:CPJ2476"), nrow = 1, ncol = 1435)
    #colnames(valueAddedFinal) <- cnf
    #rownames(valueAddedFinal) <- "value added"
    iot <- list(year = year,
                countries = countriesAsDataFrame$AUS, 
                industries = industries$`Agriculture, Hunting, Forestry and Fishing`, 
                final = final,
                inter = inter, 
                output = output, 
                valueAdded = valueAdded,
                valueAddedFinal = valueAddedFinal,
                total = total,
                totalFinal = totalFinal)
    class(iot) = "iot"
    # if the file do not exst
    # save it in the folder 
    # wiod-2013/filename.RData ...
    fileLocation <- paste(c('wiod-2016/wiod_',as.character(year), '.RData'), collapse = "")
    if (isTRUE(file.exists(fileLocation))) return (iot)
    else {
      save(iot, file = fileLocation)
      print(paste(c('A new file was created at ', getwd(),'/',fileLocation), collapse = ""))
      return(iot)
    } 
  }
}
d <- getWiodFromExcel(2000, 2016)