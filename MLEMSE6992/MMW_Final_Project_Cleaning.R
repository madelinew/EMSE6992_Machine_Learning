library(readxl)
FARS2015 <- read_excel("~/Documents/EMSE 6992 ML/Final Project/MLEMSE6992/FARS Data/FARS2015.xls", 
                       col_types = c("numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", "text"))

FARS_15new <- as.data.frame(FARS2015, header = TRUE, as.is=TRUE)


FARS2016 <- read_excel("~/Documents/EMSE 6992 ML/Final Project/MLEMSE6992/FARS Data/FARS2016.xls", 
                       col_types = c("numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", "text"))

FARS_16new <- as.data.frame(FARS2016, header = TRUE, as.is=TRUE)


library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("knitr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gridExtra", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("png", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("pander", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("tidyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

fatal_YYsetup <- function(df, YYYY) {
  #df = FARS_YY
  #YYYY = case year
  y = YYYY
  
  fatal <- as.data.frame(cbind.data.frame(df$casenum, df$statenum, df$vnumber, df$accday, df$accmon,
                                          df$caseyear, df$landuse, df$age, df$body, df$vfatcount, df$numoccs,
                                          df$modelyr, df$dridistract), 
                           header = TRUE, as.is = TRUE, stringsAsFactor=FALSE)
 
  colnames(fatal) <- c("casenum", "statenum", "vehnum","day", "month", "year", "landuse", "age","bodytype",
                         "vfatcount", "numoccs", "modelyr", "dridistract")
  #Fixing age as num
  fatal$age <- as.integer(fatal$age)
  fatal <- unique(fatal)
  totalraw <- dim(df)[1]
  #removes error coded ages
  removed <- fatal[fatal$age >= 997,]
  rem1 <- dim(removed)[1]
  fatal <- fatal[fatal$age < 997,]
  #removes unknown model years
  removed2 <- fatal[fatal$modelyr >= 9999,]
  rem2 <- dim(removed2)[1]
  fatal <- fatal[fatal$modelyr < 9999,]
  #removes unknown number of occs
  removed3 <- fatal[fatal$numoccs >= 99,]
  rem3 <- dim(removed3)[1]
  fatal <- fatal[fatal$numoccs < 99,]
  print("Total number of accidents removed: ")
  print(sum(rem1,rem2,rem3))
  print("out of ")
  print(totalraw)
  return(fatal)
}

## STORE RESULT OF FUNCTION IN MAIN DATA FRAME ##
#INPUT EXAMPLE:   fatal_16 <- fatal_YYsetup(FARS_16, 2016) #
#fatal_16na <- fatal_YYnasetup(FARS_16, 2016)
fatal_15t <- fatal_YYsetup(FARS_15new, 2015)
fatal_16t <- fatal_YYsetup(FARS_16new, 2016)

separatingDriDistract <- function(df) {
  ndf <- df
  ndf <- ndf %>% separate(dridistract, c("dridistract1", "dridistract2", "dridistract3"), ", ", extra = "warn", fill = "right")
  ndf$dridistract1 <- as.integer(ndf$dridistract1)
  ndf[is.na(ndf)] <- 0
  ndf$dridistract2 <- as.integer(ndf$dridistract2)
  ndf$dridistract3 <- as.integer(ndf$dridistract3)
  return(ndf)
}
fatal_15final <- separatingDriDistract(fatal_15t)
fatal_16final <- separatingDriDistract(fatal_16t)



findingCellCases <- function(df, YY) {
  #df = fatal_YY
  #YY = case year
  setupfataldis <- function(df) {
    setupdrf <- function(df){
      #5 = Talking or listening to cell phone
      #6 = While dialing Cell phone
      #15 = Other cell phone distraction
      df$cellphoneuse <- ifelse(df$dridistract1 == 5, 1,
                                ifelse(df$dridistract2 == 5, 1,
                                       ifelse(df$dridistract3 == 5, 1,
                                              ifelse(df$dridistract1 == 6, 1,
                                                     ifelse(df$dridistract2 == 6, 1,
                                                            ifelse(df$dridistract3 == 6, 1,
                                                                   ifelse(df$dridistract1 == 15, 1,
                                                                          ifelse(df$dridistract2 == 15, 1,
                                                                                 ifelse(df$dridistract3 == 15, 1, 0)))))))))
      drop <- c("dridistract1", "dridistract2","dridistract3")
      df = df[,!(names(df) %in% drop)]
      return(df)
    }
    fataldisYY <- setupdrf(df)
    return(fataldisYY)
  }
  fataldisYY <- setupfataldis(df)
  return(fataldisYY)
}



fatal_15dsfinal <- findingCellCases(fatal_15final, 15)
fatal_16dsfinal <- findingCellCases(fatal_16final, 16)



fulldataset <- rbind(fatal_15dsfinal, fatal_16dsfinal)



tableToCSV <- function(df) {
  #df = fulldata
  row.names(df)<- NULL
  colnames(df) <- c("casenum", "statenum", "vehnum","day", "month", "year", "landuse", "age", "bodytype",
                    "vfatcount", "numoccs", "modelyr", "cellphone_use")
  write.csv(df, file = "fulldataset1516.csv")
}
tableToCSV(fulldataset)
