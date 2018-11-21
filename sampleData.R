### This file just provides a sample data frame inputDf

tempus <- seq.Date(from=as.Date("2018-01-01"),to=as.Date("2018-12-31"),by="day")
countries <- c("CHE","COL","DEU","FRA")
gender <- c("female","male","other")
emploied <- c(FALSE,TRUE)
inputDf <- expand.grid(list(TEMPUS=tempus,COUNTRY=countries,GENDER=gender,IS_EMPLOIED=emploied))
timeLength <- length(tempus)
getNumbers <- function(totalNum,rangeWidth,land,gen,empl) {
  emploimentRate <- runif(n=1,min=3/5,max=1)
  if (!empl) {emploimentRate <- 1-emploimentRate}
  partialNum <- totalNum*emploimentRate/switch(EXPR=gen,female=2,male=2,other=1000)
  minVal <- floor((1-rangeWidth)*partialNum)
  maxVal <- floor((1+rangeWidth)*partialNum)
  return(sample(x=minVal:maxVal,size=timeLength,replace=TRUE))
}
numRows <- nrow(inputDf)
inputDf$NUM_PEOPLE <- rep(0,numRows)
for (land in countries) {
  for (gen in gender) {
    for (empl in emploied) {
      population <- switch(EXPR=land,CHE=8,COL=49,DEU=83,67) * 10^6
      inputDf[inputDf$COUNTRY==land & inputDf$GENDER==gen & inputDf$IS_EMPLOIED==empl
              ,]$NUM_PEOPLE<-getNumbers(population,runif(n=1,min=10^-3,max=10^-2),land,gen,empl)
    }
  }
}
inputDf$NUM_EMAIL <- rep(0,numRows)
for (land in countries) {
  for (gen in gender) {
    for (empl in emploied) {
      totalNumEmails <- switch(EXPR=land,CHE=7*8,COL=2*49,DEU=5*83,4*67) * 10^6
      inputDf[inputDf$COUNTRY==land & inputDf$GENDER==gen & inputDf$IS_EMPLOIED==empl
              ,]$NUM_EMAIL<-getNumbers(totalNumEmails,runif(n=1,min=1/10,max=1/2),land,gen,empl)
    }
  }
}
inputDf$NUM_PHONE <- rep(0,numRows)
for (land in countries) {
  for (gen in gender) {
    for (empl in emploied) {
      totalNumPhone <- switch(EXPR=land,CHE=5*8,COL=3*49,DEU=4*83,5*67) * 10^6
      inputDf[inputDf$COUNTRY==land & inputDf$GENDER==gen & inputDf$IS_EMPLOIED==empl
              ,]$NUM_PHONE<-getNumbers(totalNumPhone,runif(n=1,min=1/10,max=1/2),land,gen,empl)
    }
  }
}
