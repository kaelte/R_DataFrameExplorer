fitVecToRgb <- function(vec){return(c(vec[1]%%256,vec[2]%%256,vec[3]%%256))}
vecToCol <- function(vec){
  V <- fitVecToRgb(vec)
  return(rgb(red=V[1],green=V[2],blue=V[3],maxColorValue=255))
}
getFarben <- function(farbPalette=rainbow,numCols=4,claritas=1) {
  farben <- match.fun(FUN=farbPalette)(max(c(1,numCols)))
  resultat <- as.vector(sapply(USE.NAMES=FALSE,X=farben,FUN=function(x){return(vecToCol(claritas*col2rgb(x)))}))
  return(resultat)
}
getNonNullValue <- function(wert,ersatz){
  if("NULL"==class(wert)) {
    print(paste("getNonNullValue: NULL==class(wert) using ersatz",ersatz,sep="="))
    resultat <- ersatz
  } else {resultat <- wert}
  # if(is.na(resultat)) {resultat <- ersatz}
  return(resultat)
}
getFormula <- function(operator,xVec){
  if ("none" == operator) {resultat <- xVec[1]} else {resultat <- paste(xVec,collapse=operator)}
  return(resultat)
}

getTimeFormat <- function(typeString) {return(ifelse("Date"==typeString,"%Y-%m-%d","%Y-%m-%d %H:%M:%S"))}
milliSecondsToDate <- function(t) {return(as.Date(origin="1970-01-01",x=t/(24*3600*1000)))}

milliSecondsToString <- function(t,Zeitformat) {return(format(milliSecondsToDate(t),Zeitformat))}
