sortFrameByFstCol <- function(dataFrame,desc=FALSE) {
  resultat <- dataFrame[do.call(order,c(dataFrame,list(decreasing=FALSE,method="radix"))),]
  loggResultatDf("sortFrameByFstCol",df=resultat)
  return(resultat)
}
replaceNaInDf <- function(df,cols,replacement) {
  resultat <- df
  if (0==nrow(df)){
    logg("replaceNaInDf","Input data frame is empty. Nothing to do!")
  } else {
    for (c in cols){
      resultat[is.na(resultat[,c]),c] <- replacement
    }}
  return(resultat)
}
completeGroupedDataFrame <- function(df,fillValue) {
  ### df: DataFrame with 3 columns c("Gruppe","x","y")
  ### fillValue of type y
  ### this functions adds lines (Gruppe,x,fillValue)
  ### so that for all Gruppe the same set of xValues is present
  logg("completeGroupedDataFrame",paste("Completing data frame with",nrow(x=df),"rows."))
  gruppen <- unique(df$Gruppe)
  xWerte <- unique(df$x)
  yWerte <- as.vector(unique(df$y))
  missingLines <- setdiff(x=expand.grid(Gruppe=gruppen,x=xWerte),y=df[,c("Gruppe","x")])
  if (0<nrow(missingLines)) { missingLines$y <- fillValue
  }
  resultat <- rbind(x=replaceNaInDf(df=df,cols=c("y"),replacement=fillValue),y=missingLines)
  loggResultatDf("completeGroupedDataFrame",df=resultat)
  return(resultat)
}
getColTypes <- function(df) {
  colTypes <- c()
  numUnique <- c()
  for (col in colnames(df)) {
    spalte <- unique(df[,c(col)])
    typ <- class(spalte)
    if ("factor"==typ) {typ <- class(levels(spalte))}
    colTypes <- c(colTypes,typ)
    numUnique <- c(numUnique,length(spalte))
  }
  result <- data.frame(COLNAME=colnames(df),COLTYP=colTypes,NUM_UNIQUE=numUnique)
  return(result)
}

replaceBoolByChar <- function(df) {
  resultat <- df
  if (0<nrow(x=df)) {
    dfTypes <- getColTypes(df=inputDf)
    logicalCols <- as.vector(dfTypes["logical"==dfTypes$COLTYP,c("COLNAME")])
    for (col in logicalCols) {
        logg("replaceBoolByChar",paste("Column",col,"is logical. Converting to character"))
        resultat[col] <- lapply(X=resultat[col],FUN=as.character)
    }
  }
  loggResultatDf("replaceBoolByChar",df=resultat)
  return(resultat)
}

aggDf <- function(x,by,FUN) {
  if (0 == length(by[[names(by)[1]]])) {
    resultat <- data.frame(by,x)} else { resultat <- aggregate.data.frame(x,by,FUN)
    }
  return(resultat)
}
opColumn <- function(dataFrame,operator,fstCol,sndCol,newCol) {
  resultat <- dataFrame
  resultat[c(newCol)] <- NA
  resultatfstCol <- as.vector(resultat[c(fstCol)])
  resultatsndCol <- as.vector(resultat[c(sndCol)])
  if ("/"==operator) {
    inequalZero <- as.logical(!is.na(resultatsndCol) & 0!=resultatsndCol) ### true if snd col != 0
    resultat[inequalZero,c(newCol)] <- resultat[inequalZero,c(fstCol)]/resultat[inequalZero,c(sndCol)]
  } else {resultat[c(newCol)] <- get(x=operator)(resultat[c(fstCol)],resultat[c(sndCol)])}
  return(resultat)
}
