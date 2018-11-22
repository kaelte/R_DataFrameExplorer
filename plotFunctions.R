emptyPlot <- data.frame(x=c(0),y=c(0)) %>%
  ggvis(x=~x,y=~y) %>%
  add_axis(type="x",title="NO DATA") %>%
  add_axis(type="y",title="NO DATA") %>%
  layer_points()

zeigeWert <- function(dataFramito) {
  if(is.null(dataFramito)) {return(NULL)}
  else {return(paste(dataFramito$zZeig,dataFramito$xZeig,dataFramito$yZeig,sep=" , "))}
}

getGgvisPlot <- function(df,trend=TRUE,farbPalette="rainbow",gesamtgruppenname="_ALLE_"
                         ,aggFun=sum,maxPunktGroesse=2^13,sizeFactor=c(1,1),minNumRows=1
                         ,xTitel="",yTitel="",plotTitel="",showValueFun=standardShowValueFun) {
  ### df: DataFrame with 3 columns c("Gruppe","x","y")
  ### gruppenfarben: colour vector of length number of Gruppe
  ### gesamtgruppenname: name der Gruppe, die die Aggregation darstellt
  ### aggFun: Aggregierungsfunktion für Gesamtgruppe
  
  nrowDf <- nrow(x=df)
  logg("getGgvisPlot",paste("Plotting data frame with",getDfRowsColMsg(df)))
  
  if (minNumRows>nrowDf) {
    logg("getGgvisPlot","Input data frame has too few rows. Returning empty plot")
    resultat <- emptyPlot
  } else {
    gruppen <- unique(df$Gruppe)
    numGruppen <- length(gruppen)
    gruppenfarben <- getFarben(farbPalette=farbPalette,numCols=max(numGruppen,2))
    trendFarben<-gruppenfarben

    if (trend & 3*numGruppen>nrowDf) {
      ### When too little rows then omit trend
      logg("getGgvisPlot","Input data frame has too few rows. Omitting trend")
      trend <- FALSE
    }
    if (trend & !(class(df$x) %in% c("Date","integer","numeric"))) {
      trend <- FALSE
    }
    loggVar("getGgvisPlot",trend)
    if (trend) {df <- completeGroupedDataFrame(df=df,fillValue=0) %>% sortFrameByFstCol}
    if ((1 < length(gruppen)) & !is.na(gesamtgruppenname)) {
      gruppenfarben <- c(gruppenfarben,"black")
      trendFarben <- gruppenfarben
      gesamtGruppeDf <- aggDf(x=list(y=df$y),by=list(x=df$x),FUN=aggFun)
      gesamtGruppeDf$Gruppe <- gesamtgruppenname
      df <- rbind(x=df,y=sortFrameByFstCol(dataFrame=gesamtGruppeDf))
    }
    
    gruppen <- unique(df$Gruppe)
    numGruppen <- length(gruppen)
    punktgroesse <- 1 + (maxPunktGroesse-1)/nrowDf
    
    logg("getGgvisPlot",msg="Calling function ggvis.")
    resultat <- ggvis(df,x=~x,y=~y) %>%
      group_by(Gruppe) %>%
      layer_points(x=~x,y=~y,size:=punktgroesse,stroke:=NA,fill=~Gruppe)%>%
      set_options(height=sizeFactor[2]*5*2^7, width=sizeFactor[1]*3*2^9,renderer="svg")
    
    logg("getGgvisPlot",msg="Adding to plot legend, axis titles and tooltips.")
    resultat <- add_legend(vis=resultat,title="",scales="fill") %>%
      hide_legend(scales="stroke") %>%
      add_axis(type="x",title=xTitel) %>%
      add_axis(type="y",title=yTitel,title_offset=8*max(nchar(df$y))) %>%
      add_axis("x",orient="top",ticks=0
               ,title=plotTitel
               ,properties=axis_props(axis=list(stroke="white")
                                      ,labels=list(fontSize=0)
                                      ,title=list(fontSize=16,fontWeight="normal"))) %>%
      add_tooltip(html=showValueFun,on="hover") %>%
      add_tooltip(html=showValueFun,on="click")
    
    resultat <- scale_nominal(vis=resultat,property="fill",domain=gruppen,range=gruppenfarben)
    
    if (trend) {
      resultat <- layer_smooths(vis=resultat,stroke=~Gruppe) %>%
        scale_nominal(property="stroke",domain=gruppen,range=trendFarben)
    }
  }
  logg("getGgvisPlot",msg="return(resultat)")
  return(resultat)
}
