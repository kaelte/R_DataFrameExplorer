library(shiny)
library(ggvis)
library(dplyr)

#################
### functions ###
#################
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
  if(is.na(resultat)) {resultat <- ersatz}
  return(resultat)
}
getFormula <- function(operator,xVec){
  if ("none" == operator) {resultat <- xVec[1]} else {resultat <- paste(xVec,collapse=operator)}
  return(resultat)
}

getTimeFormat <- function(typeString) {return(ifelse("Date"==typeString,"%Y-%m-%d","%Y-%m-%d %H:%M:%S"))}
milliSecondsToDate <- function(t) {return(as.Date(origin="1970-01-01",x=t/(24*3600*1000)))}

milliSecondsToString <- function(t,Zeitformat) {return(format(milliSecondsToDate(t),Zeitformat))}

########################################
### functions concerning data frames ###
########################################
sortFrameByFstCol <- function(dataFrame,desc=FALSE) {
  return(dataFrame[do.call(order,c(dataFrame,list(decreasing=FALSE,method="radix"))),])
}
replaceNaInDf <- function(df,cols,replacement) {
  resultat <- df
  if (0==nrow(df)){
    print("replaceNaInDf: Input data frame is empty. Nothing to do!")
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
  gruppen <- unique(df$Gruppe)
  xWerte <- unique(df$x)
  yWerte <- as.vector(unique(df$y))
  missingLines <- setdiff(x=expand.grid(Gruppe=gruppen,x=xWerte),y=df[,c("Gruppe","x")])
  if (0<nrow(missingLines)) { missingLines$y <- fillValue
  }
  resultat <- rbind(x=replaceNaInDf(df=df,cols=c("y"),replacement=fillValue),y=missingLines)
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
##################################
### functions concerning plots ###
##################################
emptyPlot <- data.frame(x=c(0),y=c(0)) %>%
  ggvis(x=~x,y=~y) %>%
  add_axis(type="x",title="NO DATA") %>%
  add_axis(type="y",title="NO DATA") %>%
  layer_points()

zeigeWert <- function(dataFramito) {
  if(is.null(dataFramito)) {return(NULL)}
  else {return(paste(dataFramito$zZeig,dataFramito$xZeig,dataFramito$yZeig,sep=" , "))}
}

getTrendPlotGgvis <- function(df,trend=TRUE,farbPalette="rainbow",gesamtgruppenname="_ALLE_"
                              ,aggFun=sum,maxPunktGroesse=2^13,sizeFactor=c(1,1),minNumRows=1
                              ,xTitel="",yTitel="",plotTitel="",showValueFun=standardShowValueFun) {
  ### df: DataFrame with 3 columns c("Gruppe","x","y")
  ### gruppenfarben: colour vector of length number of Gruppe
  ### gesamtgruppenname: name der Gruppe, die die Aggregation darstellt
  ### aggFun: Aggregierungsfunktion für Gesamtgruppe
  
  gruppen <- unique(df$Gruppe)
  numGruppen <- length(gruppen)
  gruppenfarben <- getFarben(farbPalette=farbPalette,numCols=max(numGruppen,2))
  trendFarben<-gruppenfarben
  
  nrowDf <- nrow(x=df)
  if (minNumRows>nrowDf) {
    resultat <- emptyPlot
  } else {
    if (trend & 3*numGruppen>nrowDf) {
      ### When too little rows then omit trend
      trend <- FALSE
    }
    if (trend & !(class(df$x) %in% c("Date","integer","numeric"))) {
      trend <- FALSE
    }
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
    
    resultat <- ggvis(df,x=~x,y=~y) %>%
      group_by(Gruppe) %>%
      layer_points(x=~x,y=~y,size:=punktgroesse,stroke:=NA,fill=~Gruppe)%>%
      set_options(height=sizeFactor[2]*5*2^7, width=sizeFactor[1]*3*2^9,renderer="svg")
    
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
  return(resultat)
}


###############
### inputDf ###
###############
dataTypes <- getColTypes(df=inputDf)
kpiTypes <- c("integer","numeric")
timeTypes <- c("Date","POSIXct","POSIXt")
dims <- as.vector(dataTypes[!(dataTypes$COLTYP %in% kpiTypes),c("COLNAME")])
kpis <- as.vector(dataTypes[dataTypes$COLTYP %in% kpiTypes,c("COLNAME")])
grps <- as.vector(dataTypes[dataTypes$NUM_UNIQUE < 9,c("COLNAME")])
timCols <- as.vector(dataTypes[dataTypes$COLTYP %in% timeTypes,c("COLNAME")])

getData <- function(dimensionen,desiredKpis) {
  ### function to aggregate inputDf to the desired dimensions
  print(paste("getData: dimensionen",dimensionen,sep="="))
  print(paste("getData: desiredKpis",desiredKpis,sep="="))
  foundKpis <- intersect(x=kpis,y=desiredKpis)
  print(paste("getData: foundKpis",foundKpis,sep="="))
  if (length(dimensionen)==length(dims)) {
    resultat <- inputDf[,c(dimensionen,foundKpis)]
  } else {
    resultat <- aggDf(x=inputDf[,foundKpis],by=inputDf[,dimensionen],FUN=sum) %>% sortFrameByFstCol
    ### Wenn nur 1 kpi ausgewaehlt, dann heisst diese Spalte x, was aber nicht sein sollte
    if (1==length(foundKpis)) {colnames(resultat) <- gsub(x=colnames(resultat),pattern="x",replacement=foundKpis[1])}
  }
  return(resultat)
}




##################################
### functions concerning Shiny ###
##################################
tempusSelector <- function(INPUTID,NOMEN,AB,USQUEAD,INITIUM,tf){
  sliderInput(inputId=INPUTID
              ,label=NOMEN
              ,min=AB
              ,max=USQUEAD
              ,timeFormat=tf
              ,value=INITIUM
              ,step=1
              ,width='100%')}

getSelector <- function(INPUTID,NOMEN,delectendum) {
  return(selectInput(inputId=INPUTID
                     ,label=NOMEN
                     ,selected=delectendum[1]
                     ,selectize=TRUE
                     ,multiple=FALSE
                     ,choices=delectendum))
}

getFilter <- function(df,INPUTID,columna){
  if (!(columna %in% colnames(df))) {
    columna <- colnames(df)[1]
  }
  
  if (columna %in% timCols) {
    columnaValues <- unique(df[,c(columna)])
    columnaValues <- columnaValues[!is.na(columnaValues)]
    if (0==length(columnaValues)) {
      AB=Sys.Date()
      USQUEAD=Sys.Date()
    } else {
      AB=min(columnaValues)
      USQUEAD=max(columnaValues)
    }
    timeFormat <- ifelse("Date"==class(columnaValues),"%d/%m/%y","%y-%m-%d %H:%M")
    resultat <- tempusSelector(INPUTID=INPUTID,NOMEN=columna,AB=AB,USQUEAD=USQUEAD,INITIUM=c(AB,USQUEAD)
                               ,tf=timeFormat)
    
  } else if (class(df[,c(columna)]) %in% c("integer","numeric")) {
    range <- as.vector(df[,c(columna)])
    minimum <- min(range)
    maximum <- max(range)
    resultat <- sliderInput(inputId=INPUTID
                            ,label=columna
                            ,min=minimum
                            ,max=maximum
                            ,value=c(minimum,maximum)
                            ,width='100%')
  } else {
    auswahl <- as.vector(sort(unique(df[,c(columna)])))
    resultat <- myCheckboxGroupInput(INPUTID=INPUTID,delectenda=auswahl,NOMEN=columna,iL=TRUE,anzSelected=-1)
  }
  return(resultat)
}

myCheckboxGroupInput <- function(INPUTID,delectenda,NOMEN="select",iL=TRUE,anzSelected=4){
  if (anzSelected<0) {
    checkboxGroupInput(inputId=INPUTID
                       ,label=NOMEN
                       ,choices=delectenda
                       ,selected=delectenda
                       ,inline=iL)
  } else { checkboxGroupInput(inputId=INPUTID
                              ,label=NOMEN
                              ,choices=delectenda
                              ,selected=head(x=delectenda,n=anzSelected)
                              ,inline=iL) }
}

serverFunction <- function(input, output) {
  
  columUnum <- reactive(x={return(getNonNullValue(wert=input$columUnum,ersatz=kpis[1]))})
  
  columDuo <- reactive(x={return(getNonNullValue(wert=input$columDuo,ersatz=kpis[length(kpis)]))})
  
  abszissenCols  <- reactive(x={
    if ("none" == input$abszissenOP) {
      result <- getNonNullValue(wert=input$Abszisse,ersatz=kpis[length(kpis)])
    } else {
      result <- c(getNonNullValue(wert=input$AbszisseOne,ersatz=kpis[length(kpis)])
                  ,getNonNullValue(wert=input$AbszisseTwo,ersatz=kpis[length(kpis)]))
    }
    return(result)
  })
  
  ordinatenCols  <- reactive(x={
    if ("none" == input$ordinatenOP) {
      result <- getNonNullValue(wert=input$Ordinate,ersatz=kpis[length(kpis)])
    } else {
      result <- c(getNonNullValue(wert=input$OrdinateOne,ersatz=kpis[length(kpis)])
                  ,getNonNullValue(wert=input$OrdinateTwo,ersatz=kpis[length(kpis)]))
    }
    return(result)
  })

  analyseWuerfel <- reactive(x={
    input$setDimButton
    isolate(expr={
      resultat <- getData(dimensionen=input$dim
                          ,desiredKpis=c(columUnum(),columUnum(),columDuo()))
    })
    return(resultat)
  })
  
  output$abszissenSelector <- renderUI({
    input$abszissenOP
    input$setDimButton
    isolate(expr={
      nomen <- "Abszisse / abscisse"
      if ("none" == input$abszissenOP) {
        getSelector(INPUTID="Abszisse",NOMEN=nomen,delectendum=c(input$dim,kpis))
      } else {
        fluidRow(column(width=6,getSelector(INPUTID="AbszisseOne",NOMEN=paste(nomen,"1"),delectendum=kpis))
                 ,column(width=6,getSelector(INPUTID="AbszisseTwo",NOMEN=paste(nomen,"2"),delectendum=kpis))
        )
      }
    })
  })
  
  output$ordinatenSelector <- renderUI({
    input$ordinatenOP
    input$setDimButton
    isolate(expr={
      nomen <- "Ordinate / ordonnée"
      if ("none" == input$ordinatenOP) {
        getSelector(INPUTID="Ordinate",NOMEN=nomen,delectendum=c(input$dim,kpis))
      } else {
        fluidRow(column(width=6,getSelector(INPUTID="OrdinateOne",NOMEN=paste(nomen,"1"),delectendum=kpis))
                 ,column(width=6,getSelector(INPUTID="OrdinateTwo",NOMEN=paste(nomen,"2"),delectendum=kpis))
        )
      }
    })
  })
  
  output$gruppenSelector <- renderUI({
    input$setDimButton
    isolate(expr={
      delectendum <- intersect(x=c(input$dim,kpis),y=grps)
      getSelector(INPUTID="Gruppierung",NOMEN="Gruppierung / groupement",delectendum=delectendum)
    })
  })
  
  output$columUnumSelector <- renderUI({
    input$setDimButton
    isolate(expr={
      delectendum <- c(input$dim,kpis)
      getSelector(INPUTID="columUnum",NOMEN="Filter 1/ filtre 1",delectendum=delectendum)
    })
  })
  
  output$columDuoSelector <- renderUI({
    input$setDimButton
    isolate(expr={
      delectendum <- c(input$dim,kpis)
      getSelector(INPUTID="columDuo",NOMEN="Filter 2/ filtre 2",delectendum=delectendum)
    })
  })
  
  output$columUnumFilter <- renderUI({getFilter(df=analyseWuerfel(),INPUTID="anacolumUnum",columna=columUnum())})
  
  output$columDuoFilter <- renderUI({getFilter(df=analyseWuerfel(),INPUTID="anacolumDuo",columna=columDuo())})
  
  analyseDaten <- reactive(x={
    input$goButton
    resultat <- data.frame(x=as.Date(origin="1970-01-01",as.numeric()),y=as.numeric(),Gruppe=as.character())
    isolate(expr={
      abszissenData <- abszissenCols()
      ordinatenData <- ordinatenCols()
      colUnumData <- c(columUnum())
      colDuoData <- c(columDuo())
      gruppierung <- getNonNullValue(wert=input$Gruppierung,ersatz=grps[1])
      anacolumUnum <- getNonNullValue(wert=input$anacolumUnum,ersatz=c(0,1))
      anacolumDuo <- getNonNullValue(wert=input$anacolumDuo,ersatz=c(0,1))
      neededKpis <- intersect(x=kpis,y=c(abszissenData,ordinatenData,gruppierung,colUnumData,colDuoData))
      granular <- getData(dimensionen=input$dim,desiredKpis=neededKpis)

      # ColumnUnum
      granular$cUnum <- as.vector(granular[,c(colUnumData)])
      granular <- granular[!is.na(granular$cUnum),]
      # ColumnDuo
      granular$cDuo <- as.vector(granular[,c(colDuoData)])
      granular <- granular[!is.na(granular$cDuo),]
      
      colsToSelect <- as.vector(unique(c(abszissenData,ordinatenData,"cDuo",gruppierung)))

      # Filter 1 anwenden
      granColUnum <- as.vector(granular$cUnum)

      if (class(granColUnum) %in% c(kpiTypes,timeTypes)) {
        granular <- granular[(anacolumUnum[1]<=granColUnum) & (granColUnum<=anacolumUnum[2]),colsToSelect]
      } else {
        granular <- granular[granColUnum %in% anacolumUnum,colsToSelect]
      }

      # Filter 2 anwenden
      insColDuo <- as.vector(granular$cDuo)
      
      if (class(insColDuo) %in% c(kpiTypes,timeTypes)) {
        granular <- granular[(input$anacolumDuo[1]<=insColDuo) & (insColDuo<=input$anacolumDuo[2]),colsToSelect]
      } else {
        granular <- granular[insColDuo %in% input$anacolumDuo,colsToSelect]
      }

      if ("none" == input$abszissenOP) {
        granular$x <- granular[,c(abszissenData)]
      } else {
        granular <- opColumn(dataFrame=granular,operator=input$abszissenOP,fstCol=abszissenData[1],sndCol=abszissenData[2],newCol="x")
      }
      
      if ("none" == input$ordinatenOP) {
        granular$y <- granular[,c(ordinatenData)]
      } else {
        granular <- opColumn(dataFrame=granular,operator=input$ordinatenOP,fstCol=ordinatenData[1],sndCol=ordinatenData[2],newCol="y")
      }
      granular <- granular[!is.na(granular$y),]

      if (class(granular$y) %in% kpiTypes) {
        byFrame <- granular[,c("x",gruppierung)]
        resultat <- aggDf(x=list(y=granular$y),by=byFrame,FUN=input$aggFun)
      } else {
        resultat <- unique(granular)
      }

      # resultat$Gruppe <- as.vector(as.character(resultat[,c(gruppierung)]))
      resultat$Gruppe <- resultat[,c(gruppierung)]
      resultat <- sortFrameByFstCol(dataFrame=resultat[!is.na(resultat$y),c("Gruppe","x","y")])
    })
    return(resultat)
  })
  
  observe(x={
    input$goButton
    isolate(expr={
      xTitel <- getFormula(operator=input$abszissenOP,xVec=abszissenCols())
      yTitel <- getFormula(operator=input$ordinatenOP,xVec=ordinatenCols())
      gruppierung <- getNonNullValue(wert=input$Gruppierung,ersatz=grps[1])
      titel <- ""
      analyseShowValueFun <- function(df){
        if ("y" %in% colnames(x=df)) {
          xClass <- class(inputDf[[abszissenCols()[1]]])
          if (xClass %in% timeTypes) {df$xZeig <- milliSecondsToString(t=df$x,Zeitformat=getTimeFormat(xClass))} else {df$xZeig <- df$x}
          yClass <- class(inputDf[[ordinatenCols()[1]]])
          if (yClass %in% timeTypes) {df$yZeig <- milliSecondsToString(t=df$y,Zeitformat=getTimeFormat(yClass))} else {df$yZeig <- df$y}
          zClass <- class(inputDf[[c(gruppierung)]])
          if (zClass %in% timeTypes) {df$zZeig <- milliSecondsToString(t=df$Gruppe,Zeitformat=getTimeFormat(zClass))} else {df$zZeig <- df$Gruppe}
          resultat <-zeigeWert(dataFramito=df)
        } else {resultat <- NA}
        return(resultat)
      }
      
      getTrendPlotGgvis(df=analyseDaten(),trend=as.logical(input$Analysentrend)
                        ,xTitel=xTitel,yTitel=yTitel,plotTitel=titel,gesamtgruppenname=NA
                        ,sizeFactor=c(6/5,8/9),showValueFun=analyseShowValueFun) %>%
        bind_shiny("analysePlot")
    })
  })
}

uiFunction <- function() {
  return(fluidPage(title="Data Frame Analyser"
                   ,fluidRow(column(width=10
                                    ,myCheckboxGroupInput(INPUTID="dim"
                                                          ,delectenda=dims
                                                          ,NOMEN="Dimensions"
                                                          ,iL=TRUE))
                             ,column(width=2,actionButton(inputId="setDimButton",label="set dim"))
                   )
                   ,fluidRow(column(width=1,selectInput(inputId="abszissenOP"
                                                        ,label="operator"
                                                        ,selected="none"
                                                        ,selectize=FALSE
                                                        ,multiple=FALSE
                                                        ,choices=c("none","+","-","*","/")))
                             ,column(width=5,uiOutput("abszissenSelector"))
                             ,column(width=1,selectInput(inputId="ordinatenOP"
                                                         ,label="operator"
                                                         ,selected="none"
                                                         ,selectize=FALSE
                                                         ,multiple=FALSE
                                                         ,choices=c("none","+","-","*","/")))
                             ,column(width=5,uiOutput("ordinatenSelector"))
                   )
                   ,fluidRow(column(width=2,uiOutput("gruppenSelector"))
                             ,column(width=2,uiOutput("columUnumSelector"))
                             ,column(width=2,uiOutput("columDuoSelector"))
                             ,column(width=2,selectInput(inputId="aggFun"
                                                         ,label="aggregieren / agréger"
                                                         ,selected="sum"
                                                         ,selectize=FALSE
                                                         ,multiple=FALSE
                                                         ,choices=c("sum","mean","median","min","max"
                                                                    ,"sd","var","IQR"))
                             )
                   )
                   ,fluidRow(column(width=5,uiOutput("columUnumFilter"))
                             ,column(width=5,uiOutput("columDuoFilter"))
                             ,column(width=1,selectInput(inputId="Analysentrend"
                                                         ,label="Trend"
                                                         ,multiple=FALSE
                                                         ,choices=as.logical(c(FALSE,TRUE))
                                                         ,selectize=TRUE
                                                         ,selected=FALSE))
                             ,column(width=1,actionButton(inputId="goButton",label="Go!"))
                   )
                   ,fluidRow(column(width=12,ggvisOutput(plot_id="analysePlot")))
                   
  ))}
