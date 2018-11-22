library(shiny)
library(ggvis)
library(dplyr)

source(local=TRUE,file="basicFunctions.R")
source(local=TRUE,file="dataFrameFunctions.R")
source(local=TRUE,file="plotFunctions.R")
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
      
      getGgvisPlot(df=analyseDaten(),trend=as.logical(input$Analysentrend)
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
