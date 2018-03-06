datos=read.csv("datos.csv",header = TRUE)
datos$date=as.POSIXct(strptime(datos$date,"%Y-%m-%d %H:%M:%S","UTC"))
datos$PM10=as.numeric(datos$PM10)
datos$Anual=factor(format(datos$date,"%Y"))
datos$dia=factor(format(datos$date,"%d"))
datosm=read.csv("mapa.csv",header = TRUE)
datosm$date=as.POSIXct(strptime(datosm$date,"%d/%m/%Y %H:%M","UTC"))
datosm$pm10=as.numeric(datosm$pm10)
library(dygraphs)
library(datasets)
library(xts)
library(openair)
library(shiny)
library(leaflet)
library(shinydashboard)

shinyServer(function(input, output, session) {

  ####################SERIE DE TIEMPOS#######################
  datos1 <- reactive({
    if(input$V3=="2011"){d1=subset(datos,Anual=="2011")} 
    else if(input$V3=="2012"){d1=subset(datos,Anual=="2012")}
    else if(input$V3=="2013"){d1=subset(datos,Anual=="2013")}
    else if(input$V3=="2014"){d1=subset(datos,Anual=="2014")}
    else if(input$V3=="2015"){d1=subset(datos,Anual=="2015")}
    else if(input$V3=="2016"){d1=subset(datos,Anual=="2016")}
    else if(input$V3=="2017"){d1=subset(datos,Anual=="2017")}
    
    
    if(input$V2=="SO2"){
      yts=xts(d1[,2],order.by = d1[,1])}
    else if(input$V2=="NO2"){
      yts=xts(d1[,3],order.by = d1[,1])}
    else if(input$V2=="CO"){
      yts=xts(d1[,4],order.by = d1[,1])}
    else if(input$V2=="Ozono"){
      yts=xts(d1[,5],order.by = d1[,1])}
    else if(input$V2=="PM10"){
      yts=xts(d1[,6],order.by = d1[,1])}
    yts
  })
  
  output$dygraph <- renderDygraph({
    dygraph(datos1()) %>%
      dyRangeSelector(height = 20)
 
    
    ####################BOXPLOT###############################  
  })
  datos2 <- reactive({
    if(input$B3=="2011"){d2=subset(datos,Anual=="2011")} 
    else if(input$B3=="2012"){d2=subset(datos,Anual=="2012")}
    else if(input$B3=="2013"){d2=subset(datos,Anual=="2013")}
    else if(input$B3=="2014"){d2=subset(datos,Anual=="2014")}
    else if(input$B3=="2015"){d2=subset(datos,Anual=="2015")}
    else if(input$B3=="2016"){d2=subset(datos,Anual=="2016")}
    else if(input$B3=="2017"){d2=subset(datos,Anual=="2017")}
    d2
    
  }) 
  output$plot1 <- renderPlot({ 
    if(input$B2=="SO2"){
      if(length(na.omit(datos2()[,2]))==0){plot.new()}else{
      plot(factor(format(datos2()[,1], "%b"),labels=c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")),datos2()[,2])}}
    
    else if(input$B2=="NO2"){
      if(length(na.omit(datos2()[,3]))==0){plot.new()}else{
      plot(factor(format(datos2()[,1], "%b"),labels=c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")),datos2()[,3])}}
    else if(input$B2=="CO"){
      if(length(na.omit(datos2()[,4]))==0){plot.new()}else{
      plot(factor(format(datos2()[,1], "%b"),labels=c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")),datos2()[,4])}}
    else if(input$B2=="Ozono"){
      if(length(na.omit(datos2()[,5]))==0){plot.new()}else{
      plot(factor(format(datos2()[,1], "%b"),labels=c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")),datos2()[,5])}}
    else if(input$B2=="PM10"){
      if(length(na.omit(datos2()[,5]))==0){plot.new()}else{
      plot(factor(format(datos2()[,1], "%b"),labels=c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")),datos2()[,6])}}
 
     })
  #############################VARIACION TEMPORAL#########################################
  datos3 <- reactive({
    if(input$C3=="2011"){d3=subset(datos,Anual=="2011")} 
    else if(input$C3=="2012"){d3=subset(datos,Anual=="2012")}
    else if(input$C3=="2013"){d3=subset(datos,Anual=="2013")}
    else if(input$C3=="2014"){d3=subset(datos,Anual=="2014")}
    else if(input$C3=="2015"){d3=subset(datos,Anual=="2015")}
    else if(input$C3=="2016"){d3=subset(datos,Anual=="2016")}
    else if(input$C3=="2017"){d3=subset(datos,Anual=="2017")}
    d3
  })
  output$plot2 <- renderPlot({
    if(input$C2=="SO2"){
      if(length(na.omit(datos3()[,2]))==0){plot.new()}else{
      timeVariation (datos3(), pollutant=c("SO2"), cols=c("dodgerblue4"), alpha=0.2, main="EVOLUCION DE SO2",
                   ylab="Concentracion en ppm", xlab=c("Evolucion de las concentraciones horarias durante la semana",
                   "Concentraciones horarias", "Concentraciones mensuales", "Evolucion por dias de la semana"))}}
    else if(input$C2=="NO2"){
      if(length(na.omit(datos3()[,3]))==0){plot.new()}else{
      timeVariation (datos3(), pollutant=c("NO2"), cols=c("dodgerblue4"), alpha=0.2, main="EVOLUCION DE NO2",
                     ylab="Concentracion en ppm", xlab=c("Evolucion de las concentraciones horarias durante la semana",
                     "Concentraciones horarias", "Concentraciones mensuales", "Evolucion por dias de la semana"))}}
    else if(input$C2=="CO"){
      if(length(na.omit(datos3()[,4]))==0){plot.new()}else{
      timeVariation (datos3(), pollutant=c("CO"), cols=c("dodgerblue4"), alpha=0.2, main="EVOLUCION DE CO",
                     ylab="Concentracion en ppm", xlab=c("Evolucion de las concentraciones horarias durante la semana",
                     "Concentraciones horarias", "Concentraciones mensuales", "Evolucion por dias de la semana"))}}
    else if(input$C2=="Ozono"){
      if(length(na.omit(datos3()[,5]))==0){plot.new()}else{
      timeVariation (datos3(), pollutant=c("O3"), cols=c("dodgerblue4"), alpha=0.2, main="EVOLUCION DE OZONO",
                     ylab="Concentracion en ppm", xlab=c("Evolucion de las concentraciones horarias durante la semana",
                      "Concentraciones horarias", "Concentraciones mensuales", "Evolucion por dias de la semana"))}}
    else if(input$C2=="PM10"){
      if(length(na.omit(datos3()[,6]))==0){plot.new()}else{
      timeVariation (datos3(), pollutant=c("PM10"), cols=c("dodgerblue4"), alpha=0.2, main="EVOLUCION DE PM10",
                     ylab="Concentracion en ppm", xlab=c("Evolucion de las concentraciones horarias durante la semana",
                     "Concentraciones horarias", "Concentraciones mensuales", "Evolucion por dias de la semana"))}}
     }) 
  ###############################RESPIRA BICONTAMINANTE#################################################
datos4 <- reactive({
  if(input$D3=="2011"){d4=subset(datos,Anual=="2011")} 
  else if(input$D3=="2012"){d4=subset(datos,Anual=="2012")}
  else if(input$D3=="2013"){d4=subset(datos,Anual=="2013")}
  else if(input$D3=="2014"){d4=subset(datos,Anual=="2014")}
  else if(input$D3=="2015"){d4=subset(datos,Anual=="2015")}
  else if(input$D3=="2016"){d4=subset(datos,Anual=="2016")}
  else if(input$D3=="2017"){d4=subset(datos,Anual=="2017")}
  d4
})
  datos5 <- reactive({
    a1=rollingMean(datos4(), pollutant="O3", width=3, new.name="pmO3", data.thresh=0,align = "right")
    index=which(is.na(a1$O3))
    index
    a1$pmO3[index]=NA
    o3=round(a1$pmO3,digits = 3)
    
    a2=rollingMean(datos4(), pollutant="PM10", width=3, new.name="pm", data.thresh=0,align = "right")
    index=which(is.na(a2$PM10))
    index
    a2$pm[index]=NA
    pm10=round(a2$pm,digits = 3)
    
    A1=ifelse(pm10<=50,4,ifelse(pm10<=75,3,ifelse(pm10<=100,2,ifelse(pm10>=101,1,0))))
    B1=ifelse(o3<=0.051,4,ifelse(o3<=0.070,3,ifelse(o3<=0.092,2,ifelse(o3>=0.093,1,0))))
    Categoria=ifelse(A1>=B1,A1,B1)
    Nuevo=data.frame(datos4(),Categoria)
  })
  output$plot3 <- renderPlot({
    if(input$D2=="O3-PM10"){
      trendLevel(datos5(), pollutant = "Categoria",x="hour",y="dia",type="month",
                 main="Bicontaminante O3-PM10",
                 key.header = "Riesgo a la Salud",key.footer = "",
                 border = "white", statistic = "max",
                 breaks = c(0, 1, 2, 3,4),
                 labels = c("Muy alto", "Alto", "Moderado","Aceptable"),
                 cols = c("gray", "red", "yellow","blue"))}
    else if(input$D2=="PM10"){ }
    
  })  
  ################################### RESPIRA UNICONTAMINANTE##################################
  datos6 <- reactive({
    if(input$E3=="2011"){d6=subset(datos,Anual=="2011")} 
    else if(input$E3=="2012"){d6=subset(datos,Anual=="2012")}
    else if(input$E3=="2013"){d6=subset(datos,Anual=="2013")}
    else if(input$E3=="2014"){d6=subset(datos,Anual=="2014")}
    else if(input$E3=="2015"){d6=subset(datos,Anual=="2015")}
    else if(input$E3=="2016"){d6=subset(datos,Anual=="2016")}
    else if(input$E3=="2017"){d6=subset(datos,Anual=="2017")}
    d6
  })
  output$plot4 <- renderPlot({
    if(input$E2=="SO2"){
      if(length(na.omit(datos6()[,2]))==0){plot.new()}else{
      E=ifelse(datos6()[,2]<=0.008,4,ifelse(datos6()[,2]<=0.019,3,ifelse(datos6()[,2]<=0.048,2,ifelse(datos6()[,2]>=0.049,1,0))))
      nuevo=data.frame(datos6(),E)
      trendLevel(nuevo, pollutant = "E",x="hour",y="dia",type="month",
                                 main="Unicontaminante SO2",
                                   key.header = "Riesgo a la Salud",key.footer = "",
                                   border = "white", statistic = "max",
                                   breaks = c(0, 1, 2, 3,4),
                                   labels = c("Muy alto", "Alto", "Moderado","Aceptable"),
                                   cols = c("gray", "red", "yellow","blue"))}}
      else if(input$E2=="NO2"){
        if(length(na.omit(datos6()[,3]))==0){plot.new()}else{
      E=ifelse(datos6()[,3]<=0.106,4,ifelse(datos6()[,3]<=0.210,3,ifelse(datos6()[,3]<=0.230,2,ifelse(datos6()[,3]>=0.231,1,0))))
      nuevo=data.frame(datos6(),E)
      trendLevel(nuevo, pollutant = "E",x="hour",y="dia",type="month",
                 main="Unicontaminante NO2",
                 key.header = "Riesgo a la Salud",key.footer = "",
                 border = "white", statistic = "max",
                 breaks = c(0, 1, 2, 3,4),
                 labels = c("Muy alto", "Alto", "Moderado","Aceptable"),
                 cols = c("gray", "red", "yellow","blue"))}}
      else if(input$E2=="CO"){
        if(length(na.omit(datos6()[,4]))==0){plot.new()}else{
        E=ifelse(datos6()[,4]<=8.7,4,ifelse(datos6()[,4]<=11.0,3,ifelse(datos6()[,4]<=13.3,2,ifelse(datos6()[,4]>=13.4,1,0))))
        nuevo=data.frame(datos6(),E)
        trendLevel(nuevo, pollutant = "E",x="hour",y="dia",type="month",
                   main="Unicontaminante CO",
                   key.header = "Riesgo a la Salud",key.footer = "",
                   border = "white", statistic = "max",
                   breaks = c(0, 1, 2, 3,4),
                   labels = c("Muy alto", "Alto", "Moderado","Aceptable"),
                   cols = c("gray", "red", "yellow","blue"))}
      }
   })
  
  ######################## MAPA IMECA ###################################
  
  a1=rollingMean(datosm, pollutant="so2", width=24, new.name="pmso2", data.thresh=75,align = "right")
  index=which(is.na(a1$so2))
  index
  a1$pmso2[index]=NA
  so2a=round(a1$pmso2,digits = 3)
  SO2=so2a*100/0.13
  
  no2a=round(datosm$no2,digits = 3)
  NO2=no2a*100/0.21
  
  a2=rollingMean(datosm, pollutant="co", width=8, new.name="pmco", data.thresh=75,align = "right")
  index=which(is.na(a2$co))
  index
  a2$pmco[index]=NA
  coa=round(a2$pmco,digits = 2)
  CO=coa*100/11
  
  o3a=round(datosm$o3,digits = 3)
  K=ifelse(o3a<=0.070,714.29,ifelse(o3a<=0.095,2041.67,ifelse(o3a<=0.154,844.83,
                                                              ifelse(o3a<=0.204,1000.00,ifelse(o3a<=0.404,497.49,ifelse(o3a<=0.504,1000.00,1000.00))))))
  
  BPL0=ifelse(o3a<=0.070,0,ifelse(o3a<=0.095,0.071,ifelse(o3a<=0.154,0.096,
                                                          ifelse(o3a<=0.204,0.155,ifelse(o3a<=0.404,0.205,ifelse(o3a<=0.504,0.405,0.505))))))
  
  IL0=ifelse(o3a<=0.070,0,ifelse(o3a<=0.095,51,ifelse(o3a<=0.154,101,
                                                      ifelse(o3a<=0.204,151,ifelse(o3a<=0.404,201,ifelse(o3a<=0.504,301,401))))))
  
  O3=(K*(o3a-BPL0))+IL0
  
  a3=rollingMean(datosm, pollutant="pm10", width=24, new.name="pm", data.thresh=75,align = "right")
  index=which(is.na(a3$pm10))
  index
  a3$pm[index]=NA
  
  pm10a=round(a3$pm,digits = 2)
  
  K2=ifelse(pm10a<=40,1.2500,ifelse(pm10a<=75,1.4412,ifelse(pm10a<=214,0.3551,
                                                            ifelse(pm10a<=354,0.3525,ifelse(pm10a<=424,1.4348,ifelse(pm10a<=504,1.2532,1.0000))))))
  
  BPL02=ifelse(pm10a<=40,0,ifelse(pm10a<=75,41,ifelse(pm10a<=214,76,
                                                      ifelse(pm10a<=354,215,ifelse(pm10a<=424,355,ifelse(pm10a<=504,425,505))))))
  
  IL02=ifelse(pm10a<=40,0,ifelse(pm10a<=75,51,ifelse(pm10a<=214,101,
                                                     ifelse(pm10a<=354,151,ifelse(pm10a<=424,201,ifelse(pm10a<=504,301,401))))))
  
  PM10=(K2*(pm10a-BPL02))+IL02
  INDICE=data.frame(datosm,SO2,NO2,CO,O3,PM10)
  IMECA=INDICE[c(2,3,9,10,11,12,13)]
  IMECA[length(IMECA$SO2),3:7]
  R=which.max(IMECA[length(IMECA$SO2),3:7])
  R1=max(round(IMECA[length(IMECA$SO2),3:7],digits = 0))
  R2=IMECA[length(IMECA$SO2),1:3]
  R3=data.frame(R2,R1)
  R3
  fecha=INDICE[length(datosm$o3),1]
  Contaminante=names(R)
  
  getColor <- function(R3) {
    sapply(R3$R1, function(R1) {
      if(R1 <= 50) {
        "green"
      } else if(R1 <= 100) {
        "yellow"
      }else if(R1 <= 150) {
        "darkorange"
      } else if(R1 <= 200) {
        "red"
      } else {
        "purple"
      } })
  }
  
  icons <- awesomeIcons(
    icon = 'ios-ionic',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(R3)
  )
  output$mymap <- renderLeaflet({
    leaflet(R3) %>% addTiles() %>%
      addCircleMarkers(
        lng=~long, lat=~lat,
        color = ~getColor(R3),radius=20,
        label=~as.character(R3))%>% 
      addCircleMarkers(
        lng=-92.954173, lat=17.989449, radius = 20,
        label='Sin datos disponibles',color ="gray")
  })
  
  output$from <- renderText({
    strftime(req(fecha), "%A %d %B %Y",tz="utc")      
  })
  output$hrs <- renderText({
    strftime(req(fecha), "%H: %M %p",tz="utc")      
  })
  
  output$to <- renderText({
    paste(Contaminante)
  })
  
  ########################ACTTIONBUTTON################################
  but1 = reactiveValues(but1=FALSE)
  but2 = reactiveValues(but2=FALSE)
  but3 = reactiveValues(but3=FALSE)
  but4 = reactiveValues(but4=FALSE)
  but5 = reactiveValues(but5=FALSE)
  
  observeEvent(input$t1,
               isolate({but1$but1=TRUE
               but2$but2=FALSE
               but3$but3=FALSE
               but4$but4=FALSE
               but5$but5=FALSE
               }))
  
  observeEvent(input$t2,
               isolate({but1$but1=FALSE
               but2$but2=TRUE
               but3$but3=FALSE
               but4$but4=FALSE
               but5$but5=FALSE
               }))
  
  observeEvent(input$t3,
               isolate({but1$but1=FALSE
               but2$but2=FALSE
               but3$but3=TRUE
               but4$but4=FALSE
               but5$but5=FALSE
               }))
  observeEvent(input$t4,
               isolate({but1$but1=FALSE
               but2$but2=FALSE
               but3$but3=FALSE
               but4$but4=TRUE
               but5$but5=FALSE
               }))
  observeEvent(input$t5,
               isolate({but1$but1=FALSE
               but2$but2=FALSE
               but3$but3=FALSE
               but4$but4=FALSE
               but5$but5=TRUE
               }))
  output$uno <- renderText({
    if(but1$but1)
      paste("Se puede realizar cualquier actividad al aire libre") 
    else
      return()
  })
  
  output$dos <- renderText({
    if(but2$but2)
      paste("Las personas que son extremadamente sensibles a la 
            contaminacion deben considerar limitar los esfuerzos 
            prolongados al aire libre.") 
    else
      return()
  })
  
  
  output$tres <- renderText({
    if(but3$but3)
      paste("Los menores, adultos mayores, personas que realizan 
            actividad fisica intensa o con enfermedades 
            respiratorias y cardiovasculares, deben limitar los 
            esfuerzos prolongados al aire libre.") 
    else
      return()
  })
  output$cuatro <- renderText({
    if(but4$but4)
      paste("Los menores, adultos mayores, personas que realizan 
            actividad fisica intensa o con enfermedades respiratorias y 
            cardiovasculares, deben evitar el esfuerzo prolongado 
            al aire libre.

            La poblacion en general debe limitar el esfuerzo 
            prolongado al aire libre") 
    else
      return()
  })
  output$cinco <- renderText({
    if(but5$but5)
      paste("La poblacion en general debe suspender los esfuerzos 
            al aire libre.") 
    else
      return()
  })
  

  })
shinyApp(shinyUI, shinyUI)