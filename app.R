library(shiny)
library(shinydashboard)
library(Rthingsboard)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(rsconnect)

url = "http://scada.g-eau.fr"
publicId = "299cedc0-f3e9-11e8-9dbf-cbc1e37c11e3" 
nots <<- list(notificationItem(text = "Bienvenue sur l'interface", icon("check"), status = "success"))

ReverseLines <- function(x){
  nrx <- nrow(x)
  temp <- x[nrx,]
  j <- 1
  for(i in nrx:1){
    temp[j,] <- x[i,]
    j <- j+1
  } 
  x <- temp
  return(x)
}

InList <- function(x, key) {
  for (i in 0:length(x)+1) {
    if (x[i] == key) {
      return (i)
      }
  }
  return (-1)
}

tb_api = ThingsboardApi(url = url, publicId = publicId)
choices = list("je", "suis", "beau")
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = tags$div(img(src="logo_hallehydraulique_mini.png", width ="100px"), " - Projet hubIS"), #icon("database")
                  titleWidth = "300px",  
                  dropdownMenuOutput("notifs")),
  dashboardSidebar(
    width = "250px",
    sidebarMenu(
      menuItem("Lecture/ouverture des données", tabName = "readData", icon = icon("readme")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("poll")),
      menuItem("Liste des dispositifs disponibles", tabName = "listeCannes", icon = icon("list")),
      menuItem("À propos", tabName = "info", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Read data
      tabItem(tabName = "readData",
              fluidRow(
                column(3,
                  h1("Lecture des données"),
                  h4("Paramètres de configuration"),
                  br(),
                  dateInput("dateInputStart", "Date de début de l'acquisition des données", value = Sys.Date()-1, min = "2020-01-01", max = Sys.Date(), weekstart = 1, format = "yyyy-mm-dd", language = "fr", width = "100%"),
                  fluidRow(
                    column(4, numericInput("heureStart", label = "Heure :", value = "00", min = 0, max = 24, width = "100%")),
                    column(4, numericInput("minStart", label = "Min :", value = "00", min = 0, max = 60, width = "100%")),
                    column(4, numericInput("secStart", label = "Sec :", value = "00", min = 0, max = 60, width = "100%")),
                  ),
                  br(),
                  dateInput("dateInputEnd", "Date de fin de l'acquisition des données", value = Sys.Date(), min = "2020-01-01", max = Sys.Date(), weekstart = 1, format = "yyyy-mm-dd", language = "fr", width = "100%"),
                  fluidRow(
                    column(4, numericInput("heureEnd", label = "Heure :", value = format(Sys.time(), "%H"), min = 0, max = 24, width = "100%")),
                    column(4, numericInput("minEnd", label = "Min :", value = format(Sys.time(), "%M"), min = 0, max = 60, width = "100%")),
                    column(4, numericInput("secEnd", label = "Sec :", value = format(Sys.time(), "%S"), min = 0, max = 60, width = "100%")),
                  ),
                  br(), 
                  textInput(inputId = "textInput", label = "Token TS du device ID", value = "", width = "100%"),
                  radioButtons("checkCanne", label = "Dispostif de type canne ?",
                               choices = list("OUI" = TRUE, "NON" = FALSE), 
                               selected = TRUE, inline = TRUE),
                  actionButton(inputId = "actBtnVisualisation", label = "Visualisation",icon = icon("play"))
                ),
                column(9,
                  h1("Prévésualisation des données"),
                  br(),
                  br(),
                  #checkboxGroupInput("checkGroup", label = "Données à afficher", choices = liste_choix, selected = liste_choix, inline=TRUE)
                  dataTableOutput('dataTablePrev')
                  )
              ),
              #updateTabItems(session, "tabs", selected = "visualization")
      
      ),
      
      # visualization
      tabItem(tabName = "visualization",
              h1("Visualisation des données"),
              fluidRow(
                column(10, uiOutput(outputId = "numSelector")),
                column(2, uiOutput(outputId = "actBtnColonne"))
              ),
              plotOutput("Plot"),
              hr(),
              uiOutput(outputId ="downloadData"),
              hr(),
              dataTableOutput('dataTable')
      ),
      
      tabItem(tabName = "listeCannes",
              h1("Liste des différentes cannes avec leur Dispostif ID"),
              br(),
              h4("Copier l'ID associé à la canne dont vous voulez récupérer les données\n"),
              br(),
              dataTableOutput('dataTableCanne')
      ),
      tabItem(tabName = "info",
            h1("À propos interface données projet hubis"),
            h4("Cette application a été developpée dans le cadre d'un stage par 
                Gauthier BACCATI, si un problème survient :vous pouvez faire un ticket sur Github ou envoyer 
                un mail à :", tags$a("gauthier.achard.baccati@gmail.com")),
            h4("Accès au dépot Github : ", tags$a(href="https://github.com/GauthierBct/Interface_donnees_cannes", 
                                                  "ici" )),
            hr(),
            fluidRow(
              column(6, HTML("<center><img src=logo_hallehydraulique_mini.png width=200px></center>")),
              column(6, HTML("<center><img src=logo-polytech.png width=150px></center>"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  df <- read.csv2(paste(getwd(), "liste_cannes.csv", sep = '/'))
  output$dataTableCanne = DT::renderDataTable(df, options = list(pageLength = length(df$Nom)))
  output$notifs <- renderMenu({
    dropdownMenu(type = "notifications", icon = icon("bell"), .list=nots)
  })
  df1 = reactiveValues()
  df2 = reactiveValues()
  df3 = reactiveValues()
  df4 = reactiveValues()
  observeEvent(input$actBtnVisualisation, {
    req(input$textInput)
    heureStart <- paste(input$heureStart, input$minStart, input$secStart, sep = ":")
    heureEnd <- paste(input$heureEnd, input$minEnd, input$secEnd, sep = ":")
    dateStart <- paste(input$dateInputStart, heureStart)
    dateEnd <- paste(input$dateInputEnd, heureEnd)
    entityId <- input$textInput
    typeDisp <-input$checkCanne
    keys = tb_api$getKeys(entityId = entityId)
    df4$table <- do.call(cbind, lapply(keys[1], function(x){
      tb_api$getTelemetry(entityId,
                          keys = x,
                          startTs = as.POSIXct(dateStart, tz = "Europe/Paris"),
                          endTs = as.POSIXct(dateEnd, tz = "Europe/Paris"))}))
    df2$table <- do.call(cbind, lapply(keys[3:4], function(x){
        tb_api$getTelemetry(entityId,
                           keys = x,
                           startTs = as.POSIXct(dateStart, tz = "Europe/Paris"),
                           endTs = as.POSIXct(dateEnd, tz = "Europe/Paris"))}))
    df1$table <- do.call(cbind, lapply(keys[1:2], function(x){
      tb_api$getTelemetry(entityId,
                         keys = x,
                         startTs = as.POSIXct(dateStart, tz = "Europe/Paris"),
                         endTs = as.POSIXct(dateEnd, tz = "Europe/Paris"))}))
   if (typeDisp) {
    j = 1
   x = ReverseLines(df1$table)
   x = x[, -5]
   y = ReverseLines(df2$table)
   y = y[, -5]
   c = data.frame()
   for (i in 1:length(x[,'ts'])) {
     if (is.na(y[j, 'ts'])) {
       key <- c('latitude')
       ts <- c(x[i, 'ts'])
       value <- c(NA)
       temp = data.frame(key, ts, value)
       key.1 <- c('longitude')
       value.1 <- c(NA)
       temp2 = data.frame(key.1, value.1)
       c = rbind(c, cbind(x[i, ], cbind(temp, temp2)))
     }
     else {
       if (x[i, 'ts'] == y[j, 'ts']) {
         c = rbind(c, cbind(x[i, ], y[j, ]))
         j = j + 1
     } else {
         key <- c('latitude')
         ts <- c(x[i, 'ts'])
         value <- c(NA)
         temp = data.frame(key, ts, value)
         key.1 <- c('longitude')
         value.1 <- c(NA)
         temp2 = data.frame(key.1, value.1)
         c = rbind(c, cbind(x[i, ], cbind(temp, temp2)))
       }
     }
   }
    df3$table <- c[, -7]
    output$dataTable = DT::renderDataTable(df3$table)
    output$dataTablePrev = DT::renderDataTable(df3$table)
   }
    else {
      df3$table <- do.call(cbind, lapply(keys[1:length(keys)], function(x){
        tb_api$getTelemetry(entityId,
                            keys = x,
                            startTs = as.POSIXct(dateStart, tz = "Europe/Paris"),
                            endTs = as.POSIXct(dateEnd, tz = "Europe/Paris"))}))
      output$dataTable = DT::renderDataTable(df3$table)
      output$dataTablePrev = DT::renderDataTable(df3$table)
    }
    df1$table = ReverseLines(df1$table)
    output$Plot <-renderPlot({plot(df1$table[,3], xlab = "Nombre d'acquisition" , ylab = "Batterie", type = "l", main = "Evolution de la charge de la batterie en fonction du temps")})
    updateTabItems(session, "tabs", selected = "visualization")
    output$numSelector <- renderUI({ 
        out <- radioButtons(
           inputId = "listeKeys", 
           label = "Données à afficher",
           choices = keys, 
           selected = keys[1],
           inline = TRUE)
      return(out)
        })
    output$actBtnColonne <- renderUI({
      out <- actionButton(
        inputId = "actBtnColonne", 
        label = "Afficher",
        icon = icon("edit")
        )
      return(out)
    })
    showNotification(paste("Données correctement chargées"), duration=10, session = session, type = "message")
    not_temp = list(notificationItem(
      text = "Données correctement chargées",
      icon("upload"),
      status = "success"))
    nots <<- append(not_temp, nots)
    output$notifs <- renderMenu({
      dropdownMenu(type = "notifications", icon = icon("bell"), .list=nots)
    })
    output$downloadData <- renderUI({
      downloadButton("downloadDataHand", "Télécharger",icon('download'))
    })
  })
  output$downloadDataHand <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv2(df1$table, file)
      showNotification(paste("Données correctement téléchargées"), duration=10, session = session, type = "warning")
      not_temp = list(notificationItem(
        text = "Données correctement téléchargées",
        icon("download"),
        status = "success"))
      nots <<- append(not_temp, nots)
      output$notifs <- renderMenu({
        dropdownMenu(type = "notifications", icon = icon("bell"), .list=nots)
      })
    }
  )
  observeEvent(input$actBtnColonne, {
    df = reactiveValues()
    entityId <- input$textInput
    heureStart <- paste(input$heureStart, input$minStart, input$secStart, sep = ":")
    heureEnd <- paste(input$heureEnd, input$minEnd, input$secEnd, sep = ":")
    dateStart <- paste(input$dateInputStart, heureStart)
    dateEnd <- paste(input$dateInputEnd, heureEnd)
    keys = tb_api$getKeys(entityId = entityId)
    indice = InList(keys, input$listeKeys)
    print(indice)
    df$table <- do.call(cbind, lapply(keys[indice], function(x){
      tb_api$getTelemetry(entityId,
                          keys = x,
                          startTs = as.POSIXct(dateStart, tz = "Europe/Paris"),
                          endTs = as.POSIXct(dateEnd, tz = "Europe/Paris"))}))
    df$table = ReverseLines(df$table)
    output$Plot <-renderPlot({plot(df$table[,3], xlab = "Nombre d'acquisitions" , ylab = keys[indice], type = "l", main = paste("Evolution", keys[indice] ,"en fonction du temps"))})
    output$dataTable = DT::renderDataTable(df$table)
    updateTabItems(session, "tabs", selected = "visualization")
  })
  
}

shinyApp(ui, server)