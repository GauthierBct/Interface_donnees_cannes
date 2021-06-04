library(shiny)
library(shinydashboard)
library(Rthingsboard)
library(DT)
library(ggplot2)
library(shinyWidgets)
#library(rsconnect)

url = "http://scada.g-eau.fr"
publicId = "299cedc0-f3e9-11e8-9dbf-cbc1e37c11e3" 
nots <<- list(notificationItem(text = "Bienvenue sur l'interface", icon("check"), status = "success"))
df = reactiveValues()
df1 = reactiveValues()
df2 = reactiveValues()
df3 = reactiveValues()
df4 = reactiveValues()
nom = c()

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
  l = c()
  for (i in 0:(length(key)-1)) {
    for(j in 0:(length(x)-1)) {
      if (key[i+1] ==  x[j+1]) {
        l[i+1] = j 
        break
      }
    }
  }
  return (l)
}

tb_api = ThingsboardApi(url = url, publicId = publicId)

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = tags$div(img(src="logo_hubis.png", width ="100px"), " - Projet hubIS"), #icon("database")
                  titleWidth = "300px",  
                  dropdownMenuOutput("notifs")),
  dashboardSidebar(
    width = "275px",
    sidebarMenu(
      id = "tabs",
      menuItem("Lecture/ouverture des données", tabName = "readData", icon = icon("readme")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("poll")),
      menuItem("Liste des dispositifs disponibles", tabName = "listeCannes", icon = icon("list")),
      menuItem("Paramètres", tabName = "settings", icon = icon("cog")),
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
                  dateInput("dateInputStart", "Date de début de l'acquisition des données", value = Sys.Date()-7, min = Sys.Date()-32, max = Sys.Date(), weekstart = 1, format = "yyyy-mm-dd", language = "fr", width = "100%"),
                  fluidRow(
                    column(4, numericInput("heureStart", label = "Heure :", value = "00", min = 0, max = 24, step = 1, width = "100%")),
                    column(4, numericInput("minStart", label = "Min :", value = "00", min = 0, max = 60, step =1, width = "100%")),
                    column(4, numericInput("secStart", label = "Sec :", value = "00", min = 0, max = 60, step =1, width = "100%")),
                  ),
                  br(),
                  dateInput("dateInputEnd", "Date de fin de l'acquisition des données", value = Sys.Date(), min = Sys.Date()-32, max = Sys.Date(), weekstart = 1, format = "yyyy-mm-dd", language = "fr", width = "100%"),
                  fluidRow(
                    column(4, numericInput("heureEnd", label = "Heure :", value = format(Sys.time(), "%H"), min = 0, max = 24, step =1, width = "100%")),
                    column(4, numericInput("minEnd", label = "Min :", value = format(Sys.time(), "%M"), min = 0, max = 60, step =1, width = "100%")),
                    column(4, numericInput("secEnd", label = "Sec :", value = format(Sys.time(), "%S"), min = 0, max = 60, step =1, width = "100%")),
                  ),
                  br(), 
                  textInput(inputId = "textInput", label = "Token TS du device ID", value = "", width = "100%"),
                  radioButtons("checkCanne", label = "Dispostif de type canne ?",
                               choices = list("OUI" = TRUE, "NON" = FALSE), 
                              selected = TRUE, inline = TRUE),
                  actionButton(inputId = "actBtnVisualisation", label = "Visualisation",icon = icon("play"))
                ),
                column(9,
                  h1("Prévisualisation des données"),
                  dataTableOutput('dataTablePrev')
                  )
              ),
      ),
      
      # visualization
      tabItem(tabName = "visualization",
              h1("Visualisation des données"),
              h5("Vous devez dans un premier temps charger des données dans 'Lecture/ouverture des données'"),
              h5("Attention dans cette version, il est impossible d'afficher plus de 3 graphiques !"),
              fluidRow(
                column(10, uiOutput(outputId = "numSelector")),
                column(2, uiOutput(outputId = "actBtnColonne"))
              ),
              uiOutput(outputId ="Plot"),
              hr(),
              uiOutput(outputId ="downloadData"),
              hr(),
              dataTableOutput('dataTable')
      ),
      
      tabItem(tabName = "listeCannes",
              h1("Liste des différentes cannes avec leur Dispostif ID"),
              h4("Selectionner le device souhaité, les champs 'TOKEN' et 'Device de type canne ?' vont se remplir automatiquement\n"),
              h3("Ajout d'un device"),
              fluidRow(align = "left",
                column(4, textInput("DeviceName", "Nom du device", value ="", width = "100%")),
                column(4, textInput("DeviceID", "ID du device", value ="", width = "100%")),
                column(4, textInput("DeviceType", "Type de device", value ="", width = "100%"))),
              fluidRow(
                column(12, actionButton("saveDevice", label = " - Ajouter le device", icon = icon("save")))
                ),
              h3("Liste des devices"),
              dataTableOutput('dataTableCanne')
      ),
      tabItem(tabName = "info",
            h1("À propos interface données projet hubis"),
            hr(),
            HTML("<p>Cette application a été developpée dans le cadre d'un stage par 
                Gauthier BACCATI, si un problème survient :</p><p>- Vous pouvez faire un ticket sur Github </p><p>- Envoyer 
                un mail à : gauthier.achard.baccati@gmail.com</p>"),
            hr(),
            h4("Accès au dépot Github : ", tags$a(href="https://github.com/GauthierBct/Interface_donnees_cannes", 
                                                  "ici" )),
            hr(),
            fluidRow(
              column(4, HTML("<center><img src=logo-supagro.png width=400px></center>")),
              column(4, HTML("<center><img src=logo_hubis.png width=250px></center>")),
              column(4, HTML("<center><img src=logo-polytech.png width=125px></center>"))
          )
      ),
      tabItem(tabName = "settings",
          h1("Paramètres de l'application"),
          h3("Choix des cannes"),
          HTML("<div>Retour sur le menu <I>'Lecture/ouverture des données'</I>, 
                       lors d'un appui sur une canne dans le menu <I>'Liste des dispositifs disponibles'</I></div>"),
          radioGroupButtons("retourMenuChoix", label='',
                               choices = list(
                                 "Activé" = TRUE,
                                 "Désactivé" = FALSE), 
                            selected = TRUE, checkIcon = list("yes" = icon("check-circle"))
          )
      )
    )
  )
)

server <- function(input, output, session) {
  dataf <- read.csv2("./liste_cannes.csv") #read.csv2(paste(getwd(), "liste_cannes.csv", sep = '/'))
  output$dataTableCanne = DT::renderDataTable(dataf, options = list(pageLength = length(dataf$Nom)), 
                                              selection = 'single', callback = JS("table.on('click.dt', 
            'td', function() {
             var data=table.row(this).data();
           Shiny.onInputChange('clickData', data );
    });"))
  output$notifs <- renderMenu({
    dropdownMenu(type = "notifications", icon = icon("bell"), .list=nots)
  })
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
   y = ReverseLines(df2$table)
   c = data.frame()
   for (i in 1:length(x[,'ts'])) {
     if (is.na(y[j, 'ts']) ) {
       temp = do.call(cbind, lapply(c('latitude', 'longitude'), function(nom) {
         key <- c(nom)
         ts <- c(x[i, 'ts'])
         value <- c(NA)
         temp = data.frame(key, ts, value)
       }))
       c = rbind(c, cbind(x[i, ], temp))
     }
     else {
       if (x[i, 'ts'] == y[j, 'ts']) {
         c = rbind(c, cbind(x[i, ], y[j, ]))
         j = j + 1
       } else {
         tab = c('latitude', 'longitude')
         temp = do.call(cbind, lapply(tab, function(ind) {
           key <- c(ind)
           ts <- c(x[i, 'ts'])
           value <- c(NA)
           temp = data.frame(key, ts, value)
         }))
           c = rbind(c, cbind(x[i, ], temp))
         }
     }
   }
    df3$table <- c
    output$dataTable = DT::renderDataTable(df3$table, options=list(scrollX=T))
    output$dataTablePrev = DT::renderDataTable(df3$table, options=list(scrollX=T))
   }
    else {
      df3$table <- do.call(cbind, lapply(keys[1:length(keys)], function(x){
        tb_api$getTelemetry(entityId,
                            keys = x,
                            startTs = as.POSIXct(dateStart, tz = "Europe/Paris"),
                            endTs = as.POSIXct(dateEnd, tz = "Europe/Paris"))}))
      output$dataTable = DT::renderDataTable(df3$table, options=list(scrollX=T))
      output$dataTablePrev = DT::renderDataTable(df3$table, options=list(scrollX=T))
      }
    df1$table = ReverseLines(df1$table)
    vecteur = 1:(length(df3$table)%/%3)
    output$Plot <- renderUI({
      do.call(fluidRow, lapply(vecteur, function(x) {
        k = 3*x-2
        nom <- df3$table[1,k]
        column(6, renderPlot(plot(df3$table[,3*x], type = "l", ylab="", main=nom)))
      }))
    })
    updateTabItems(session, "tabs", selected = "visualization")
    output$numSelector <- renderUI({ 
        out <- checkboxGroupInput(
           inputId = "listeKeys", 
           label = "Données à afficher",
           choices = keys, 
           selected = keys[1:4],
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
    req(input$listeKeys)
    entityId <- input$textInput
    heureStart <- paste(input$heureStart, input$minStart, input$secStart, sep = ":")
    heureEnd <- paste(input$heureEnd, input$minEnd, input$secEnd, sep = ":")
    dateStart <- paste(input$dateInputStart, heureStart)
    dateEnd <- paste(input$dateInputEnd, heureEnd)
    keys <- tb_api$getKeys(entityId = entityId)
    indice <- InList(keys, input$listeKeys)
    df$table <- do.call(cbind, lapply(indice, function(x) {
      key <- df3$table[,3*x+1]
      ts <-  df3$table[,3*x+2]
      value <- df3$table[,3*x+3]
      data.frame(key, ts, value)
    }))
    output$Plot <-renderUI({
       do.call(fluidRow, lapply(indice, function(x) {
          k = 3*(x+1)-2
         nom <- df3$table[1,k]
         column(6, renderPlot(plot(df3$table[,3*(x+1)], type = "l", ylab="", main=nom)))
     })
     )})
    output$dataTable = DT::renderDataTable(df$table, options=list(scrollX=T))
    updateTabItems(session, "tabs", selected = "visualization")
  })
  observeEvent(input$clickData, {
    ligne <- input$clickData
    updateTextInput(session, "textInput", value = ligne[3])
    if (ligne[4] == "canne") {
      updateRadioButtons(session, "checkCanne", selected = TRUE)
    } else {
      updateRadioButtons(session, "checkCanne", selected = FALSE)
    }
    showNotification(paste(ligne[2], "est le device sélectionné"), duration=10, session = session, type = "default")
    not_temp = list(notificationItem(
      text = paste(ligne[2], "est le device sélectionné"),
      icon("list-ul"),
      status = "primary"))
    nots <<- append(not_temp, nots)
    output$notifs <- renderMenu({
      dropdownMenu(type = "notifications", icon = icon("bell"), .list=nots)
    })
    if(input$retourMenuChoix) {
      updateTabItems(session, "tabs", selected = "readData")
    }
  })
  observeEvent(input$saveDevice, {
    req(input$DeviceName)
    req(input$DeviceID)
    req(input$DeviceType)
    dataf <- read.csv2("./liste_cannes.csv")
    cat("\n", file= "./liste_cannes.csv", append = TRUE)
    Indice <- length(dataf$Name)+1
    Nom <- input$DeviceName
    ID <- input$DeviceID
    Type <- input$DeviceType
    if(!(input$DeviceName %in% dataf[,'Nom']) & !(input$DeviceID %in% dataf[,'ID'])) {
      cat(Nom, ID, Type, file= "./liste_cannes.csv", sep =";", append = TRUE, labels = c("Nom", "ID", "Type"))
      dataf <- read.csv2("./liste_cannes.csv")
      output$dataTableCanne = DT::renderDataTable(dataf, options = list(pageLength = length(dataf$Nom)),
                                                  selection = 'single', callback = JS("table.on('click.dt',
              'td', function() {
               var data=table.row(this).data();
             Shiny.onInputChange('clickData', data );
      });"))
      showNotification("Ajouté avec succès !", duration=10, session = session, type = "message")
      not_temp = list(notificationItem(
        text = "Ajouté avec succès !",
        icon("check-square"),
        status = "success"))
      nots <<- append(not_temp, nots)
      output$notifs <- renderMenu({
        dropdownMenu(type = "notifications", icon = icon("bell"), .list=nots)
      })
      updateTextInput(session, "DeviceName", value ="")
      updateTextInput(session, "DeviceID", value ="")
      updateTextInput(session, "DeviceType", value ="")
    }
    else {
      showNotification("Nom et/ou ID du device déjà existant !", duration=10, session = session, type = "error")
      not_temp = list(notificationItem(
        text = "Nom et/ou ID du device déjà existant !",
        icon("times"),
        status = "danger"))
      nots <<- append(not_temp, nots)
      output$notifs <- renderMenu({
        dropdownMenu(type = "notifications", icon = icon("bell"), .list=nots)
      })
    }
  })
}
shinyApp(ui, server)