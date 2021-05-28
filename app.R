library(shiny)
library(shinydashboard)
library(Rthingsboard)
library(DT)
#library(rsconnect)

url = "http://scada.g-eau.fr"
publicId = "299cedc0-f3e9-11e8-9dbf-cbc1e37c11e3" 

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

tb_api = ThingsboardApi(url = url, publicId = publicId)

ui <- dashboardPage(
  dashboardHeader(title = "UMR G-Eau - Cannes avertisseuses", titleWidth = "250px"),
  dashboardSidebar(
    width = "250px",
    sidebarMenu(
      menuItem("Lecture/sauvegarde des données", tabName = "readData", icon = icon("readme")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("poll")),
      menuItem("Liste des cannes", tabName = "listeCannes", icon = icon("list"))
    )
  ),
  dashboardBody(
    tabItems(
      # Read data
      tabItem(tabName = "readData",
              fluidRow(
                column(3,
                  h1("Lecture des données"),
                  h3("Paramètres de configuration"),
                  br(),
                  dateInput("dateInputStart", "Date de début de l'acquisition des données :", value = Sys.Date()-1, min = "2020-01-01", max = Sys.Date(), weekstart = 1, format = "yyyy-mm-dd", language = "fr", width = "100%"),
                  fluidRow(
                    column(4, numericInput("heureStart", label = "Heure :", value = "00", min = 0, max = 24, width = "100%")),
                    column(4, numericInput("minStart", label = "Min :", value = "00", min = 0, max = 60, width = "100%")),
                    column(4, numericInput("secStart", label = "Sec :", value = "00", min = 0, max = 60, width = "100%")),
                  ),
                  br(), br(),
                  dateInput("dateInputEnd", "Date de fin de l'acquisition des données :", value = Sys.Date(), min = "2020-01-01", max = Sys.Date(), weekstart = 1, format = "yyyy-mm-dd", language = "fr", width = "100%"),
                  fluidRow(
                    column(4, numericInput("heureEnd", label = "Heure :", value = format(Sys.time(), "%H"), min = 0, max = 24, width = "100%")),
                    column(4, numericInput("minEnd", label = "Min :", value = format(Sys.time(), "%M"), min = 0, max = 60, width = "100%")),
                    column(4, numericInput("secEnd", label = "Sec :", value = format(Sys.time(), "%S"), min = 0, max = 60, width = "100%")),
                  ),
                  br(), br(),
                  textInput(inputId = "textInput", label = "Token TS du device ID", value = "", width = "100%"),
                  actionButton(inputId = "actBtnVisualisation", label = "Visualisation",icon = icon("play"))
                ),
                column(9,
                  h1("Prévésualisation des données"),
                  br(),
                  downloadButton("downloadData", "Télécharger"),
                  br(),
                  #checkboxGroupInput("checkGroup", label = "Données à afficher", choices = liste_choix, selected = liste_choix, inline=TRUE),
                  fluidRow(
                    column(6, uiOutput(outputId = "numSelector")),
                    column(3, uiOutput(outputId = "actBtnColonne"))
                  ),
                  dataTableOutput('dataTablePrev')
                  )
              ),
              #updateTabItems(session, "tabs", selected = "visualization")
      
      ),
      
      # visualization
      tabItem(tabName = "visualization",
              h1("Visualisation des données"),
              plotOutput("Plot"),
              dataTableOutput('dataTable')
      ),
      
      tabItem(tabName = "listeCannes",
              h1("Liste des différentes cannes avec leur Dispostif ID"),
              br(),
              div("Copier l'ID associé à la canne dont vous voulez récupérer les données\n"),
              br(),
              dataTableOutput('dataTableCanne')
      )
    )
  )
)

server <- function(input, output, session) {
  df <- read.csv2(paste(getwd(), "liste_cannes.csv", sep = '/'))
  output$dataTableCanne = DT::renderDataTable(df)
  df1 = reactiveValues()
  df2 = reactiveValues()
  df3 = reactiveValues()
  observeEvent(input$actBtnVisualisation, {
    req(input$textInput)
    heureStart <- paste(input$heureStart, input$minStart, input$secStart, sep = ":")
    heureEnd <- paste(input$heureEnd, input$minEnd, input$secEnd, sep = ":")
    dateStart <- paste(input$dateInputStart, heureStart)
    dateEnd <- paste(input$dateInputEnd, heureEnd)
    entityId <- input$textInput
    keys = tb_api$getKeys(entityId = entityId)
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
   j = 1
   x = ReverseLines(df1$table)
   x = x[, -5]
   y = ReverseLines(df2$table)
   y = y[, -5]
   for (i in 1:81) {
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
    df3$table <- c[, -7]
    output$dataTablePrev = DT::renderDataTable(df3$table)
    output$dataTable = DT::renderDataTable(df3$table)
    output$Plot <-renderPlot({plot(df1$table[,3], xlab = "Nombre d'acquisition" , ylab = "Batterie", type = "l", main = "Evolution de la charge de la batterie en fonction du temps")})
    updateTabItems(session, "tabs", selected = "visualization")
    output$numSelector <- renderUI({
      out <- checkboxGroupInput(
        inputId = "numSelector",
        label   = "Select the numbers to filter on:",
        choices = keys,
        inline = TRUE
      )
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
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv2(df$table, file)
    }
  )
  # observeEvent(input$actBtnColonne, {
  #   choix = input$numSelector
  #   tab = df3$table
  #   k = 0
  #   bat = FALSE
  #   detect = FALSE
  #   lat = FALSE
  #   long = FALSE
  #   choix_cour = choix[k] 
  #   while (is.null(choix_cour)) {
  #     if (choix_cour == key[1]) {
  #       bat = TRUE
  #     }
  #     if (choix_cour == key[2] ) {
  #       detect = TRUE
  #     }
  #     if (choix_cour == key[3] ) {
  #       lat = TRUE
  #     }
  #     if (choix_cour == key[4] ) {
  #       long = TRUE
  #     }
  #     k = k + 1
  #     choix_cour = choix[k]
  #   }
  #   
  #   if (!bat) {
  #     tab = tab[, -1]
  #     tab = tab[, -2]
  #   }
  #   if (!detect){
  #     print(colnames(tab))
  #     tab = tab[, -'key1.1']
  #   }
  #   output$dataTablePrev = DT::renderDataTable(tab)
  # })
}

shinyApp(ui, server)