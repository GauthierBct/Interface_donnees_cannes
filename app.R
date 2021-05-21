library(shiny)
library(shinydashboard)
library(Rthingsboard)
library(DT)
library(ggplot2)
library(shinyWidgets)
#library(rsconnect)

url = "http://scada.g-eau.fr"
publicId = "299cedc0-f3e9-11e8-9dbf-cbc1e37c11e3" 
#entityId = "b5f12530-8ce5-11e9-9807-af0bb1a11174"  #94e89600-878d-11e9-9807-af0bb1a11174
#startDate = as.POSIXct("2021-05-12 10:00:00", tz = "Europe/Paris")
#endDate = as.POSIXct("2021-05-12 18:00:00", tz = "Europe/Paris")

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
                    column(4,numericInput("minEnd", label = "Min :", value = format(Sys.time(), "%M"), min = 0, max = 60, width = "100%")),
                    column(4,numericInput("secEnd", label = "Sec :", value = format(Sys.time(), "%S"), min = 0, max = 60, width = "100%")),
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
  observeEvent(input$actBtnVisualisation, {
    req(input$textInput)
    heureStart <- paste(input$heureStart, input$minStart, input$secStart, sep = ":")
    heureEnd <- paste(input$heureEnd, input$minEnd, input$secEnd, sep = ":")
    dateStart <- paste(input$dateInputStart, heureStart)
    dateEnd <- paste(input$dateInputEnd, heureEnd)
    entityId <- input$textInput
    keys = tb_api$getKeys(entityId = entityId)
    df1$table <- do.call(cbind, lapply(keys[2], function(x){
      tb_api$getTelemetry(entityId,
                         keys = x,
                         startTs = as.POSIXct(dateStart),
                         endTs = as.POSIXct(dateEnd))}))
    output$dataTablePrev = DT::renderDataTable(df1$table)
    output$dataTable = DT::renderDataTable(df1$table)
    output$Plot <-renderPlot({plot(df1$table$value, xlab = "Nombre d'acquisition" , ylab = "Détection", type = "l", main = "Détections en fonction du temps")})
    updateTabItems(session, "tabs", selected = "visualization")
  })
    output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv2(df1$table, file)
    }
  )
}

shinyApp(ui, server)
