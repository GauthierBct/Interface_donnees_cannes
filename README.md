# Interface_donnees_cannes
Ce programme consiste à proposer une interface visuelle, pour récupérer les données de ThingsBoard. 

## Librairies utilisées
Les librairies utilisées sont :
* `Rthingsboard` : pour récupérer les données de Thingsboard
* `shiny` : pour l'interface
* `shinydashboard` : pour la création du dashboard
* `DT` : pour l'affichage de tableaux  

Un petit script R `firsttime.R` est disponible pour télécharger toutes les libraires necéssaires (contient les `install.packages()` adéquats).

## Configuration
Dans un premier temps, chargez toutes les librairies :
```R
library(shiny)
library(shinydashboard)
library(Rthingsboard)
library(DT)
```
Il faut maintenant, donner les informations necessaires pour se connecter à l'API de Thingsboard :
```R
url = "http://scada.g-eau.fr"
publicId = "299cedc0-f3e9-11e8-9dbf-cbc1e37c11e3"
tb_api = ThingsboardApi(url = url, publicId = publicId)
```
- url : est l'adresse du thingsboard.io
- publicId : l'ID du "client" public (vous le trouverez dans thingsboard -> clients -> public -> copier l'id du client)

## Interface utilisateur (ui)
On utilise les librairies shiny et shinydashboard, notre programme se structure de la manière la suivante :
```R
ui <- dashboardPage(
   dashboardHeader(title = "UMR G-Eau - Cannes avertisseuses", titleWidth = "250px"),
   dashboardSidebar(width = "250px",
    sidebarMenu(
      menuItem("Lecture/sauvegarde des données", tabName = "readData", icon = icon("readme")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("poll")),
      menuItem("Liste des cannes", tabName = "listeCannes", icon = icon("list"))
     )
   ),
   dashboardBody(
    tabItems(
     tabItem(tabName = "readData",
        #...
        #Mettre du code ici
        #...
     ),
     tabItem(tabName = "visualization",
         #...
         #Mettre du code ici
         #...
      ),
     tabItem(tabName = "listeCannes",
          #...
          #Mettre du code ici
          #...
      )
   )
)

server <- function(input, output, session) {
   #...
   #Mettre du code ici
   #...
}

shinyApp(ui, server)
```
Vous avez la structure générale du programme, le programme complet est disponible dans ``app.R``.
Dans mon cas, je télécharges les données lorsque qu'on appuie sur le bouton "Visualisation", du côté du **server** (``server <- function(input, output, session) {}``) nous allons faire :
```R
observeEvent(input$actBtnVisualisation, {
    #On bloque le code ici tant que le textInput est vide. 
    #Si on fait une requête de connexion sur l'API TB, le programme plante. On fait ça pour éviter tout beug
    req(input$textInput)
    #On affecte les valeurs des widgets à des variables
    heureStart <- paste(input$heureStart, input$minStart, input$secStart, sep = ":")
    heureEnd <- paste(input$heureEnd, input$minEnd, input$secEnd, sep = ":")
    dateStart <- paste(input$dateInputStart, heureStart)
    dateEnd <- paste(input$dateInputEnd, heureEnd)
    entityId <- input$textInput
    #On récupère les clefs (detection, batterie, ts)
    keys = tb_api$getKeys(entityId = entityId)
    #On met dans une date frame les données
    df1$table <- do.call(cbind, lapply(keys[2], function(x){
      tb_api$getTelemetry(entityId,
                         keys = x,
                         startTs = as.POSIXct(dateStart),
                         endTs = as.POSIXct(dateEnd))}))
    #On met à jour les tableaux de visualisation des données
    output$dataTablePrev = DT::renderDataTable(df1$table)
    output$dataTable = DT::renderDataTable(df1$table)
    #On trace l'évolution des détections en fonction du temps
    output$Plot <-renderPlot({plot(df1$table$value, xlab = "Nombre d'acquisition" , ylab = "Détection", type = "l", main = "Détections en fonction du temps")})
    updateTabItems(session, "tabs", selected = "visualization")
  })
```
   
