library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$a(
      href = "#",
      tags$img(
        src = "logo.png",
        height = "40px",
        style = "background-color: white; padding: 4px; border-radius: 5px;"
      )
    ),
    titleWidth = 200
  ),
  
  dashboardSidebar(
    disable = FALSE,
    collapsed = FALSE,
    sidebarMenu(
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Résultats", tabName = "results", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Lato:wght@300;400;700&display=swap');

        .main-header .sidebar-toggle {
          display: none !important;
        }

        .main-header .logo {
          background-color: #ffc001 !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          padding: 0 !important;
        }

        .main-header .navbar {
          background-color: #ffc001 !important;
        }

        .main-sidebar {
          background-color: #fcd253 !important;
        }

        .skin-black .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #ffc001 !important;
          color: white !important;
        }

        .skin-black .main-sidebar .sidebar .sidebar-menu a {
          color: #000000 !important;
        }

        .skin-black .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #ffc001 !important;
          color: #000000 !important;
        }

        .btn-primary {
          background-color: #ffc001 !important;
          border-color: #e0a800 !important;
          color: black !important;
        }

        .btn-success {
          background-color: #fcd253 !important;
          border-color: #fbc02d !important;
          color: black !important;
        }

        .btn-info {
          background-color: #ffc001 !important;
          border-color: #e0a800 !important;
          color: black !important;
        }

        .progress-bar {
          background-color: #ffc001 !important;
        }

        .box.box-primary > .box-header,
        .box.box-info > .box-header,
        .box.box-warning > .box-header {
          background: #ffc001 !important;
          color: black !important;
        }

        .welcome-container {
          max-width: 900px;
          margin: 0 auto;
          padding: 50px 30px;
          background-color: white;
          border-radius: 15px;
          box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
          animation: fadeInUp 1s ease-out;
          font-family: 'Lato', sans-serif;
        }

        .welcome-logo {
          display: flex;
          justify-content: center;
          margin-bottom: 30px;
        }

        .welcome-title {
          font-size: 42px;
          font-weight: 700;
          color: #222;
          text-align: center;
          margin-bottom: 25px;
        }

        .subtitle-text {
          color: #666;
          font-size: 18px;
          text-align: center;
          margin-bottom: 30px;
        }

        .welcome-text {
          font-size: 15px;
          color: #444;
          line-height: 1.7;
          text-align: justify;
          max-width: 750px;
          margin: 0 auto;
          padding: 0 15px;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "home",
              div(
                class = "welcome-container",
                div(class = "welcome-logo",
                    img(src = "logo.png", class = "center-logo")
                ),
                h2(class = "welcome-title", icon("chart-line"), " BIENVENUE DANS VOTRE INTERFACE D'ANALYSE"),
                h4(class = "text-center subtitle-text", 
                   "Une plateforme simple pour vos analyses prédictives"
                ),
                tags$div(
                  class = "welcome-text",
                  p("Cette interface vous permet de réaliser diverses analyses de classification ou de régression."),
                  p("L'application est conçue pour fonctionner avec n'importe quelle base de données au format CSV, tant qu'elle respecte un certain nombre de critères."),
                  p("Pour plus de précision et pour les nouveaux utilisateurs, veuillez lire attentivement la section Aide afin de prendre connaissance des conditions d'utilisation des différents modèles disponibles.")
                ),
                div(
                  style = "text-align: center; margin-top: 30px;",
                  p("Besoin d'aide ou de précisions sur l'utilisation de l'application ?"),
                  actionButton("maintenance_guide_btn", "Aide complète", class = "btn btn-primary")
                )
              )
      ),
      
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "Chargement des données", status = "warning", solidHeader = TRUE, width = 12,
                  fileInput("datafile", "Choisir un fichier CSV", accept = ".csv"),
                  textInput("sep", "Séparateur :", value = "", placeholder = "Exemple : , ou ;"),
                  textInput("dec", "Caractère décimal :", value = "", placeholder = "Exemple : . ou ,"),
                  actionButton("load_data", "Charger les données", class = "btn btn-primary"),
                  tags$hr(),
                  textOutput("data_dimensions")
                )
              ),
              fluidRow(
                box(
                  title = "Paramètres d'analyse", status = "warning", solidHeader = TRUE, width = 12,
                  numericInput("num_targets", "Nombre de variables cibles", value = 1, min = 1),
                  selectInput("analysis_type", "Type d'analyse", choices = c("Classification", "Régression")),
                  uiOutput("analysis_choices_ui"),
                  tags$hr(),
                  numericInput("train_split", "Pourcentage de données pour l'apprentissage", value = 80, min = 50, max = 90),
                  actionButton("adjust_params", "Ajuster les paramètres", class = "btn btn-primary"),
                  actionButton("run_analysis", "RUN", class = "btn btn-success"),
                  div(style = "margin-top: 15px;"),
                  progressBar(id = "progress", value = 0, display_pct = TRUE)
                )
              )
      ),
      
      tabItem(tabName = "results",
              fluidPage(
                h3("Résultats des Analyses"),
                uiOutput("result_summaries"),
                plotlyOutput("roc_plot"),
                uiOutput("confusion_matrices"),
                
         ) 
       )
    )
  )
)