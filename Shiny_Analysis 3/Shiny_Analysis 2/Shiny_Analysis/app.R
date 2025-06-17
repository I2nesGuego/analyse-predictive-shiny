# app.R

# Chargement des dépendances globales
source("global.R")

# Chargement de l'interface utilisateur et du serveur
source("ui.R")
source("server.R")

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)