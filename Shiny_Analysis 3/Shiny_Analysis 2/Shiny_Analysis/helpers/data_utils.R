# helpers/data_utils.R

# Met à jour dynamiquement la liste des modèles disponibles selon le type d'analyse
update_analysis_choices_ui <- function(analysis_type, output) {
  if (analysis_type == "Classification") {
    # Si l'utilisateur choisit "Classification", on affiche les modèles adaptés à ce type
    output$analysis_choices_ui <- renderUI({
      checkboxGroupInput("analysis_choices", "Choisissez les analyses de classification :",
                         choices = c("Régression logistique", "Réseaux de neurones", "Arbres de Décision", "Random Forest"),
                         selected = "Régression logistique")
    })
  } else if (analysis_type == "Régression") {
    # Sinon, pour une "Régression", on affiche une autre liste de modèles adaptés
    output$analysis_choices_ui <- renderUI({
      checkboxGroupInput("analysis_choices", "Choisissez les analyses de régression :",
                         choices = c("Régression linéaire", "Régression logistique", "Réseaux de neurones", "Random Forest", "Arbres de Décision"),
                         selected = "Régression linéaire")
    })
  }
}


# Fonction de chargement du fichier CSV + vérifications de base
handle_data_upload <- function(input, output, data) {
  req(input$datafile)  # Vérifie que l'utilisateur a bien sélectionné un fichier
  
  # Vérifie que le séparateur est spécifié
  if (input$sep == "") {
    showNotification("Veuillez entrer un séparateur (par exemple, ',' ou ';').", type = "error")
    return(NULL)
  }
  
  # Vérifie que le caractère décimal est spécifié
  if (input$dec == "") {
    showNotification("Veuillez entrer un caractère décimal (par exemple, '.' ou ',').", type = "error")
    return(NULL)
  }
  
  # Tente de lire le fichier CSV avec les paramètres spécifiés
  df <- tryCatch({
    read.csv(input$datafile$datapath, sep = input$sep, dec = input$dec, header = TRUE)
  }, error = function(e) {
    # Affiche une erreur si le fichier n’est pas lisible
    showNotification(paste("Erreur lors du chargement du fichier CSV :", e$message), type = "error")
    return(NULL)
  })
  
  if (is.null(df)) return(NULL)  # Si lecture échouée, on sort
  
  # Vérifie que le fichier a suffisamment de colonnes par rapport au nombre de cibles
  if (ncol(df) < input$num_targets + 1) {
    showNotification("Le fichier CSV doit contenir au moins autant de colonnes +1 que le nombre de variables cibles spécifiées.", type = "error")
    return(NULL)
  }
  
  # Vérifie que le fichier ne contient pas de valeurs manquantes
  if (anyNA(df)) {
    showNotification("Les données contiennent des valeurs manquantes. Veuillez les compléter avant de continuer.", type = "error")
    return(NULL)
  }
  
  # Si toutes les vérifications sont passées, on sauvegarde les données
  data(df)
  showNotification("Les données ont été chargées avec succès.", type = "message")
  
  # Affiche les dimensions du fichier (lignes, colonnes) dans l'interface
  output$data_dimensions <- renderText({
    paste("Nombre de lignes : ", nrow(df), ", Nombre de colonnes : ", ncol(df))
  })
}
