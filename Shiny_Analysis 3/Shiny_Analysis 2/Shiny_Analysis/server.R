# server.R

server <- function(input, output, session) {
  # Variables réactives
  data <- reactiveVal(NULL)
  
  model_params <- reactiveValues(
    nn_layers = 1, nn_neurons = list(10), nn_activation = "relu",
    rf_trees = 100,
    logistic_threshold = 0.5, logistic_threshold_type = "alpha",
    tree_threshold = 0.5, tree_threshold_type = "alpha",
    linreg_targets = 1
  )
  
  analysis_results <- reactiveValues(
    train_results = list(),
    test_results = list(),
    train_roc = list(),
    test_roc = list(),
    train_r2 = list(),
    test_r2 = list(),
    train_conf_matrices = list(),
    test_conf_matrices = list(),
    logistic_threshold = NULL
  )
  
  # Mise à jour dynamique des modèles selon le type d’analyse
  observeEvent(input$analysis_type, {
    update_analysis_choices_ui(input$analysis_type, output)
  })
  
  # Chargement des données
  observeEvent(input$load_data, {
    handle_data_upload(input, output, data)
  })
  
  # Fenêtre de paramétrage des modèles
  observeEvent(input$adjust_params, {
    show_parameter_modal(input, model_params, output, session)
  })
  
  # Sauvegarde des paramètres dans la fenêtre modale
  observeEvent(input$save_params, {
    save_model_parameters(input, model_params)
    removeModal()
  })
  
  # Lancement de l’analyse
  observeEvent(input$run_analysis, {
    run_all_analyses(input, output, session, data, model_params, analysis_results)
  })
  
  # Matrices de confusion
  observe({
    render_confusion_matrices(output, analysis_results)
  })
  
  # Résumés, R², AUC, courbes ROC, importance
  observe({
    render_summaries_and_plots(output, analysis_results)
  })

  # Pop-up d’aide
  observeEvent(input$maintenance_guide_btn, {
    showModal(modalDialog(
      title = "Guide d'utilisation et de maintenance",
      size = "l",
      easyClose = TRUE,
      footer = NULL,
      tabsetPanel(
        
        tabPanel("Présentation",
                 tagList(
                   h4("Contexte"),
                   p("Cette interface Shiny a été développée dans le cadre d’un mémoire-projet réalisé en 3e année de licence MIASHS (IMA – UCO Angers)."),
                   p("Elle vise à faciliter l’analyse de tables décisionnelles à travers des méthodes de classification et de régression."),
                   p("Attention : Il s'agit d'un outil universitaire expérimental. Des limitations ou imperfections peuvent subsister.")
                 )
        ),
        
        tabPanel("Chargement de données",
                 tagList(
                   h4("Format accepté"),
                   p("• Fichier CSV uniquement"),
                   p("• Taille maximale : 350 Mo"),
                   h4("Paramètres configurables dans l'interface"),
                   p("• Séparateur (`;`, `,`...) et caractère décimal (`.` ou `,`) à renseigner avant l'import."),
                   h4("Encodage conseillé"),
                   p("• Encodage UTF-8 recommandé pour éviter les erreurs de lecture.")
                 )
        ),
        
        tabPanel("Fonctionnement",
                 tagList(
                   h4("Prise en compte des paramètres utilisateur"),
                   p("Tous les paramètres définis dans l’interface sont transmis aux modèles : pourcentage d’apprentissage, minsplit, nombre de neurones, etc."),
                   h4("Barre de progression"),
                   p("Elle reflète l’état réel d’avancement de l’analyse."),
                   h4("Structure du code"),
                   p("Le code est découpé en plusieurs fichiers pour plus de clarté."),
                   h4("Neuralnet"),
                   p("Le package `neuralnet` est utilisé pour un meilleur contrôle de l’architecture réseau (contrairement à `nnet`).")
                 )
        ),
        
        tabPanel("Classification",
                 tagList(
                   h4("Modèles disponibles"),
                   tags$ul(
                     tags$li("Régression logistique"),
                     tags$li("Random Forest"),
                     tags$li("Arbres de décision"),
                     tags$li("Réseaux de neurones (neuralnet)")
                   ),
                   h4("Résultats générés"),
                   tags$ul(
                     tags$li("Courbes ROC"),
                     tags$li("AUC (aire sous la courbe ROC)"),
                     tags$li("Matrices de confusion"),
                     tags$li("Importance des variables (Random Forest)")
                   ),
                   h4("Interprétation"),
                   p("• Une AUC proche de 1 indique une très bonne performance."),
                   p("• La matrice de confusion permet de visualiser les bonnes et mauvaises prédictions.")
                 )
        ),
        
        tabPanel("Régression",
                 tagList(
                   h4("Modèles disponibles"),
                   tags$ul(
                     tags$li("Régression linéaire"),
                     tags$li("Régression logistique"),
                     tags$li("Random Forest"),
                     tags$li("Réseaux de neurones (neuralnet)"),
                     tags$li("Arbres de décision")
                   ),
                   h4("Résultats générés"),
                   tags$ul(
                     tags$li("Affichage des résultats du modèle (coefficients, résidus, etc.)"),
                     tags$li("Score R² (coefficient de détermination)"),
                     tags$li("Pas de courbe ROC (non applicable en régression)"),
                     tags$li("Graphiques et indicateurs d’erreur à venir (RMSE, MAE, etc.)")
                   )
                 )
        ),
        
        tabPanel("FAQ / Conseils",
                 tagList(
                   h4("Bonnes pratiques"),
                   tags$ul(
                     tags$li("Vérifiez que les cibles sont bien placées en fin de tableau."),
                     tags$li("Supprimez ou traitez les valeurs manquantes avant l'analyse."),
                     tags$li("Utilisez des noms de colonnes sans caractères spéciaux.")
                   ),
                   h4("Limitations connues"),
                   tags$ul(
                     tags$li("ROC uniquement pour les modèles binaires."),
                     tags$li("Les réseaux de neurones peuvent être sensibles à la normalisation des données.")
                   )
                 )
        ),
        
        tabPanel("Dépannage",
                 tagList(
                   h4("Modèles qui ne s'exécutent pas"),
                   h5("Format attendu pour la variable cible"),
                   tags$ul(
                     tags$li("Numérique (régression)"),
                     tags$li("Facteur (classification)")
                   ),
                   p("• Évitez les colonnes vides, constantes ou contenant des identifiants uniques."),
                   h4("Problèmes de chargement de fichier"),
                   p("• Fichier trop volumineux : vérifiez qu'il ne dépasse pas 350 Mo."),
                   p("• Encodage incorrect : privilégiez UTF-8.")
                 )
        ),
        
        tabPanel("Contact",
                 tagList(
                   h4("Contacts"),
                   p("Pour toute question ou retour :"),
                   tags$ul(
                     tags$li("Clémence Lambert – ", strong("lambert.clemence05@gmail.com")),
                     tags$li("Inès Guego – ", strong("guegoines2@gmail.com"))
                   )
                 )
        )
      )
    ))
  })
}