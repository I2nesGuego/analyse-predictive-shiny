# helpers/plotting.R

# Fonction pour afficher une matrice de confusion stylisée avec ggplot2
plot_conf_matrix <- function(conf_matrix, model_name, dataset_type) {
  conf_data <- as.data.frame(as.table(conf_matrix))
  colnames(conf_data) <- c("Actual", "Predicted", "Freq")
  
  conf_data$Predicted <- factor(conf_data$Predicted, levels = rev(levels(conf_data$Predicted)))
  conf_data$Type <- rep(c("TP", "FN", "FP", "TN"), length.out = nrow(conf_data))
  
  ggplot(conf_data, aes(x = Predicted, y = Actual, fill = Type)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 6, color = "white", fontface = "bold") +
    scale_fill_manual(values = c("TP" = "#98FB98", "FN" = "#F08080", "FP" = "#F08080", "TN" = "#98FB98")) +
    labs(
      title = paste("Matrice de Confusion -", model_name, "(", dataset_type, ")"),
      x = "Valeur Prédite", y = "Valeur Réelle"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#F5F5FF"),
      legend.position = "right"
    ) +
    scale_x_discrete(position = "top")
}

#  Fonction pour générer toutes les matrices de confusion dans l'UI
render_confusion_matrices <- function(output, analysis_results) {
  output$confusion_matrices <- renderUI({
    req(analysis_results$train_conf_matrices, analysis_results$test_conf_matrices)
    
    matrices <- lapply(names(analysis_results$train_conf_matrices), function(model_name) {
      fluidRow(
        column(6, h4(paste("Matrice de confusion -", model_name, " (Train)")),
               plotOutput(paste0("train_conf_matrix_plot_", model_name))),
        column(6, h4(paste("Matrice de confusion -", model_name, " (Test)")),
               plotOutput(paste0("test_conf_matrix_plot_", model_name)))
      )
    })
    do.call(tagList, matrices)
  })
  
  lapply(names(analysis_results$train_conf_matrices), function(model_name) {
    output[[paste0("train_conf_matrix_plot_", model_name)]] <- renderPlot({
      plot_conf_matrix(analysis_results$train_conf_matrices[[model_name]], model_name, "Train")
    })
    output[[paste0("test_conf_matrix_plot_", model_name)]] <- renderPlot({
      plot_conf_matrix(analysis_results$test_conf_matrices[[model_name]], model_name, "Test")
    })
  })
}

# Résumé, R², AUC, summary(), affichage dans l'UI + ROC + arbres de décision + importance + neurones
render_summaries_and_plots <- function(output, analysis_results) {
  output$result_summaries <- renderUI({
    req(analysis_results$train_results)
    
    summaries <- lapply(names(analysis_results$train_results), function(model_name) {
      train_result <- analysis_results$train_results[[model_name]]
      test_result  <- analysis_results$test_results[[model_name]]
      
      train_r2_value <- if (!is.null(analysis_results$train_r2[[model_name]]) && is.numeric(analysis_results$train_r2[[model_name]])) round(analysis_results$train_r2[[model_name]], 2) else NA
      test_r2_value  <- if (!is.null(analysis_results$test_r2[[model_name]]) && is.numeric(analysis_results$test_r2[[model_name]])) round(analysis_results$test_r2[[model_name]], 2) else NA
      
      train_auc_value <- if (!is.null(analysis_results$train_roc[[model_name]])) round(pROC::auc(analysis_results$train_roc[[model_name]]), 2) else NA
      test_auc_value  <- if (!is.null(analysis_results$test_roc[[model_name]])) round(pROC::auc(analysis_results$test_roc[[model_name]]), 2) else NA
      
      logistic_threshold <- if (!is.null(analysis_results$logistic_threshold)) analysis_results$logistic_threshold else NA
      
      components <- list(
        h4(model_name),
        h5("Données d'apprentissage"),
        if (!is.na(train_r2_value)) p(paste("R² :", train_r2_value)),
        if (!is.na(train_auc_value)) p(paste("AUC :", train_auc_value)),
        if (inherits(train_result, "glm") && !is.na(logistic_threshold)) p(paste("Seuil de classification :", logistic_threshold)),
        verbatimTextOutput(paste0("train_summary_", model_name)),
        
        h5("Données de test"),
        if (!is.na(test_r2_value)) p(paste("R² :", test_r2_value)),
        if (!is.na(test_auc_value)) p(paste("AUC :", test_auc_value)),
        verbatimTextOutput(paste0("test_summary_", model_name))
      )
      
      if (inherits(train_result, "rpart")) {
        components <- append(components, list(
          h5("Arbre de Décision - Apprentissage"),
          plotOutput(paste0("tree_plot_train_", model_name)),
          h5("Arbre de Décision - Test"),
          plotOutput(paste0("tree_plot_test_", model_name))
        ))
      }
      
      if (inherits(train_result, "randomForest")) {
        components <- append(components, list(
          h5("Importance des Variables - Random Forest (Train)"),
          plotOutput(paste0("rf_importance_train_", model_name)),
          h5("Importance des Variables - Random Forest (Test)"),
          plotOutput(paste0("rf_importance_test_", model_name))
        ))
      }
      
      if (inherits(train_result, "nn")) {
        components <- append(components, list(
          h5("Architecture du Réseau de Neurones"),
          plotOutput(paste0("nn_plot_", model_name))
        ))
      }
      
      components <- append(components, list(hr()))
      do.call(tagList, components)
    })
    
    do.call(tagList, summaries)
  })
  
  lapply(names(analysis_results$train_results), function(model_name) {
    train_result <- analysis_results$train_results[[model_name]]
    test_result  <- analysis_results$test_results[[model_name]]
    
    output[[paste0("train_summary_", model_name)]] <- renderPrint({ summary(train_result) })
    output[[paste0("test_summary_", model_name)]]  <- renderPrint({ summary(test_result) })
    
    if (inherits(train_result, "rpart")) {
      output[[paste0("tree_plot_train_", model_name)]] <- renderPlot({ rpart.plot::rpart.plot(train_result, main = paste("Arbre -", model_name, "(Train)")) })
      output[[paste0("tree_plot_test_", model_name)]] <- renderPlot({ rpart.plot::rpart.plot(test_result, main = paste("Arbre -", model_name, "(Test)")) })
    }
    
    if (inherits(train_result, "randomForest")) {
      output[[paste0("rf_importance_train_", model_name)]] <- renderPlot({
        varImp <- caret::varImp(train_result)
        varImpDF <- data.frame(Variables = rownames(varImp), Importance = varImp$Overall)
        ggplot(varImpDF, aes(x = reorder(Variables, Importance), y = Importance)) +
          geom_col(fill = "steelblue") +
          coord_flip() +
          labs(title = paste("Importance des Variables -", model_name, "(Train)"), x = "Variables", y = "Score d'Importance") +
          theme_minimal()
      })
      output[[paste0("rf_importance_test_", model_name)]] <- renderPlot({
        varImp <- caret::varImp(test_result)
        varImpDF <- data.frame(Variables = rownames(varImp), Importance = varImp$Overall)
        ggplot(varImpDF, aes(x = reorder(Variables, Importance), y = Importance)) +
          geom_col(fill = "darkgreen") +
          coord_flip() +
          labs(title = paste("Importance des Variables -", model_name, "(Test)"), x = "Variables", y = "Score d'Importance") +
          theme_minimal()
      })
    }
    
    if (inherits(train_result, "nn")) {
      output[[paste0("nn_plot_", model_name)]] <- renderPlot({
        plot(train_result, rep = "best")
      })
    }
  })
  
  output$roc_plot <- renderPlotly({
    req(analysis_results$train_roc, analysis_results$test_roc)
    
    valid_models <- names(analysis_results$train_roc)[
      sapply(analysis_results$train_roc, function(x) !is.null(x) && inherits(x, "roc"))
    ]
    
    if (length(valid_models) == 0) return(NULL)
    
    plots <- lapply(valid_models, function(model_name) {
      train_roc <- analysis_results$train_roc[[model_name]]
      test_roc  <- analysis_results$test_roc[[model_name]]
      
      plot_ly(x = 1 - train_roc$specificities, y = train_roc$sensitivities,
              type = 'scatter', mode = 'lines', name = paste(model_name, "- Train")) %>%
        add_trace(x = 1 - test_roc$specificities, y = test_roc$sensitivities,
                  type = 'scatter', mode = 'lines', name = paste(model_name, "- Test")) %>%
        layout(title = paste("Courbe ROC -", model_name),
               xaxis = list(title = "1 - Spécificité"),
               yaxis = list(title = "Sensibilité"))
    })
    
    subplot(plots, nrows = 1, shareX = TRUE, shareY = TRUE)
  })
}
