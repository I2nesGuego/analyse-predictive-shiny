# helpers/model_utils.R

run_analysis <- function(df, model, analysis_type, num_targets, model_params, input, session, output, analysis_results) {
  library(caret)
  library(randomForest)
  library(neuralnet)
  library(rpart)
  library(pROC)
  
  # Garder uniquement les colonnes numériques et non constantes
  df <- df[, sapply(df, is.numeric)]
  df <- df[, sapply(df, function(col) length(unique(col)) > 1)]
  
  # Séparer variables cibles et features
  target_cols <- tail(names(df), num_targets)
  feature_cols <- setdiff(names(df), target_cols)
  
  target_vector <- if (num_targets == 1) df[[target_cols]] else df[target_cols]
  
  data_preprocessed <- list(
    features = df[, feature_cols],
    targets = target_vector
  )
  
  set.seed(123)
  sample <- sample.split(data_preprocessed$targets, SplitRatio = input$train_split / 100)
  
  train_features <- data_preprocessed$features[sample == TRUE, ]
  test_features  <- data_preprocessed$features[sample == FALSE, ]
  train_targets  <- data_preprocessed$targets[sample == TRUE]
  test_targets   <- data_preprocessed$targets[sample == FALSE]
  
  if (analysis_type == "Classification") {
    train_targets <- as.factor(train_targets)
    test_targets <- as.factor(test_targets)
  } else {
    train_targets <- as.numeric(train_targets)
    test_targets <- as.numeric(test_targets)
  }
  
  
  # RANDOM FOREST
  if (model == "Random Forest") {
    rf_model <- randomForest(train_features, train_targets, ntree = model_params$rf_trees)
    train_pred <- predict(rf_model, train_features)
    test_pred  <- predict(rf_model, test_features)
    
    analysis_results$train_results[[model]] <- rf_model
    analysis_results$test_results[[model]]  <- rf_model
    
    if (analysis_type == "Classification") {
      analysis_results$train_conf_matrices[[model]] <- as.matrix(confusionMatrix(train_pred, train_targets)$table)
      analysis_results$test_conf_matrices[[model]]  <- as.matrix(confusionMatrix(test_pred, test_targets)$table)
      analysis_results$train_roc[[model]] <- roc(as.numeric(train_targets), as.numeric(train_pred))
      analysis_results$test_roc[[model]]  <- roc(as.numeric(test_targets), as.numeric(test_pred))
    } else {
      analysis_results$train_r2[[model]] <- R2(train_pred, train_targets)
      analysis_results$test_r2[[model]]  <- R2(test_pred, test_targets)
    }
  }
  
  # REGRESSION LOGISTIQUE : Classification OU Régression
  if (model == "Régression logistique") {
    train_data <- data.frame(train_features, train_targets)
    test_data  <- data.frame(test_features)
    colnames(train_data)[ncol(train_data)] <- "train_targets"
    
    if (analysis_type == "Classification") {
      glm_model <- glm(train_targets ~ ., data = train_data, family = binomial)
      train_pred_prob <- predict(glm_model, newdata = train_data, type = "response")
      test_pred_prob  <- predict(glm_model, newdata = test_data, type = "response")
      
      logistic_threshold <- model_params$logistic_threshold
      if (is.null(logistic_threshold) || is.na(logistic_threshold)) logistic_threshold <- 0.5
      
      train_pred_class <- ifelse(train_pred_prob >= logistic_threshold, 1, 0)
      test_pred_class  <- ifelse(test_pred_prob >= logistic_threshold, 1, 0)
      
      analysis_results$train_results[[model]] <- glm_model
      analysis_results$test_results[[model]]  <- glm_model
      analysis_results$train_conf_matrices[[model]] <- confusionMatrix(as.factor(train_pred_class), train_targets)
      analysis_results$test_conf_matrices[[model]]  <- confusionMatrix(as.factor(test_pred_class), test_targets)
      analysis_results$train_roc[[model]] <- roc(as.numeric(train_targets), train_pred_prob)
      analysis_results$test_roc[[model]]  <- roc(as.numeric(test_targets), test_pred_prob)
      
    } else if (analysis_type == "Régression") {
      train_targets <- as.numeric(train_targets)
      test_targets  <- as.numeric(test_targets)
      
      lm_model <- lm(train_targets ~ ., data = train_data)
      train_pred <- predict(lm_model, newdata = train_data)
      test_pred  <- predict(lm_model, newdata = test_data)
      
      analysis_results$train_r2[[model]] <- R2(train_pred, train_targets)
      analysis_results$test_r2[[model]]  <- R2(test_pred, test_targets)
      analysis_results$train_results[[model]] <- lm_model
      analysis_results$test_results[[model]]  <- lm_model
    }
  }
  
  # REGRESSION LINEAIRE
  if (model == "Régression linéaire") {
    lin_model <- lm(train_targets ~ ., data = data.frame(train_features, train_targets))
    train_pred <- predict(lin_model, newdata = train_features)
    test_pred  <- predict(lin_model, newdata = test_features)
    
    analysis_results$train_r2[[model]] <- R2(train_pred, train_targets)
    analysis_results$test_r2[[model]]  <- R2(test_pred, test_targets)
    
    analysis_results$train_results[[model]] <- lin_model
    analysis_results$test_results[[model]]  <- lin_model
  }
  
  # RESEAUX DE NEURONES
  if (model == "Réseaux de neurones") {
    hidden_layers <- unlist(model_params$nn_neurons)
    
    train_scaled <- as.data.frame(scale(train_features))
    test_scaled  <- as.data.frame(scale(test_features))
    
    colnames(train_scaled) <- colnames(train_features)
    colnames(test_scaled)  <- colnames(train_features)
    
    train_data_nn <- data.frame(train_scaled, target = as.numeric(as.character(train_targets)))
    train_data_nn <- na.omit(train_data_nn)
    
    if (any(is.na(train_data_nn$target)) || any(is.infinite(train_data_nn$target))) {
      stop("Erreur : la variable cible contient des NA ou des valeurs infinies.")
    }
    
    formula_nn <- as.formula(paste("target ~", paste(colnames(train_features), collapse = " + ")))
    
    nn_model <- neuralnet(formula = formula_nn, data = train_data_nn,
                          hidden = hidden_layers,
                          linear.output = (analysis_type == "Régression"),
                          stepmax = 1e6)
    
    train_pred <- predict(nn_model, train_scaled)
    test_pred  <- predict(nn_model, test_scaled)
    
    analysis_results$train_results[[model]] <- nn_model
    analysis_results$test_results[[model]]  <- nn_model
    analysis_results$nn_model_plots[[model]] <- nn_model  # Ajout pour affichage du graphique
    
    if (analysis_type == "Classification") {
      train_pred_class <- ifelse(train_pred > 0.5, 1, 0)
      test_pred_class  <- ifelse(test_pred > 0.5, 1, 0)
      
      analysis_results$train_conf_matrices[[model]] <- confusionMatrix(as.factor(train_pred_class), train_targets)
      analysis_results$test_conf_matrices[[model]]  <- confusionMatrix(as.factor(test_pred_class), test_targets)
      
      analysis_results$train_roc[[model]] <- roc(as.numeric(train_targets), train_pred)
      analysis_results$test_roc[[model]]  <- roc(as.numeric(test_targets), test_pred)
    } else {
      analysis_results$train_r2[[model]] <- R2(train_pred, train_targets)
      analysis_results$test_r2[[model]]  <- R2(test_pred, test_targets)
    }
  }
  
  
  # ARBRES DE DECISION
  if (model == "Arbres de Décision") {
    method_type <- ifelse(analysis_type == "Classification", "class", "anova")
    tree_model <- rpart(train_targets ~ ., data = data.frame(train_features, train_targets), method = method_type)
    
    analysis_results$train_results[[model]] <- tree_model
    analysis_results$test_results[[model]]  <- tree_model
    
    if (analysis_type == "Classification") {
      train_pred <- predict(tree_model, newdata = train_features, type = "class")
      test_pred  <- predict(tree_model, newdata = test_features, type = "class")
      
      analysis_results$train_conf_matrices[[model]] <- confusionMatrix(train_pred, train_targets)
      analysis_results$test_conf_matrices[[model]]  <- confusionMatrix(test_pred, test_targets)
      
      train_prob <- predict(tree_model, newdata = train_features, type = "prob")
      test_prob  <- predict(tree_model, newdata = test_features, type = "prob")
      
      analysis_results$train_roc[[model]] <- roc(as.numeric(train_targets), train_prob[, 2])
      analysis_results$test_roc[[model]]  <- roc(as.numeric(test_targets), test_prob[, 2])
    } else {
      train_pred <- predict(tree_model, newdata = train_features)
      test_pred  <- predict(tree_model, newdata = test_features)
      
      analysis_results$train_r2[[model]] <- R2(train_pred, train_targets)
      analysis_results$test_r2[[model]]  <- R2(test_pred, test_targets)
    }
  }
}


run_all_analyses <- function(input, output, session, data, model_params, analysis_results) {
  req(data(), input$analysis_choices)
  
  updateProgressBar(session = session, id = "progress", value = 0)
  df <- data()
  total_models <- length(input$analysis_choices)
  
  # Réinitialisation avant la boucle
  analysis_results$train_results <- list()
  analysis_results$test_results <- list()
  analysis_results$train_conf_matrices <- list()
  analysis_results$test_conf_matrices <- list()
  analysis_results$train_roc <- list()
  analysis_results$test_roc <- list()
  analysis_results$train_r2 <- list()
  analysis_results$test_r2 <- list()
  analysis_results$nn_model_plots <- list()
  
  
  for (i in seq_along(input$analysis_choices)) {
    model <- input$analysis_choices[i]
    updateProgressBar(session, id = "progress", value = round((i / total_models) * 100))
    
    tryCatch({
      run_analysis(df, model, input$analysis_type, input$num_targets, model_params, input, session, output, analysis_results)
    }, error = function(e) {
      showNotification(paste("Erreur pour le modèle", model, ":", e$message), type = "error")
    })
  }
  
  updateProgressBar(session, id = "progress", value = 100)
  showNotification("Analyse terminée !", type = "message")
}