<p align="center">
  <img src="logo.png" alt="Logo" width="300"/>
</p>

# Application Shiny – Analyse Prédictive

Cette application a été développée dans le cadre d’un **mémoire-projet de Licence MIASHS (IMA – UCO Angers)**.  
Elle permet d’effectuer des analyses de **classification** et de **régression** à partir de jeux de données sous format CSV, via une interface interactive.

---

##  Présentation

L’application vise à faciliter l’analyse de tables décisionnelles pour des utilisateurs non spécialistes.  
Elle intègre plusieurs modèles prédictifs et fournit des résultats visuels interprétables.

Cet outil est à visée expérimentale et universitaire. Des limitations peuvent exister.

---

## Chargement de données

- **Format accepté** : CSV uniquement
- **Taille maximale** : 350 Mo
- **Paramètres configurables** :
  - Séparateur (`;`, `,`, etc.)
  - Caractère décimal (`.` ou `,`)
- **Encodage conseillé** : UTF-8

---

## Fonctionnement

- Tous les paramètres définis par l'utilisateur sont transmis aux modèles (ex. nombre de neurones, split d'apprentissage, etc.).
- Une **barre de progression** indique l’avancement de l’analyse.
- Le code est organisé en modules : `data_utils.R`, `model_utils.R`, `plotting.R`, etc.
- Le package `neuralnet` est utilisé pour plus de contrôle sur les réseaux de neurones.

---

## Classification

### Modèles disponibles :
- Régression logistique
- Arbres de décision
- Random Forest
- Réseaux de neurones (neuralnet)

### Résultats générés :
- Matrices de confusion (train / test)
- Courbes ROC
- AUC (aire sous la courbe)
- Importance des variables (Random Forest)

### Interprétation :
- Une AUC proche de 1 indique une bonne performance.
- La matrice de confusion visualise les bonnes et mauvaises prédictions.

---

## Régression

### Modèles disponibles :
- Régression linéaire
- Régression logistique (en mode régression)
- Arbres de décision
- Random Forest
- Réseaux de neurones

### Résultats générés :
- Résumés de modèles (coefficients, résidus)
- Score R²
- Pas de courbe ROC (non applicable)
- (À venir) RMSE, MAE, graphes prédiction vs réel

---

##  FAQ / Conseils

### Bonnes pratiques :
- Placez les variables cibles en **fin de tableau**.
- Supprimez ou traitez les **valeurs manquantes**.
- Évitez les noms de colonnes avec des caractères spéciaux.

### Limitations connues :
- ROC uniquement pour les modèles **binaires**
- Les réseaux de neurones nécessitent souvent une **normalisation préalable** des données

---

##  Dépannage

- Format attendu pour la variable cible :
  - Numérique pour la régression
  - Facteur pour la classification

- Problèmes fréquents :
  - **Colonnes vides ou constantes**
  - **Colonnes d’identifiants uniques**
  - **Fichier trop volumineux**
  - **Encodage non UTF-8**

---

## Contact

Pour toute question ou retour :

- **Clémence Lambert** – [lambert.clemence05@gmail.com](mailto:lambert.clemence05@gmail.com)  
- **Inès Guego** – [guegoines2@gmail.com](mailto:guegoines2@gmail.com)

---

