args <- commandArgs(trailingOnly = TRUE)
input_text <- args[1]
cat(input_text)

library(quanteda)
library(Matrix)         # Bleibt, falls du später sparse brauchst
library(randomForest)   # Statt LiblineaR

# Modell & Metadaten laden
model <- readRDS("/Users/dominik/coding/analyzing-mbti/mbti_model_rf.rds")
label_levels <- readRDS("/Users/dominik/coding/analyzing-mbti/mbti_label_levels_rf.rds")
train_features <- readRDS("/Users/dominik/coding/analyzing-mbti/mbti_features_rf.rds")

# Text vorbereiten
corpus <- corpus(input_text)
toks <- tokens(corpus,
               remove_url = TRUE,
               remove_symbols = TRUE,
               remove_punct = TRUE,
               remove_numbers = TRUE)
toks <- tokens_remove(toks, pattern = "^[^a-zA-Z]+$", valuetype = "regex")
toks <- tokens_tolower(toks)

# DFM mit passenden Features
dfm_input <- dfm(toks)
dfm_input <- dfm_match(dfm_input, features = train_features)
# cat(dfm_input)

# Prüfen, ob überhaupt bekannte Features enthalten sind
if (nfeat(dfm_input) == 0) {
  cat("UNKNOWN\n")
  quit(status = 0)
}

# TF-IDF und dichte Matrix erzeugen
dfm_input <- dfm_tfidf(dfm_input)
cat(dfm_input)
X_input <- as.matrix(dfm_input)  # Wichtig: randomForest braucht dense matrix
cat(X_input)

# Vorhersage
pred <- predict(model, X_input)  # Ergebnis ist direkt ein Vektor von Faktor-Labels
mbti <- as.character(pred)
cat(mbti, "\n")