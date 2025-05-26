args <- commandArgs(trailingOnly = TRUE)
input_text <- args[1]

library(quanteda)
library(Matrix)
library(LiblineaR)

model <- readRDS("/Users/dominik/coding/analyzing-mbti/mbti_model_linear.rds")
label_levels <- readRDS("/Users/dominik/coding/analyzing-mbti/mbti_label_levels.rds")
train_features <- readRDS("/Users/dominik/coding/analyzing-mbti/mbti_features.rds")

# Text vorbereiten
corpus <- corpus(input_text)
toks <- tokens(corpus,
               remove_url = TRUE,
               remove_symbols = TRUE,
               remove_punct = TRUE,
               remove_numbers = TRUE)
toks <- tokens_remove(toks, pattern = "^[^a-zA-Z]+$", valuetype = "regex")
toks <- tokens_tolower(toks)

dfm_input <- dfm(toks)
dfm_input <- dfm_match(dfm_input, features = train_features)

# Prüfen, ob überhaupt bekannte Features enthalten sind
if (nfeat(dfm_input) == 0) {
  cat("UNKNOWN")  # oder gib eine Default-Klasse zurück
  quit(status = 0)
}

# TF-IDF
dfm_input <- dfm_tfidf(dfm_input)
X_input <- as(dfm_input, "dgCMatrix")

# Vorhersage
pred <- predict(model, X_input)
mbti <- label_levels[pred$predictions]
cat("test", mbti)
