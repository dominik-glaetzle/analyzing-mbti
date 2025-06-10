args <- commandArgs(trailingOnly = TRUE)
input_text <- args[1]

library(quanteda)
library(Matrix)
library(randomForest)

model <- readRDS("/Users/dominik/coding/analyzing-mbti/R/mbti_model_rf.rds")
label_levels <- readRDS("/Users/dominik/coding/analyzing-mbti/R/mbti_label_levels_rf.rds")
train_features <- readRDS("/Users/dominik/coding/analyzing-mbti/R/mbti_features_rf.rds")


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

if (nfeat(dfm_input) == 0) {
  cat("UNKNOWN\n")
  quit(status = 0)
}

X_input <- as.matrix(dfm_input)


pred <- predict(model, X_input)
mbti <- as.character(pred)
cat(mbti, "\n")