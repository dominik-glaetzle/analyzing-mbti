args <- commandArgs(trailingOnly = TRUE)
input_text <- args[1]

library(quanteda)
library(Matrix)
library(LiblineaR)

model <- readRDS("/Users/dominik/coding/analyzing-mbti/mbti_model_linear.rds")
label_levels <- readRDS("/Users/dominik/coding/analyzing-mbti/mbti_label_levels.rds")
train_features <- readRDS("/Users/dominik/coding/analyzing-mbti/mbti_features.rds")

corpus <- corpus(input_text)
toks <- tokens(corpus,
               remove_url = TRUE,
               remove_symbols = TRUE,
               remove_punct = TRUE,
               remove_numbers = TRUE)
toks <- tokens_remove(toks, pattern = "^[^a-zA-Z]+$", valuetype = "regex")
cat(toks[0])
toks <- tokens_tolower(toks)

dfm_input <- dfm(toks)
dfm_input <- dfm_match(dfm_input, features = train_features)

if (nfeat(dfm_input) == 0) {
  cat("UNKNOWN")
  quit(status = 0)
}

dfm_input <- dfm_tfidf(dfm_input)
X_input <- as(dfm_input, "dgCMatrix")
print(dfm_input)

pred <- predict(model, X_input)
mbti <- label_levels[pred$predictions]
cat("test", toks)
