# 0. Pakete laden
library(quanteda)
library(Matrix)
library(LiblineaR)

df <- read.csv("/Users/dominik/coding/analyzing-mbti/data/mbti_1.csv", stringsAsFactors = FALSE)
set.seed(42)

corpus <- corpus(df$posts)
n_total <- ndoc(corpus)
docvars(corpus, "id") <- 1:n_total
train_ids <- sample(1:n_total, size = 0.8 * n_total, replace = FALSE)

corpus_train <- corpus_subset(corpus, id %in% train_ids)
corpus_test  <- corpus_subset(corpus, !id %in% train_ids)

clean_tokens <- function(corp) {
  toks <- tokens(corp,
                 remove_url = TRUE,
                 remove_symbols = TRUE,
                 remove_punct = TRUE,
                 remove_numbers = TRUE)
  toks <- tokens_remove(toks, pattern = "^[^a-zA-Z]+$", valuetype = "regex")
  toks <- tokens_tolower(toks)
  return(toks)
}

toks_train <- clean_tokens(corpus_train)
toks_test  <- clean_tokens(corpus_test)

dfm_train <- dfm(toks_train)
dfm_train <- dfm_trim(dfm_train, min_termfreq = 5)
dfm_test  <- dfm(toks_test)
dfm_test  <- dfm_match(dfm_test, features = featnames(dfm_train))

tfidf_train <- dfm_tfidf(dfm_train)
tfidf_test  <- dfm_tfidf(dfm_test)

labels <- as.factor(df$type)
y_train <- labels[train_ids]
y_test  <- labels[-train_ids]

y_train_int <- as.integer(y_train)
y_test_int  <- as.integer(y_test)
label_levels <- levels(labels)

X_train <- as(tfidf_train, "dgCMatrix")
X_test  <- as(tfidf_test, "dgCMatrix")

model <- LiblineaR(data = X_train, target = y_train_int, type = 3)

pred <- predict(model, X_test)
predicted_classes <- factor(label_levels[pred$predictions], levels = label_levels)
actual_classes <- factor(y_test, levels = label_levels)

accuracy <- mean(predicted_classes == actual_classes)
cat("Accuracy:", accuracy, "\n")

saveRDS(model, "mbti_model_linear.rds")
saveRDS(label_levels, "mbti_label_levels.rds")
saveRDS(featnames(dfm_train), "mbti_features.rds")