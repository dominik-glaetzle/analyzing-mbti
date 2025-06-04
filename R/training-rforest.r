# 0. Pakete laden
library(quanteda)
library(Matrix)
library(randomForest)

# 1. Daten laden
df_full <- read.csv("/Users/dominik/coding/analyzing-mbti/data/mbti_1.csv", stringsAsFactors = FALSE)
set.seed(42)
df <- df_full[sample(1:nrow(df_full), size = floor(nrow(df_full) / 3)), ]

# 2. Corpus + Split
corpus <- corpus(df$posts)
n_total <- ndoc(corpus)
docvars(corpus, "id") <- 1:n_total
train_ids <- sample(1:n_total, size = 0.8 * n_total, replace = FALSE)

corpus_train <- corpus_subset(corpus, id %in% train_ids)
corpus_test  <- corpus_subset(corpus, !id %in% train_ids)

# 3. Tokenisierung & Preprocessing
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
# print(toks_train[1:5])
toks_test  <- clean_tokens(corpus_test)

# 4. DFM & TF-IDF
dfm_train <- dfm(toks_train)
dfm_train <- dfm_trim(dfm_train, min_termfreq = 1)
# print("df train (after trim):")
# print(dfm_train)
dfm_test  <- dfm(toks_test)
dfm_test  <- dfm_match(dfm_test, features = featnames(dfm_train))

tfidf_train <- dfm_tfidf(dfm_train)
print("TF-IDF train:")
print(tfidf_train)
tfidf_test  <- dfm_tfidf(dfm_test)

# 5. Labels vorbereiten
labels <- as.factor(df$type)
y_train <- labels[train_ids]
y_test  <- labels[-train_ids]

# 6. Dense Matrix für randomForest
X_train_dense <- as.matrix(tfidf_train)
X_test_dense  <- as.matrix(tfidf_test)

# 7. Modell trainieren (Random Forest)
model_rf <- randomForest(x = X_train_dense, y = y_train, ntree = 20)

# 8. Vorhersage + Accuracy
pred_rf <- predict(model_rf, X_test_dense)
accuracy_rf <- mean(pred_rf == y_test)
cat("Accuracy (Random Forest):", accuracy_rf, "\n")

# 9. Modell und Features speichern
saveRDS(model_rf, "mbti_model_rf.rds")
saveRDS(levels(y_train), "mbti_label_levels_rf.rds")
saveRDS(featnames(dfm_train), "mbti_features_rf.rds")