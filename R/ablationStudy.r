library(quanteda)
library(randomForest)
library(lexicon)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidytext)

df <- read.csv("/Users/dominik/coding/analyzing-mbti/data/mbti_1.csv", stringsAsFactors = FALSE)
set.seed(42)
df <- df[sample(1:nrow(df), floor(nrow(df)/2)), ]

# load bing-lexikon
bing <- get_sentiments("bing")
hash_sentiment_bing <- data.frame(x = bing$word, y = bing$sentiment)

# extract linguistic features
count_words <- function(posts, word_list) {
  counts <- sapply(word_list, function(w) str_count(posts, fixed(w)))
  rowSums(counts)
}

# calculate sentiment score using the Bing lexicon
df$sentiment_score <- count_words(df$posts, hash_sentiment_bing$x[hash_sentiment_bing$y == "positive"]) -
                      count_words(df$posts, hash_sentiment_bing$x[hash_sentiment_bing$y == "negative"])

df$pronouns <- str_count(tolower(df$posts), "\\b(i|me|my|you|your|we|us|they|he|she|it|his|her|them)\\b")

# calculate punctuation ratio
df$punctuation <- ifelse(nchar(df$posts) > 0,
                         str_count(df$posts, "[[:punct:]]") / nchar(df$posts),
                         0)

df$length <- str_count(df$posts, "\\S+") # word count
df$vowels <- str_count(df$posts, "[aeiouAEIOU]")
df$consonants <- str_count(df$posts, "[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]")
df$vowel_ratio <- df$vowels / (df$consonants + 1)

# create corpus + tokenization
corp <- corpus(df$posts)
toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
toks <- tokens_tolower(toks)
dfm_tfidf <- dfm(toks) |> dfm_trim(min_termfreq = 5) |> dfm_tfidf()

# create feature matrix
X_linguistic <- df[, c("sentiment_score", "pronouns", "punctuation", "length", "vowel_ratio")]

dfm_df <- convert(dfm_tfidf, to = "data.frame")

X_all <- cbind(X_linguistic, dfm_df)


set.seed(42)
n <- nrow(X_all)
train_ids <- sample(1:n, size = floor(0.8 * n), replace = FALSE)
X_train <- X_all[train_ids, ]
X_test  <- X_all[-train_ids, ]
y_train <- as.factor(df$type[train_ids])
y_test  <- as.factor(df$type[-train_ids])

model <- randomForest(x = X_train, y = y_train, ntree = 80)
pred <- predict(model, X_test)
print("Accuracy with all features:", mean(pred == y_test), "\n")

# feature importance for linguistic features
X_linguistic_train <- as.data.frame(X_train[, 1:5])
model_linguistic <- randomForest(X_linguistic_train, y_train)
imp_df <- data.frame(Feature = colnames(X_linguistic_train),
                     Importance = importance(model_linguistic)[, 1])


ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  ggtitle("Feature Importance (Linguistische Merkmale)") +
  theme_minimal()

feature_sets <- list(
  "TF-IDF only" = dfm_df,
  "Linguistic only" = as.data.frame(X_linguistic),
  "Without Sentiment" = cbind(as.data.frame(X_linguistic[, -1]), dfm_df),
  "Without Pronouns" = cbind(as.data.frame(X_linguistic[, -2]), dfm_df),
  "Without Punctuation" = cbind(as.data.frame(X_linguistic[, -3]), dfm_df),
  "Without Vowel Ratio" = cbind(as.data.frame(X_linguistic[, -5]), dfm_df)
)

for (set_name in names(feature_sets)) {
  X <- feature_sets[[set_name]]
  X_train <- X[train_ids, ]
  X_test <- X[-train_ids, ]
  model <- randomForest(x = X_train, y = y_train, ntree = 80)
  acc <- mean(predict(model, X_test) == y_test)
  print(paste("Accuracy (", set_name, "): ", round(acc, 4), sep = ""))
}