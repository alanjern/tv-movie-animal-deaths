# analyze_comments_nrc.R
#
# Uses the NRC word-emotion lexicon (via tidytext) as a lexicon-based
# alternative to LLM scoring (see analyze_comments_llm.R) for measuring
# the emotional tone of DDTD comments describing animal deaths.
#
# For each comment, counts words associated with anger, sadness, and
# disgust (the emotions most relevant to my research question), normalizes
# by comment word count, and compares average scores between companion
# animals (dog, cat) and other animals using Wilcoxon rank-sum tests with
# effect sizes. The analysis is run separately for movies and TV series.
#
# Unlike the LLM analysis, only comments for titles with a vote-confirmed
# death are included, and up to the top 3 comments per title/topic are kept
# (rather than just the single top comment).

# load libraries
library(tidyverse)
library(tidytext)
library(textdata)
library(rstatix)

# Read in DDTD meta-data
dtdd_items <- read_csv("data/dtdd_dog_cat_animal_items.csv")

# Read in DDTD comments
dtdd_comments <- read_csv("data/dtdd_dog_cat_animal_comments.csv")

# Get topic name lookup (topicid -> topic name)
topic_lookup <- dtdd_items |>
  distinct(TopicId, TopicName)

# Define topic IDs
DOG_TOPIC_ID <- 153
CAT_TOPIC_ID <- 186
ANIMAL_TOPIC_ID <- 189

# Read in movie and TV series data
results_all_movies <- readRDS("data/imdb_5000_dtdd_results.rds")
results_movie_details <- readRDS("data/imdb_5000_dtdd_movie_details.rds")
results_all_tvseries <- readRDS("data/imdb_tvseries_5000_dtdd_results.rds")
results_tvseries_details <- readRDS("data/imdb_tvseries_5000_dtdd_series_details.rds")

# Comments for movies

# We take the top 3 comments (somewhat arbitrary) for each movie and topic (with 
# positive vote sum). When there are ties, we take the comment with the highest 
# number of yes votes. If comments are still tied, we take the first one (arbitrary).
# We also only include titles where a death was confirmed by vote (i.e., 
# dog_death, cat_death, or animal_death is TRUE).
movie_comments_nrc <- dtdd_comments |>
  inner_join(results_all_movies |> select(id, name), by = c("itemid" = "id")) |>
  left_join(topic_lookup, by = c("topicid" = "TopicId")) |>
  select(itemid, name, topicid, TopicName, comment, yes, no, voteSum) |>
  filter(voteSum > 0) |>
  # Filter to titles where a death was confirmed by vote
  left_join(results_movie_details |> select(id, dog_death, cat_death, animal_death),
            by = c("itemid" = "id")) |>
  filter(
    (topicid == DOG_TOPIC_ID & dog_death == TRUE) |
    (topicid == CAT_TOPIC_ID & cat_death == TRUE) |
    (topicid == ANIMAL_TOPIC_ID & animal_death == TRUE)
  ) |>
  select(-dog_death, -cat_death, -animal_death) |>
  # Take top 3 comments per title/topic
  group_by(itemid, topicid) |>
  slice_max(order_by = tibble(voteSum, yes), n = 3, with_ties = FALSE) |>
  ungroup()

# Comments for TV series

tvseries_comments_nrc <- dtdd_comments |>
  inner_join(results_all_tvseries |> select(id, name), by = c("itemid" = "id")) |>
  left_join(topic_lookup, by = c("topicid" = "TopicId")) |>
  select(itemid, name, topicid, TopicName, comment, yes, no, voteSum) |>
  filter(voteSum > 0) |>
  left_join(results_tvseries_details |> select(id, dog_death, cat_death, animal_death),
            by = c("itemid" = "id")) |>
  filter(
    (topicid == DOG_TOPIC_ID & dog_death == TRUE) |
    (topicid == CAT_TOPIC_ID & cat_death == TRUE) |
    (topicid == ANIMAL_TOPIC_ID & animal_death == TRUE)
  ) |>
  select(-dog_death, -cat_death, -animal_death) |>
  group_by(itemid, topicid) |>
  slice_max(order_by = tibble(voteSum, yes), n = 3, with_ties = FALSE) |>
  ungroup()


# Load NRC lexicon (word -> emotion/sentiment associations)
nrc <- get_sentiments("nrc")

# CHECK: How much of the comment vocabulary is
# actually covered by the NRC lexicon (unmatched words contribute
# nothing to the emotion scores below).
# Tokenize comments
comment_words <- movie_comments_nrc |>
  filter(voteSum > 0) |>
  unnest_tokens(word, comment)

# Check coverage
total_words <- nrow(comment_words)
matched_words <- comment_words |>
  inner_join(nrc, by = "word", relationship = "many-to-many") |>
  nrow()

cat("Total words:", total_words, "\n")
cat("Matched words:", matched_words, "\n")
cat("Coverage:", round(matched_words / total_words * 100, 1), "%\n")

# What proportion of unique words match
total_unique <- comment_words |> distinct(word) |> nrow()
matched_unique <- comment_words |>
  distinct(word) |>
  inner_join(nrc, by = "word") |>
  distinct(word) |>
  nrow()

cat("Unique words:", total_unique, "\n")
cat("Matched unique words:", matched_unique, "\n")
cat("Unique word coverage:", round(matched_unique / total_unique * 100, 1), "%\n")

# Same coverage check, for TV series comments
tv_comment_words <- tvseries_comments_nrc |>
  filter(voteSum > 0) |>
  unnest_tokens(word, comment)

total_words_tv <- nrow(tv_comment_words)
matched_words_tv <- tv_comment_words |>
  inner_join(nrc, by = "word", relationship = "many-to-many") |>
  nrow()

cat("Total words (TV):", total_words_tv, "\n")
cat("Matched words (TV):", matched_words_tv, "\n")
cat("Coverage (TV):", round(matched_words_tv / total_words_tv * 100, 1), "%\n")

total_unique_tv <- tv_comment_words |> distinct(word) |> nrow()
matched_unique_tv <- tv_comment_words |>
  distinct(word) |>
  inner_join(nrc, by = "word") |>
  distinct(word) |>
  nrow()

cat("Unique words (TV):", total_unique_tv, "\n")
cat("Matched unique words (TV):", matched_unique_tv, "\n")
cat("Unique word coverage (TV):", round(matched_unique_tv / total_unique_tv * 100, 1), "%\n")


############################################################
# Emotion scoring: movies
#
# For each comment: count words matching anger/sadness/disgust
# in the NRC lexicon, normalized by the comment's total word
# count, then average within each animal group (dog/cat/other).
############################################################

# Get NRC lexicon filtered to the three emotions of interest
nrc_filtered <- get_sentiments("nrc") |>
  filter(sentiment %in% c("anger", "sadness", "disgust"))

# Tokenize comments and compute normalized emotion scores
movie_comments_nrc_scored <- movie_comments_nrc |>
  unnest_tokens(word, comment) |>
  group_by(itemid, topicid, TopicName) |>
  mutate(total_words = n()) |>
  ungroup() |>
  left_join(nrc_filtered, by = "word", relationship = "many-to-many") |>
  group_by(itemid, topicid, TopicName, total_words) |>
  summarise(
    anger = sum(sentiment == "anger", na.rm = TRUE) / first(total_words),
    sadness = sum(sentiment == "sadness", na.rm = TRUE) / first(total_words),
    disgust = sum(sentiment == "disgust", na.rm = TRUE) / first(total_words),
    .groups = "drop"
  ) |>
  mutate(
    animal_group = case_when(
      topicid == DOG_TOPIC_ID ~ "dog",
      topicid == CAT_TOPIC_ID ~ "cat",
      topicid == ANIMAL_TOPIC_ID ~ "other"
    )
  )

# Compute average normalized scores by animal group
movie_comments_nrc_scored |>
  group_by(animal_group) |>
  summarise(
    n = n(),
    mean_anger = mean(anger),
    mean_sadness = mean(sadness),
    mean_disgust = mean(disgust)
  )

############################################################
# Statistical comparison: movies
#
# Wilcoxon rank-sum test per emotion, comparing dog-vs-other and
# cat-vs-other groups, with rank-biserial effect size (wilcox_effsize).
# Wilcoxon is used because the normalized emotion scores are skewed
# counts, not normally distributed.
############################################################

 # Prepare comparison datasets
dog_vs_other_nrc <- movie_comments_nrc_scored |>
  filter(animal_group %in% c("dog", "other"))

cat_vs_other_nrc <- movie_comments_nrc_scored |>
  filter(animal_group %in% c("cat", "other"))

# Function to run Wilcoxon test and effect size for one emotion and one comparison
run_comparison <- function(data, emotion) {
  formula <- as.formula(paste(emotion, "~ animal_group"))
  test <- wilcox.test(formula, data = data)
  effect <- wilcox_effsize(data, formula)
  tibble(
    emotion = emotion,
    W = test$statistic,
    p = test$p.value,
    effsize = effect$effsize,
    magnitude = effect$magnitude
  )
}

emotions <- c("anger", "sadness", "disgust")

# Dog vs. other
dog_results <- map_df(emotions, ~ run_comparison(dog_vs_other_nrc, .x)) |>
  mutate(comparison = "dog vs. other")

# Cat vs. other
cat_results <- map_df(emotions, ~ run_comparison(cat_vs_other_nrc, .x)) |>
  mutate(comparison = "cat vs. other")

bind_rows(dog_results, cat_results) |>
  select(comparison, emotion, W, p, effsize, magnitude) |>
  arrange(comparison, emotion)




############################################################
# Re-run the analysis for TV comments
#
# Same emotion-scoring and Wilcoxon comparison approach as the
# movie analysis above, applied to tvseries_comments_nrc.
############################################################

# Tokenize comments and compute normalized emotion scores
tvseries_comments_nrc_scored <- tvseries_comments_nrc |>
  unnest_tokens(word, comment) |>
  group_by(itemid, topicid, TopicName) |>
  mutate(total_words = n()) |>
  ungroup() |>
  left_join(nrc_filtered, by = "word", relationship = "many-to-many") |>
  group_by(itemid, topicid, TopicName, total_words) |>
  summarise(
    anger = sum(sentiment == "anger", na.rm = TRUE) / first(total_words),
    sadness = sum(sentiment == "sadness", na.rm = TRUE) / first(total_words),
    disgust = sum(sentiment == "disgust", na.rm = TRUE) / first(total_words),
    .groups = "drop"
  ) |>
  mutate(
    animal_group = case_when(
      topicid == DOG_TOPIC_ID ~ "dog",
      topicid == CAT_TOPIC_ID ~ "cat",
      topicid == ANIMAL_TOPIC_ID ~ "other"
    )
  )

# Compute average normalized scores by animal group
tvseries_comments_nrc_scored |>
  group_by(animal_group) |>
  summarise(
    n = n(),
    mean_anger = mean(anger),
    mean_sadness = mean(sadness),
    mean_disgust = mean(disgust)
  )

 # Prepare comparison datasets
dog_vs_other_nrc <- tvseries_comments_nrc_scored |>
  filter(animal_group %in% c("dog", "other"))

cat_vs_other_nrc <- tvseries_comments_nrc_scored |>
  filter(animal_group %in% c("cat", "other"))

# Function to run Wilcoxon test and effect size for one emotion and one comparison
run_comparison <- function(data, emotion) {
  formula <- as.formula(paste(emotion, "~ animal_group"))
  test <- wilcox.test(formula, data = data)
  effect <- wilcox_effsize(data, formula)
  tibble(
    emotion = emotion,
    W = test$statistic,
    p = test$p.value,
    effsize = effect$effsize,
    magnitude = effect$magnitude
  )
}

emotions <- c("anger", "sadness", "disgust")

# Dog vs. other
dog_results <- map_df(emotions, ~ run_comparison(dog_vs_other_nrc, .x)) |>
  mutate(comparison = "dog vs. other")

# Cat vs. other
cat_results <- map_df(emotions, ~ run_comparison(cat_vs_other_nrc, .x)) |>
  mutate(comparison = "cat vs. other")

bind_rows(dog_results, cat_results) |>
  select(comparison, emotion, W, p, effsize, magnitude) |>
  arrange(comparison, emotion)