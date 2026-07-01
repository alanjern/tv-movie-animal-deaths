# load libraries
library(tidyverse)
library(tidytext)
library(textdata)

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
# We take the comment with the highest vote sum for each movie and topic, and only keep comments 
# with a positive vote sum. When there are ties, we take the comment with the highest number of yes votes. 
# If comments are still tied, we take the first one (arbitrary).
# movie_comments <- dtdd_comments |>
#   inner_join(results_all_movies |> select(id, name), by = c("itemid" = "id")) |>
#   left_join(topic_lookup, by = c("topicid" = "TopicId")) |>
#   select(itemid, name, topicid, TopicName, comment, yes, no, voteSum) |>
#   filter(voteSum > 0) |>
#   group_by(itemid, topicid) |>
#   slice_max(order_by = tibble(voteSum, yes), n = 1, with_ties = FALSE) |>
#   ungroup()

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
# tvseries_comments <- dtdd_comments |>
#   inner_join(results_all_tvseries |> select(id, name), by = c("itemid" = "id")) |>
#   left_join(topic_lookup, by = c("topicid" = "TopicId")) |>
#   select(itemid, name, topicid, TopicName, comment, yes, no, voteSum) |> 
#   filter(voteSum > 0)

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


# Load NRC lexicon
nrc <- get_sentiments("nrc")

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
  nrow()

cat("Unique words:", total_unique, "\n")
cat("Matched unique words:", matched_unique, "\n")
cat("Unique word coverage:", round(matched_unique / total_unique * 100, 1), "%\n")


# For each comment in the movie comments dataset: 
# - Count the number of words associated with: (1) anger, (2) sadness, (3) disgust
#   - These emotions are most relevant to the research question
# - Normalize each count by total word length of the comment
# Then compute the average normalized counts for each emotion for:
# - Dog comments, Cat comments, and Animal comments
# Compare these averages
# - Dog vs. Animal; Cat vs. Animal

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




# Re-run the analysis for TV comments

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