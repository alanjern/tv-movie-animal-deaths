# load libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(irr)

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
movie_comments <- dtdd_comments |>
  inner_join(results_all_movies |> select(id, name), by = c("itemid" = "id")) |>
  left_join(topic_lookup, by = c("topicid" = "TopicId")) |>
  select(itemid, name, topicid, TopicName, comment, yes, no, voteSum) |>
  filter(voteSum > 0) |>
  group_by(itemid, topicid) |>
  slice_max(order_by = tibble(voteSum, yes), n = 1, with_ties = FALSE) |>
  ungroup()

# Comments for TV series
tvseries_comments <- dtdd_comments |>
  inner_join(results_all_tvseries |> select(id, name), by = c("itemid" = "id")) |>
  left_join(topic_lookup, by = c("topicid" = "TopicId")) |>
  select(itemid, name, topicid, TopicName, comment, yes, no, voteSum) |> 
  filter(voteSum > 0)

###################################################################################
## LLM labeling analysis
###################################################################################

# Features to look at
# 0. Whether the comment identifies that an animal died or not
# 1. The emotionality of the death
# 2. Whether the death happened on or off screen
# 3. Species
# 4. Killed deliberately or accidentally
# 5. Whether the death was a major plot point or not
# 6. Named individual or not

llm_prompt <- r"(You are scoring comments from a website where users describe whether and how animals die in movies and TV shows. Each comment was written by a member of the public to help other viewers know what to expect.

Your task is to score each comment on the following dimensions. Return your response as a JSON object with the exact keys specified below.

1. "death_confirmed": Does the comment describe an actual animal death?
   - "yes": a death is clearly described
   - "no": the comment explicitly states no death occurs, or the animal survives
   - "ambiguous": it is unclear whether a death occurs (e.g. death is implied but not confirmed, the commenter is uncertain, or the comment is not a description of the film's content at all)

Score the comment with respect to the specific animal type being asked about. For example, if the topic is "a dog dies" and the comment mentions only a cat dying, score "death_confirmed" as "no".
If "death_confirmed" is "no", set all remaining fields to null and stop.
If "death_confirmed" is "ambiguous", score all remaining fields as completely as possible based on what the comment describes. Only set fields to null if there is genuinely no information in the comment to score them.

2. "emotionality": How emotionally is the death portrayed in the film, based on what the comment describes? Rate on a scale of 1 to 5:
   - 1: The death is purely incidental with no emotional weight. It happens in the background or is mentioned in passing with no narrative significance.
   - 2: The death is minor but acknowledged. It is shown or mentioned but not dwelt upon.
   - 3: The death has some emotional weight. It is given some attention in the narrative but is not a central moment.
   - 4: The death is portrayed as significant and is likely to affect the viewer emotionally.
   - 5: The death is portrayed as highly emotional or distressing. It is a major moment in the film that is likely to have a strong emotional impact on the viewer.

3. - "on_screen": Is the death or its aftermath shown on screen?
   - "on_screen": the actual moment of death is explicitly shown happening
   - "aftermath": the death itself is not shown but the dead body or clear physical evidence of the death is shown (e.g. a dead body, blood, remains)
   - "off_screen": neither the death nor its aftermath is shown —- the death is only implied, described in dialogue, or mentioned in passing
   - "ambiguous": it is genuinely unclear from the comment which of the above applies

4. "intentionality": Was the death deliberate or accidental?
   - "deliberate": a character intentionally kills the animal
   - "accidental": the animal dies by accident, natural causes, or as an unintended consequence
   - "ambiguous": unclear from the comment

5. "species": What species of animal died? Provide a short answer (e.g. "dog", "horse", "sheep; rabbit"). If multiple animals of different species die, list them all and separate them with semicolons. If the species is unclear or unspecified, write "unknown".

6. "named_animal": Was the animal a named or personally significant animal — that is, an animal that is treated as an individual character in the story rather than a background or incidental animal?
   - "yes": the animal has a name or is clearly treated as an individual character that the audience is meant to care about
   - "no": the animal is unnamed and plays no individual role in the story
   - "ambiguous": unclear from the comment

7. "plot_point": Was the death a major plot point — that is, did it have a significant effect on the story or characters?
   - "yes": the death clearly affects the story or characters in a significant way
   - "no": the death appears incidental and does not affect the story
   - "ambiguous": unclear from the comment

Important guidelines:
- Base your scoring solely on what the comment says. Do not use any prior knowledge of the movie or TV show.
- Score the emotional portrayal of the death in the film, not the emotional tone of the comment itself. A comment written in a matter-of-fact style may still describe a highly emotional scene.
- If a death is described as comedic, score emotionality based on how impactful the scene is described as being, not on whether it is funny.
- If the comment is not a description of film content (for example, if it is arguing with other users or discussing voting), set "death_confirmed" to "ambiguous" and all other fields to null.
- If the comment refers to events in a book, novel, or other source material rather than the film or TV show being discussed, set "death_confirmed" to "ambiguous" and all other fields to null.

Here is the topic and comment to score:

Topic: [TOPIC]
Comment: [COMMENT])"


# Function: Score a comment using the LLM
# Model: Claude Sonnet 4.6
score_comment <- function(comment, topic, prompt_template, max_retries = 3) {
  
  # Insert the comment and topic into the prompt
  prompt <- gsub("\\[COMMENT\\]", comment, prompt_template)
  prompt <- gsub("\\[TOPIC\\]", topic, prompt)
  
  for (attempt in 1:max_retries) {
    result <- tryCatch({
      response <- POST(
        url = "https://api.anthropic.com/v1/messages",
        add_headers(
          "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
          "anthropic-version" = "2023-06-01",
          "content-type" = "application/json"
        ),
        body = toJSON(list(
          model = "claude-sonnet-4-6",
          max_tokens = 1000,
          # From the docs: Users may encounter non-determinism in APIs. 
          # Even with temperature set to 0, the results will not be fully deterministic
          temperature = 0,
          messages = list(
            list(role = "user", content = prompt)
          )
        ), auto_unbox = TRUE)
      )
      
      status <- status_code(response)
      
      # Retry on rate limit or server errors
      if (status == 429 || status >= 500) {
        warning(paste("Attempt", attempt, "failed with status", status, "- retrying"))
        Sys.sleep(2^attempt)  # exponential backoff
        return(NULL)
      }
      
      if (status != 200) {
        warning(paste("API error with status", status, ":", content(response, "text")))
        return(NA_character_)
      }
      
      content_parsed <- fromJSON(content(response, "text", encoding = "UTF-8"))
      return(content_parsed$content$text)
      
    }, error = function(e) {
      warning(paste("Attempt", attempt, "failed with error:", e$message))
      Sys.sleep(2^attempt)
      return(NULL)
    })
    
    if (!is.null(result)) return(result)
  }
  
  # All retries exhausted
  warning("All retries exhausted, returning NA")
  return(NA_character_)
}



############################################################
# Set up a movie testing regime for the LLM labeling agent
############################################################
set.seed(2026)
movie_comments_devset <- movie_comments |>
  slice_sample(n = 50)
movie_comments_testset <- movie_comments |>
  filter(!itemid %in% movie_comments_devset$itemid) |>
  slice_sample(n = 75)


#############################################
# Run on the development set
#############################################

if (!file.exists("data/movie_comments_devset.csv")) {
  movie_comments_devset <- movie_comments_devset |>
    mutate(
      scores = map2(comment, TopicName, ~ score_comment(.x, .y, llm_prompt)),
      scores_clean = map_chr(scores, ~ gsub("```json\\n|```", "", .x)),
      parsed = map(scores_clean, ~ fromJSON(.x)),
      death_confirmed = map_chr(parsed, "death_confirmed"),
      emotionality = map_dbl(parsed, ~ ifelse(is.null(.x$emotionality), NA, .x$emotionality)),
      on_screen = map_chr(parsed, ~ ifelse(is.null(.x$on_screen), NA_character_, .x$on_screen)),
      intentionality = map_chr(parsed, ~ ifelse(is.null(.x$intentionality), NA_character_, .x$intentionality)),
      species = map_chr(parsed, ~ ifelse(is.null(.x$species), NA_character_, .x$species)),
      named_animal = map_chr(parsed, ~ ifelse(is.null(.x$named_animal), NA_character_, .x$named_animal)),
      plot_point = map_chr(parsed, ~ ifelse(is.null(.x$plot_point), NA_character_, .x$plot_point))
    ) |>
    select(-scores_clean, -parsed)
  
  # save to file
  write_csv(movie_comments_devset, "data/movie_comments_devset.csv")
} else {
  movie_comments_devset <- read_csv("data/movie_comments_devset.csv")
}

###################################################################################
# Note: Based on the development set, I added the following to the LLM prompt:
# 1. What to do when comments don't refer to the movie or TV show
# 2. Still score all fields when death_confirmed is "ambiguous"
# 3. Added instructions to mark species as "unknown" when the species is unclear or unspecified
# 4. Added instructions to separate multiple species with semicolons
###################################################################################


#############################################
# Run on the test set
#############################################

# First generate a blank document containing the test set for manual scoring
if (!file.exists("data/movie_comments_testset_humanscored.csv")) {
  movie_comments_testset |>
    select(itemid, name, topicid, TopicName, comment) |>
    mutate(
    death_confirmed = NA,
    emotionality = NA,
    on_screen = NA,
    intentionality = NA,
    species = NA,
    named_animal = NA,
    plot_point = NA 
  ) |> 
    write_csv("data/movie_comments_testset_humanscored.csv")
}

# Then run the LLM scoring on the test set
if (!file.exists("data/movie_comments_testset.csv")) {
movie_comments_testset_llmscored <- movie_comments_testset |>
  mutate(
    scores = map2(comment, TopicName, ~ score_comment(.x, .y, llm_prompt)),
    scores_clean = map_chr(scores, ~ gsub("```json\\n|```", "", .x)),
    parsed = map(scores_clean, ~ fromJSON(.x)),
    death_confirmed = map_chr(parsed, "death_confirmed"),
    emotionality = map_dbl(parsed, ~ ifelse(is.null(.x$emotionality), NA, .x$emotionality)),
    on_screen = map_chr(parsed, ~ ifelse(is.null(.x$on_screen), NA_character_, .x$on_screen)),
    intentionality = map_chr(parsed, ~ ifelse(is.null(.x$intentionality), NA_character_, .x$intentionality)),
    species = map_chr(parsed, ~ ifelse(is.null(.x$species), NA_character_, .x$species)),
    named_animal = map_chr(parsed, ~ ifelse(is.null(.x$named_animal), NA_character_, .x$named_animal)),
    plot_point = map_chr(parsed, ~ ifelse(is.null(.x$plot_point), NA_character_, .x$plot_point))
  ) |>
  select(-scores_clean, -parsed)
  
  # save to file
  write_csv(movie_comments_testset_llmscored, "data/movie_comments_testset.csv")
} else {
  movie_comments_testset_llmscored <- read_csv("data/movie_comments_testset.csv")
}

# Compare LLM scores to human scores
movie_comments_testset_humanscored <- read_csv("data/movie_comments_testset_humanscored.csv")

testset_comparison <- movie_comments_testset_humanscored |>
  select(itemid, topicid, death_confirmed, emotionality, on_screen, 
         intentionality, named_animal, plot_point) |>
  inner_join(
    movie_comments_testset_llmscored |>
      select(itemid, topicid, death_confirmed, emotionality, on_screen,
             intentionality, named_animal, plot_point),
    by = c("itemid", "topicid"),
    suffix = c("_human", "_llm")
  )

# Compute kappa for categorical variables
categorical_vars <- c("death_confirmed", "on_screen", "intentionality", 
                      "named_animal", "plot_point")

kappa_results <- map(categorical_vars, ~ {
  ratings <- testset_comparison |>
    select(human = paste0(.x, "_human"), llm = paste0(.x, "_llm")) |>
    drop_na() # ignore cases where either coder marked null b/c it answered "no" for death_confirmed
  kappa2(ratings)
}) |>
  set_names(categorical_vars)

# Print kappa results
map(kappa_results, ~ tibble(kappa = .x$value, p = .x$p.value)) |>
  bind_rows(.id = "variable")

# ICC for emotionality
emotionality_ratings <- testset_comparison |>
  select(human = emotionality_human, llm = emotionality_llm) |>
  drop_na()

icc(emotionality_ratings, model = "twoway", type = "agreement")

# Notes based on the initial test results
#   variable        kappa        p
#   <chr>           <dbl>    <dbl>
# 1 death_confirmed 0.767 0       
# 2 on_screen       0.430 3.67e- 5
# 3 intentionality  0.769 4.91e-11
# 4 named_animal    0.308 1.06e- 3
# 5 plot_point      0.407 8.29e- 5
# 
# Emotionality ICC: 
# 
#    Model: twoway 
#    Type : agreement 
#
#    Subjects = 43 
#      Raters = 2 
#    ICC(A,1) = 0.7
#
#  F-Test, H0: r0 = 0 ; H1: r0 > 0 
#    F(42,42) = 5.68 , p = 5.91e-08 
#
#  95%-Confidence Interval for ICC Population Values:
#   0.511 < ICC < 0.825
#
# Based on these results and looking at the disagreements,
# - named_animal and plot_point are too inherently ambiguous
# - most of the disagreemnts boil down to judgment calls or about whether to 
#     default to ambiguous or no
# - therefore, I will exclude these variables from analysis
# - on_screen disagreements mostly result from LLM errors / not following the prompt
#     - I will try to fix this by adding more explicit instructions to the prompt
#
# Follow-up note: After updating the prompt and re-running, kappa for on_screen only improved modeestly
# so I will exclude it from analysis as well.
#
# Summing up: The following variables will be excluded from analysis:
# - named_animal
# - plot_point
# - on_screen

###########################################
# Run on the full dataset
###########################################

llm_prompt_revised <- r"(You are scoring comments from a website where users describe whether and how animals die in movies and TV shows. Each comment was written by a member of the public to help other viewers know what to expect.

Your task is to score each comment on the following dimensions. Return your response as a JSON object with the exact keys specified below.

1. "death_confirmed": Does the comment describe an actual animal death?
   - "yes": a death is clearly described
   - "no": the comment explicitly states no death occurs, or the animal survives
   - "ambiguous": it is unclear whether a death occurs (e.g. death is implied but not confirmed, the commenter is uncertain, or the comment is not a description of the film's content at all)

Score the comment with respect to the specific animal type being asked about. For example, if the topic is "a dog dies" and the comment mentions only a cat dying, score "death_confirmed" as "no".
If "death_confirmed" is "no", set all remaining fields to null and stop.
If "death_confirmed" is "ambiguous", score all remaining fields as completely as possible based on what the comment describes. Only set fields to null if there is genuinely no information in the comment to score them.

2. "emotionality": How emotionally is the death portrayed in the film, based on what the comment describes? Rate on a scale of 1 to 5:
   - 1: The death is purely incidental with no emotional weight. It happens in the background or is mentioned in passing with no narrative significance.
   - 2: The death is minor but acknowledged. It is shown or mentioned but not dwelt upon.
   - 3: The death has some emotional weight. It is given some attention in the narrative but is not a central moment.
   - 4: The death is portrayed as significant and is likely to affect the viewer emotionally.
   - 5: The death is portrayed as highly emotional or distressing. It is a major moment in the film or series that is likely to have a strong emotional impact on the viewer.

3. "intentionality": Was the death deliberate or accidental?
   - "deliberate": a character intentionally kills the animal
   - "accidental": the animal dies by accident, natural causes, or as an unintended consequence
   - "ambiguous": unclear from the comment

4. "species": What species of animal died? Provide a short answer (e.g. "dog", "horse", "sheep; rabbit"). If multiple animals of different species die, list them all and separate them with semicolons. If the species is unclear or unspecified, write "unknown".

Important guidelines:
- Base your scoring solely on what the comment says. Do not use any prior knowledge of the movie or TV show.
- Score the emotional portrayal of the death in the film, not the emotional tone of the comment itself. A comment written in a matter-of-fact style may still describe a highly emotional scene.
- If a death is described as comedic, score emotionality based on how impactful the scene is described as being, not on whether it is funny.
- If the comment is not a description of film content (for example, if it is arguing with other users or discussing voting), set "death_confirmed" to "ambiguous" and all other fields to null.
- If the comment refers to events in a book, novel, or other source material rather than the film or TV show being discussed, set "death_confirmed" to "ambiguous" and all other fields to null.

Here is the topic and comment to score:

Topic: [TOPIC]
Comment: [COMMENT])"


# Function to safely parse a single response
safe_parse_response <- function(text) {
  json_str <- str_extract(text, "\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}")
  
  if (is.na(json_str)) {
    return(list(death_confirmed = NA_character_, emotionality = NA_real_,
                intentionality = NA_character_, species = NA_character_))
  }
  
  tryCatch({
    fromJSON(json_str)
  }, error = function(e) {
    warning(paste("Failed to parse JSON:", json_str))
    list(death_confirmed = NA_character_, emotionality = NA_real_,
         intentionality = NA_character_, species = NA_character_)
  })
}

if (!file.exists("data/movie_comments_llmscored.csv")) {

  # Process in batches with checkpointing
  batch_size <- 500
  n_total <- nrow(movie_comments)
  n_batches <- ceiling(n_total / batch_size)

  results_list <- list()

  for (i in 1:n_batches) {
    start_row <- (i - 1) * batch_size + 1
    end_row <- min(i * batch_size, n_total)
    
    checkpoint_file <- paste0("data/checkpoint_batch_", i, ".csv")
    
    if (file.exists(checkpoint_file)) {
      message(paste("Batch", i, "already exists, loading from checkpoint"))
      results_list[[i]] <- read_csv(checkpoint_file)
    } else {
      message(paste("Processing batch", i, "of", n_batches, "(rows", start_row, "to", end_row, ")"))
      
      batch_data <- movie_comments[start_row:end_row, ] |>
        mutate(
          scores = map2(comment, TopicName, ~ score_comment(.x, .y, llm_prompt_revised)),
          scores_clean = map_chr(scores, ~ gsub("```json\\n|```", "", .x)),
          parsed = map(scores_clean, safe_parse_response),
          death_confirmed = map_chr(parsed, "death_confirmed"),
          emotionality = map_dbl(parsed, ~ ifelse(is.null(.x$emotionality), NA, as.numeric(.x$emotionality))),
          intentionality = map_chr(parsed, ~ ifelse(is.null(.x$intentionality), NA_character_, .x$intentionality)),
          species = map_chr(parsed, ~ ifelse(is.null(.x$species), NA_character_, .x$species))
        ) |>
        select(-scores_clean, -parsed)
      
      write_csv(batch_data, checkpoint_file)
      results_list[[i]] <- batch_data
    }
  }

  # Combine all batches
  movie_comments_full_llmscored <- bind_rows(results_list)
  write_csv(movie_comments_full_llmscored, "data/movie_comments_llmscored.csv")

} else {
  movie_comments_full_llmscored <- read_csv("data/movie_comments_llmscored.csv")
}

# Check how well the comment data matches the vote data in terms
# of death totals

# Reshape the vote-based classifications into long format, matching itemid + topic
vote_based_long <- results_movie_details |>
  select(id, dog_death, cat_death, animal_death) |>
  pivot_longer(
    cols = c(dog_death, cat_death, animal_death),
    names_to = "death_type",
    values_to = "vote_classification"
  ) |>
  mutate(
    topicid = case_when(
      death_type == "dog_death" ~ DOG_TOPIC_ID,
      death_type == "cat_death" ~ CAT_TOPIC_ID,
      death_type == "animal_death" ~ ANIMAL_TOPIC_ID
    )
  ) |>
  select(id, topicid, vote_classification)

# Join with the LLM-scored comment data
convergent_validity <- movie_comments_full_llmscored |>
  inner_join(vote_based_long, by = c("itemid" = "id", "topicid" = "topicid")) |>
  select(itemid, name, TopicName, comment, death_confirmed, vote_classification)

# Cross-tabulate
convergent_validity |>
  count(vote_classification, death_confirmed) |>
  pivot_wider(names_from = death_confirmed, values_from = n, values_fill = 0)

# Results
#
#   vote_classification ambiguous    no   yes
#   <lgl>                   <int> <int> <int>
# 1 FALSE                     207   966    69
# 2 TRUE                      322    64   703
#
# Overall agreement rate (excluding amiguous): (966+703) / (966+69+64+703) ~= 92.6%

# Run the tests to see if emotionality scores differ between companion 
# and non-companion animals

library(rstatix)

# Filter to confirmed deaths only
emotionality_data <- movie_comments_full_llmscored |>
  filter(death_confirmed == "yes") |>
  mutate(
    animal_group = case_when(
      topicid == DOG_TOPIC_ID ~ "dog",
      topicid == CAT_TOPIC_ID ~ "cat",
      topicid == ANIMAL_TOPIC_ID ~ "other"
    )
  )

# Dog vs. Other
dog_vs_other <- emotionality_data |>
  filter(animal_group %in% c("dog", "other"))

# Here I use Wilcoxon rank-sum test (Mann-Whitney U test) because emotionality is ordinal
wilcox_dog <- wilcox.test(emotionality ~ animal_group, data = dog_vs_other)
print(wilcox_dog)

# Effect size for dog vs other
dog_vs_other |>
  wilcox_effsize(emotionality ~ animal_group)

# Cat vs. Other
cat_vs_other <- emotionality_data |>
  filter(animal_group %in% c("cat", "other"))

wilcox_cat <- wilcox.test(emotionality ~ animal_group, data = cat_vs_other)
print(wilcox_cat)

# Effect size for cat vs other
cat_vs_other |>
  wilcox_effsize(emotionality ~ animal_group)

# Descriptive summary for context
emotionality_data |>
  group_by(animal_group) |>
  summarise(
    n = n(),
    median = median(emotionality, na.rm = TRUE),
    mean = mean(emotionality, na.rm = TRUE),
    sd = sd(emotionality, na.rm = TRUE)
  )

# Do the same for intentionality

# Dog vs. Other - intentionality
dog_vs_other_intent <- dog_vs_other |>
  filter(!is.na(intentionality))

intent_table_dog <- table(dog_vs_other_intent$animal_group, dog_vs_other_intent$intentionality)
print(intent_table_dog)

chisq_dog <- chisq.test(intent_table_dog)
print(chisq_dog)

# Effect size (Cramer's V)
dog_vs_other_intent |>
  select(animal_group, intentionality) |>
  table() |>
  cramer_v()

# Cat vs. Other - intentionality
cat_vs_other_intent <- cat_vs_other |>
  filter(!is.na(intentionality))

intent_table_cat <- table(cat_vs_other_intent$animal_group, cat_vs_other_intent$intentionality)
print(intent_table_cat)

chisq_cat <- chisq.test(intent_table_cat)
print(chisq_cat)

# Effect size (Cramer's V)
cat_vs_other_intent |>
  select(animal_group, intentionality) |>
  table() |>
  cramer_v()






############################################################
# Set up a TV test set for the LLM labeling agent
############################################################

tv_comments_testset <- tvseries_comments |>
  slice_sample(n = 75)


#############################################
# Run on the test set
#############################################

# First generate a blank document containing the test set for manual scoring
if (!file.exists("data/tv_comments_testset_humanscored.csv")) {
  tv_comments_testset |>
    select(itemid, name, topicid, TopicName, comment) |>
    mutate(
    death_confirmed = NA,
    emotionality = NA,
    intentionality = NA,
    species = NA
  ) |> 
    write_csv("data/tv_comments_testset_humanscored.csv")
}

# Note to self: When scoring these, I realized I left out of the prompt the detail that when 
# the category is 'an animal dies' this means an animal *other than a cat or a dog*. 
# Because this isn't a super important detail for my analyses, I'm just going to code the test set
# as if I'm reading the prompt without prior knowledge and accept that there will be some 
# misclassifications in the death_confirmed variable
#
# But this shouldn't affect the emotionality or intentionality analyses, which aren't based on the death_confirmed
# variable anyway.
#
# BUT: The fact that I've found some cat and dog comments under "an animal dies" is an indication that this data
# is far from clean and worth noting.

# Then run the LLM scoring on the test set
if (!file.exists("data/tv_comments_testset.csv")) {
  tv_comments_testset_llmscored <- tv_comments_testset |>
    mutate(
      scores = map2(comment, TopicName, ~ score_comment(.x, .y, llm_prompt_revised)),
      parsed = map(scores, ~ {
      json_str <- str_extract(.x, "\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}")
      if (is.na(json_str)) {
        return(list(death_confirmed = NA_character_, emotionality = NA_real_,
                    intentionality = NA_character_, species = NA_character_))
      }
      tryCatch({
        fromJSON(json_str)
      }, error = function(e) {
        warning(paste("Failed to parse JSON:", json_str))
        list(death_confirmed = NA_character_, emotionality = NA_real_,
            intentionality = NA_character_, species = NA_character_)
      })
    }),
    death_confirmed = map_chr(parsed, "death_confirmed"),
    emotionality = map_dbl(parsed, ~ ifelse(is.null(.x$emotionality), NA, as.numeric(.x$emotionality))),
    intentionality = map_chr(parsed, ~ ifelse(is.null(.x$intentionality), NA_character_, .x$intentionality)),
    species = map_chr(parsed, ~ ifelse(is.null(.x$species), NA_character_, .x$species))
    ) |>
    select(-parsed)
    
    # save to file
    write_csv(tv_comments_testset_llmscored, "data/tv_comments_testset.csv")
} else {
  tv_comments_testset_llmscored <- read_csv("data/tv_comments_testset.csv")
}

# Compare LLM scores to human scores
tv_comments_testset_humanscored <- read_csv("data/tv_comments_testset_humanscored.csv")

tv_comments_testset_humanscored_deduped <- tv_comments_testset_humanscored |>
  distinct(itemid, topicid, .keep_all = TRUE)

tv_comments_testset_llmscored_deduped <- tv_comments_testset_llmscored |>
  distinct(itemid, topicid, .keep_all = TRUE)

testset_comparison <- tv_comments_testset_humanscored_deduped |>
  select(itemid, topicid, death_confirmed, emotionality, intentionality) |>
  inner_join(
    tv_comments_testset_llmscored_deduped |>
      select(itemid, topicid, death_confirmed, emotionality, intentionality),
    by = c("itemid", "topicid"),
    suffix = c("_human", "_llm")
  )


# Compute kappa for categorical variables
categorical_vars <- c("death_confirmed", "intentionality")

kappa_results <- map(categorical_vars, ~ {
  ratings <- testset_comparison |>
    select(human = paste0(.x, "_human"), llm = paste0(.x, "_llm")) |>
    drop_na() # ignore cases where either coder marked null b/c it answered "no" for death_confirmed
  kappa2(ratings)
}) |>
  set_names(categorical_vars)

# Print kappa results
map(kappa_results, ~ tibble(kappa = .x$value, p = .x$p.value)) |>
  bind_rows(.id = "variable")

# ICC for emotionality
emotionality_ratings <- testset_comparison |>
  select(human = emotionality_human, llm = emotionality_llm) |>
  drop_na()

icc(emotionality_ratings, model = "twoway", type = "agreement")