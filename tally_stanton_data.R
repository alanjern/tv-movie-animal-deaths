library(readxl)
library(tidyverse)

file_path <- "data/WDAS_Project_Species_Count.xlsx"
sheet_names <- excel_sheets(file_path)

# Read each sheet, drop fully-empty trailing columns (the Mammals sheet has
# many blank columns after the last real species column), and pivot to
# long format (one row per movie-species combination).
species_long <- map_df(sheet_names, function(sheet) {
  df <- read_excel(file_path, sheet = sheet)
  
  # Drop columns that are entirely NA (trailing blank columns)
  df <- df |> select(where(~ !all(is.na(.x))))
  
  df |>
    rename(movie = `Animated Film`, year = Year) |>
    mutate(movie = str_trim(movie)) |>
    pivot_longer(
      cols = -c(movie, year),
      names_to = "species",
      values_to = "present"
    ) |>
    # A cell is "Y"/"y" if the species appears, NA otherwise
    mutate(present = !is.na(present))
})

# Pivot to wide format: one row per movie, one column per species,
# boolean values indicating presence
species_wide <- species_long |>
  # Drop erroneous rows that were picked up during the parsing step
  filter(!is.na(movie)) |> 
  # In case a species column name is duplicated across sheets, or a movie
  # appears more than once for the same species, take the max (TRUE wins)
  group_by(movie, year, species) |>
  summarise(present = any(present), .groups = "drop") |>
  pivot_wider(
    names_from = species,
    values_from = present,
    values_fill = FALSE
  )

print(species_wide)

# Compute the proportion of movies that feature each species
species_proportions <- species_wide |>
  select(-c(movie, year)) |>
  summarise_all(mean) |> 
  print()

# Print out proportions for just Dogs and Cats
species_proportions |>
  select(Dogs, Cats) |>
  print()

# Sum up proportions for all animals except Dogs, Cats, and Horses
proportion_other_animal <- species_wide |>
  select(-c(movie, year, Dogs, Cats, Horses)) |>
  mutate(has_other_animal = rowSums(across(everything())) > 0) |>
  summarise(proportion = mean(has_other_animal)) |> 
  print()