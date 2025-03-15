# Binary Logistic Regression
# Firewise Shiny App
# Nico, Kyle, Sarayu
# Wed March 5 2025
#########################################

#Goal: identify which characteristics are most predictive of firesafe plants

# View characteristics data 
library(tidyverse)
library(dplyr)
library(tidyr)
library(yardstick)
library(broom)
library(workflowsets)
library(tidymodels)
library(dplyr)


all_plants <- read_csv(here::here("data", "all_plants.csv"))

# Convert to factors
plant_df <- all_plants |>
  mutate(fire_resistance = factor(fire_resistance)) |>
  mutate(growth_rate = factor(growth_rate)) |>
  mutate(drought_tolerance = factor(drought_tolerance)) |>
  mutate(fire_tolerance = factor(fire_tolerance)) |>
  mutate(moisture_use = factor(moisture_use)) |>
  mutate(bloom_period = factor(bloom_period)) |>
  mutate(growth_habit = factor(growth_habit)) |>
  mutate(growth_period = factor(growth_period))


# Split data
set.seed(123)

plant_split <- initial_split(plant_df, prop = 0.8, strata = fire_resistance)

plant_train_df <- training(plant_split)
plant_test_df <- testing(plant_split)

# Define Recipe 2 without planting_density
rec2 <- recipe(fire_resistance ~ moisture_use + growth_period + height + 
                 planting_density + root_depth, data = plant_train_df) |>
  step_normalize(height, planting_density, root_depth)

# Define the logistic regression model specification
log_md <- logistic_reg() %>%
  set_engine("glm")

# Create and fit the workflow using the modified Recipe 2
log_fit2 <- workflow() %>%
  add_recipe(rec2) %>%
  add_model(log_md) %>%
  fit(plant_train_df)

