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


# Pseudocode 
# 1. import data 
# a. create new csv called "ca_plant_search_fixed.csv"
# b. Couldn't use "ca_plant_search.csv" because two top rows -- read_csv couldn't read i
# 2. clean data 
# a. clean names 
# b. select columns
# c. remove characteristics_data = no
# 2.1 address columns that have multiple inputs per cell
# a. columns = growth_habit, growth_period
# 3. join data: ca_plants and characteristics_clean (this gives us CA data)
####### this is where I left off - Nico 

####### this is rough pseudocode moving forward
# BINARY LOGISTIC REGRESSION (ref. lab 3)
# 4. make factors factors
# 5. Split data
# 6. Build the model
# a. Write recipes (we're going to ultimately want to test multiple recipes)
# b. Create workflow
# c. Fit data and get predictions 
# d. evaluate performance with accuracy and roc_auc from yardstick package 
# 7. Cross-Fold validation
# 8. Finalize model

# Thinking about doing random forest instead/additionally? 

# IMPORT DATA 
#### import characteristics data 
characteristics_raw <- read_csv(here::here("data", "PLANTS_Characteristics_Plus_Data.csv"), 
                                locale = locale(encoding = "latin1")) |>
  janitor::clean_names()

#### import CA plant search data 
ca_plants <- read.csv(here::here("data", "ca_plant_search_fixed.csv")) |>
  janitor::clean_names()


# CLEAN DATA 
#### select columns 
chrctr_wrangle <- characteristics_raw |>
  select(accepted_symbol, scientific_name, characteristics_data, growth_habit, growth_period = active_growth_period, 
         fire_resistance, growth_rate, height = height_mature_feet, drought_tolerance, fire_tolerance,
         moisture_use, planting_density = planting_density_per_acre_maximum, 
         root_depth = root_depth_minimum_inches, bloom_period) |>
  filter(characteristics_data != "No") 

#### make growth_habit tidy
chrctr_habit <- chrctr_wrangle |>
  separate(growth_habit, into = c("habit_1", "habit_2", "habit_3"), 
           sep = ", ", fill = "right", extra = "drop") |>
  pivot_longer(cols = habit_1:habit_3, 
               names_to = "habit_number",
               values_to = "growth_habit") |>
  drop_na() |>
  select(-habit_number)

#### make growth_period tidy
chrctr_period <- chrctr_habit |>
  separate(growth_period, into = c("growth_period_1", "growth_period_2"),
           sep = " and ", fill = "right", extra = "drop") |>
  separate(growth_period_1, into = c("growth_period_1", "growth_period_2", 
                                     "growth_period_3"),
           sep = ", ", fill = "right", extra = "drop") |>
  pivot_longer(cols = growth_period_1:growth_period_3,
               names_to = "growth_number",
               values_to = "growth_period") |>
  drop_na() |>
  select(-growth_number)

#### clean characteristics dataset 
characteristics_clean <- chrctr_period |>
  select(-characteristics_data)


# JOIN DATA 
ca_full_data <- full_join(ca_plants, characteristics_clean, by = "accepted_symbol") |>
  select(-scientific_name.x) |>
  rename(scientific_name = scientific_name.y) |>
  drop_na()

# EXPORT CSV 
write.csv(ca_full_data, here::here("data", "ca_plants_clean_chrctr.csv"))


#### BINARY LOGISTIC REGRESSION ######
# Pseudocode 

# BINARY LOGISTIC REGRESSION (ref. lab 3)
# 4. make factors factors
# 5. Split data
# 6. Build the model
# a. Write recipes (we're going to ultimately want to test multiple recipes)
# b. Create workflow
# c. Fit data and get predictions 
# d. evaluate performance with accuracy and roc_auc from yardstick package 
# 7. Cross-Fold validation
# 8. Finalize model


# Convert to factors
plant_df <- ca_full_data |>
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


# BUILD MODEL 

library(tidymodels)

# Assuming plant_train_df is your training data
# Create Recipe B: recode growth_period and then use it along with height and root_depth
library(tidymodels)
library(dplyr)

# Assume plant_train_df is already created from your data split

# Define Recipe 2 without planting_density
rec2 <- recipe(fire_resistance ~ moisture_use + growth_period + height + root_depth, 
               data = plant_train_df) %>%
  step_normalize(height, root_depth)

# Define the logistic regression model specification
log_md <- logistic_reg() %>%
  set_engine("glm")

# Create and fit the workflow using the modified Recipe 2
log_fit2 <- workflow() %>%
  add_recipe(rec2) %>%
  add_model(log_md) %>%
  fit(plant_train_df)

# (Optional) View model coefficients
library(broom)
tidy(log_fit2)
