# Load libraries
library(tidyverse)
library(palmerpenguins) #example dataset
library(broom)

# 1. Import
penguins <- penguins   # comes preloaded from palmerpenguins

# 2. Tidy
# Already tidy: each row = penguin, columns = variables

# 3. Transform
penguins_summary <- penguins |>
  drop_na(bill_length_mm, species) |>
  group_by(species) |>
  summarise(mean_bill = mean(bill_length_mm))

# 4. Visualize
ggplot(penguins, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot() +
  labs(title = "Penguin Bill Length by Species")

# 5. Model
model <- lm(bill_length_mm ~ species, data = penguins)
tidy(model)   # clean, tabular model output
