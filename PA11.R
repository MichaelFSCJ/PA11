# COP2073c - Programming Assignment 11
# Michael Carpenter
# 11/22/2026
# Fits wt ~ cyl in mtcars, predicts group means, and plots them in red.

install.packages(c("tidyverse", "modelr"))
library(tidyverse)
library(modelr)

# 1. Create a tibble with cyl (as a categorical variable) and wt from mtcars
mtcars_tbl <- as_tibble(mtcars) %>%
  select(cyl, wt) %>%
  mutate(cyl = factor(cyl))

# 2. Fit a linear model predicting wt based on cyl
mod_wt_cyl <- lm(wt ~ cyl, data = mtcars_tbl)

# Optional: inspect model coefficients and predicted group means
coef(mod_wt_cyl)

# 3. Create a data grid from cyl and add predictions using pipes
pred_grid <- mtcars_tbl %>%
  data_grid(cyl) %>%
  add_predictions(mod_wt_cyl)

pred_grid

# 4. Plot original data and predicted values (red, size 4)
ggplot(mtcars_tbl, aes(x = cyl, y = wt)) +
  geom_point() +
  geom_point(
    data = pred_grid,
    aes(y = pred),
    color = "red",
    size = 4
  ) +
  labs(
    x = "cyl",
    y = "wt"
  )

# 5. Analysis of results
# - Because 'cyl' is a category predictor, the linear model estimates the
#   mean vehicle weight, 'wt', in 1,000 lbs, for each cyl group.
# - Avg weight increases consistently as the number of cylinders increases:
#   4-cyl vehicles have the lowest mean weight (~2.29), 6-cyl
#   vehicles are in the middle (~3.12), and 8-cylinder vehicles have the
#   highest mean weight (~4.00).
# - The red points mark these group means, while the black points show the
#   variation of individual vehicles around each category's average.