lapply(c(
    "ggplot2", "dplyr", "tidyverse", "patchwork", "pacman", "ggbreak",
    "scales", "devtools", "ggbiplot", "readxl", "pheatmap", "RColorBrewer", "circlize", "tidyr", "reshape2", "ggthemes", "ggsignif",
    "cowplot", "plotly", "caret", "e1071", "rpart", "rpart.plot", "randomForest",
    "xgboost", "gbm", "purrr", "usethis", "orca", "leaflet", "chmloader"
), require, character.only = TRUE)

install.packages("ranger")
library(ranger)

library(palmerpenguins)

penguins %>% View()

penguins %>% count(species)

penguins %>% count(island)

penguins %>% count(island, species, sex)

penguins %>% count(sex)

penguins %>%
  filter(!is.na(sex)) %>%
ggplot(aes(flipper_length_mm, bill_length_mm, color = sex, size = body_mass_g)) +
  geom_point(alpha = 0.5) + 
  facet_wrap(~species)

penguins_df <- penguins %>%
  filter(!is.na(sex)) %>%
    select(-year, -island)

penguins_df %>% View()


install.packages("tidymodels")
library(tidymodels)

set.seed(123)
penguins_split <- initial_split(penguins_df, strata = sex)
penguins_split

penguins_train <- training(penguins_split)
penguins_test <- testing(penguins_split)

penguins_train

set.seed(234)
penguins_boot <- bootstraps(penguins_train)
penguins_boot

# model specification

glm_spec <- logistic_reg() %>%
  set_engine("glm")
glm_spec

rc_spec <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")
rc_spec

penguins_wf <- workflow()%>%
    add_formula(sex ~ .)

penguins_wf

glm_rs <- penguins_wf %>%
  add_model(glm_spec) %>%
    fit_resamples(resamples = penguins_boot, control = control_resamples(save_pred = TRUE, verbose = TRUE))

glm_rs

rf_rs <- penguins_wf %>%
  add_model(rc_spec) %>%
    fit_resamples(resamples = penguins_boot, control = control_resamples(save_pred = TRUE, verbose = TRUE))
rf_rs


collect_metrics(rf_rs)

collect_metrics(glm_rs)

rf_rs %>%
    conf_mat_resampled()

glm_rs %>%
    conf_mat_resampled()


rf_rs %>%
  collect_predictions() %>%
  group_by(id)%>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()

glm_rs %>%
  collect_predictions() %>%
  group_by(id)%>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()


penguins_final <- penguins_wf %>%
    add_model(rc_spec) %>%
    last_fit(penguins_split)

penguins_final

collect_metrics(penguins_final)

collect_predictions(penguins_final)%>%
    conf_mat(sex, .pred_class)
