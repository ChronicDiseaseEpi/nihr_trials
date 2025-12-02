# 02_trial_detection

library(tidyverse)
# df must contain a column with the grant description text
# rename `description` below if yours is called something else.
# df <- tibble(id = ..., description = ...)

a <- read_csv("Data/nihr-summary-view.csv.gz")
names(a) <- str_to_lower(names(a))

trial_patterns <- c(
  "randomised",
  "\\btrial\\b",
  "\\brct\\b"
)
trms <- map(trial_patterns, ~ str_detect(a$scientific_abstract %>% str_to_lower(), .x))
names(trms) <- paste0("t", seq_along(trial_patterns))
trms <- bind_cols(trms)
trms$anyhit <- rowMeans(trms) >0
a <- a %>% 
  mutate(istrial = trms$anyhit)
a %>% 
  count(istrial)

a %>% 
  filter(istrial == sample(c(T,F), 1)) %>% 
  mutate(scientific_abstract = paste0(istrial, "\n", scientific_abstract)) %>% 
  pull(scientific_abstract) %>% 
  sample(1) %>% 
  write_lines("temp.txt")
