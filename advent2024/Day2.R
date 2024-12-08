library(tidyverse)

# Task 1

orig_input = tibble(report = read_lines("daytwo_input.txt"))

input2 = orig_input %>% 
  rowid_to_column("report_id") %>%
  separate_rows(report, sep = " ", convert = TRUE)

# Task 1 
input2 %>%
  group_by(report_id) %>%
  mutate(level = report - lag(report)) %>%
  drop_na() %>%
  mutate(unsafe = level == 0 | abs(level) > 3) %>%
  filter(all(!unsafe)) %>%
  summarise(safe = all(level < 0) | all(level > 0)) %>%
  count(safe)
