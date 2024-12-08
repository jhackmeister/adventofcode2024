library(tidyverse)

# Task 1

input <- read.csv("dayone_input1.txt", header = FALSE, col.names = "lists") %>%
  separate(col = lists, into = c("list_1", "list_2"), sep = "  ") %>%
  mutate(
    list_1 = as.numeric(list_1),
    list_2 = as.numeric(list_2)) %>%
  {as.data.frame(apply(., 2, sort))} %>%
  mutate(difference = abs(list_1 - list_2))

answer1 <- sum(input$difference)
answer1

# Task 2 

input <- input %>% 
  rowwise() %>%
  mutate(
    l_count = sum(list_1 == input$list_2), 
  ) 

answer_2 <- sum(input$list_1 * input$l_count)
answer_2
