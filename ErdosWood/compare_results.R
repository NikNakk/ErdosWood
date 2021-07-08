library(tidyverse)
ew1 <- read_lines("c:/Users/nickl/source/repos/ErdosWood/ErdosWood/ew_results.txt")
ew2 <- read_lines("c:/Users/nickl/source/repos/ErdosWood/ErdosWood/ew_results_broader.txt")
ew_proc_l <- list(ew1, ew2) %>%
  map(
    ~str_split_fixed(.x, " -> | skipping ", 2) %>%
    as.data.frame() %>% set_names(c("k", "value")) %>%
    mutate(type = rep(c("search_space", "result"), nrow(.) / 2)) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    separate(search_space, c("prime_sets", "primes_either_side", "total_search")) %>%
    separate(result, c("a(k)", "timing", "total timing"), sep = " : | secs; total ")
  ) %>%
  reduce(left_join, by = "k")
