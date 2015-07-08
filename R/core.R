# Copyright (c) Konrad Grzanek
# Created 2015-07-08

library(dplyr)

osize  <- pryr::object_size # Just alias
memuse <- pryr::mem_used    # Likewise

rgc <- function(n = 10) {
  ## Calls gc() n times, 1 when n is negative.
  if (n > 1) {
    for (i in 1:(n - 1)) {
      gc()
    }
  }
  gc()
}

printCondition <- function(prefix = "ERROR: ") {
  # Returns a condition handler that prints a prefixed conditionMessage.
  function(e) {
    print(paste(prefix, conditionMessage(e), sep = ""))
  }
}

mostFrequent <- function(x, n = 10) {
  # Returns a data-frame of n most frequent elements in vector x
  # together with their frequencies.
  data_frame(x) %>% group_by(x) %>% summarise(freq = n()) %>%
    arrange(desc(freq)) %>% head(n)
}

modes <- function(x) {
  # Returns a data-frame of mode values, together with their frequency.
  df <- data_frame(x) %>% group_by(x) %>% summarise(freq = dplyr::n())
  df %>% filter(freq == max(df$freq))
}
