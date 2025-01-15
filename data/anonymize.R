library(tidyverse)

raw_data <- read_csv("raw_data.csv")

intermediate <- raw_data %>%
	rename(original_pid = pid) %>%
	mutate(pid = as.integer(as.factor(original_pid)))

write_csv(intermediate, "raw_data_intermediate.csv")

clean <- intermediate %>%
	select(-original_pid, -ra, -village,
	       -date, -start_time, -end_time)

write_csv(clean, "raw_data_anonymized.csv")
