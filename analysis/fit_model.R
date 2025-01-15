library(tidyverse)
library(rstan)

d <- read_csv("../data/raw_data_anonymized.csv")

demog_tab <- d %>%
	select(pid, age, culture) %>%
	unique %>%
	arrange(pid) %>%
	mutate(culture = as.integer(as.factor(culture)))	# Bandongo = 1, Bayaka = 2

stan_data = list(
                           N_data = nrow(d),
                           N_part = length(unique(d$pid)),
                           age = pull(demog_tab, age),
                           culture = pull(demog_tab, culture),
                           part_id = d$pid,
                           condition = d$intra + 1,		# Inter = 1, Intra = 2
                           shared = d$share
		   )

stan_code <- read_file("stan_code.c")
model <- stan(model_code = stan_code, data=stan_data,
	  warmup=7500, iter=15000, chains=4, cores=4,
	  control=list(adapt_delta=0.95)
		 )
saveRDS(model, "model.rds")
