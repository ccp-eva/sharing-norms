library(tidyverse)
library(rstan)

d <- read_csv("../data/raw_data_anonymized.csv")

demog_tab <- d %>%
	select(pid, sex, age, culture) %>%
	unique %>%
	arrange(pid) %>%
	mutate(culture = as.integer(as.factor(culture)),
	       sex = if_else(sex == "MALE", -0.5, 0.5))
# Bandongo = 1, Bayaka = 2
# Male = -0.5, Female = 0.5

stan_data = list(
                           N_data = nrow(d),
                           N_part = length(unique(d$pid)),
                           sex = pull(demog_tab, sex),
                           age = pull(demog_tab, age),
                           culture = pull(demog_tab, culture),
                           part_id = d$pid,
                           condition = d$intra + 1,		# Inter = 1, Intra = 2
                           shared = d$share
		   )

stan_code <- read_file("stan_code.c")

# With sex effects
model <- stan(model_code = stan_code, data=stan_data,
	  warmup=7500, iter=15000, chains=4, cores=4,
	  control=list(adapt_delta=0.95)
		 )
saveRDS(model, "model_sex.rds")

# Without sex effects
stan_data$sex <- rep(0, stan_data$N_part)
model <- stan(model_code = stan_code, data=stan_data,
	  warmup=7500, iter=15000, chains=4, cores=4,
	  control=list(adapt_delta=0.95)
		 )
saveRDS(model, "model_no_sex.rds")
