library(MASS)
library(rstan)
library(tidyverse)

CHILD_MIN_AGE <- 5
CHILD_MAX_AGE <- 16.5
ADULT_MIN_AGE <- 16.5
ADULT_MAX_AGE <- 70

simulate_dataset <- function(n_child, n_adult, common_start,
			     intra_norm_1, intra_centre_1, intra_slope_1,
			     inter_norm_1, inter_centre_1, inter_slope_1,
			     intra_norm_2, intra_centre_2, intra_slope_2,
			     inter_norm_2, inter_centre_2, inter_slope_2) {

	norms <- c(intra_norm_1, inter_norm_1, intra_norm_2, inter_norm_2)
	centres <- c(intra_centre_1, inter_centre_1, intra_centre_2, inter_centre_2)
	slopes <- c(intra_slope_1, inter_slope_1, intra_slope_2, inter_slope_2)

	n_per_culture <- n_child + n_adult
	n_participants <- 2*n_per_culture
	n_rows <- 2*n_participants

	cultures <- c(rep(1, n_per_culture), rep(2, n_per_culture))
	child_ages <- CHILD_MIN_AGE + ((CHILD_MAX_AGE - CHILD_MIN_AGE) / n_child)*(1:n_child-1)
	adult_ages <- ADULT_MIN_AGE + ((ADULT_MAX_AGE - ADULT_MIN_AGE) / n_adult)*(1:n_adult-1)
	ages <- c(child_ages, adult_ages, child_ages, adult_ages)

	d <- tibble(id=numeric(n_rows),
		    culture=numeric(n_rows),
		    age=numeric(n_rows),
		    condition=integer(n_rows),
		    .index=numeric(n_rows),
		    .norm=numeric(n_rows),
		    .centre=numeric(n_rows),
		    .slope=numeric(n_rows),
		    .weight=numeric(n_rows),
		    .p=numeric(n_rows),
		    shared=integer(n_rows),
		    )

	d$id <- rep(1:n_participants, 2)
	d$culture <- cultures[d$id]
	d$age <- ages[d$id]
	d$condition  <- c(rep(1, n_participants), rep(2, n_participants))

	# start vs norm weights
	d$.index <- 2*(d$culture-1) + d$condition 
	d$.norm <- norms[d$.index]
	d$.centre <- centres[d$.index]
	d$.slope <- slopes[d$.index]
	d$.weight <- 1 / (1 + exp(-1*d$.slope*(d$age - d$.centre)))

	# coin tosses
	d$.p <- common_start + d$.weight*(d$.norm - common_start)
	d$shared <- rbinom(n_rows, 1, d$.p)

	# Format return value
	stan_format = list(
			   N_data = nrow(d),
			   N_part = max(d$id),
			   age = ages,
			   culture = cultures,
			   part_id = d$id,
			   condition = d$condition,
			   shared = d$shared
			   )

	return(list(stan=stan_format, tibble=d,
		    params=list(common_start=common_start,
			     intra_norm_1=intra_norm_1,
			     intra_centre_1=intra_centre_1,
			     intra_slope_1=intra_slope_1,
			     inter_norm_1=inter_norm_1,
			     inter_centre_1=inter_centre_1,
			     inter_slope_1=inter_slope_1,
			     intra_norm_2=intra_norm_2,
			     intra_centre_2=intra_centre_2,
			     intra_slope_2=intra_slope_2,
			     inter_norm_2=inter_norm_2,
			     inter_centre_2=inter_centre_2,
			     inter_slope_2=inter_slope_2)))


}

sample_data_from_priors <- function() {

	repeat {
		common_start <- rbeta(1, 1.4, 1.4)
		intra_norm_1 <- rbeta(1, 0.75, 0.75)
		inter_norm_1 <- rbeta(1, 0.75, 0.75)
		intra_norm_2 <- rbeta(1, 0.75, 0.75)
		inter_norm_2 <- rbeta(1, 0.75, 0.75)
	
		params <- mvrnorm(4, c(10.0, 0.80), matrix(c(15.0, 0.75, 0.75, 0.3), nrow=2))

		intra_centre_1 <- params[1, 1]
		inter_centre_1 <- params[2, 1]
		intra_centre_2 <- params[3, 1]
		inter_centre_2 <- params[4, 1]
	
		intra_slope_1 <- params[1, 2]
		inter_slope_1 <- params[2, 2]
		intra_slope_2 <- params[3, 2]
		inter_slope_2 <- params[4, 2]

		if(intra_centre_1 > 0 && inter_centre_1 > 0 && intra_centre_2 > 0 && inter_centre_2 > 0 &&
		   intra_slope_1 > 0  && inter_slope_1 > 0  && intra_slope_2 >0   && inter_slope_2 > 0) {
			break
		}
	}

	d <- simulate_dataset(60, 40 , common_start,
			     intra_norm_1, intra_centre_1, intra_slope_1,
			     inter_norm_1, inter_centre_1, inter_slope_1,
			     intra_norm_2, intra_centre_2, intra_slope_2,
			     inter_norm_2, inter_centre_2, inter_slope_2)
	return(d)
}

fit_full_model <-function(stan_data) {

    stan_code <- read_file("stan_code.c")
    model <- stan(model_code = stan_code, data=stan_data,
	          warmup=7500, iter=15000, chains=4, cores=4,
		  control=list(adapt_delta=0.95)
		 )
    return(model)

}
