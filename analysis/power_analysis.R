source("common.R")

add_delta_to_p <- function(p_start, delta) {
	polarity <- sample(c(-1, 1), 1)
	p <- p_start + polarity*delta
	if(p < 0 || p > 1) {
		p <- p_start - polarity*delta
	}
	return(p)
}

get_norms <- function(common_start) {
	min_delta <- runif(1, 0, 1.0)
	max_delta <- max(abs(1 - common_start), common_start)
	while(min_delta > max_delta) {
		common_start <- runif(1, 0, 1.0)
		max_delta <- max(abs(1 - common_start), common_start)
	}
	greater_delta <- runif(1, min_delta, max_delta)
	norms <- numeric(2)
	norms[1] <- add_delta_to_p(common_start, min_delta)
	norms[2] <- add_delta_to_p(common_start, greater_delta)
	norms <- sample(norms)
	return(norms)
}

sample_data_for_pa <- function() {

	common_start <- runif(1, 0, 1.0)
	norms <- get_norms(common_start)
	intra_norm_1 <- norms[1]
	inter_norm_1 <- norms[2]
	norms <- get_norms(common_start)
	intra_norm_2 <- norms[1]
	inter_norm_2 <- norms[2]

	intra_end_1 <- rnorm(1, 10.5, 1.25)
	intra_duration_1 <- runif(1, 0.5, min(10, intra_end_1))
	intra_centre_1 <- intra_end_1 - 0.5*intra_duration_1
	intra_slope_1 <- 7.221836 / intra_duration_1

	inter_end_1 <- intra_end_1 + runif(1, 0.5, 5)
	inter_duration_1 <- runif(1, 0.5, min(10, inter_end_1))
	inter_centre_1 <- inter_end_1 - 0.5*inter_duration_1
	inter_slope_1 <- 7.221836 / inter_duration_1

	intra_end_2 <- rnorm(1, 10.5, 1.25)
	intra_duration_2 <- runif(1, 0.5, min(10, intra_end_2))
	intra_centre_2 <- intra_end_2 - 0.5*intra_duration_2
	intra_slope_2 <- 7.221836 / intra_duration_2

	inter_end_2 <- intra_end_2 + runif(1, 0.5, 5)
	inter_duration_2 <- runif(1, 0.5, min(10, inter_end_2))
	inter_centre_2 <- inter_end_2 - 0.5*inter_duration_2
	inter_slope_2 <- 7.221836 / inter_duration_2

	d <- simulate_dataset(60, 40 , common_start,
			     intra_norm_1, intra_centre_1, intra_slope_1,
			     inter_norm_1, inter_centre_1, inter_slope_1,
			     intra_norm_2, intra_centre_2, intra_slope_2,
			     inter_norm_2, inter_centre_2, inter_slope_2)

}

N=30000

pa <- tibble(
	true_common_start = numeric(N),
	true_intra_norm_1 = numeric(N),
	true_intra_centre_1 = numeric(N),
	true_intra_slope_1 = numeric(N),
	true_inter_norm_1 = numeric(N),
	true_inter_centre_1 = numeric(N),
	true_inter_slope_1 = numeric(N),
	true_intra_norm_2 = numeric(N),
	true_intra_centre_2 = numeric(N),
	true_intra_slope_2 = numeric(N),
	true_inter_norm_2 = numeric(N),
	true_inter_centre_2 = numeric(N),
	true_inter_slope_2 = numeric(N),
	est_common_start = numeric(N),
	est_intra_norm_1 = numeric(N),
	est_intra_centre_1 = numeric(N),
	est_intra_slope_1 = numeric(N),
	est_inter_norm_1 = numeric(N),
	est_inter_centre_1 = numeric(N),
	est_inter_slope_1 = numeric(N),
	poster_intra_first_1 = numeric(N),
	est_intra_norm_2 = numeric(N),
	est_intra_centre_2 = numeric(N),
	est_intra_slope_2 = numeric(N),
	est_inter_norm_2 = numeric(N),
	est_inter_centre_2 = numeric(N),
	est_inter_slope_2 = numeric(N),
	poster_intra_first_2 = numeric(N),
)

d <- sample_data_from_priors()
m <- fit_full_model(d$stan)

for(i in 1:N) {
	d <- sample_data_for_pa()
	m <- stan(fit=m, data=d$stan,
		      warmup=7500, iter=10000,
		      chains=4, cores=4,
		      control=list(adapt_delta=0.95)
		      )

	estimates <- as_tibble(rstan::extract(m))
	e <- estimates
	e$est_intra_completion_1 = e$age_params[,1,1] - log(0.05) / e$age_params[,1,2]
	e$est_inter_completion_1 = e$age_params[,2,1] - log(0.05) / e$age_params[,2,2]
	e$est_intra_completion_2 = e$age_params[,3,1] - log(0.05) / e$age_params[,3,2]
	e$est_inter_completion_2 = e$age_params[,4,1] - log(0.05) / e$age_params[,4,2]
	ests <- c(est_common_start = mean(estimates$start_prob),
		est_intra_norm_1 = mean(estimates$end_prob[,1]),
		est_intra_centre_1 = mean(estimates$age_params[,1,1]),
		est_intra_slope_1 = mean(estimates$age_params[,1,2]),
		est_inter_norm_1 = mean(estimates$end_prob[,2]),
		est_inter_centre_1 = mean(estimates$age_params[,2,1]),
		est_inter_slope_1 = mean(estimates$age_params[,2,2]),
		poster_intra_first_1 = mean(e$est_intra_completion_1 < e$est_inter_completion_1),
		est_intra_norm_2 = mean(estimates$end_prob[,3]),
		est_intra_centre_2 = mean(estimates$age_params[,3,1]),
		est_intra_slope_2 = mean(estimates$age_params[,3,2]),
		est_inter_norm_2 = mean(estimates$end_prob[,4]),
		est_inter_centre_2 = mean(estimates$age_params[,4,1]),
		est_inter_slope_2 = mean(estimates$age_params[,4,2]),
		poster_intra_first_2 = mean(e$est_intra_completion_2 < e$est_inter_completion_2)
	)

	pa[i,] <- c(d$params, ests)

}

# Calculate "completion times"
pa$true_inter_completion_1 <- pa$true_inter_centre_1 - log(0.05) / pa$true_inter_slope_1
pa$true_inter_completion_2 <- pa$true_inter_centre_2 - log(0.05) / pa$true_inter_slope_2
pa$true_intra_completion_1 <- pa$true_intra_centre_1 - log(0.05) / pa$true_intra_slope_1
pa$true_intra_completion_2 <- pa$true_intra_centre_2 - log(0.05) / pa$true_intra_slope_2
pa$est_inter_completion_1 <- pa$est_inter_centre_1 - log(0.05) / pa$est_inter_slope_1
pa$est_inter_completion_2 <- pa$est_inter_centre_2 - log(0.05) / pa$est_inter_slope_2
pa$est_intra_completion_1 <- pa$est_intra_centre_2 - log(0.05) / pa$est_intra_slope_2
pa$est_intra_completion_2 <- pa$est_intra_centre_2 - log(0.05) / pa$est_intra_slope_2

# Compute measures of effect size
pa$true_completion_delta_1 <- abs(pa$true_intra_completion_1 - pa$true_inter_completion_1)
pa$true_completion_delta_2 <- abs(pa$true_intra_completion_2 - pa$true_inter_completion_2)
pa$true_min_completion_delta <- pmin(pa$true_completion_delta_1, pa$true_completion_delta_2)
delta_1 <- abs(pa$true_inter_norm_1 - pa$true_intra_norm_1)
delta_2 <- abs(pa$true_common_start - pa$true_inter_norm_1)
delta_3 <- abs(pa$true_common_start - pa$true_intra_norm_1)
delta_4 <- abs(pa$true_inter_norm_2 - pa$true_intra_norm_2)
delta_5 <- abs(pa$true_common_start - pa$true_inter_norm_2)
delta_6 <- abs(pa$true_common_start - pa$true_intra_norm_2)
pa$true_min_prob_delta <- pmin(delta_1, delta_2, delta_3, delta_4, delta_5, delta_6)

# Figure out whether our posterior mean estimates of which norm is
# learned fastest is the right way around
pa$true_fastest_1 <- if_else(pa$true_inter_completion_1 > pa$true_intra_completion_1, "intra", "inter")
pa$est_fastest_1 <- if_else(pa$est_inter_completion_1 > pa$est_intra_completion_1, "intra", "inter")
pa$true_fastest_2 <- if_else(pa$true_inter_completion_2 > pa$true_intra_completion_2, "intra", "inter")
pa$est_fastest_2 <- if_else(pa$est_inter_completion_2 > pa$est_intra_completion_2, "intra", "inter")
pa$s_error_1 <- pa$true_fastest_1 != pa$est_fastest_1
pa$s_error_2 <- pa$true_fastest_2 != pa$est_fastest_2
pa$s_error <- pa$s_error_1 | pa$s_error_2

write_csv(pa, "power_analysis.csv")

# Visualisation

d <- pa %>%
	mutate(age_lag=round(true_completion_delta_1))

d_aux <- d %>% 
	group_by(age_lag=round(true_completion_delta_1)) %>%
	summarise(med_p=median(poster_intra_first_1), mean_p=mean(poster_intra_first_1)) %>%
	filter(age_lag < 11)

d %>%
	filter(age_lag < 11) %>%
	ggplot() +
	geom_density(aes(x=poster_intra_first_1), fill="red", alpha=0.2) +
	geom_vline(data=d_aux, aes(xintercept=mean_p)) +
	facet_wrap(~age_lag, dir="v", ncol=1)
ggsave("pa_plot_1.png")

# Heat map

d$x=round(d$true_completion_delta_1*2)/2
d$y_a=abs(d$true_common_start - d$true_inter_norm_1)
d$y_b=abs(d$true_common_start - d$true_intra_norm_1)
d$y=round(pmin(d$y_a, d$y_b), 1)

print(d)

d %>%
	group_by(x, y) %>%
	summarise(poster=mean(poster_intra_first_1)) %>%
	filter(x > 0, y > 0) %>%
	ggplot() +
	geom_tile(aes(x=x, y=y, fill=poster)) +
	geom_contour(aes(x=x, y=y, z=poster, color="green"), breaks=0.75)
ggsave("pa_plot_2.png")

d %>%
	group_by(x, y) %>%
	summarise(N=length(poster_intra_first_1))
