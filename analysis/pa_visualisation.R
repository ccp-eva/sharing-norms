library(tidyverse)

d <- read_csv("power_analysis_new_sampling_500.csv")

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

# Heat map

d$x=round(d$true_completion_delta_1*2)/2
d$y_a=abs(d$true_common_start - d$true_inter_norm_1)
d$y_b=abs(d$true_common_start - d$true_intra_norm_1)
d$y=round(pmin(d$y_a, d$y_b), 1)

d_fin %>%
	group_by(x, y) %>%
#	summarise(poster=mean(poster_intra_first_1), N=length(poster_intra_first_1)) %>%
	summarise(poster=mean(poster_intra_first_1)) %>%
	filter(x > 0, y > 0) %>%
	ggplot() +
	geom_tile(aes(x=x, y=y, fill=poster)) +
	geom_contour(aes(x=x, y=y, z=poster, color="green"), breaks=0.75)

d_fin %>%
	group_by(x, y) %>%
	summarise(N=length(poster_intra_first_1))
