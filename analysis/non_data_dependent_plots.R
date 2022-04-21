source("common.R")

library(ggpubr)

make_explanatory_plot <- function(filename) {
	d <- sample_data_from_priors()
	m <- fit_full_model(d$stan)
	estimates <- as_tibble(as_tibble(rstan::extract(m))$age_params)
	d <- tibble(intra_a = estimates$`1.1`,
                    inter_a = estimates$`2.1`,
                    intra_b = estimates$`1.2`,
                    inter_b = estimates$`2.2`)
	d$inter_completion <- d$inter_a - log(0.05) / d$inter_b
	d$intra_completion <- d$intra_a - log(0.05) / d$intra_b
	d$completion_diff <- d$inter_completion - d$intra_completion

	null_theme <- theme(legend.position = "none",
        		axis.title.y = element_blank(),
        		axis.text.y = element_blank(),
        		axis.ticks.y = element_blank())

	# Leftmost part of plot
	# Individual a and b parameters for inter and intra
	left_col <- d %>%
		dplyr::select(intra_a, inter_a, intra_b, inter_b) %>%
		pivot_longer(cols=everything()) %>%
		mutate(name = factor(case_when(
					name == "intra_a" ~ "Intra-ethnic norm learning parameter a",
					name == "intra_b" ~ "Intra-ethnic norm learning parameter b",
					name == "inter_a" ~ "Inter-ethnic norm learning parameter a",
					name == "inter_b" ~ "Inter-ethnic norm learning parameter b",
					))) %>%
		mutate(name = factor(name,
				     levels=c("Intra-ethnic norm learning parameter a",
				              "Intra-ethnic norm learning parameter b",
				              "Inter-ethnic norm learning parameter a",
				              "Inter-ethnic norm learning parameter b"))) %>%
		ggplot() +
		geom_density(aes(x=value, fill=name), alpha=0.1) +
		facet_wrap(~name, dir="v", ncol=1, scales="free") +
		null_theme +
		ggtitle("(a) Raw model parameters") +
		labs(x="Parameter values")

	# Centre part of plot
	# Inter and intra completion ages
	mid_col <- d %>%
		dplyr::select(intra_completion, inter_completion) %>%
		pivot_longer(cols=everything()) %>%
		mutate(name = case_when(
					name == "inter_completion" ~ "Inter-ethnic norm learning completion age",
					name == "intra_completion" ~ "Intra-ethnic norm learning completion age",
					)) %>%
		mutate(name = factor(name, levels=c("Intra-ethnic norm learning completion age",
						    "Inter-ethnic norm learning completion age"))) %>%
		ggplot() +
		geom_density(aes(x=value, fill=name), alpha=0.1, show.legend = FALSE) +
		xlim(c(5,30)) +
		facet_wrap(~name, dir="v", ncol=1) +
		null_theme +
		ggtitle("(b) Corresponding completion ages") +
		labs(x="Completion ages (years)")

	# Rightmost part of plot
	# Completion age difference
	right_col <- d %>%
		dplyr::select(completion_diff) %>%
		ggplot() +
		geom_density(aes(x=completion_diff), alpha=0.1, show.legend = FALSE) +
		xlim(c(-10,10)) +
		geom_vline(xintercept=0, linetype="dotted") +
		null_theme +
		ggtitle("(c) Difference between completion ages") +
		labs(x="Age difference (years)")
	right_col_data <- ggplot_build(right_col)$data[[1]]
	right_col <- right_col +
		geom_area(data=subset(right_col_data, x > 0),
		aes(x=x, y=y),
		fill="red", alpha=0.1)

	whole <- ggarrange(left_col, mid_col, right_col, nrow=1)
	ggsave(filename, whole)
	return(whole)
}

make_explanatory_plot("methodology_explanation.png")
