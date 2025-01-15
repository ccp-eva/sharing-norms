data{
    // Participant data
    int N_part;
    real age[N_part];
    int  culture[N_part];

    // Task data
    int N_data;
    int<lower=1, upper=N_part> part_id[N_data];
    int<lower=1, upper=2> condition[N_data];
    int<lower=0, upper=1> shared[N_data];
}

parameters{
    real<lower=0, upper=1> start_prob;
    vector<lower=0, upper=1>[4] end_prob;
    vector<lower=0>[2] age_params[4];
}

model{
   
    // Convenience vector
    vector[N_data] p;
    int index;
    vector[2] mu;
    matrix[2, 2] cov;

    // Priors
    start_prob ~ beta(1.4, 1.4);
    end_prob ~ beta(0.75, 0.75);

    // Joint prior
    mu[1] = 10;
    mu[2] = 0.2;
    cov[1, 1] = 15;
    cov[1, 2] = 0.75;
    cov[2, 1] = 0.75;
    cov[2, 2] = 0.20;
    age_params ~ multi_normal(mu, cov);

    // Likelihood
    for(i in 1:N_data) {
	index = 2*(culture[part_id[i]] - 1) + condition[i];
        p[i] = start_prob + (end_prob[index] - start_prob)*inv_logit(age_params[index, 2]*(age[part_id[i]] - age_params[index, 1]));
    }
    shared ~ bernoulli(p);
}

generated quantities {
    vector[N_data] log_lik;
    vector[N_data] p;
    int index;
    for(i in 1:N_data) {
	index = 2*(culture[part_id[i]] - 1) + condition[i];
        p[i] = start_prob + (end_prob[index] - start_prob)*inv_logit(age_params[index, 2]*(age[part_id[i]] - age_params[index, 1]));
        log_lik[i] = bernoulli_lpmf(shared[i] | p[i]);
    }
}
