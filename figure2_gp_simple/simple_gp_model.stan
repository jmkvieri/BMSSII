
data {
  int<lower=1> N_obs;
  real x[N_obs];
  real eta_real;
  real rho_real;
  real sigma_real;
}

model {
}

generated quantities {
  matrix[N_obs, N_obs] K;
  vector[N_obs] mu;
  vector[N_obs] y_sim;
  
  real<lower=0> eta;
  real<lower=0> rho; 
  real<lower=0> sigma;
  
 
  eta = eta_real;
  rho = rho_real;
  sigma = sigma_real;
  
  K = cov_exp_quad(x, eta, rho);
  mu = rep_vector(0, N_obs);
 
   for (n in 1:N_obs) {
    K[n, n] = K[n, n] + square(sigma);
  }

  
  y_sim = multi_normal_rng(mu, K);
 
}
