
data {
  int<lower=1> N; // sample size
  int<lower=1> N_loc; // no of cluster
  int<lower=1> M; // M predictors
  matrix[N, M] X; // predictor matrix
  real eta_real;   // fixed parameters
  real rho_real;  // fixed parameters
  real sigma_real; // fixed parameters
  real lat[N_loc]; // latitude
  real lon[N_loc]; // longitude
}


transformed data {
  row_vector[2] dist[N];
  for (n in 1:N) {
    dist[n, 1] = lat[n];
    dist[n, 2] = lon[n];
  }
}


parameters{
}

model {
}

generated quantities {
  matrix[N, N] K;
  matrix[N, N] L;
  vector[N] k;
  vector[N] z_2;
  
  real<lower=0> eta;
  real<lower=0> rho; 
  real<lower=0> sigma;
  vector[M] beta;
  real<lower=0> phi = 20000; //make variability low
  
  
  vector[N] mu;
  vector[N] y_sim;
  
  
  eta = eta_real;
  rho = rho_real;
  sigma = sigma_real;
  
  beta[1] = 0;
  
  //non-centered parameterisation
  for (i in 1:N) {
    z_2[i] = normal_rng(0, 1);
  }
  
  //quadratic exponentiated
  K = cov_exp_quad(dist, eta, rho);
  
  for (n in 1:N) {
    K[n, n] = K[n, n] + square(sigma);
  }
  
  L = cholesky_decompose(K);
  
  
  k = L * z_2;
  
  //linear function
  
  for (i in 1 : N) {
    mu[i] = inv_logit(X[i] * beta + k[i]);
  }
  
  
  for (i in 1 : N) {
    real mu_new;
    
    mu_new =  inv_logit(X[i] * beta + k[i]);
    y_sim[i] = beta_rng(mu_new * phi, (1.0 - mu_new) * phi);
  }
  
  
  
}

