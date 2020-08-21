
  data {
    int N;
    vector[N] x;
    int y[N];
  
    //Allowing to parametrize the priors (useful later)
    real alpha_prior_mean;
    real beta_prior_mean;
    real<lower=0> alpha_beta_prior_precision;
    real<lower=0> tau_nu_prior_shape;
    real<lower=0> tau_nu_prior_rate; 
  }

  transformed data {
    //Stan parametrizes normal with sd not precision
    real alpha_beta_prior_sigma = sqrt(1 / alpha_beta_prior_precision);
  }

  parameters {
    real alpha;
    real beta;
    vector[N] nu_normalized;
    real<lower=0> tau_nu;
  }

  model {
    real nu_sigma = sqrt(1 / tau_nu);
    vector[N] nu = nu_normalized * nu_sigma;

    //taking advantage of Stan's implicit vectorization here
    nu_normalized ~ normal(0,1);
    //The built-in poisson_log(x) === poisson(exp(x))
    y ~ poisson_log(alpha + beta*x + nu); 

    alpha  ~ normal(alpha_prior_mean, alpha_beta_prior_sigma);
    beta  ~ normal(beta_prior_mean, alpha_beta_prior_sigma); 
    tau_nu ~ gamma(tau_nu_prior_shape,tau_nu_prior_rate);
  }

//Uncomment this to have the model generate mu values as well
//Currently commented out as storing the samples of mu consumes 
//a lot of memory for the big models
/*  
  generated quantities {
    vector[N] mu = exp(alpha + beta*x + nu_normalized * nu_sigma);
  }
*/

