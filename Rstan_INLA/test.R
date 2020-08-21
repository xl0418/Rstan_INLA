school_stan <- 'data {
  int<lower=0> J;         
  real y[J];              
  real<lower=0> sigma[J]; 
}

parameters {
  real mu;                
  real<lower=0> tau;      
  vector[J] eta;          
}
transformed parameters {
  vector[J] theta = mu + tau * eta;        
}

model {
  target += normal_lpdf(eta | 0, 1);       
  target += normal_lpdf(y | theta, sigma); 
}'

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- rstan::stan(model_code = school_stan, data = schools_dat)
