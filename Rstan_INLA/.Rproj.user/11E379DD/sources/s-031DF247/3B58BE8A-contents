library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(INLA)
library(tidyverse)
set.seed(6619414)

cache_dir = "_stan_vs_inla_cache/"
if(!dir.exists(cache_dir)){
  dir.create(cache_dir)
}


#The sizes of datasets to work with
N_values = c(100, 500, 5000)
data = list()
for(count_N in c(1:length(N_values))) {
  N = N_values[count_N]
  x = rnorm(N, mean=5,sd=1) 
  nu = rnorm(N,0,0.1)
  mu = exp(1 + 0.5*x + nu) 
  y = rpois(N,mu) 
  
  
  data[[count_N]] = list(
    N = N,
    x = x,
    y = y
  )  
}

# 
# write("
#   data {
#     int N;
#     vector[N] x;
#     int y[N];
#   
#     //Allowing to parametrize the priors (useful later)
#     real alpha_prior_mean;
#     real beta_prior_mean;
#     real<lower=0> alpha_beta_prior_precision;
#     real<lower=0> tau_nu_prior_shape;
#     real<lower=0> tau_nu_prior_rate; 
#   }
# 
#   transformed data {
#     //Stan parametrizes normal with sd not precision
#     real alpha_beta_prior_sigma = sqrt(1 / alpha_beta_prior_precision);
#   }
# 
#   parameters {
#     real alpha;
#     real beta;
#     vector[N] nu_normalized;
#     real<lower=0> tau_nu;
#   }
# 
#   model {
#     real nu_sigma = sqrt(1 / tau_nu);
#     vector[N] nu = nu_normalized * nu_sigma;
# 
#     //taking advantage of Stan's implicit vectorization here
#     nu_normalized ~ normal(0,1);
#     //The built-in poisson_log(x) === poisson(exp(x))
#     y ~ poisson_log(alpha + beta*x + nu); 
# 
#     alpha  ~ normal(alpha_prior_mean, alpha_beta_prior_sigma);
#     beta  ~ normal(beta_prior_mean, alpha_beta_prior_sigma); 
#     tau_nu ~ gamma(tau_nu_prior_shape,tau_nu_prior_rate);
#   }
# 
# //Uncomment this to have the model generate mu values as well
# //Currently commented out as storing the samples of mu consumes 
# //a lot of memory for the big models
# /*  
#   generated quantities {
#     vector[N] mu = exp(alpha + beta*x + nu_normalized * nu_sigma);
#   }
# */
# ","model1.stan")

# stanc("model1.stan")


# Rstan

model_code = "model1.stan"
model = stan_model(file = model_code)

stan_times_file = paste0(cache_dir, "stan_times.csv")
stan_summary_file = paste0(cache_dir, "stan_summary.csv")
run_stan = TRUE
if(file.exists(stan_times_file) && file.exists(stan_summary_file)) {
  stan_times = read.csv(stan_times_file)
  stan_summary = read.csv(stan_summary_file) 
  if(setequal(stan_times$N, N_values) && setequal(stan_summary$N, N_values)) {
    run_stan = FALSE
  }
} 

if(run_stan) {
  stan_times_values = numeric(length(N_values))
  stan_summary_list = list()
  step = 1
  for(N in N_values) {
    data_stan = data[[step]]
    data_stan$alpha_prior_mean = 0
    data_stan$beta_prior_mean = 0
    data_stan$alpha_beta_prior_precision = 0.001
    data_stan$tau_nu_prior_shape = 0.01
    data_stan$tau_nu_prior_rate = 0.01
    
    
    fit = sampling(model, data = data_stan);
    stan_summary_list[[step]] = 
      as.data.frame(
        rstan::summary(fit, pars = c("alpha","beta","tau_nu"))$summary
      ) %>% rownames_to_column("parameter")
    stan_summary_list[[step]]$N = N
    
    all_times = get_elapsed_time(fit)
    stan_times_values[step] = max(all_times[,"warmup"] + all_times[,"sample"])
    
    step = step + 1
  }
  stan_times = data.frame(N = N_values, stan_time = stan_times_values)
  stan_summary = do.call(rbind, stan_summary_list)
  
  write.csv(stan_times, stan_times_file,row.names = FALSE)
  write.csv(stan_summary, stan_summary_file,row.names = FALSE)
}

traceplot(fit, pars = c("alpha", "beta"), inc_warmup = TRUE, nrow = 2)
plot(fit, pars = c("alpha", "beta"))

# INLA

inla_times_file = paste0(cache_dir,"inla_times.csv")
inla_summary_file = paste0(cache_dir,"inla_summary.csv")
run_inla = TRUE
if(file.exists(inla_times_file) && file.exists(inla_summary_file)) {
  inla_times = read.csv(inla_times_file)
  inla_summary = read.csv(inla_summary_file) 
  if(setequal(inla_times$N, N_values) && setequal(inla_summary$N, N_values)) {
    run_inla = FALSE
  }
} 

if(run_inla) {
  inla_times_values = numeric(length(N_values))
  inla_summary_list = list()
  step = 1
  for(N in N_values) {
    nu = 1:N 
    fit_inla = inla(y ~ x + f(nu,model="iid"), family = c("poisson"), 
                    data = data[[step]], control.predictor=list(link=1)) 
    
    inla_times_values[step] = fit_inla$cpu.used["Total"]
    inla_summary_list[[step]] = 
      rbind(fit_inla$summary.fixed %>% select(-kld),
            fit_inla$summary.hyperpar) %>% 
      rownames_to_column("parameter")
    inla_summary_list[[step]]$N = N
    
    step = step + 1
  }
  inla_times = data.frame(N = N_values, inla_time = inla_times_values)
  inla_summary = do.call(rbind, inla_summary_list)
  
  write.csv(inla_times, inla_times_file,row.names = FALSE)
  write.csv(inla_summary, inla_summary_file,row.names = FALSE)
}