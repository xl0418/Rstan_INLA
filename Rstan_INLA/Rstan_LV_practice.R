library(rstan)
source('Lotka_volterra_sim.R')
sim_time <- 21
paras <- c(0.055, 0.0028, 0.084, .0026)
z_ini <- c(50,50)
z_sim <- Lotka_Volterra_sim(ts = sim_time, paras = paras, z_ini = z_ini)

N <- nrow(z_sim) - 1
time_series <- 1:N
lv_data <- list(N = N, ts = time_series, y_ini = z_ini, y = z_sim[2:(N+1),])

model <- stan_model("Lotka_volterra_stan.stan")
fit <- sampling(model, data = lv_data, seed = 18, iter=10000, chains=3, cores=3)
