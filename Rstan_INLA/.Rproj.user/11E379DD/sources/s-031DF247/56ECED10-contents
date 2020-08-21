library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(13)

file = "https://raw.githubusercontent.com/stan-dev/example-models/master/knitr/lotka-volterra/hudson-bay-lynx-hare.csv"
# https://github.com/bblais/Systems-Modeling-Spring-2015-Notebooks/blob/master/data/Lynx%20and%20Hare%20Data/lynxhare.csv

lynx_hare_df = read.csv(file, header=TRUE, comment.char="#")
# lynx_hare_df

N = length(lynx_hare_df$Year) - 1
ts = 1:N
y_init = c(lynx_hare_df$Hare[1], lynx_hare_df$Lynx[1])
y = as.matrix(lynx_hare_df[2:(N + 1), 2:3])
y = cbind(y[ , 2], y[ , 1]); # hare, lynx order
lynx_hare_data = list(N, ts, y_init, y)

thething = "
functions {
  real[] dz_dt(real t, real[] z, real[] theta,
             real[] x_r, int[] x_i) {
      real u = z[1];
      real v = z[2];

      real alpha = theta[1];
      real beta = theta[2];
      real gamma = theta[3];
      real delta = theta[4];

      real du_dt = (alpha - beta * v) * u;
      real dv_dt = (-gamma + delta * u) * v;
      return { du_dt, dv_dt };
    }
}

data {
  int N;           // num measurements
  real ts[N];                 // measurement times > 0
  real y_init[2];             // initial measured population
  real y[N, 2];    // measured population at measurement times
}

parameters {
  real theta[4];   // theta = {alpha, beta, gamma, delta}
  real z_init[2];  // initial population
  real sigma[2];   // error scale
}

transformed parameters {
  real z[N, 2]
    = integrate_ode_rk45(dz_dt, z_init, 0, ts, theta,
                         rep_array(0.0, 0), rep_array(0, 0),
                         1e-6, 1e-5, 1e3);
}

model {
  theta[{1, 3}] ~ normal(1, 0.5);
  theta[{2, 4}] ~ normal(0.05, 0.05);
  sigma ~ lognormal(-1, 1);
  z_init ~ lognormal(log(10), 1);
  for (k in 1:2) {
    y_init[k] ~ lognormal(log(z_init[k]), sigma[k]);
    y[ , k] ~ lognormal(log(z[, k]), sigma[k]);
  }
}
"

fit = stan(model_code=thething, data=lynx_hare_data, iter=5000, chains=3, cores=3)

print(fit, pars=c("theta", "sigma", "z_init"),
      probs=c(0.1, 0.5, 0.9), digits = 3)