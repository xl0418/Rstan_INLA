  functions {
      real[] LV_model(
          real t,
          real[] z,
          real[] paras,
          real[] x_r,  // unused arguments that are default in integrate_ode_rk45
          int[] x_i
          ) {
        real z1 = z[1];
        real z2 = z[2];
        
        real alpha = paras[1];
        real beta = paras[2];
        real gamma = paras[3];
        real nu = paras[4];
        
        real dz1_dt = (alpha - beta * z2) * z1;
        real dz2_dt = (-gamma + nu * z1) * z2;
        return { dz1_dt, dz2_dt };
      }
  }
  
  data {
    int<lower = 0> N;
    real ts[N];
    real y_ini[2];
    real<lower = 0> y[N,2];
  }
  
  parameters {
    real<lower = 0> paras[4];
    real<lower = 0> z_ini[2];
    real<lower = 0> sigma[2];   // error scale
  }
  
  transformed parameters {
    real z[N,2] = integrate_ode_rk45(LV_model,
                                     z_ini,
                                     0,
                                     ts,
                                     paras,
                                     rep_array(0.0, 0),
                                     rep_array(0, 0),
                                     1e-5,
                                     1e-3,
                                     5e2);
  }
   
  model {
    paras[{1,3}] ~ normal(1, 0.5);
    paras[{2,4}] ~ normal(.05,0.05);
    sigma ~ lognormal(-1, 1);
    z_ini ~ normal(50,10);
    for (k in 1:2) {
      y_ini[k] ~ normal(z_ini[k], sigma[k]);
      y[ ,k] ~ normal(z[ ,k], sigma[k]);
    }
  }
