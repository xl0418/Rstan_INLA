Lotka_Volterra_sim <- function(ts, paras, z_ini,seed = 1) {
  set.seed(seed)
  z <- matrix(0, nrow = ts, ncol = 2)
  z[1, ] <- z_ini
  for (i in c(2:ts)) {
    z[i, 1] <- z[i - 1, 1] + (paras[1] - paras[2] * z[i - 1, 2]) * z[i - 1, 1]
    z[i, 2] <- z[i - 1, 2] + (-paras[3] + paras[4] * z[i - 1, 1]) * z[i - 1, 2]
  }
  return(z)
}