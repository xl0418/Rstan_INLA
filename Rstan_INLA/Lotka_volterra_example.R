lynx_hare_df <-
  read.csv("hare_data.csv",
           comment.char="#")
N <- length(lynx_hare_df$Year) - 1
ts <- 1:N
y_init <- c(lynx_hare_df$Hare[1], lynx_hare_df$Lynx[1])
y <- as.matrix(lynx_hare_df[2:(N + 1), 2:3])
y <- cbind(y[ , 2], y[ , 1]); # hare, lynx order
lynx_hare_data <- list(N = N, ts = ts, y_init = y_init, y = y)

model <- stan_model("Lotka_volterra_example.stan")
fit <- sampling(model, data = lynx_hare_data, seed = 123)
