# Lese inn data

antocyan <- c(
  525, 587, 547, 558, 591, 531, 571, 551,
  566, 622, 561, 502, 556, 565, 562
)


# a) Konfidensintervall for μ med t-fordeling

n <- length(antocyan)
x_bar <- mean(antocyan)
s <- sd(antocyan)

# Kritisk t-verdi for 95% KI
t_crit <- qt(0.975, df = n - 1)

# 95% KI for μ
ci_mu <- x_bar + c(-1, 1) * t_crit * s / sqrt(n)
signif(ci_mu, 3)


## b) Simulert dekningsgrad for t-basert KI

N <- 10000
mu <- 558
sigma <- 30

ci_mat <- matrix(0, nrow = N, ncol = 2)

for (i in 1:N) {
  x <- rnorm(n, mu, sigma)
  x_bar <- mean(x)
  s <- sd(x)
  
  ci_mat[i, ] <- x_bar + c(-1, 1) * t_crit * s / sqrt(n)
}

# Sjekker om μ ligger i intervallet
mu_in_int <- (ci_mat[, 1] <= mu) & (ci_mat[, 2] >= mu)
mean(as.numeric(mu_in_int))   # Dekningsgrad


# c) Samme som b), men med normal-approksimasjon (1.96)

ci_mat_alt <- matrix(0, nrow = N, ncol = 2)

for (i in 1:N) {
  x <- rnorm(n, mu, sigma)
  x_bar <- mean(x)
  s <- sd(x)
  
  ci_mat_alt[i, ] <- x_bar + c(-1, 1) * 1.96 * s / sqrt(n)
}

mu_in_int_alt <- (ci_mat_alt[, 1] <= mu) & (ci_mat_alt[, 2] >= mu)
mean(as.numeric(mu_in_int_alt))   # Dekningsgrad


# d) Konfidensintervall for σ med χ²-fordeling

ci_mat_sigma <- matrix(0, nrow = N, ncol = 2)

for (i in 1:N) {
  x <- rnorm(n, mu, sigma)
  s <- sd(x)
  
  # KI for σ basert på χ²
  ci_mat_sigma[i, ] <- s * sqrt(n - 1) /
    sqrt(c(qchisq(0.975, n - 1), qchisq(0.025, n - 1)))
}

sigma_in_int <- (ci_mat_sigma[, 1] <= sigma) &
  (ci_mat_sigma[, 2] >= sigma)

mean(as.numeric(sigma_in_int))   # Dekningsgrad


# e) Konfidensintervall for μ når data kommer fra t-fordeling

nu <- 7   # frihetsgrader i t-fordelingen
ci_mat_t <- matrix(0, nrow = N, ncol = 2)

for (i in 1:N) {
  t_vals <- rt(n, nu)
  x <- mu + sigma * t_vals
  
  x_bar <- mean(x)
  s <- sd(x)
  
  ci_mat_t[i, ] <- x_bar + c(-1, 1) * t_crit * s / sqrt(n)
}

mu_in_int_t <- (ci_mat_t[, 1] <= mu) &
  (ci_mat_t[, 2] >= mu)

mean(as.numeric(mu_in_int_t))   # Dekningsgrad


# f) Konfidensintervall for σ når data kommer fra t-fordeling

ci_mat_sigma_t <- matrix(0, nrow = N, ncol = 2)

for (i in 1:N) {
  t_vals <- rt(n, nu)
  x <- mu + sigma * t_vals
  
  s <- sd(x)
  
  ci_mat_sigma_t[i, ] <- s * sqrt(n - 1) /
    sqrt(c(qchisq(0.975, n - 1), qchisq(0.025, n - 1)))
}

# Sann σ for t-fordeling (varians = nu/(nu-2))
sigma_true <- sigma * sqrt(nu / (nu - 2))

sigma_in_int_t <- (ci_mat_sigma_t[, 1] <= sigma_true) &
  (ci_mat_sigma_t[, 2] >= sigma_true)

mean(as.numeric(sigma_in_int_t))   # Dekningsgrad
