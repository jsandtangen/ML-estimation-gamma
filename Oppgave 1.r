# Laster inn data
x <- scan("https://www.uio.no/studier/emner/matnat/math/STK1110/data/forsikringskrav.txt")


# a) MOM-estimatorer

# Gjennomsnitt
bar_x <- mean(x)

# Empirisk varians (ikke delt på n-1)
s2 <- mean((x - bar_x)^2)

# MOM-estimat for alpha og gamma
mom_alpha <- bar_x^2 / s2
mom_gamma <- bar_x / s2

show(c(mom_alpha, mom_gamma))


### b) Log-likelihood for gammafordelingen

loglik_gamma <- function(alpha, gamma, x) {
  n <- length(x)
  sum_x <- sum(x)
  sum_log_x <- sum(log(x))
  
  logL <- n * alpha * log(gamma) -
    n * lgamma(alpha) +
    (alpha - 1) * sum_log_x -
    gamma * sum_x
  
  return(logL)
}

# Log-likelihood evaluert i MOM-estimatene
show(loglik_gamma(mom_alpha, mom_gamma, x))


# d) Negativ log-likelihood (for optim)

negloglik_gamma <- function(log_alpha, x = x) {
  n <- length(x)
  
  # Parametertransformasjon
  alpha <- exp(log_alpha)
  gamma <- alpha / mean(x)
  
  logL <- n * alpha * log(gamma) -
    n * lgamma(alpha) +
    (alpha - 1) * sum(log(x)) -
    gamma * sum(x)
  
  return(-logL)   # optim minimerer → vi returnerer negativ logL
}

# ML-estimering
fit_ml <- optim(
  par = log(mom_alpha),          # startverdi
  fn = negloglik_gamma,
  x = x,
  method = "BFGS",
  hessian = TRUE
)

ml_alpha <- exp(fit_ml$par)
ml_gamma <- ml_alpha / mean(x)

logL_ml <- loglik_gamma(ml_alpha, ml_gamma, x)

show(c(ml_alpha, ml_gamma, logL_ml))


# Bootstrapping

B <- 1000
n <- length(x)

est_mom <- matrix(NA, nrow = B, ncol = 2)
est_ml  <- matrix(NA, nrow = B, ncol = 2)

for (b in 1:B) {
  
  # Trekker bootstrap-utvalg
  x_samp <- sample(x, n, replace = TRUE)
  
  # MOM-estimater i bootstrap-utvalget
  mean_samp <- mean(x_samp)
  mean_samp2 <- mean(x_samp^2)
  
  est_mom[b, 1] <- mean_samp^2 / (mean_samp2 - mean_samp^2)
  est_mom[b, 2] <- mean_samp / (mean_samp2 - mean_samp^2)
  
  # ML-estimater i bootstrap-utvalget
  fit_ml <- optim(
    par = log(est_mom[b, 1]),
    fn = negloglik_gamma,
    x = x_samp,
    method = "BFGS",
    hessian = TRUE
  )
  
  est_ml[b, 1] <- exp(fit_ml$par)              # alpha
  est_ml[b, 2] <- est_ml[b, 1] / mean_samp     # gamma
}

# Standardavvik (bootstrap-SE)
se_alpha <- sd(est_ml[, 1])
se_gamma <- sd(est_ml[, 2])

show(c(se_alpha, se_gamma))

# Konfidensintervall basert på normalapproksimasjon
show(ml_alpha + c(-1, 1) * qnorm(0.975) * se_alpha)
show(ml_gamma + c(-1, 1) * qnorm(0.975) * se_gamma)

# Percentil-baserte KI
show(quantile(est_ml[, 1], c(0.025, 0.975)))


## f) Konfidensintervall for μ = alpha/gamma


est_mu <- est_ml[, 1] / est_ml[, 2]

# 95% KI
show(quantile(est_mu, c(0.025, 0.975)))

# 99% KI
show(quantile(est_mu, c(0.005, 0.995)))
