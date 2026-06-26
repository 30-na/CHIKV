# Thermal response functions for CHIKV R0 calculations

in_range <- function(T, lower, upper) {
  T > lower & T < upper
}


####### Biting Rate ##########

# Species: Ae. aegypti
# Confirmed Original Source: Scott et al., 2000
a_aegypti_scott_2000 <- function(T) {
  b_m <- rep(0, length(T))
  idx1 <- T >= 21 & T <= 32
  b_m[idx1] <- 0.0943 + 0.0043 * T[idx1]
  return(b_m)
}

# Species: Ae. aegypti
# Confirmed Original Source: Mordecai et al., 2017
a_aegypti_mordecai_2017 <- function(T) {
  a <- rep(0, length(T))
  idx1 <- in_range(T, 13.35, 40.08)
  a[idx1] <- 2.02E-4 * T[idx1] * (T[idx1] - 13.35) * sqrt(40.08 - T[idx1])
  return(a)
}

# Species: Ae. albopictus
# Confirmed Original Source: Mordecai et al., 2017
a_albopictus_mordecai_2017 <- function(T) {
  a <- rep(0, length(T))
  idx1 <- in_range(T, 10.25, 38.32)
  a[idx1] <- 1.93E-4 * T[idx1] * (T[idx1] - 10.25) * sqrt(38.32 - T[idx1])
  return(a)
}

# Species: Ae. albopictus
# Confirmed Original Source: Scott et al., 2000
a_albopictus_scott_2000 <- function(T) {
  a <- 0.5 * (0.0043 * T + 0.0943)
  return(a)
}


####### Transmission Probability: Vector -> Human ##########

# Species: Ae. aegypti
# Confirmed Original Source: Lambrechts et al., 2011
b_aegypti_lambrechts_2011 <- function(T) {
  b_prob <- rep(0, length(T))
  idx1 <- T >= 12.4 & T <= 26.1
  idx2 <- T > 26.1 & T <= 32.5
  b_prob[idx1] <- -0.9037 + 0.0729 * T[idx1]
  b_prob[idx2] <- 1
  return(b_prob)
}

# Species: Ae. aegypti
# Confirmed Original Source: Mordecai et al., 2017
b_aegypti_mordecai_2017 <- function(T) {
  b_prob <- rep(0, length(T))
  idx1 <- in_range(T, 17.05, 35.83)
  b_prob[idx1] <- 8.49E-4 * T[idx1] * (T[idx1] - 17.05) * sqrt(35.83 - T[idx1])
  return(b_prob)
}

# Species: Ae. albopictus
# Confirmed Original Source: Mordecai et al., 2017
b_albopictus_mordecai_2017 <- function(T) {
  b_prob <- rep(0, length(T))
  idx1 <- in_range(T, 15.84, 36.40)
  b_prob[idx1] <- 7.35E-4 * T[idx1] * (T[idx1] - 15.84) * sqrt(36.40 - T[idx1])
  return(b_prob)
}


####### Transmission Probability: Human -> Vector ##########

# Species: Ae. aegypti
# Confirmed Original Source: Lambrechts et al., 2011
c_aegypti_lambrechts_2011 <- function(T) {
  c <- rep(0, length(T))
  idx1 <- in_range(T, 12.286, 32.461)
  c[idx1] <- 0.001044 * T[idx1] * (T[idx1] - 12.286) * sqrt(32.461 - T[idx1])
  return(c)
}

# Species: Ae. aegypti
# Confirmed Original Source: Mordecai et al., 2017
# Original parameter name in the table: pMI(T)
c_aegypti_mordecai_2017 <- function(T) {
  c <- rep(0, length(T))
  idx1 <- in_range(T, 12.22, 37.46)
  c[idx1] <- 4.91E-4 * T[idx1] * (T[idx1] - 12.22) * sqrt(37.46 - T[idx1])
  return(c)
}

# Species: Ae. albopictus
# Confirmed Original Source: Mordecai et al., 2017
c_albopictus_mordecai_2017 <- function(T) {
  c <- rep(0, length(T))
  idx1 <- in_range(T, 3.62, 36.82)
  c[idx1] <- 4.39E-4 * T[idx1] * (T[idx1] - 3.62) * sqrt(36.82 - T[idx1])
  return(c)
}


####### Adult Mosquito Lifespan ##########

# Species: Ae. aegypti
# Confirmed Original Source: Brady et al., 2013
lf_aegypti_brady_2013 <- function(T) {
  lf <- rep(0, length(T))
  idx1 <- T < 22
  idx2 <- T >= 22
  lf[idx1] <- 1 / (1 / (1.22 + exp(-3.05 + 0.72 * T[idx1])) + 0.196)
  lf[idx2] <- 1 / (1 / (1.14 + exp(51.4 - 1.3 * T[idx2])) + 0.192)
  return(lf)
}

# Species: Ae. aegypti
# Confirmed Original Source: Yang et al., 2009
lf_aegypti_yang_2009 <- function(T) {
  lf <- 1 / (0.8692 - 0.1590 * T + 0.01116 * T^2 - 0.0003408 * T^3 + 0.000003809 * T^4)
  return(lf)
}

# Species: Ae. albopictus
# Confirmed Original Source: Poletti et al., 2011
lf_albopictus_poletti_2011 <- function(T) {
  lf <- 1 / (0.031 + 95820 * exp(T - 50.4))
  return(lf)
}

# Species: Ae. albopictus
# Confirmed Original Source: Ruiz-Moreno et al., 2012
lf_albopictus_ruiz_moreno_2012 <- function(T) {
  lf <- rep(0, length(T))
  idx1 <- T < -5
  idx2 <- T >= -5 & T < 15
  idx3 <- T >= 15 & T < 38.5
  idx4 <- T >= 38.5
  lf[idx1] <- 0
  lf[idx2] <- 5.6 + 1.12 * T[idx2]
  lf[idx3] <- 40.27 - 1.38 * T[idx3] + 0.01 * T[idx3]^2
  lf[idx4] <- 0
  return(lf)
}

# Species: Ae. albopictus
# Confirmed Original Source: Brady et al., 2013
lf_albopictus_brady_2013 <- function(T) {
  lf <- rep(0, length(T))
  idx1 <- T < 15
  idx2 <- T >= 15 & T < 26.3
  idx3 <- T >= 26.3
  lf[idx1] <- 1 / (1 / (1.1 + exp(-4.04 + 0.576 * T[idx1])) + 0.12)
  lf[idx2] <- 1 / (0.000339 * T[idx2]^2 - 0.0189 * T[idx2] + 0.336)
  lf[idx3] <- 1 / (1 / (1.065 + exp(32.2 - 0.92 * T[idx3])) + 0.0747)
  return(lf)
}

# Species: Ae. albopictus
# Confirmed Original Source: Ng et al., 2017
lf_albopictus_ng_2017 <- function(T) {
  lf <- 1 / (
    1.33048 - 2.32772E-1 * T + 1.68529E-2 * T^2 -
      5.61719E-4 * T^3 + 7.91643E-6 * T^4 - 2.72E-8 * T^5
  )
  return(lf)
}

# Species: Ae. aegypti
# Confirmed Original Source: Mordecai et al., 2017
lf_aegypti_mordecai_2017 <- function(T) {
  lf <- rep(0, length(T))
  idx1 <- in_range(T, 9.16, 37.73)
  lf[idx1] <- -1.48E-1 * (T[idx1] - 9.16) * (T[idx1] - 37.73)
  return(lf)
}

# Species: Ae. albopictus
# Confirmed Original Source: Mordecai et al., 2017
lf_albopictus_mordecai_2017 <- function(T) {
  lf <- rep(0, length(T))
  idx1 <- in_range(T, 13.41, 31.51)
  lf[idx1] <- -1.43 * (T[idx1] - 13.41) * (T[idx1] - 31.51)
  return(lf)
}


####### Parasite Development Rate ##########

# Species: Ae. albopictus
# Confirmed Original Source: Mordecai et al., 2017
PDR_albopictus_mordecai_2017 <- function(T) {
  PDR <- rep(0, length(T))
  idx1 <- in_range(T, 10.39, 43.05)
  PDR[idx1] <- 1.09E-4 * T[idx1] * (T[idx1] - 10.39) * sqrt(43.05 - T[idx1])
  return(PDR)
}

# Species: Ae. aegypti
# Confirmed Original Source: Mordecai et al., 2017
PDR_aegypti_mordecai_2017 <- function(T) {
  PDR <- rep(0, length(T))
  idx1 <- in_range(T, 10.68, 45.90)
  PDR[idx1] <- 6.65E-5 * T[idx1] * (T[idx1] - 10.68) * sqrt(45.90 - T[idx1])
  return(PDR)
}

# Species: Ae. aegypti
# Confirmed Original Source: Focks et al., 1995
PDR_aegypti_focks_1995 <- function(T) {
  PDR <- 1 / (4 + exp(5.15 - 0.123 * T))
  return(PDR)
}

# Species: Ae. albopictus
# Confirmed Original Source: Chan & Johansson, 2012
PDR_albopictus_chan_johansson_2012 <- function(T) {
  PDR <- 1 / exp(log(6) * exp(-0.08 * (T - 28)))
  return(PDR)
}

# Species: Ae. albopictus
# Confirmed Original Source: Chan & Johansson, 2012
PDR_albopictus_chan_johansson_2012_alt <- function(T) {
  PDR <- 1 / (3.0 * exp(-0.21 * (T - 28.0)))
  return(PDR)
}

# Species: Ae. albopictus
# Confirmed Original Source: Ruiz-Moreno et al., 2012
PDR_albopictus_ruiz_moreno_2012 <- function(T) {
  EIP <- rep(0, length(T))
  idx1 <- T < 10
  idx2 <- T >= 10 & T <= 32
  idx3 <- T > 32
  EIP[idx1] <- 4
  EIP[idx2] <- 113 / 22 - (2.4 / 22) * T[idx2]
  EIP[idx3] <- 1.5
  PDR <- 1 / EIP
  return(PDR)
}


####### Fecundity ##########

# Species: Ae. aegypti
# Confirmed Original Source: Yang et al., 2009
EFD_aegypti_yang_2009 <- function(T) {
  EFD <- -341.8585 +
    108.7373 * T -
    12.3447 * T^2 +
    0.6367451 * T^3 -
    0.0149469 * T^4 +
    0.0001294166 * T^5
  return(EFD)
}

# Species: Ae. albopictus
# Confirmed Original Source: Mordecai et al., 2017
TFD_albopictus_mordecai_2017 <- function(T) {
  EFD <- rep(0, length(T))
  idx1 <- in_range(T, 8.02, 35.65)
  EFD[idx1] <- 4.88E-2 * T[idx1] * (T[idx1] - 8.02) * sqrt(35.65 - T[idx1])
  return(EFD)
}

# Species: Ae. aegypti
# Confirmed Original Source: Mordecai et al., 2017
EFD_aegypti_mordecai_2017 <- function(T) {
  EFD <- rep(0, length(T))
  idx1 <- in_range(T, 14.58, 34.61)
  EFD[idx1] <- 8.56E-3 * T[idx1] * (T[idx1] - 14.58) * sqrt(34.61 - T[idx1])
  return(EFD)
}


####### Egg-Adult Survival Probability ##########

# Species: Ae. aegypti
# Confirmed Original Source: Mordecai et al., 2017
pEA_aegypti_mordecai_2017 <- function(T) {
  pEA <- rep(0, length(T))
  idx1 <- in_range(T, 13.56, 38.29)
  pEA[idx1] <- 5.99E-3 * (T[idx1] - 13.56) * (38.29 - T[idx1])
  return(pEA)
}

# Species: Ae. albopictus
# Confirmed Original Source: Mordecai et al., 2017
pEA_albopictus_mordecai_2017 <- function(T) {
  pEA <- rep(0, length(T))
  idx1 <- in_range(T, 9.04, 39.33)
  pEA[idx1] <- -3.61E-3 * (T[idx1] - 9.04) * (T[idx1] - 39.33)
  return(pEA)
}

# Species: Ae. albopictus
# Confirmed Original Source: Yang et al., 2009
# Converted using pEA(T) = exp[-mortality(T) * development_duration(T)]
pEA_albopictus_yang_2009 <- function(T) {
  mortality <- 2.13 -
    0.3787 * T +
    0.02457 * T^2 -
    6.778E-04 * T^3 +
    6.794E-06 * T^4

  maturation_rate <- 0.131 -
    0.05723 * T +
    0.01164 * T^2 -
    0.001341 * T^3 +
    8.723E-05 * T^4 -
    3.017E-06 * T^5 +
    5.153E-08 * T^6 -
    3.42E-10 * T^7

  pEA <- exp(-mortality / maturation_rate)
  return(pEA)
}

# Species: Ae. albopictus
# Confirmed Original Source: Poletti et al., 2011
# Converted using stage survival probabilities: egg * larva * pupa
pEA_albopictus_poletti_2011 <- function(T) {
  egg_mortality <- 506 - 506 * exp(-((T - 25) / 27.3)^6)
  egg_duration <- 6.9 - 4.0 * exp(-((T - 20) / 4.1)^2)

  larva_mortality <- 0.029 + 858 * exp(T - 43.4)
  larva_duration <- 0.12 * T^2 - 6.6 * T + 98

  pupa_mortality <- 0.021 + 37 * exp(T - 36.8)
  pupa_duration <- 0.027 * T^2 - 1.7 * T + 27.7

  pEA <- exp(-egg_mortality * egg_duration) *
    exp(-larva_mortality * larva_duration) *
    exp(-pupa_mortality * pupa_duration)

  return(pEA)
}


####### Aquatic Maturation / MDR ##########

# Species: Ae. aegypti
# Confirmed Original Source: Yang et al., 2009
phi_v_aegypti_yang_2009 <- function(T) {
  phi_v <- (
    0.131 -
      0.05723 * T +
      0.01164 * T^2 -
      0.001341 * T^3 +
      8.723E-5 * T^4 -
      3.017E-6 * T^5 +
      5.153E-8 * T^6 -
      3.42E-10 * T^7
  )
  return(phi_v)
}

# Species: Ae. aegypti
# Confirmed Original Source: Mordecai et al., 2017
MDR_aegypti_mordecai_2017 <- function(T) {
  MDR <- rep(0, length(T))
  idx1 <- in_range(T, 11.36, 39.17)
  MDR[idx1] <- 7.86E-5 * T[idx1] * (T[idx1] - 11.36) * sqrt(39.17 - T[idx1])
  return(MDR)
}

# Species: Ae. albopictus
# Confirmed Original Source: Mordecai et al., 2017
MDR_albopictus_mordecai_2017 <- function(T) {
  MDR <- rep(0, length(T))
  idx1 <- in_range(T, 8.60, 39.66)
  MDR[idx1] <- 6.38E-5 * T[idx1] * (T[idx1] - 8.60) * sqrt(39.66 - T[idx1])
  return(MDR)
}

# Species: Ae. albopictus
# Confirmed Original Source: Poletti et al., 2011
# Converted by summing egg, larval, and pupal durations, then using MDR = 1 / duration
MDR_albopictus_poletti_2011 <- function(T) {
  MDR <- 1 / (0.193 * T^2 - 11.07 * T + 177.9 - 4 * exp(-((T - 20) / 4.1)^2))
  return(MDR)
}


####### R0 ##########

relative_R0_mordecai <- function(a, b, c, mu, PDR, EFD, pEA, MDR, N = 1, r = 1) {
  R0 <- sqrt(
    (
      a^2 *
        b *
        c *
        exp(-(mu / PDR)) *
        EFD *
        pEA *
        MDR
    ) /
      (N * r * mu^3)
  )

  return(R0)
}

standardize_R0 <- function(R0) {
  R0[!is.finite(R0)] <- NA
  R0 <- R0 / max(R0, na.rm = TRUE)
  return(R0)
}

