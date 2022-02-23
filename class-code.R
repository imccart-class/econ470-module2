if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, hrbrthemes,
               scales, cobalt, ivpack, stargazer, ggthemes)

## Simulate data
set.seed(12345678)
n <- 5000
b.true <- 5.25
iv.dat <- tibble(
  z = rnorm(n,0,2),
  eps = rnorm(n,0,1),
  d = (z + 1.5*eps + rnorm(n,0,1) > 0.25),
  y = 2.5 + b.true*d + eps + rnorm(n,0,0.5)
)

ols <- lm(y~d, data=iv.dat)
iv <- ivreg(y ~ d | z, data=iv.dat)

## check the first stage
summary(lm(d~z, data=iv.dat))

## check the reduced form
summary(lm(y~z, data=iv.dat))

## in two stages
step1 <- lm(d ~ z, data=iv.dat)
d.hat <- predict(step1)
step2 <- lm(y ~ d.hat, data=iv.dat)
summary(step2)
