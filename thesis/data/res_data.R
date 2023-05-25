#!/usr/bin/env Rscript
# script som finner residualer og scoring rules for ukentlige data
CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/data/"
setwd(CWD)

library("data.table")

load("../data/weekly_data.RData")
load("../data/params_nb.RData")
load("../data/params_ln.RData")

names(params_ln)[4] <- "SD"


# Funksjoner for Ranked probablity score ----------------------------------

RPS_nb <-function(x, r, p, n=2000) {
  if (length(x) != length(r) | length(x) != length(p)) {
    stop("x, r, and p must be of same length.")
  }
  rps <- vector(mode="numeric", length=length(x))
  for (i in 1:length(x)){
    if (is.na(x[i]) | is.na(r[i]) | is.na(p[i])) {
      rps[i] <- NA
    } else {
      X1 <- rnbinom(n, size=r[i], prob=p[i])
      X2 <- rnbinom(n, size=r[i], prob=p[i])
      rps[i] <- mean(abs(X1 - x[i])) - mean(abs(X1-X2))/ 2
    }
  }
  return(rps)
}

RPS_ln <-function(x, yhat, sd, n=2000) {
  if (length(x) != length(yhat) | length(x) != length(sd)) {
    stop("x, yhat, and sigma must be of same length.")
  }
  rps <- vector(mode="numeric", length=length(x))
  for (i in 1:length(x)){
    if (is.na(x[i]) | is.na(yhat[i]) | is.na(sd[i])) {
      rps[i] <- NA
    } else {
      X1 <- exp(rnorm(n, mean = yhat[i], sd = sd)) - 1
      X2 <- exp(rnorm(n, mean = yhat[i], sd = sd)) - 1
      
      rps[i] <- mean(abs(X1 - x[i])) - mean(abs(X1-X2))/ 2
    }
  }
  return(rps)
}


generate_residuals_nb <- function(data, params) {
  data <- merge(data, params)
  data[,yhat:= a + b * shift(battledeaths, n=1), by=country_names]
  data[,residual := battledeaths - yhat]
  data[,std_residual:= residual / sqrt((yhat * (c + 1))/ c)]
  data[, se := std_residual ^ 2]
  data[,dss:= se + 2 * log(sqrt((yhat + (c +1))/ c))]
  data[,logs := -dnbinom(battledeaths, size = c * yhat, prob = c/(c+1), log = TRUE)]
  data[, RPS := RPS_nb(battledeaths, r = c * yhat, p=c / (c+1))]
  data[, model := "NB"]
  return(data[,!(a:c)])
}

generate_residuals_ln <- function(data, params) {
  data <- merge(data, params)
  data[,yhat:=a + b * log(shift(battledeaths, n=1) + 1), by=country_names]
  data[,residual := battledeaths - (exp(yhat + (SD^2)/2) - 1) ]
  data[,std_residual:=residual/sqrt(((exp(SD^2) - 1) * exp(2 * yhat + SD^2)))]
  data[, se := std_residual ^ 2]
  data[,dss:= se + 2 * log(sqrt(((exp(SD^2) - 1) * exp(2 * yhat + SD^2))))]
  data[,logs := -dnorm(x =log(battledeaths + 1), mean = yhat, sd = SD, log = TRUE)]
  data[, RPS := RPS_ln(x = battledeaths, yhat = yhat, sd=SD)]
  data[, model := "LN"]
  return(data[,!(a:SD)])
}


# residual_data <- rbind(generate_residuals_nb(weekly_data, params_nb),
#                        generate_residuals_ln(weekly_data, params_ln))
# 
# residual_data[, bd_scaled := scale(battledeaths, center = FALSE), by=.(country_names, model)]
# 
# countries_with_obs <- weekly_data[,.(n_obs = sum(battledeaths > 0, na.rm = TRUE)), 
#                                   by=country_names][n_obs > 100, country_names]

res_nb <- generate_residuals_nb(weekly_data, 
                                params_nb)

res_ln <- generate_residuals_ln(weekly_data, 
                                params_ln)

# Generer data ----------------------------------------------------------

res_data <- rbind(res_nb, res_ln)

save(res_data, file="res_data.RData")