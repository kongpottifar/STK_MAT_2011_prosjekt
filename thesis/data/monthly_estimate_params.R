#!/usr/bin/env Rscript
# Script for å estimere paramtrene for månedsdata

CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/data/"

setwd(CWD)
library("data.table")

load("./monthly_data.RData")

#' LogLik til NB modellen
#'
#' @param y 
#' @param x 
#'
#' @return funksjon som kan optimeres med 'L-BFGS-B'
ll_nbmodel <- function(y, x) {
  force(y); force(x)
  function(para){
    a <- para[1]
    b <- para[2]
    c <- para[3]
    -sum(dnbinom(y, size=c*(a+b*x), p= c / (1 + c), log = TRUE))
  }  
}


#' Optimering av NB
#'
#' @param y 
#' @param start 
#'
#' @return Se optim
ml_fit <- function(y, start){
  x <- shift(y, n=1)[-1]
  y <- y[-1]
  fit <- optim(par = start,
               fn = ll_nbmodel(y, x),
               method = "L-BFGS-B",
               lower = c(1e-15, 1e-15, 1e-15),
               upper = c(Inf, 1, Inf))
  return(fit)
}


#' LogLikfunksjon for lognormal transformasjon
#'
#' @param z transformerte data z = log(y+1)
#' @param x lag z
#'
#' @return Funskjon til optimering
ll_lnmodel <- function(z, x) {
  force(x); force(z)
  function(para) {
    a <- para[1]
    b <- para[2]
    sigma <- para[3]
    -sum(dnorm(z, mean = a + b*x, sd = sigma, log = TRUE))
  }
}



#' Optimer LN-modellen
#'
#' @param y 
#' @param start 
#'
#' @return Se optim
ml_fit_ln <- function(y, start) {
  z <- log(y + 1)
  x <- shift(z, n=1)[-1]
  z <- z[-1]
  fit <- optim(par = start,
               fn = ll_lnmodel(z, x),
               method = "L-BFGS-B",
               lower = c(-Inf, 1e-15, 1e-15),
               upper = c(Inf, 1, Inf))
  return(fit)
}

#' Estimate parameters for negative binomial model
#'
#' @param bd weekly number of battle deaths
#'
#' @return named list with 3 paramters, a, b and c, returns NA if no optimum is found
#' 

estimate_params_nb <- function(bd) {
  estimates <- data.table(a=numeric(),
                          b=numeric(),
                          c=numeric(),
                          nll=numeric())
  for(i in 1:10) {
    start <- c(exp(rnorm(1)), runif(1), exp(rnorm(1)))
    fit <- tryCatch(ml_fit(bd, start=start), 
                    error=function(e) rep(NA, 4))
    if(class(fit) != "list") {
      fit = rep(NA, 4)
    } else {
      fit <- c(fit$par, fit$value)
    }
    names(fit) <- names(estimates)
    estimates <- rbind(estimates, as.list(fit))
    
  }
  return(as.list(estimates[which.min(nll), a:c]))
}

#' Estimate parameters for lognormal model
#'
#' @param bd weekly number of battle deaths
#'
#' @return named list with 3 paramters, a, b and sigma, 
#' returns NA if no optimum is found
estimate_params_ln <- function(bd) {
  estimates <- data.table(a=numeric(),
                          b=numeric(),
                          sigma=numeric(),
                          nll=numeric())
  for(i in 1:10) {
    start <- c(rnorm(1), runif(1), exp(rnorm(1)))
    fit <- tryCatch(ml_fit_ln(bd, start=start), 
                    error=function(e) rep(NA, 4))
    if(class(fit) != "list") {
      fit = rep(NA, 4)
    } else {
      fit <- c(fit$par, fit$value)
    }
    names(fit) <- names(estimates)
    estimates <- rbind(estimates, as.list(fit))
    
  }
  return(as.list(estimates[which.min(nll), a:sigma]))
}


# Estimering --------------------------------------------------------------


params_nb <- monthly_data[, estimate_params_nb(battledeaths), by=country_names]
params_ln <- monthly_data[, estimate_params_ln(battledeaths), by=country_names]

save(params_nb, file="monthly_params_nb.RData")
save(params_ln, file="monthly_params_ln.RData")