#!/usr/bin/env Rscript
# script som lager plot over ukentlige dødsfall
CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/img/"
setwd(CWD)

library("data.table")
library("ggplot2")

load("../data/weekly_data.RData")
load("../data/params_nb.RData")
load("../data/params_ln.RData")

theme_set(theme_classic())

# PIT-funksjoner ----------------------------------------------------------


F_nb <- function(yhat, pc){
  func <- function(y) {
    result <- vector(mode = "numeric", length = length(y))
    result[y < 0] <- 0
    result[y>=0] <- pnbinom(y[y>=0], pc*yhat[y>=0], pc/(1+pc))
    return(result)
  }
  return(func)
}

F_ln <- function(yhat, sigma){
  func <- function(y) {
    result <- vector(mode="numeric", length=length(y))
    result[y < 0] <- 0
    result[y >=0] <- pnorm(log(y[y>=0]+1), yhat[y>=0], sigma)
    return(result)
  }
  return(func)
}

PIT <- function(u, y, func){
  F_u = vector(mode = "numeric", length = length(y))
  ft_1 <- func(y-1)
  ft <- func(y)
  F_u[u < ft_1] <- 0
  F_u[u > ft] <- 1
  index <- ft_1 <= u & u <= ft
  F_u[index] <- (u - ft_1[index]) / (ft[index] - ft_1[index])
  return(mean(F_u, na.rm=TRUE))
}


# Generer data ------------------------------------------------------------

generate_PIT_nb <- function(data, u) {
  a <- data$a[1]
  b <- data$b[1]
  c <- data$c[1]
  yhat <- a + b * shift(data$battledeaths, n=1)[-1]
  F_u <- sapply(u, PIT, y=data$battledeaths[-1], func=F_nb(yhat, c))
  return(F_u)
}

generate_PIT_ln <- function(data, u) {
  a <- data$a[1]
  b <- data$b[1]
  sigma <- data$sigma[1]
  yhat <- a + b * log(shift(data$battledeaths, n=1)[-1] + 1)
  F_u <- sapply(u, PIT, y=data$battledeaths[-1], func=F_ln(yhat, sigma))
  return(F_u)
}

F_u_data <- data.table(country_names=character(), 
                       model=character(), 
                       u=numeric(), 
                       F_u = numeric())
data_nb <- merge(weekly_data, params_nb)
data_ln <- merge(weekly_data, params_ln)
u = seq(0,1, by=0.05)

for(country in unique(weekly_data$country_names)) {
  F_u_data <- rbind(F_u_data, data.table(country_names = country, 
                                         model = "NB", 
                                         u = u, 
                                         F_u = generate_PIT_nb(
                                           data_nb[country_names==country], u)
  ))
  F_u_data <- rbind(F_u_data, data.table(country_names = country,
                                         model = "LN",
                                         u=u,
                                         F_u=generate_PIT_ln(
                                           data_ln[country_names==country], u)
  ))
}


# Plotting ----------------------------------------------------------------

selection <- c("Afghanistan","Chad")
F_u_data[,Fj := F_u - shift(F_u, n=1), by=.(country_names, model)]
PIT_plot <- ggplot(F_u_data[country_names %in% selection], aes(x=u, y=Fj)) +
  geom_col(position = "dodge") +
  facet_grid(rows=vars(country_names), cols=vars(model)) +
  theme(strip.background = element_blank())

ggsave("PIT_plot.pdf", PIT_plot, units = "cm", width = 14, height=10)