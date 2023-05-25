#!/usr/bin/env Rscript
CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/img/"
setwd(CWD)

library("data.table")
library("ggplot2")
library("latex2exp")

load("../data/params_ln.RData")
load("../data/params_nb.RData")
load("../data/res_data.RData")

theme_set(theme_classic())

# prepare data for plotting -----------------------------------------------


country <- c("Afghanistan", "Chad")

CI_ln <- function(x, a, b, s, ci=0.95) {
  z <- log(x + 1)
  zhat <- a + z * b
  yhat <- exp(zhat + (s^2)/2) - 1
  lower <- qnorm((1 - ci) / 2, zhat, s)
  upper <- qnorm(ci + (1 - ci) / 2, zhat, s)
  return(list(x =x,modell="LN",yhat= yhat, lower=exp(lower)-1, upper=exp(upper)-1))
}

CI_nb <- function(x, a, b, c, ci=0.95) {
  r <- c * (a + x * b)
  p <- c / (c + 1)
  lower <- qnbinom((1 - ci) / 2, size=r, prob = p)
  upper <- qnbinom(ci + (1 -ci) / 2, size=r, prob = p)
  return(list(x=x,modell="NB", yhat= a + b*x, lower=lower, upper=upper))
}

params_nb <- params_nb[res_data[,.(bd_max=max(battledeaths)), by=country_names]]
params_ln <- params_ln[res_data[,.(bd_max=max(battledeaths)), by=country_names]]


data_nb <- params_nb[country_names %in% country,
                     CI_nb(0:bd_max, a, b, c),
                     by=country_names]

data_ln <- params_ln[country_names %in% country,
                     CI_ln(0:bd_max, a, b, sigma),
                     by=country_names]

plot_data <- rbind(data_nb, data_ln)


# Plot --------------------------------------------------------------------

pred_plot <- ggplot(plot_data, aes(x=x)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=modell), alpha=0.4) +
  geom_line(aes(y=yhat, linetype=modell)) +
  geom_point(data = res_data[country_names %in% country], aes(x=shift(battledeaths, n=1), y=battledeaths),
             size=0.5) +
  facet_wrap(vars(country_names), scales = "free") + 
  theme(strip.background = element_blank(),
        legend.position = "top") +
  xlab(TeX("$Y_{t-1}$")) +
  ylab(TeX("$Y_t$"))
  
ggsave("pred_plot.pdf", pred_plot, units="cm", width = 14, height=8)