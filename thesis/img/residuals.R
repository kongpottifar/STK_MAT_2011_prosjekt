#!/usr/bin/env Rscript
# script som lager plot over ukentlige dødsfall
CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/img/"
setwd(CWD)

library("data.table")
library("ggplot2")

load("../data/weekly_data.RData")
load("../data/params_nb.RData")
load("../data/params_ln.RData")

generate_residuals_nb <- function(data, params) {
  data <- merge(data, params)
  data[,yhat:= a + b * shift(battledeaths, n=1), by=country_names]
  data[,residual := battledeaths - yhat]
  data[,std_residual:= residual / sqrt((yhat * (c + 1))/ c)]
  return(data[,.(country_names, battledeaths, yhat, residual, std_residual, model="NB")])
}

generate_residuals_ln <- function(data, params) {
  data <- merge(data, params)
  data[,yhat:=a + b * log(shift(battledeaths, n=1) + 1), by=country_names]
  data[,residual := battledeaths - (exp(yhat + (sigma^2)/2) - 1) ]
  data[,std_residual:=residual/sqrt(((exp(sigma^2) - 1) * exp(2 * yhat + sigma^2)))]
  return(data[,.(country_names, battledeaths, yhat, residual, std_residual, model="LN")])
}


theme_set(theme_classic())

bins_afgh <- c(0,1,10,20,30,40,50,100, 150, 200, 300, 600, 1000, Inf)


residual_data <- rbind(generate_residuals_nb(weekly_data, params_nb),
                       generate_residuals_ln(weekly_data, params_ln))

residual_data[, bd_scaled := scale(battledeaths, center = FALSE), by=.(country_names, model)]

countries_with_obs <- weekly_data[,.(n_obs = sum(battledeaths > 0, na.rm = TRUE)), 
                                    by=country_names][n_obs > 100, country_names]

residual_plot <- ggplot(residual_data[country_names %in% c("Afghanistan", "Chad")],
                        aes(x=cut(battledeaths, 
                                       bins_afgh, right=FALSE, include.lowest=FALSE),
                            y=std_residual,
                            colour=model)) +
  geom_boxplot(size=0.5, linewidth=0.5) +
  facet_wrap(vars(country_names), scales = "free_y") +
  theme(axis.text.x = element_text(angle=90, hjust=1),
        strip.background = element_blank(),
        legend.position = "top") +
  xlab("Ukentlige dødsfall") +
  ylab("Standardiserte residualer")


ggsave("residuals.pdf", residual_plot, units="cm", height = 10, width=14)

