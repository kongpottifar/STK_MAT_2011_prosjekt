#!/usr/bin/env Rscript
# script for å lage plot over residualer på månedlige data


# Setup -------------------------------------------------------------------

CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/img/"
setwd(CWD)

library("data.table")
library("ggplot2")
theme_set(theme_classic())

load("../data/weekly_data.RData")
load("../data/monthly_params_ln.RData")
load("../data/monthly_params_nb.RData")


# Prepare data ------------------------------------------------------------

res_data_nb <- weekly_data[params_nb]
res_data_ln <- weekly_data[params_ln]

res_data_nb[, yhat:= .(a + b * shift(battledeaths, n=1)), by=country_names]
res_data_ln[, yhat:= exp(a + b * shift(log(battledeaths + 1), n=1) + (sigma^2)/2) - 1
            , by=country_names]

res_data_nb[,SD:=sqrt((yhat * (c+1)) / c)]
res_data_ln[,SD:=sqrt((exp(sigma^2)-1)*exp(2*(a + b*log(shift(battledeaths, n=1) + 1)) + sigma^2))]

res_data <- rbind(res_data_nb[,.(country_names, model="NB", battledeaths, yhat, SD)],
                  res_data_ln[,.(country_names, model="LN", battledeaths, yhat, SD)])

# Plotting ----------------------------------------------------------------

countries <- c("Afghanistan", "Chad")

monthly_res_plot <- ggplot(res_data[country_names %in% countries & !is.na(battledeaths)],
                           aes(x=cut_width(log(battledeaths + 1), 1, boundary = 0), y=(battledeaths - yhat)/SD)) +
  geom_boxplot(aes(color=model)) +
  facet_wrap(vars(country_names)) +
  theme(strip.background = element_blank()) +
  xlab("log(battledeaths + 1)") +
  ylab("Standardiserte residualer")

ggsave("monthly_res_plot.pdf", monthly_res_plot, units="cm", width=14, height = 8)
