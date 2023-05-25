#!/usr/bin/env Rscript
# script som lager plot over ukentlige dødsfall
CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/img/"
setwd(CWD)

library("data.table")
library("ggplot2")
library("latex2exp")
load("../data/weekly_data.RData")

theme_set(theme_classic())


# calculate ---------------------------------------------------------------

countries_with_obs <- weekly_data[
  battledeaths > 0, .(N=.N),by=country_names
  ][
    N>=100, country_names
    ]

index <- weekly_data[country_names %in% countries_with_obs,
                     .(
                       p0=mean(battledeaths==0),
                       disp_index=var(battledeaths)/mean(battledeaths)
                     ),
                     by=country_names]


# plot --------------------------------------------------------------------

sel_country <- c("Afghanistan", "Chad")

index_plot <- ggplot(index[!(country_names %in% sel_country)], 
       aes(x=p0, y=log(disp_index))) +
  geom_point()+
  geom_point(data=index[country_names %in% sel_country], col="salmon") +
  geom_text(data=index[country_names %in% sel_country],
            aes(label=country_names, y=log(disp_index)+0.7)) +
  xlab(TeX("$\\hat{p_0}$")) +
  ylab(TeX("$\\log(I_{disp})$"))
  
ggsave("index_plot.pdf", index_plot, units = "cm", height = 6, width = 14)