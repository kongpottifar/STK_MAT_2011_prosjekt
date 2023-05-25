#!/usr/bin/env Rscript
# script som lager plot over ukentlige dødsfall
CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/img/"
setwd(CWD)

load("../data/weekly_data.RData")

library("data.table")
library("ggplot2")
theme_set(theme_classic())

countries <- c("Afghanistan", "Chad")
country_means <- weekly_data[country_names %in% countries, 
                             .(mean_bd=mean(battledeaths)),
                             by=country_names]

weekly_plot <- ggplot(weekly_data[country_names %in% countries],
                      aes(x=date, y=battledeaths)) +
        geom_abline(slope=0, intercept = 0, colour="lightgrey", linewidth=0.3)+
        geom_step(linewidth=0.3) +
        geom_abline(data = country_means, aes(intercept=mean_bd, slope=04), 
                    colour="salmon",
                    linewidth=0.3) +
        facet_wrap(vars(country_names), ncol=1, scales="free_y") +
        theme(strip.background=element_blank(),
              axis.line.x = element_blank()) +
        labs(x="Dato", y="Antall dødsfall pr uke")

ggsave(filename = "weekly_deaths.pdf",
       weekly_plot,
       width = 14,
       height=10,
       units = "cm")
