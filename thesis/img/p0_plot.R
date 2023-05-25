#!/usr/bin/env Rscript
# script som lager plot over ukentlige dødsfall
CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/img/"
setwd(CWD)

library("data.table")
library("ggplot2")

theme_set(theme_classic())

load("../data/weekly_data.RData")
load("../data/res_data.RData")


# data --------------------------------------------------------------------


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

scoring_rules <- res_data[country_names %in% countries_with_obs,
                          .(RMSE=mean(sqrt(residual^2), na.rm=TRUE),
                            RMNSE=mean(sqrt(std_residual^2), na.rm=TRUE),
                            "log(DSS)"=log(mean(dss, na.rm=TRUE))),
                          by=.(country_names, model)]


score_data <- merge(index, scoring_rules, by="country_names")
score_data <- melt(score_data[,2:7],
                   id=c("model", "p0", "disp_index"),
                   measure.vars=4:6
)
# plot --------------------------------------------------------------------

disp_plot <- ggplot(score_data) +
  geom_point(aes(x=p0, y=value, color=model), size=0.5) +
  facet_wrap(vars(variable),scales = "free_y") +
  theme(strip.background = element_blank(),
        axis.title.y = element_blank())

ggsave("p0_plot.pdf", disp_plot, units="cm", width = 14, height=6)

