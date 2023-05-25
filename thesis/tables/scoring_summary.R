#!/usr/bin/env Rscript
CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/tables/"
setwd(CWD)

library("data.table")

load("../data/res_data.RData")
load("../data/weekly_data.RData")


countries_with_obs <- weekly_data[,
                                  .(uker_med_hendelser=sum(battledeaths > 0, na.rm=TRUE)),
                                  by=country_names][
                                    uker_med_hendelser > 100,
                                    country_names
                                  ]
countries <- c("Afghanistan", "Chad", "Alle land")

simple_model <- weekly_data[country_names %in% countries_with_obs,
                            .(battledeaths, yhat=shift(battledeaths, n=1)), 
                            by=country_names][
                              ,residual:=battledeaths - yhat
                            ][
                              ,RSE := abs(residual)
                            ][
                              ,RNSE := RSE / sqrt(sum(RSE^2, na.rm=TRUE)/(.N-2)),
                              by=country_names
                            ][
                              ,dss := RNSE^2 + 2 * log(sqrt(sum(RSE^2, na.rm=TRUE)/(.N-2))),
                              by=country_names
                            ][,
                              .(model="NC",
                                RMSE=mean(RSE, na.rm=TRUE),
                                RMNSE=mean(RNSE, na.rm=TRUE),
                                DSS=mean(dss, na.rm=TRUE),
                                RPS=NA),
                              by=country_names
                            ]

scoring_summary <- res_data[country_names %in% countries_with_obs,
                              .(RMSE=mean(abs(residual), na.rm=TRUE),
                              RMNSE=mean(abs(std_residual), na.rm=TRUE),
                              DSS=mean(dss, na.rm=TRUE),
                              RPS=mean(RPS, na.rm=TRUE)),
                            by=.(country_names, model)][
                              order(country_names, model)
                            ]

scoring_summary <- rbind(scoring_summary, simple_model)

scoring_total <- scoring_summary[
  , .(country_names = "Alle land",
      RMSE=mean(RMSE, na.rm=TRUE),
      RMNSE=mean(RMNSE, na.rm=TRUE),
      DSS=mean(DSS, na.rm=TRUE),
      RPS=mean(RPS, na.rm=TRUE)),
  by=.(model)
][order(country_names, model)]

scoring_summary <- rbind(scoring_summary,scoring_total)

names(scoring_summary)[1:2] <- c("Land", "Modell")

out_table <- kableExtra::kbl(scoring_summary[Land %in% countries][order(Land, Modell)],
                          format="latex",
                          booktabs=TRUE,
                          linesep=c("", "","\\addlinespace"),
                          digits=2)

# out_table <- kableExtra::collapse_rows(out_table, 
#                                    columns=1,
#                                    latex_hline = "none",
#                                    headers_to_remove = "country_names")

out_table <- gsub("(NaN|NA)", "-", out_table)

cat(out_table, file="scoring_summary.tex")