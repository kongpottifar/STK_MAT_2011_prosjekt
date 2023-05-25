#!/usr/bin/env Rscript

CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/data/"

setwd(CWD)

library("data.table")

load("./weekly_data.RData")

simple_model <- weekly_data[,.(battledeaths, yhat=shift(battledeaths, n=1)), 
                            by=country_names][
                              ,residual:=battledeaths - yhat
                            ][
                              ,RSE := abs(residual)
                            ][
                              ,RSEN := RSE / sqrt(sum(RSE^2, na.rm=TRUE)/(.N-2)),
                              by=country_names
                            ][
                              ,dss := RSEN^2 + 2 * log(sqrt(sum(RSE^2, na.rm=TRUE)/(.N-2))),
                              by=country_names
                            ][,
                              .(MRSE=mean(RSE, na.rm=TRUE),
                                MRSEN=mean(RSEN, na.rm=TRUE),
                                dss=mean(dss, na.rm=TRUE)),
                              by=country_names
                              ]
