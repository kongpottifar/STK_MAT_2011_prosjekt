#!/usr/bin/env Rscript
# Script for å generere ukentlige data

CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/data/"

setwd(CWD)

source("aggregate_func.R")
source("data.R")

from_date <- "1989-01-01"
to_date <- "2021-01-01"

weekly_data <- ged_aggregate_week(data_ged,
                                  from_date = from_date,
                                  to_date = to_date)

save(weekly_data, file="weekly_data.RData")
