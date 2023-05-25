#!/usr/bin/env Rscript
# Script for å generere måndelige data

CWD <- "~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/data/"

setwd(CWD)

source("aggregate_func.R")
source("data.R")

from_date <- "1989-01-01"
to_date <- "2021-01-01"

monthly_data <- ged_aggregate_month(data_ged,
                                  from_date = from_date,
                                  to_date = to_date)

save(monthly_data, file="monthly_data.RData")
