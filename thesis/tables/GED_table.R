#!/usr/bin/env Rscript
library("data.table")

setwd("~/Dokumenter/MAMI/STK_MAT2011_prosjekt/thesis/tables/")

GED_data <- fread("../data/GEDEvent_v22_1.csv")

select_data <- GED_data[c(1:3),.(date_start, country, date_prec, type_of_violence,best, high, low)]

lt_table <- kableExtra::kbl(select_data, format="latex", booktabs = TRUE)

dot_row <- paste(rep("$\\vdots$", 7), collapse = " & ")
dot_row <- paste(dot_row, "\\\\ \n \\bottomrule", collapse=" ")
lt_table <- sub("\\bottomrule", dot_row, lt_table, fixed = TRUE)

cat(lt_table, file = "GED_table.tex")

