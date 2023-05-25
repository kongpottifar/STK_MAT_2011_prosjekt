# 1) UCDP 
#   https://ucdp.uu.se/ 
# 2) From https://ucdp.uu.se/downloads/ download: 
#   a) UCDP Georeferenced Event Dataset (GED) Global version 22.1 (csv)
#   b) UCDP/PRIO Armed Conflict Dataset version 22.1
# 3) Read about data.table package:
#   a) https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
#   b) https://www.datacamp.com/cheat-sheet/the-datatable-r-package-cheat-sheet
# 4) To run the script below, correct path for datasets in fread(...) below.
# 5) Run code. 
# 6) Make a table for the number of events for each country (use top 5 as table for oblig?)
# 7) Plot all events for "Chad", with date on x-axis and "battledeaths" on y-axis, make sure that 
#    date is plotted correctly (consider to use https://ggplot2.tidyverse.org/ for plotting).
# 8) Make a script that for a given country aggregates data to a daily, weekly or monthly resolution, i.e. 
#    make a data set where "battledeaths" is now sum of all events for that day, week or month.
#    Hint: Make a data table/set with one row for each day/week/month from 1989-01-01 to 2021-12-31 
#          and for each row in that table, find the correct events (in the large data table) and 
#          sum the number of events for a given country in that day/week/month (there should be 
#          many rows with zero battle deaths). 
# 9) Make a plot of the aggregated number of battle deaths for each week for "Chad", with first date 
#    of the week on the x-axis and number of battledeaths on y-axis.


# Delete old data

library(data.table)




# Data ---------------------------------------------------------------------------------------------

# GED
# Note, correct path below 
data_ged <- fread(file = "GEDEvent_v22_1.csv", sep = ',', header = TRUE)
data_ged <- data_ged[, list(year              = year, 
                            country_names     = country, 
                            country_ids       = country_id, 
                            date              = as.Date(date_start),
                            date_start        = as.Date(date_start), 
                            date_end          = as.Date(date_end), 
                            conflict_id       = conflict_new_id, 
                            conflict_name     = conflict_name, 
                            type_of_violence  = type_of_violence, 
                            date_precision    = date_prec,
                            battledeaths_low  = low, 
                            battledeaths_best = best,
                            battledeaths_high = high)]

data_ged[, battledeaths := battledeaths_best]
data_ged[battledeaths == 0, battledeaths := floor((battledeaths_low + battledeaths_high)/2)]

# ACD
data_acd <- fread(file = "ucdp-prio-acd-221.csv", sep = ',', header = TRUE)
data_acd <- data_acd[, list(conflict_id, type_of_conflict)]
data_acd_sub <- data_acd[type_of_conflict %in% c(3, 4)]


# Select events of type of violence == 1 and type of conflict == 3 or 4
data_ged <- data_ged[type_of_violence == 1][conflict_id %in% unique(data_acd$conflict_id)]
setkey(data_ged, country_ids, year)
# --------------------------------------------------------------------------------------------------








