library("data.table")

#' Agregerer antall dødsfall pr uke
#'
#' @param ged_data data.table
#' @param from_date str: fra dato
#' @param to_date str: til dato
#'
#' @return data.table
ged_aggregate_week <- function(ged_data, from_date, to_date){
  weeks <- round(seq(as.IDate(as.Date(from_date)), as.IDate(as.Date(to_date)), 
                     by="days"), "weeks")
  weeks <- unique(weeks)
  weeks <- CJ(country_names = unique(ged_data$country_names), date=weeks)
  agg_weeks <- data_ged[,.(battledeaths=sum(battledeaths)), 
                        by=.(country_names, date=round(as.IDate(date), "weeks"))]
  agg_weeks <- merge(weeks, agg_weeks, all.x=TRUE)
  agg_weeks <- agg_weeks[is.na(battledeaths), battledeaths := 0]
  return(agg_weeks)
}

#' Agregerer antall dødsfall pr måned
#'
#' @param ged_data 
#' @param from_date 
#' @param to_date 
#'
#' @return data.table
ged_aggregate_month <- function(ged_data, from_date, to_date) {
  month <- round(seq(as.IDate(from_date), as.IDate(to_date), by="days"),
                  "months")
  month <- unique(month)
  month <- CJ(country_names = unique(ged_data[,country_names]), date=month)
  agg_month <- data_ged[,.(battledeaths=sum(battledeaths)),
                       by=.(country_names, date=round(as.IDate(date), "months"))]
  agg_month <- merge(month, agg_month, all.x=TRUE)
  agg_month  <- agg_month[is.na(battledeaths), battledeaths := 0]
  return(agg_month)
}