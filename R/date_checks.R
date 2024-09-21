#' Checks vector/s from a data set to make sure that that vector/s has date values arranged as ymd
#'
#' @param df_date  vector to be tested to the input file
#' @param date_order date order to be tested.
#' @return tested vector

#' @export
#get only date columns from any df
only_date_cols = function(df_date){
  dates_only <- unlist(lapply(df_date, lubridate::is.Date), use.names = FALSE)
  date_cols = df_date[, dates_only]
  return(date_cols)
}

#warning vs error
#' @export
isdate <- function(df_date, date_order){
  tryCatch(
    expr = {
      message(!is.na(lubridate::parse_date_time(df_date, orders= date_order)))
      message('executed call')
    },
    error = function(err) {
      message('Caught Error!')
      print(err)
    },
    finally = {
      message('Finished, quitting')
    })
}


#' @export
date_stop = function(df_date, date_order){
  stopifnot(
    !is.na(lubridate::parse_date_time(df_date, orders= date_order)
    ), return(df_date)
  )
}

#' @export
date_stop = function(df_date, date_order){

  #get only date columns
  date_cols = df_date%>%only_date_cols()

  #check all date columns for correct date order

  for (date_col in date_cols){
    stopifnot(
      !is.na(lubridate::parse_date_time(date_col, orders= date_order)
      )
      , return(date_cols)
    )
  }
}

#date_stop(ccbh, 'ymd')
