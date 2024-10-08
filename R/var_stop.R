#' Compare values in one variable between two data sets
#'
#' @param df_1 Current data set.
#' @param df_2 Previous data set.
#' @param var_name Variable name.
#' @returns data set.
#' @export
var_stop = function(df_1, df_2, var_name){
  substring = "Q1"
  if (grepl(substring, var_stop_path))
  {
    # Q1 present
    print("This is quarter 1.")

    # if Q1 not present in string, then execute
  } else {
    stopifnot(
      df_1[[var_name]] >= df_2[[var_name]],
      return(df_1)
    )
  }
}

var_check = function(df_events, data_2, var_name){
  df_new = data.frame(current_q_var = df_events[[var_name]], prev_q_var = data_2[[var_name]])
  df_new = df_new%>%
    mutate(
      compare = ifelse(current_q_var >= prev_q_var, "Good", 'NA')
    )
  return(df_new)
}
