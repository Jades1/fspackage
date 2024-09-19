#Imports:
# dplyr

#' @export
threem_greater_sec = function(df_1, df_2){
  joined = inner_join(df_1, df_2)

  for (col in 1:ncol(joined)){
    colnames(joined)[col] <-  sub("^.*\\.", "", colnames(joined)[col])
  }

  joined = joined%>%
    mutate(total = On_Time + `126-200_days` + `201-365_days` + more_than_365_days,
           compare = ifelse(
             Matched == total, 'Good', 'NA'
           )
    )

  stopifnot(
    joined$Matched==joined$On_Time + joined$`126-200_days` + joined$`201-365_days` + joined$more_than_365_days,
    return(joined)
  )
}

