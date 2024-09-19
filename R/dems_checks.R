
#' @export
dems_check = function(dems_df){

  dems_check = dems_df%>%
    mutate(total = Female + Male + Other_Gender,
           compare = ifelse(
             Total == total, 'Good', 'NA'
           )
    )

  stopifnot(dems_check$Total==dems_check$total | dems_check$Min_Age >= 0 | dems_check$Max_Age < 26,
            return(dems_check))
}

