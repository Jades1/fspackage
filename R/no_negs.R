#' Checks that no negative values exist in data set
#'
#' @param latest_df Any data frame
#' @returns A data frame.
#'
#' @export
no_negs = function(latest_df){
  #get only numeric cols
  nums_only <- unlist(lapply(latest_df, is.numeric), use.names = FALSE)

  numbers = latest_df[, nums_only]

  stopifnot(any(numbers)>0,
            return(latest_df))

}

