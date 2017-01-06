#' this function join multiple dfs using dplyr
#' @param fun types of join you wish to perform
#' @param list_of_df a number of dfs you require to join
#' @examples

#' df <- insensitive.join(left_join, list(df_x,df_y,df_z))
#' @export
serial.joiner <- function(fun = inner_join, list_of_df) {
  new_fun <- fun
    huge_table <- Reduce(function(...) new_fun(...),
                         list_of_df)
    return(huge_table)
}

