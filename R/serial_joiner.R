# this function join multiple dfs
# example:
# df <- insensitive.join(left_join, list(df_x,df_y,df_z))

serial.joiner <- function(fun = inner_join, list_of_df) {
  new_fun <- fun
    huge_table <- Reduce(function(...) new_fun(...),
                         list_of_df)
    return(huge_table)
}
