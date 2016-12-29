serial.joiner <- function(fun = inner_join, list_of_df) {
  new_fun <- fun
    huge_table <- Reduce(function(...) new_fun(...),
                         list_of_df)
}
