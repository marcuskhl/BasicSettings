rbind.fill.DT <- function(ll) {
  # changed sapply to lapply to return a list always
  all.names <- lapply(ll, names)
  unq.names <- unique(unlist(all.names))
  ll.m <- rbindlist(lapply(seq_along(ll), function(x) {
    tt <- ll[[x]]
    setattr(tt, 'class', c('data.table', 'data.frame'))
    data.table:::settruelength(tt, 0L)
    invisible(alloc.col(tt))
    tt[, c(unq.names[!unq.names %chin% all.names[[x]]]) := NA_character_]
    setcolorder(tt, unq.names)
  }))
}