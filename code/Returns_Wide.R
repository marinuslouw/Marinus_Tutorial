Returns_Wide <- function(data) {
  data_adj <-
  data %>%
    arrange(Date) %>%
    mutate_at(.vars = vars(-Date), .funs = funs(./lag(.) - 1))

  data_adj
}
