# danish holidays, easter from the gregorian algorithm

library(dplyr)
library(lubridate)

easter <- function(y) {
  a <- y %% 19; b <- y %/% 100; c <- y %% 100
  d <- b %/% 4; e <- b %% 4; f <- (b + 8) %/% 25; g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4; k <- c %% 4
  l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451
  month <- (h + l - 7 * m + 114) %/% 31
  day   <- ((h + l - 7 * m + 114) %% 31) + 1
  make_date(y, month, day)
}

dk_holidays <- function(years) {
  bind_rows(lapply(years, \(y) {
    e <- easter(y)
    tibble(date = c(make_date(y, 1, 1), e - 3, e - 2, e, e + 1,
                    if (y < 2024) e + 26,             # store bededag, abolished from 2024
                    e + 39, e + 49, e + 50,
                    make_date(y, 6, 5), make_date(y, 12, 24), make_date(y, 12, 25),
                    make_date(y, 12, 26), make_date(y, 12, 31)))
  })) |>
    distinct() |>
    arrange(date)
}
