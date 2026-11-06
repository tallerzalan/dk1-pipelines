# exogenous variables per day

library(dplyr)
library(tidyr)
library(lubridate)

# hourly utc to days x 24
to_daily <- function(df, col, dates) {
  d <- df |>
    transmute(local = with_tz(time, "Europe/Copenhagen"), value = .data[[col]]) |>
    mutate(date = as_date(local), hour = hour(local)) |>
    group_by(date, hour) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    mutate(value = if_else(is.nan(value), NA_real_, value)) |>
    pivot_wider(names_from = hour, values_from = value, names_prefix = "h")
  for (h in paste0("h", 0:23)) if (!h %in% names(d)) d[[h]] <- NA_real_
  d <- tibble(date = dates) |>
    left_join(d, by = "date") |>
    select(all_of(paste0("h", 0:23))) |>
    mutate(h2 = if_else(is.na(h2), (h1 + h3) / 2, h2))
  as.matrix(d)
}

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
                    if (y < 2024) e + 26,             # store bededag, until 2023
                    e + 39, e + 49, e + 50,
                    make_date(y, 6, 5), make_date(y, 12, 24), make_date(y, 12, 25),
                    make_date(y, 12, 26), make_date(y, 12, 31)))
  })) |>
    distinct() |>
    arrange(date)
}

# open-meteo, made 48h ahead
# wind via a simple power curve, wx = dk1, wxde = germany
make_weather <- function(dates, weather, prefix = "wx") {
  w <- weather |>
    transmute(time,
              ms    = rowMeans(across(starts_with("wind_"))) / 3.6,   # km/h to m/s
              wpow  = pmin(pmax((ms - 3) / (12 - 3), 0), 1)^3,
              solar = rowMeans(across(starts_with("solar_"))))
  # radiation is the mean of the hour before its timestamp
  s1    <- transmute(w, time = time - hours(1), solar)
  wpow  <- to_daily(w, "wpow", dates);  colnames(wpow)  <- paste0(prefix, "wind_d0_h", 0:23)
  solar <- to_daily(s1, "solar", dates); colnames(solar) <- paste0(prefix, "solar_d0_h", 0:23)
  cbind(wpow, solar)
}

make_exog <- function(dates, neighbours, holidays) {
  areas <- c("DE", "DK2", "NO2", "SE3", "SE4")
  nb <- do.call(cbind, lapply(areas, \(a) {
    m <- lag_days(to_daily(neighbours, paste0("price_", a), dates), 1)
    colnames(m) <- paste0(tolower(a), "_d1_h", 0:23)
    m
  }))
  
  hol <- cbind(hol_d0 = as.numeric(dates %in% holidays$date),
               hol_d1 = as.numeric((dates - 1) %in% holidays$date))
  
  list(nb = nb, hol = hol)
}

# transform on training window
transform_exog <- function(exog, train) {
  block <- function(X, f) {
    if (is.null(X)) return(NULL)
    series <- sub("_h[0-9]+$", "", colnames(X))
    for (s in unique(series)) {
      j <- series == s
      X[, j] <- f(X[, j, drop = FALSE], X[train, j, drop = FALSE])
    }
    X
  }
  list(
    nb    = block(exog$nb, \(x, tr) {
      s <- mad(tr, na.rm = TRUE); if (!is.finite(s) || s == 0) s <- sd(tr, na.rm = TRUE)
      asinh((x - median(tr, na.rm = TRUE)) / s)
    }),
    hol   = exog$hol,
    wx    = block(exog$wx, \(x, tr) (x - mean(tr, na.rm = TRUE)) / sd(tr, na.rm = TRUE)),
    wxde  = block(exog$wxde, \(x, tr) (x - mean(tr, na.rm = TRUE)) / sd(tr, na.rm = TRUE))
  )
}