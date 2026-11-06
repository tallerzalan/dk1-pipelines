# the input groups the forecast uses, the same list live (05) and in the backtest (09d)
forecast_groups <- c("nb", "hol", "wx", "wxde")

# the line between the forecast and the hindsight analysis. stops if the forecast uses a group that isn't
# in inputs.csv as used and published before the auction, or if forecast code reads from the analysis folder.
# run when the dashboard is built, so a mistake shows on the push and never costs a forecast
check_divider <- function(groups = forecast_groups) {
  reg <- read.csv("inputs.csv")
  ok  <- reg$group[reg$status == "used" & reg$before_auction == "yes"]
  bad <- setdiff(groups, ok)
  if (length(bad)) stop("Not in inputs.csv as used before the auction: ", paste(bad, collapse = ", "))
  code <- c(list.files("R", "\\.R$", full.names = TRUE), list.files("scripts", "\\.R$", full.names = TRUE))
  hit  <- Filter(\(f) any(grepl(paste0("analysis", "/"), sub("#.*", "", readLines(f, warn = FALSE)), fixed = TRUE)), code)
  if (length(hit)) stop("Forecast code reads from the analysis folder: ", paste(hit, collapse = ", "))
  reg
}
