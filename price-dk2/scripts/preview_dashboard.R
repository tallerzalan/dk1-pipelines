

update_prices <- TRUE   # FALSE = only render

if (!file.exists("index.qmd")) {
  stop("Open the dk1-price-forecasting project in RStudio first (the .Rproj file).")
}
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# find quarto
quarto_bin <- Sys.which("quarto")
if (!nzchar(quarto_bin)) {
  bundled    <- file.path(dirname(Sys.getenv("RSTUDIO_PANDOC")), c("quarto.exe", "quarto"))
  quarto_bin <- bundled[file.exists(bundled)][1]
}
if (is.na(quarto_bin) || !nzchar(quarto_bin)) {
  stop("Quarto not found. Install it from https://quarto.org or run this from RStudio.")
}

# back up forecast file
file_fc  <- "forecasts/forecasts_live.csv"
had_file <- file.exists(file_fc)
backup   <- tempfile(fileext = ".csv")
if (had_file) file.copy(file_fc, backup)

Sys.setenv(DK1_PREVIEW = "true")   # allow forecast after noon

tryCatch({
  steps <- c(if (update_prices) c("01_download.R", "02_clean.R"), "05_forecast_tomorrow.R")
  for (s in steps) {
    message("\n--- Running ", s, " ---")
    source(file.path("scripts", s), local = new.env())
  }

  message("\n--- Rendering index.qmd ---")
  status <- system2(quarto_bin, c("render", "index.qmd"))
  if (status != 0) stop("Quarto render failed, see the messages above.")

  browseURL(normalizePath("index.html"))
  message("\nDone: the dashboard is open in your browser.")
}, finally = {
  Sys.unsetenv("DK1_PREVIEW")
  # restore forecast file
  if (had_file) {
    file.copy(backup, file_fc, overwrite = TRUE)
  } else if (file.exists(file_fc)) {
    file.remove(file_fc)
  }
})
