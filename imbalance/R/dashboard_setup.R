# shared by index.qmd and backtest.qmd

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(plotly)

Sys.setlocale("LC_TIME", "C")   # english dates

grey <- "#6b7280"
ink  <- "#1f2328"
dir_colors <- c(down = "#2563eb", none = "#cbd5e1", up = "#f97316")
dir_labels <- c(down = "Down-regulation", none = "No regulation", up = "Up-regulation")
model_labels <- c(ens = "Ensemble", mnl = "Multinomial lasso", xgb = "XGBoost",
                  clim = "Benchmark (last 28 days)")
horizon_labels <- c("0-1h" = "0–1 h", "1-2h" = "1–2 h", "2-3h" = "2–3 h", "3-4h" = "3–4 h",
                    "day-ahead" = "Day-ahead")

# all from forecasts/ and results/
read_if <- function(f) if (file.exists(f)) read_csv(f, show_col_types = FALSE) else NULL
# latest forecasts from the logs
intra_all <- read_if("forecasts/intraday_log.csv")
intra  <- if (!is.null(intra_all)) filter(intra_all, issue == max(issue))
outcomes <- read_if("results/outcomes.csv")
dayah  <- read_if("forecasts/dayahead_log.csv")
if (!is.null(dayah)) dayah <- filter(dayah, date == max(date))
live   <- read_if("results/live_scored.csv")
scores <- read_if("results/direction_scores.csv")
tests  <- read_if("results/direction_tests.csv")
calib  <- read_if("results/calibration.csv")
spread <- read_if("results/spread_scores.csv")
sptest <- read_if("results/spread_tests.csv")
info   <- read_if("results/backtest_info.csv")
info_v <- function(n) info$value[info$name == n]

pct   <- function(x, d = 0) paste0(format(round(100 * x, d), nsmall = d), "%")
eur   <- function(x) paste0(if (x >= 0) "+" else "−", format(round(abs(x)), big.mark = ","))
p_txt <- function(p) if (p < 0.001) "p < 0.001" else paste("p =", format(round(p, 3), nsmall = 3))
dk    <- function(t) with_tz(t, "Europe/Copenhagen")

skill_of <- function(h, m = "ens") scores$skill[scores$horizon == h & scores$model == m]
test_of  <- function(h, a = "ens", b = "clim") tests[tests$horizon == h & tests$model_a == a & tests$model_b == b, ]

chart_layout <- function(p, xtitle = "", ytitle = "") {
  p |>
    layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           font = list(family = "Segoe UI, Helvetica Neue, Arial, sans-serif", color = grey),
           xaxis = list(title = xtitle, showgrid = FALSE, zeroline = FALSE, color = grey, linecolor = "#e5ddd3"),
           yaxis = list(title = ytitle, color = grey, gridcolor = "#f1ebe4", zerolinecolor = "#cfc6bb"),
           legend = list(orientation = "h", x = 0, y = -0.18, font = list(color = ink)),
           hovermode = "x unified",
           hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#efe6dc", font = list(color = ink)),
           margin = list(t = 20)) |>
    config(displayModeBar = FALSE)
}

# stacked probabilities per quarter
# quarters in time order, plotly sorts text labels otherwise (00:00 before 23:45)
x_axis <- function(fc, day) {
  if (day) list(type = "date", tickformat = "%H:%M", dtick = 2 * 3600 * 1000)
  else list(type = "category", categoryorder = "array", categoryarray = fc$x)
}

# hh:mm, with the time zone on the hour that comes twice when the clocks go back
quarter_label <- function(time) {
  lab <- format(dk(time), "%H:%M")
  twice <- lab %in% lab[duplicated(lab)]
  lab[twice] <- format(dk(time[twice]), "%H:%M %Z")
  lab
}

prob_chart <- function(fc, xtitle, day = FALSE) {
  fc$x <- if (day) dk(fc$time) else fc$label
  p <- plot_ly(fc, x = ~x)
  for (cl in c("down", "none", "up")) {
    p <- p |> add_bars(y = fc[[cl]], name = dir_labels[[cl]], marker = list(color = dir_colors[[cl]]),
                       hovertemplate = paste0("%{y:.0%}<extra>", dir_labels[[cl]], "</extra>"))
  }
  p |> chart_layout(xtitle) |>
    layout(barmode = "stack", bargap = 0.15, yaxis = list(tickformat = ".0%", range = c(0, 1)),
           xaxis = x_axis(fc, day))
}

# expected spread with the 80% band
spread_chart <- function(fc, xtitle, day = FALSE) {
  fc$x <- if (day) dk(fc$time) else fc$label
  plot_ly(fc, x = ~x) |>
    add_lines(y = ~q10, line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") |>
    add_lines(y = ~q90, line = list(width = 0), fill = "tonexty", fillcolor = "rgba(249,115,22,0.15)",
              name = "80% band", hovertemplate = "%{y:.0f} €/MWh<extra>90%</extra>") |>
    add_lines(y = ~mean, name = "Expected spread", line = list(color = "#f97316", width = 3),
              hovertemplate = "%{y:.1f} €/MWh<extra>Expected</extra>") |>
    chart_layout(xtitle) |>
    layout(xaxis = x_axis(fc, day))
}

# latest intraday forecast (ensemble)
has_intra <- !is.null(intra) && any(intra$model == "ens")
if (has_intra) {
  ie <- intra |> filter(model == "ens") |> arrange(time) |>
    mutate(label = quarter_label(time))
  made <- dk(ie$made_utc[1])
  b1 <- min(ie$bucket)
  h1 <- ie |> filter(bucket == b1) |> summarise(across(c(down, none, up, mean, q10, q90), mean))
  h1_dir <- names(which.max(unlist(h1[, c("down", "none", "up")])))
  ic <- intra |> filter(model == "clim", bucket == b1) |> summarise(across(c(down, none, up), mean))
}

# last 24 hours: the newest forecast made before each quarter, and what happened
has_recent <- FALSE
if (!is.null(intra_all) && !is.null(outcomes)) {
  last_known <- max(outcomes$time)
  dirs <- c("down", "none", "up")
  recent <- intra_all |>
    filter(time > last_known - hours(24), time <= last_known) |>
    group_by(time, model) |>
    slice_min(bucket, n = 1, with_ties = FALSE) |>
    ungroup() |>
    inner_join(select(outcomes, time, dir, spread_actual = spread), by = "time") |>
    mutate(pick = dirs[max.col(cbind(down, none, up), ties.method = "first")],
           happened = dirs[dir + 2], hit = pick == happened)
  re <- recent |> filter(model == "ens") |> arrange(time)
  rc <- recent |> filter(model == "clim")
  has_recent <- nrow(re) > 0
}

# probabilities with what happened as a square on top
happened_colors <- c(down = "#2563eb", none = "#94a3b8", up = "#f97316")
recent_prob_chart <- function(x) {
  prob_chart(x, "", day = TRUE) |>
    add_markers(x = dk(x$time), y = rep(1.07, nrow(x)), text = unname(dir_labels[x$happened]),
                marker = list(symbol = "square", size = 8, color = unname(happened_colors[x$happened])),
                showlegend = FALSE, hovertemplate = "%{text}<extra>What happened</extra>") |>
    layout(yaxis = list(range = c(0, 1.13), tickvals = c(0, 0.25, 0.5, 0.75, 1)))
}

# expected spread and band, with the actual spread as dots
recent_spread_chart <- function(x) {
  spread_chart(x, "", day = TRUE) |>
    add_markers(x = dk(x$time), y = x$spread_actual, name = "What happened",
                marker = list(color = ink, size = 5),
                hovertemplate = "%{y:.0f} €/MWh<extra>Actual</extra>")
}

# latest day-ahead forecast (ensemble)
has_da <- !is.null(dayah)
if (has_da) {
  de <- dayah |> filter(model == "ens") |> arrange(time) |>
    mutate(label = quarter_label(time))
  da_day <- as_date(dayah$date[1])
  da_avg <- de |> summarise(across(c(down, none, up, mean), mean))
  da_dir_counts <- table(factor(c("down", "none", "up")[max.col(as.matrix(de[, c("down", "none", "up")]))],
                                levels = c("down", "none", "up")))
}

# live track record, ensemble vs benchmark
has_live <- !is.null(live) && nrow(live) > 0
if (has_live) {
  live_tab <- live |>
    group_by(horizon) |>
    summarise(quarters = n(), from = min(time), ens = mean(ll_ens), clim = mean(ll_clim), .groups = "drop") |>
    mutate(skill = 1 - ens / clim, h = factor(horizon_labels[horizon], levels = horizon_labels)) |>
    arrange(h)
  live_skill <- function(h) { r <- live_tab[live_tab$horizon == h, ]; if (nrow(r) == 0) "\u2013" else pct(r$skill, 1) }
  live_from  <- format(dk(min(live$time)), "%d %b %Y")
}

# accuracy text
has_eval <- !is.null(scores)
eval_txt <- if (has_eval) {
  s1 <- skill_of("0-1h"); s4 <- skill_of("3-4h"); sd <- skill_of("day-ahead")
  t1 <- test_of("0-1h"); td <- test_of("day-ahead")
  sprintf(paste("Log loss %s lower than the benchmark for the next hour (DM %s), %s three to four hours ahead",
                "and %s for tomorrow (DM %s). Imbalances are the errors in everyone's plans, so most are surprises."),
          pct(s1, 1), p_txt(t1$dm_p), pct(s4, 1), pct(sd, 1), p_txt(td$dm_p))
} else ""
period_txt <- if (!is.null(info)) sprintf("Backtest %s to %s, %s days, all horizons on the same days.",
                                          format(as_date(info_v("from")), "%d %b %Y"),
                                          format(as_date(info_v("to")), "%d %b %Y"), info_v("days")) else ""
has_spread <- !is.null(spread) && "horizon" %in% names(spread)
sp_of <- function(h, m, v = "pinball") spread[[v]][spread$horizon == h & spread$model == m]
sp_p  <- function(h, v) sptest$dm_p[sptest$horizon == h & sptest$metric == v]
eur1  <- function(x) paste0(if (x < 0) "−" else "", format(round(abs(x), 1), nsmall = 1), " €/MWh")
spread_txt <- if (has_spread) {
  sprintf(paste("Spread: pinball loss %s lower for the next hour and %s for tomorrow. Knowing the direction would",
                "make it %s lower, so most of the uncertainty is the direction."),
          pct(sp_of("0-1h", "ens", "skill"), 1), pct(sp_of("day-ahead", "ens", "skill"), 1),
          pct(sp_of("day-ahead", "perfect", "skill"), 0))
} else ""
value_txt <- if (has_spread) {
  sprintf(paste("Picking a side with the expected spread (1 MWh at the imbalance price or at spot) earns %s for",
                "the next hour (DM %s) and %s for tomorrow (DM %s). The benchmark earns %s and %s, knowing the",
                "direction %s. Spot stands in for the intraday price, which isn't public."),
          eur1(sp_of("0-1h", "ens", "value")), p_txt(sp_p("0-1h", "value")),
          eur1(sp_of("day-ahead", "ens", "value")), p_txt(sp_p("day-ahead", "value")),
          eur1(sp_of("0-1h", "clim", "value")), eur1(sp_of("day-ahead", "clim", "value")),
          eur1(sp_of("0-1h", "perfect", "value")))
} else ""

# day-ahead to imbalance positions
da_bt_daily <- read_if("results/da_imbalance_daily.csv")
da_bt_sum   <- read_if("results/da_imbalance_summary.csv")
da_bt_tests <- read_if("results/da_imbalance_tests.csv")
da_bt_info  <- read_if("results/da_imbalance_info.csv")
da_live     <- read_if("results/da_imbalance_live.csv")
da_log      <- read_if("forecasts/da_imbalance_log.csv")
da_labels <- c(ens = "Forecast", clim = "Benchmark forecast", long = "Always long", short = "Always short",
               perfect = "Perfect foresight")
da_colors <- c(ens = "#f97316", clim = "#6b7280", long = "#16a34a", short = "#2563eb")
eur0 <- function(x) paste0(ifelse(x < 0, "−", ""), format(round(abs(x)), big.mark = ","), " €")

# cumulative P&L per strategy from a daily table
cum_chart <- function(daily) {
  few <- nrow(daily) < 20
  p <- plot_ly()
  for (s in names(da_colors)) {
    p <- p |> add_trace(x = as_date(daily$date), y = cumsum(daily[[s]]), name = da_labels[[s]], type = "scatter",
                        mode = if (few) "lines+markers" else "lines",
                        line = list(color = da_colors[[s]], width = if (s == "ens") 3 else 2),
                        marker = if (few) list(color = da_colors[[s]], size = 7) else NULL,
                        hovertemplate = paste0("%{y:,.0f} €<extra>", da_labels[[s]], "</extra>"))
  }
  xa <- list(type = "date", tickformat = "%d %b")
  if (few) xa$dtick <- 86400000
  p |> chart_layout("", "€, 1 MW") |> layout(xaxis = xa)
}

has_da_bt <- !is.null(da_bt_sum)
da_bt_txt <- if (has_da_bt) {
  e  <- da_bt_sum[da_bt_sum$strategy == "ens", ]
  pf <- da_bt_sum[da_bt_sum$strategy == "perfect", ]
  vs <- function(s) eur0(da_bt_tests$diff_day[da_bt_tests$versus == s])
  worth <- da_bt_info$value[da_bt_info$name == "worth_trading"] == "TRUE"
  paste(
    sprintf("Backtest %s to %s, %s days. With 1 MW the forecast's positions earned %s a day after fees (%s in total, %s per MWh traded), %s with Newey-West.",
            format(as_date(da_bt_info$value[da_bt_info$name == "from"]), "%d %b %Y"),
            format(as_date(da_bt_info$value[da_bt_info$name == "to"]), "%d %b %Y"), e$days,
            eur0(e$mean_day), eur0(e$total), sprintf("%.1f €", e$per_mwh), p_txt(e$p)),
    sprintf("It made %s a day more than the benchmark forecast, %s more than always long and %s more than always short, with Holm p = %s.",
            vs("clim"), vs("long"), vs("short"), format(round(min(da_bt_tests$p_holm), 2), nsmall = 2)),
    if (worth) "By the rule set before running, it is worth trading." else
      "By the rule set before running, that is not enough to call it worth trading. It runs live as paper trading anyway, as the real test.",
    sprintf("Knowing the direction in advance would earn %s a day, so nearly all the money is in the direction, which is hard to see the day before.",
            eur0(pf$mean_day)))
} else ""

# summary for the backtest page
short_txt <- if (has_eval && has_spread) {
  sig <- function(p) if (p < 0.05) "significant" else "not significant"
  paste(
    sprintf("The next hour is fairly predictable: log loss %s lower than the benchmark, and picking a side earns %s (DM %s).",
            pct(skill_of("0-1h"), 1), eur1(sp_of("0-1h", "ens", "value")), p_txt(sp_p("0-1h", "value"))),
    sprintf("Tomorrow only a little: log loss %s lower, and picking a side earns %s, which is %s (DM %s).",
            pct(skill_of("day-ahead"), 1), eur1(sp_of("day-ahead", "ens", "value")),
            sig(sp_p("day-ahead", "value")), p_txt(sp_p("day-ahead", "value"))),
    if (has_da_bt) sprintf("Before the auction it is about the same, %s lower log loss, and the day-ahead to imbalance test %s its rule.",
                           pct(as.numeric(da_bt_info$value[da_bt_info$name == "direction_skill"]), 1),
                           if (da_bt_info$value[da_bt_info$name == "worth_trading"] == "TRUE") "passes" else "doesn't pass") else "",
    "So the edge is in the next hour, which is traded on the intraday market. Its prices aren't public, so spot stands in for them here.")
} else ""

has_da_log  <- !is.null(da_log) && nrow(da_log) > 0
if (has_da_log) {
  da_next <- da_log |> filter(model == "ens", date == max(date)) |> arrange(time)
  da_next_day <- as_date(da_next$date[1])
}
has_da_live <- !is.null(da_live) && nrow(da_live) > 0

# banner when the page is more than 3 hours old
writeLines(sprintf('<meta name="built" content="%s">
<style>.stale { position: fixed; left: 0; right: 0; bottom: 0; z-index: 2000; background: #fef2f2; color: #991b1b;
  border-top: 1px solid #fecaca; font-size: 14px; padding: 10px 20px; text-align: center; }</style>
<script>
document.addEventListener("DOMContentLoaded", () => {
  const built = new Date(document.querySelector("meta[name=built]").content);
  if (Date.now() - built > 3 * 3600 * 1000) {
    const el = document.createElement("div");
    el.className = "stale";
    el.textContent = "Not updated since " + built.toLocaleString("en-GB", { timeZone: "Europe/Copenhagen",
      day: "numeric", month: "short", hour: "2-digit", minute: "2-digit" }) + " (Danish time). The hourly update has not run.";
    document.body.appendChild(el);
  }
});
</script>', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), "stale.html")
